# --- analyze_backbone_violin_only_items_plus_scores.R ------------------------
# Violin plots only:
#   - MAIN: per-item violins (item columns in the input)
#   - BONUS: per-scale violins for existing score_* columns (already computed in input)
# No scoring, no histograms, no density helpers.

# ===== Pre-flight =====
rm(list = ls(all.names = TRUE)); invisible(gc())
try(cat("\014"), silent = TRUE)

# ===== Packages =====
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c("readr","tibble","dplyr","ggplot2","fs","glue","stringr","rprojroot"))

# ===== Root helpers =====
script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

project_root <- function() {
  env <- Sys.getenv("BACKBONE_ROOT", unset = NA)
  if (!is.na(env) && dir.exists(env)) return(normalizePath(env, winslash = "/"))
  sd <- script_dir(); if (dir.exists(sd)) return(sd)
  root <- tryCatch(
    rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root),
    error = function(e) NA
  )
  if (!is.na(root)) return(normalizePath(root, winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}

safe_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "var" else x
}

as_num <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

# ===== CONFIG =====
ROOT   <- project_root()
SAMPLE <- "adults"

# Input (your new location)
master_csv <- fs::path(ROOT, "02_cleaned", SAMPLE, glue::glue("{SAMPLE}_clean_master.csv"))
stopifnot(fs::file_exists(master_csv))

# What to plot
PLOT_ITEMS  <- TRUE   # MAIN
PLOT_SCORES <- TRUE   # BONUS: plots existing score_* columns too

# Split/facet behavior
BY_GROUP   <- TRUE
BY_PROJECT <- TRUE

# Item detection:
# Most questionnaire items in your file look like "ASRS[001]" / "OBQ[01]" etc.
ITEM_REGEX <- "\\[[0-9]+\\]$"   # ends with [digits]

# Output
date_str   <- format(Sys.Date(), "%Y-%m-%d")
OUT_BASE   <- fs::path(ROOT, "out", "internal_data_analysis", "violin_plots_only",
                       paste0(date_str, "_", SAMPLE))
DIR_PLOTS  <- fs::path(OUT_BASE, "plots")
DIR_ITEMS  <- fs::path(DIR_PLOTS, "per_item_violins")
DIR_SCORES <- fs::path(DIR_PLOTS, "per_scale_violins")
DIR_LOGS   <- fs::path(ROOT, "logs")

if (fs::dir_exists(DIR_PLOTS)) fs::dir_delete(DIR_PLOTS)
fs::dir_create(DIR_ITEMS,  recurse = TRUE)
fs::dir_create(DIR_SCORES, recurse = TRUE)
fs::dir_create(DIR_LOGS,   recurse = TRUE)

log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_violin_only_log.txt"))
log_write <- function(txt) {
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0(stamp, " ", txt, "\n"), file = log_path, append = TRUE)
}

log_write(paste0("Input: ", master_csv))

# ===== Read CSV (handles optional 'sep=;' first line) =====
first_line <- readr::read_lines(master_csv, n_max = 1)
delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
  sub("^sep=", "", first_line, ignore.case = TRUE)
} else ";"
skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L

df <- suppressMessages(
  readr::read_delim(
    master_csv, delim = delim, skip = skip_n,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
)

# ===== Normalize group/project columns =====
if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  # keep a stable order if those labels exist
  if (all(c("HC","patient") %in% unique(df$group))) {
    df$group <- factor(df$group, levels = c("HC","patient"))
  } else {
    df$group <- factor(df$group)
  }
}

proj_col <- if ("project" %in% names(df)) "project" else NA_character_

# ===== Column selection =====
item_cols <- character(0)
score_cols <- character(0)

if (PLOT_ITEMS) {
  item_cols <- names(df)[stringr::str_detect(names(df), ITEM_REGEX)]
}

if (PLOT_SCORES) {
  score_cols <- names(df)[stringr::str_detect(names(df), stringr::regex("^score_", ignore_case = TRUE))]
}

item_cols  <- unique(item_cols)
score_cols <- unique(score_cols)

log_write(paste0("Item columns:  ", length(item_cols)))
log_write(paste0("Score columns: ", length(score_cols)))

if (PLOT_ITEMS && !length(item_cols))  log_write("⚠️ No item columns matched ITEM_REGEX.")
if (PLOT_SCORES && !length(score_cols)) log_write("⚠️ No score_* columns found.")

# ===== Plot function =====
save_violin <- function(d, col, out_dir, by_group = TRUE, by_project = TRUE,
                        width = 7.5, height = 5.2) {
  
  v <- as_num(d[[col]])
  keep <- is.finite(v)
  if (!any(keep)) return(invisible(FALSE))
  
  dat <- tibble::tibble(value = v[keep])
  
  if (by_group && "group" %in% names(d)) {
    dat$group <- droplevels(as.factor(d$group[keep]))
  }
  if (by_project && !is.na(proj_col) && proj_col %in% names(d)) {
    dat$project <- droplevels(as.factor(d[[proj_col]][keep]))
  }
  
  dat$x <- if ("group" %in% names(dat)) dat$group else factor("All")
  
  n_ok <- nrow(dat)
  rng  <- range(dat$value, na.rm = TRUE)
  cap  <- paste0("N=", n_ok, " | Range: [", signif(rng[1],4), ", ", signif(rng[2],4), "]")
  
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = value)) +
    ggplot2::geom_violin(trim = FALSE, scale = "width") +
    ggplot2::geom_jitter(width = 0.08, height = 0, alpha = 0.25, size = 0.8) +
    ggplot2::labs(
      title = paste0("Violin — ", col),
      x = if ("group" %in% names(dat)) "Group" else NULL,
      y = "Value",
      caption = cap
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  
  base <- safe_filename(col)
  
  ggplot2::ggsave(fs::path(out_dir, paste0("violin__overall__", base, ".png")),
                  p, width = width, height = height, dpi = 150)
  
  if (by_project && "project" %in% names(dat)) {
    p2 <- p + ggplot2::facet_wrap(~project, scales = "free_x")
    ggplot2::ggsave(fs::path(out_dir, paste0("violin__by_project__", base, ".png")),
                    p2, width = width * 1.2, height = height * 1.2, dpi = 150)
  }
  
  TRUE
}

# ===== Run =====
message("== Violin plots ==")

if (PLOT_ITEMS && length(item_cols)) {
  message("Items: ", length(item_cols))
  for (col in item_cols) {
    ok <- try(save_violin(df, col, DIR_ITEMS, by_group = BY_GROUP, by_project = BY_PROJECT), silent = TRUE)
    if (inherits(ok, "try-error") || identical(ok, FALSE)) {
      log_write(paste0("⚠️ Item violin failed: ", col))
    }
  }
}

if (PLOT_SCORES && length(score_cols)) {
  message("Scores: ", length(score_cols))
  for (col in score_cols) {
    ok <- try(save_violin(df, col, DIR_SCORES, by_group = BY_GROUP, by_project = BY_PROJECT), silent = TRUE)
    if (inherits(ok, "try-error") || identical(ok, FALSE)) {
      log_write(paste0("⚠️ Score violin failed: ", col))
    }
  }
}

message("Per-item violins:  ", DIR_ITEMS)
message("Per-scale violins: ", DIR_SCORES)
message("Log file:          ", log_path)
log_write("Done.")
