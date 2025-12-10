# --- analyze_backbone_scales_plots.R -----------------------------------------
# Creates overall histograms, per-project overlay density plots, group variants,
# AND per-item violin plots to spot oddities.

# ===== Pre-flight =====
rm(list = ls(all.names = TRUE)); invisible(gc())
try(cat("\014"), silent = TRUE)

# ===== Setup =====
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readr","jsonlite","tibble","dplyr","tidyr","purrr","stringr","ggplot2",
  "fs","glue","janitor","forcats","rprojroot","readxl"
))

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
  root <- tryCatch(rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root),
                   error = function(e) NA)
  if (!is.na(root)) return(normalizePath(root, winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}

# ===== CONFIG =====
ROOT <- project_root()
DIR_INFO           <- fs::path(ROOT, "information")
DIR_QUESTIONNAIRES <- fs::path(ROOT, "01_project_data", "all_projects_backbone", "questionnaires")
DIR_EXPORT         <- fs::path(ROOT, "02_cleaned")
DIR_KEYS           <- fs::path(DIR_EXPORT, "keys")
DIR_FUNCTIONS      <- fs::path(ROOT, "functions")
DIR_LOGS           <- fs::path(ROOT, "logs")

SAMPLE              <- "adults"
DELIM               <- ";"
MIN_PROP_ITEMS      <- 0.50
MAKE_GROUP_VARIANTS <- TRUE

# ===== Source helpers used by plots =====
source(file.path(DIR_FUNCTIONS, "plot_density_by_project.R"))
source(file.path(DIR_FUNCTIONS, "write_excel_friendly_csv.R"))
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))
source(file.path(DIR_FUNCTIONS, "extract_date_string.R"))
source(file.path(DIR_FUNCTIONS, "latest_questionnaire_for_sample.R"))
source(file.path(DIR_FUNCTIONS, "safe_var.R"))
source(file.path(DIR_FUNCTIONS, "setup_logging.R"))

# ===== Input / Output paths =====
master_csv <- fs::path(DIR_EXPORT, SAMPLE, glue::glue("{SAMPLE}_clean_master.csv"))
keys_json  <- fs::path(DIR_KEYS, glue::glue("{SAMPLE}_keys.json"))
stopifnot(fs::file_exists(master_csv), fs::file_exists(keys_json))

latest_scoring <- function(info_dir = DIR_INFO) {
  files <- fs::dir_ls(info_dir, glob = "*_Scoring.xlsx", fail = FALSE)
  if (!length(files)) return(NA_character_)
  dt <- stringr::str_match(basename(files), "^(\\d{4}-\\d{2}-\\d{2})_")[,2]
  files[order(as.Date(dt), decreasing = TRUE)][1]
}
q_file   <- latest_questionnaire_for_sample(SAMPLE, DIR_QUESTIONNAIRES)
date_str <- if (!is.na(q_file)) extract_date_string(q_file) else format(Sys.Date(), "%Y-%m-%d")

OUT_BASE  <- fs::path(ROOT, "out", "internal_data_analysis",
                      "distribution_of_backbone_scores_and_internal_consistency",
                      paste0(date_str, "_", SAMPLE))
DIR_PLOTS <- fs::path(OUT_BASE, "plots")
if (fs::dir_exists(DIR_PLOTS)) fs::dir_delete(DIR_PLOTS)
fs::dir_create(DIR_PLOTS)
DIR_VIOLIN <- fs::path(DIR_PLOTS, "per_item_violins")
fs::dir_create(DIR_VIOLIN)
fs::dir_create(DIR_LOGS)

# Logger
log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_data_analysis_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

# ===== Palette =====
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3","p5"="#E7298A",
  "p6 children"="#66A61E","p7"="#E6AB02","p8 children"="#000000",
  "p8 adults"="#A6761D","p9"="#7A7A7A"
)

# ===== Read master CSV =====
first_line <- readr::read_lines(master_csv, n_max = 1)
delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
  sub("^sep=", "", first_line, ignore.case = TRUE)
} else DELIM
skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L

df <- suppressMessages(
  readr::read_delim(
    master_csv, delim = delim, skip = skip_n, show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
) %>% janitor::clean_names()

# normalize groups / project col
if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  df$group <- factor(df$group, levels = c("HC","patient"))
}
proj_col <- get_project_col(df)

# ===== Column map and keys =====
col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id(names(df)))
keys_raw <- jsonlite::read_json(keys_json, simplifyVector = FALSE)

items_by_scale <- tibble::tibble(
  scale = purrr::map_chr(keys_raw$items_by_scale, ~ .x$scale %||% NA_character_),
  items = purrr::map(keys_raw$items_by_scale, ~ as.character(.x$items %||% character(0)))
)
items_by_subscale <- tibble::tibble(
  scale    = purrr::map_chr(keys_raw$items_by_subscale, ~ .x$scale %||% NA_character_),
  subscale = purrr::map_chr(keys_raw$items_by_subscale, ~ .x$subscale %||% NA_character_),
  items    = purrr::map(keys_raw$items_by_subscale, ~ as.character(.x$items %||% character(0)))
)

# ===== scoring helpers just for plots (unchanged from your script, trimmed) ===
# (Keep score_mean / score_sum and special scorers you need for histograms/densities)
score_sum <- function(d, items, min_prop = MIN_PROP_ITEMS, scale_label=NULL, sub_label=NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  orig_cols  <- col_map$orig[match(keep_norm, col_map$item_norm)]
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  n_nonmiss <- rowSums(!is.na(as.matrix(M))); thresh <- ceiling(min_prop * ncol(M))
  out <- rowSums(M, na.rm = TRUE); out[n_nonmiss < thresh] <- NA_real_; out
}
score_mean <- function(d, items, min_prop = MIN_PROP_ITEMS, scale_label=NULL, sub_label=NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  orig_cols  <- col_map$orig[match(keep_norm, col_map$item_norm)]
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  n_nonmiss <- rowSums(!is.na(as.matrix(M))); thresh <- ceiling(min_prop * ncol(M))
  out <- rowMeans(M, na.rm = TRUE); out[n_nonmiss < thresh] <- NA_real_; out
}
# (add back any other special scorers you rely on in your plots if needed)

# ===== Helpers reused from your plotting code (trimmed to needed ones) =====
.bins_from_values <- function(x, max_bins = 15L) {
  n_vals <- length(unique(x[is.finite(x)]))
  bins <- min(max_bins, max(1L, n_vals)); as.integer(bins)
}
save_hist <- function(vec, out_png, title, xlab, mode_text = NA, vlines = NULL, xlim = NULL) {
  d <- tibble::tibble(x = as.numeric(vec)) %>% dplyr::filter(is.finite(x))
  if (!nrow(d)) return(invisible(FALSE))
  subtitle <- if (!is.na(mode_text)) glue::glue("Scoring: {mode_text}") else NULL
  rng_emp <- range(d$x, na.rm = TRUE); m_emp <- mean(d$x, na.rm = TRUE); n_emp <- nrow(d)
  cap <- paste0(
    "Empirical range: [", signif(rng_emp[1],4), ", ", signif(rng_emp[2],4), "]",
    " | Mean: ", signif(m_emp,4),
    " | N: ", n_emp
  )
  bins_use <- .bins_from_values(d$x, max_bins = 15L)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = bins_use) +
    { if (!is.null(vlines)) ggplot2::geom_vline(xintercept = vlines, linetype = "dotted") } +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = "Count", caption = cap) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    { if (!is.null(xlim) && all(is.finite(xlim))) ggplot2::coord_cartesian(xlim = xlim) }
  ggplot2::ggsave(out_png, p, width = 7.5, height = 5.2, dpi = 150)
  TRUE
}

get_project_col_safe <- function() proj_col

# ======= NEW: per-item violin plots ==========================================
# Creates:
#   - One overall violin per item
#   - If project column exists: a faceted per-project violin (optional heavy)
#   - If group exists: side-by-side violins split by group to eyeball HC/patient oddities
violin_per_item <- function(d, item_col, item_label,
                            by_project = TRUE, by_group = TRUE,
                            width = 7.5, height = 5.2) {
  v <- suppressWarnings(as.numeric(d[[item_col]]))
  dat <- tibble::tibble(value = v)
  dat <- dat[is.finite(dat$value), , drop = FALSE]
  if (!nrow(dat)) return(invisible(FALSE))
  
  # Add splits if present
  if (!is.null(proj_col) && by_project) dat$project <- as.factor(d[[proj_col]][is.finite(v)])
  if ("group" %in% names(d) && by_group) dat$group <- droplevels(as.factor(d$group[is.finite(v)]))
  
  base_title <- paste0("Item violin — ", item_label)
  
  # overall + (optional) group dodge
  p <- ggplot2::ggplot(dat,
                       ggplot2::aes(x = if ("group" %in% names(dat)) group else factor("All"),
                                    y = value)) +
    ggplot2::geom_violin(trim = FALSE, scale = "width") +
    ggplot2::geom_jitter(width = 0.08, height = 0, alpha = 0.25, size = 0.8) +
    ggplot2::labs(title = base_title,
                  x = if ("group" %in% names(dat)) "Group" else NULL,
                  y = "Score / response") +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(fs::path(DIR_VIOLIN, paste0("violin__overall__", safe_var(item_label), ".png")),
                  p, width = width, height = height, dpi = 150)
  
  # per-project facet (if exists)
  if (!is.null(proj_col) && by_project) {
    p2 <- p + { if ("project" %in% names(dat)) ggplot2::facet_wrap(~project, scales = "free_x") }
    ggplot2::ggsave(fs::path(DIR_VIOLIN, paste0("violin__by_project__", safe_var(item_label), ".png")),
                    p2, width = width*1.2, height = height*1.2, dpi = 150)
  }
  TRUE
}

# ===== Build list of items we’ll make violins for ============================
all_items_from_keys <- unique(c(
  unlist(items_by_scale$items, use.names = FALSE),
  unlist(items_by_subscale$items, use.names = FALSE)
))
# Map to actual column names in df
item_norm <- normalize_id(all_items_from_keys)
keep_norm <- intersect(item_norm, normalize_id(names(df)))
if (!length(keep_norm)) {
  warning("No matching item columns found for per-item violins.")
}
col_map_items <- tibble::tibble(
  item_norm = keep_norm,
  orig = names(df)[match(keep_norm, normalize_id(names(df)))]
)

# ======== Histograms + densities (as in your original) =======================
# (You can paste the rest of your score_and_plot_one() + loops here if you want
#  to keep those figures; omitted for brevity. This file is where they belong.)

# ======== Run per-item violins ===============================================
message("== Per-item violins ==")
for (k in seq_len(nrow(col_map_items))) {
  item_col   <- col_map_items$orig[k]
  item_label <- col_map_items$item_norm[k]
  ok <- try(violin_per_item(df, item_col, item_label,
                            by_project = TRUE, by_group = TRUE),
            silent = TRUE)
  if (inherits(ok, "try-error")) {
    logger$write(glue::glue("⚠️ Violin failed for item {item_label} ({item_col})"))
  } else {
    message("  • ", item_label)
  }
}

message("Per-item violins written to: ", DIR_VIOLIN)
message("Log file: ", log_path)
try(logger$close(), silent = TRUE)
