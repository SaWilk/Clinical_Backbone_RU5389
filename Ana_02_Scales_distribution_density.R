# --- analyze_backbone_scores_hist_density_from_existing_scores.R -------------
# Plots only (NO SCORING):
# - Uses pre-scored columns already present in the master CSV:
#     score_<scale> and score_<scale>__<subscale>
# - Produces, for each score column:
#     (1) overall histogram per group (HC / patient)
#     (2) density overlays by project within each group (if project column exists)
# - No keys JSON, no scoring workbook, no item-level scoring.

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
  "readr","tibble","dplyr","stringr","ggplot2","fs","glue","forcats","rprojroot"
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
ROOT   <- project_root()
SAMPLE <- "adults"
DELIM  <- ";"          # German Excel
MAKE_GROUP_SPLITS <- TRUE

# ===== Directories =====
DIR_EXPORT    <- fs::path(ROOT, "02_cleaned")
DIR_FUNCTIONS <- fs::path(ROOT, "functions")
DIR_LOGS      <- fs::path(ROOT, "logs")

# Input master (already contains score_* columns)
master_csv <- fs::path(DIR_EXPORT, SAMPLE, glue::glue("{SAMPLE}_clean_master.csv"))
stopifnot(fs::file_exists(master_csv))

# Output
date_str <- format(Sys.Date(), "%Y-%m-%d")
OUT_BASE <- fs::path(ROOT, "out", "internal_data_analysis",
                     "distribution_of_backbone_scores_and_internal_consistency",
                     paste0(date_str, "_", SAMPLE))
DIR_PLOTS <- fs::path(OUT_BASE, "plots_hist_density_from_scores")

# ===== Source helpers (plotting only) =====
source(file.path(DIR_FUNCTIONS, "plot_density_by_project.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))
source(file.path(DIR_FUNCTIONS, "safe_var.R"))
source(file.path(DIR_FUNCTIONS, "setup_logging.R"))

# ===== Folder handling (Windows-lock safe) =====
close_all_graphics <- function() {
  while (!is.null(dev.list())) try(grDevices::dev.off(), silent = TRUE)
}
safe_empty_dir <- function(path, retries = 3, sleep_sec = 0.5) {
  fs::dir_create(path)
  for (i in seq_len(retries)) {
    ok <- try({
      files <- fs::dir_ls(path, recurse = TRUE, all = TRUE, type = "file")
      if (length(files)) suppressWarnings(fs::file_delete(files))
      subdirs <- fs::dir_ls(path, recurse = TRUE, type = "directory")
      if (length(subdirs)) suppressWarnings(fs::dir_delete(rev(subdirs)))
      TRUE
    }, silent = TRUE)
    if (!inherits(ok, "try-error")) return(invisible(TRUE))
    Sys.sleep(sleep_sec)
  }
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  new_path <- fs::path(path, paste0("run_", ts))
  fs::dir_create(new_path)
  message("Could not fully clear '", path, "'. Writing into ", new_path)
  assign("DIR_PLOTS", new_path, envir = .GlobalEnv)
  invisible(FALSE)
}

close_all_graphics()
safe_empty_dir(DIR_PLOTS)
fs::dir_create(DIR_LOGS)

# ===== Logger =====
log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_plotting_only_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

# ===== Palette (used by plot_density_by_project) =====
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3","p5"="#E7298A",
  "p6 children"="#66A61E","p7"="#E6AB02","p8 children"="#000000",
  "p8 adults"="#A6761D","p9"="#7A7A7A"
)

# ===== Read master CSV (robust sep= handling) =====
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
)

# ===== Normalize group + detect project column =====
# (We keep the same behavior: missing/empty group -> HC)
if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  df$group <- factor(df$group, levels = c("HC","patient"))
} else {
  df$group <- factor(rep("HC", nrow(df)), levels = c("HC","patient"))
  logger$write("⚠️ 'group' column missing; created it and set all rows to 'HC'.")
}

proj_col <- get_project_col(df) # may return "project" / "p" / NULL
if (is.null(proj_col)) logger$write("⚠️ No project column detected; project-split densities will be skipped.")

# ===== Find score columns =====
score_cols <- names(df)[grepl("^score_", names(df), ignore.case = TRUE)]
if (!length(score_cols)) stop("No columns found matching '^score_' in the master CSV.")

# Parse into scale/subscale using double-underscore convention
score_meta <- tibble::tibble(score_col = score_cols) %>%
  dplyr::mutate(
    # strip leading score_
    rest = stringr::str_replace(score_col, "(?i)^score_", ""),
    has_sub = stringr::str_detect(rest, "__"),
    scale = ifelse(has_sub, stringr::str_split_fixed(rest, "__", 2)[,1], rest),
    subscale = ifelse(has_sub, stringr::str_split_fixed(rest, "__", 2)[,2], NA_character_),
    level = ifelse(has_sub, "subscale", "scale")
  ) %>%
  dplyr::arrange(.data$level, .data$scale, .data$subscale)

# ===== Simple x-limits helper (consistent across group plots for a given score col) =====
xlim_for_vec <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NULL)
  xr <- range(x, na.rm = TRUE)
  pad <- if (is.finite(diff(xr))) diff(xr) * 0.02 else 0
  c(xr[1] - pad, xr[2] + pad)
}

# cap histogram bins by # of distinct observed values
.bins_from_values <- function(x, max_bins = 15L) {
  x <- x[is.finite(x)]
  n_vals <- length(unique(x))
  bins <- min(max_bins, max(1L, n_vals))
  as.integer(bins)
}

save_hist <- function(vec, out_png, title, xlab, xlim = NULL) {
  d <- tibble::tibble(x = suppressWarnings(as.numeric(vec))) %>% dplyr::filter(is.finite(x))
  if (!nrow(d)) { message("  • (hist) no data -> skipped: ", out_png); return(invisible(FALSE)) }
  
  rng_emp <- range(d$x, na.rm = TRUE)
  m_emp   <- mean(d$x, na.rm = TRUE)
  n_emp   <- nrow(d)
  
  cap <- paste0(
    "Empirical range: [", signif(rng_emp[1],4), ", ", signif(rng_emp[2],4), "]",
    " | Mean: ", signif(m_emp,4),
    " | N: ", n_emp
  )
  
  bins_use <- .bins_from_values(d$x, max_bins = 15L)
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = bins_use) +
    ggplot2::labs(title = title, x = xlab, y = "Count", caption = cap) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    { if (!is.null(xlim) && all(is.finite(xlim))) ggplot2::coord_cartesian(xlim = xlim) }
  
  ggplot2::ggsave(out_png, p, width = 7.5, height = 5.2, dpi = 150)
  TRUE
}

# ===== Plot one score column =====
plot_one_scorecol <- function(d, score_col, scale, subscale = NA_character_, level = "scale") {
  lvl <- tolower(level)
  label <- if (lvl == "subscale") paste0(scale, " — ", subscale) else scale
  
  # Compute xlim once from full dataset for consistent split plots
  xlim_use <- xlim_for_vec(d[[score_col]])
  
  # If no finite data at all, skip
  if (is.null(xlim_use)) {
    logger$write(glue::glue("⚠️ {score_col} has no finite values; skipped."))
    return(invisible(FALSE))
  }
  
  # Group splits only (HC/patient)
  if (MAKE_GROUP_SPLITS) {
    for (grp in c("HC","patient")) {
      dd <- d[d$group %in% grp, , drop = FALSE]
      if (!nrow(dd)) next
      
      suf <- if (grp == "HC") "controls" else "patients"
      
      # Histogram (per group)
      fn_hist <- fs::path(
        DIR_PLOTS,
        glue::glue("{safe_var(score_col)}_{lvl}_hist_{suf}.png")
      )
      save_hist(
        vec   = dd[[score_col]],
        out_png = fn_hist,
        title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {label} (overall, {suf})"),
        xlab  = "Score",
        xlim  = xlim_use
      )
      
      # Density by project within group
      if (!is.null(proj_col)) {
        dd_local <- dd
        # ensure numeric score for density
        dd_local[[score_col]] <- suppressWarnings(as.numeric(dd_local[[score_col]]))
        
        fn_den <- fs::path(
          DIR_PLOTS,
          glue::glue("{safe_var(score_col)}_{lvl}_density_by_project_{suf}.png")
        )
        plot_density_by_project(
          df = dd_local,
          var = score_col,
          proj_col = proj_col,
          out_png = fn_den,
          title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {label} (by project, {suf})"),
          xlab  = "Score",
          palette = vanilla_colors,
          group_filter = NULL,   # already filtered dd_local to grp
          adjust = NULL,
          mode_text = NA_character_, # no scoring subtitle
          xlim = xlim_use,
          vlines = NULL
        )
      }
    }
  }
  
  invisible(TRUE)
}

# ===== MAIN LOOP =====
plots_attempted <- 0L
message("== Plotting from existing score_* columns (no scoring) ==")

for (i in seq_len(nrow(score_meta))) {
  sc_col <- score_meta$score_col[i]
  sc     <- score_meta$scale[i]
  ssc    <- score_meta$subscale[i]
  lvl    <- score_meta$level[i]
  
  message(glue::glue("[{i}/{nrow(score_meta)}] {sc_col}  ->  {lvl}: {sc}{if (!is.na(ssc)) paste0(' — ', ssc) else ''}"))
  
  ok <- try(plot_one_scorecol(df, sc_col, sc, ssc, lvl), silent = TRUE)
  if (inherits(ok, "try-error")) {
    logger$write(glue::glue("Error plotting {sc_col}: {as.character(ok)}"))
  } else {
    plots_attempted <- plots_attempted + 1L
  }
}

message("== Summary ==")
message("Plots written to: ", DIR_PLOTS)
message("Total score columns attempted: ", plots_attempted)
message("Log file:        ", log_path)

try(logger$close(), silent = TRUE)
