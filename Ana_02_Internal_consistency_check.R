# --- analyze_backbone_scales.R -----------------------------------------------
# Computes backbone (sub)scale scores, plots overall histograms and per-project
# overlay density plots (area-normalized to 100% per project), optionally by
# HC/patient, and outputs McDonald's omega (ωt).
# - Logs skipped participants (missing items) to ROOT/logs/<DATE>_data_analysis_log.txt
# - Empties the plots folder at start
# - Densities use consistent x-axes across plots of the same (sub)scale

# ===== Pre-flight: clear console + workspace =====
rm(list = ls(all.names = TRUE)); invisible(gc())
try(cat("\014"), silent = TRUE)  # clear console in R/RStudio

# ===== Setup =====
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readr","jsonlite","tibble","dplyr","tidyr","purrr","stringr","ggplot2",
  "fs","glue","janitor","forcats","psych","rprojroot","readxl"
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
DELIM               <- ";"          # German Excel
MIN_PROP_ITEMS      <- 0.50         # 50% answered items to score
MAKE_GROUP_VARIANTS <- TRUE         # will be skipped if 'group' is empty

# ===== Source helpers =====
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
fs::dir_create(OUT_BASE)

# Empty plots folder first, then recreate
DIR_PLOTS <- fs::path(OUT_BASE, "plots")
if (fs::dir_exists(DIR_PLOTS)) fs::dir_delete(DIR_PLOTS)
fs::dir_create(DIR_PLOTS)

fs::dir_create(DIR_LOGS)

# Logger (free-form lines; include ⚠️ marks)
log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_data_analysis_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

# ===== Palette =====
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3","p5"="#E7298A",
  "p6 children"="#66A61E","p7"="#E6AB02","p8 children"="#000000",
  "p8 adults"="#A6761D","p9"="#7A7A7A"
)

# ===== Read master CSV robustly (handles "sep=" header + BOM) =====
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

# Map missing group to controls ("HC")
if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  df$group <- factor(df$group, levels = c("HC","patient"))
}


proj_col <- get_project_col(df) # "project" or "p" or NULL

# ===== Column map for items =====
col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id(names(df)))

# Quick diagnostics helper
match_items <- function(items) {
  items_norm <- normalize_id(items)
  hit <- intersect(items_norm, col_map$item_norm)
  list(n_in_key = length(items_norm), n_found = length(hit), found = hit)
}

# ===== Read & flatten the keys JSON safely =====
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

# (Optional) higher-order map to support BISBAS BAS-only total if available
items_by_higher <- try({
  tibble::tibble(
    higher_order_subscale = purrr::map_chr(keys_raw$items_by_higher_order, ~ .x$higher_order_subscale %||% NA_character_),
    items = purrr::map(keys_raw$items_by_higher_order, ~ as.character(.x$items %||% character(0)))
  )
}, silent = TRUE)
if (inherits(items_by_higher, "try-error")) items_by_higher <- tibble::tibble()

# ===== Scoring mode (from <DATE>_Scoring.xlsx) =====
SCORING_PATH <- latest_scoring(DIR_INFO)
scoring_df <- if (!is.na(SCORING_PATH)) {
  suppressMessages(readxl::read_excel(SCORING_PATH)) %>% janitor::clean_names()
} else {
  warning("Scoring workbook not found; plot subtitles will omit 'mode'.")
  tibble::tibble()
}

get_mode_text <- function(scale, subscale = NULL) {
  if (!nrow(scoring_df) || !"mode" %in% names(scoring_df)) return(NA_character_)
  # normalize IDs for robust matching
  norm <- function(x) if (is.null(x)) NA_character_ else normalize_id(as.character(x))
  s_need  <- norm(scale)
  ss_need <- norm(subscale)
  
  scoring_df$.scale_norm    <- norm(scoring_df$scale)
  scoring_df$.subscale_norm <- if ("subscale" %in% names(scoring_df)) norm(scoring_df$subscale) else NA_character_
  
  # (scale, subscale) exact if subscale is provided
  if (!is.null(subscale) && "subscale" %in% names(scoring_df)) {
    m <- scoring_df %>% dplyr::filter(.data$.scale_norm == s_need, .data$.subscale_norm == ss_need)
    if (nrow(m) && !is.na(m$mode[1])) return(as.character(m$mode[1]))
  }
  # fallback: match scale only
  m <- scoring_df %>% dplyr::filter(.data$.scale_norm == s_need)
  if (nrow(m) && !is.na(m$mode[1])) return(as.character(m$mode[1]))
  NA_character_
}


# ===== Utility: participant id =====
get_id_vec <- function(z) {
  if ("vpid" %in% names(z)) z$vpid
  else if ("vp" %in% names(z)) z$vp
  else if ("participantid" %in% names(z)) z$participantid
  else rep(NA_character_, nrow(z))
}

# ===== Base score helpers =====
score_sum <- function(d, items, min_prop = MIN_PROP_ITEMS,
                      scale_label = NULL, sub_label = NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  orig_cols  <- col_map$orig[match(keep_norm, col_map$item_norm)]
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  n_nonmiss <- rowSums(!is.na(as.matrix(M)))
  thresh <- ceiling(min_prop * ncol(M))
  skip <- n_nonmiss < thresh
  if (any(skip)) {
    ids <- get_id_vec(d)
    lbl <- paste0(scale_label, if (!is.null(sub_label)) paste0(" / ", sub_label))
    for (j in which(skip)) logger$write(glue::glue("⚠️ Skipping participant {ids[j]} for {lbl}: missing items >= {100*(1-min_prop)}%"))
  }
  out <- rowSums(M, na.rm = TRUE)
  out[skip] <- NA_real_
  out
}

score_mean <- function(d, items, min_prop = MIN_PROP_ITEMS,
                       scale_label = NULL, sub_label = NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  orig_cols  <- col_map$orig[match(keep_norm, col_map$item_norm)]
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  n_nonmiss <- rowSums(!is.na(as.matrix(M)))
  thresh <- ceiling(min_prop * ncol(M))
  skip <- n_nonmiss < thresh
  if (any(skip)) {
    ids <- get_id_vec(d)
    lbl <- paste0(scale_label, if (!is.null(sub_label)) paste0(" / ", sub_label))
    for (j in which(skip)) logger$write(glue::glue("⚠️ Skipping participant {ids[j]} for {lbl}: missing items >= {100*(1-min_prop)}%"))
  }
  out <- rowMeans(M, na.rm = TRUE)
  out[skip] <- NA_real_
  out
}

# ===== Special scoring =====
# CAPE (prefix style only: CAPEfreq.NNN / CAPEdistr.NNN)
score_CAPE <- function(d, items, min_prop = MIN_PROP_ITEMS,
                       scale_label = "CAPE", sub_label = NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (length(keep_norm) == 0) return(rep(NA_real_, nrow(d)))
  
  orig <- col_map$orig[match(keep_norm, col_map$item_norm)]
  nn   <- normalize_id(orig)
  
  is_freq <- grepl("^capefreq", nn, ignore.case = TRUE)
  is_dist <- grepl("^capedistr", nn, ignore.case = TRUE)
  if (!any(is_freq) || !any(is_dist)) {
    warning("CAPE prefix columns (capefreq*/capedistr*) not found; returning NA.")
    logger$write("⚠️ CAPE scoring skipped: no CAPEfreq*/CAPEdistr* columns detected.")
    return(rep(NA_real_, nrow(d)))
  }
  
  item_id <- function(v) stringr::str_extract(v, "\\d+")
  ids_freq <- item_id(nn[is_freq]); ids_dist <- item_id(nn[is_dist])
  valid_freq <- !is.na(ids_freq);    valid_dist <- !is.na(ids_dist)
  if (!any(valid_freq) || !any(valid_dist)) {
    logger$write("⚠️ CAPE scoring skipped: could not parse numeric item ids from prefixes.")
    return(rep(NA_real_, nrow(d)))
  }
  ids_freq  <- ids_freq[valid_freq]; ids_dist <- ids_dist[valid_dist]
  orig_freq <- orig[is_freq][valid_freq]
  orig_dist <- orig[is_dist][valid_dist]
  
  common <- intersect(ids_freq, ids_dist)
  if (!length(common)) {
    logger$write("⚠️ CAPE scoring skipped: no matching freq/dist item ids.")
    return(rep(NA_real_, nrow(d)))
  }
  
  df_freq <- d[, orig_freq, drop = FALSE][, match(common, ids_freq), drop = FALSE]
  df_dist <- d[, orig_dist, drop = FALSE][, match(common, ids_dist), drop = FALSE]
  
  df_freq[] <- lapply(df_freq, function(z) suppressWarnings(as.numeric(z)))
  df_dist[] <- lapply(df_dist, function(z) suppressWarnings(as.numeric(z)))
  
  n_items   <- ncol(df_freq)
  n_nonmiss <- rowSums(!is.na(df_freq) & !is.na(df_dist))
  thresh    <- ceiling(min_prop * n_items)
  skip      <- n_nonmiss < thresh
  if (any(skip)) {
    ids <- get_id_vec(d)
    lbl <- paste0(scale_label, if (!is.null(sub_label)) paste0(" / ", sub_label))
    for (j in which(skip)) logger$write(glue::glue("⚠️ Skipping participant {ids[j]} for {lbl}: missing CAPE item pairs >= {100*(1-min_prop)}%"))
  }
  
  prod_mat <- as.matrix(df_freq) * as.matrix(df_dist)
  sc <- rowSums(prod_mat, na.rm = TRUE) / n_items
  sc[skip] <- NA_real_
  sc
}

score_AQ <- function(d, items, min_prop = MIN_PROP_ITEMS,
                     scale_label = "AQ", sub_label = NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  orig_cols  <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  per_item_max <- NULL
  if (nrow(scoring_df) && all(c("item","max") %in% names(scoring_df))) {
    scoring_df$item_norm <- normalize_id(scoring_df$item)
    per_item_max <- scoring_df %>%
      dplyr::filter(.data$item_norm %in% keep_norm) %>%
      dplyr::select(item_norm, max)
  }
  
  n_nonmiss <- rowSums(!is.na(as.matrix(X)))
  thresh <- ceiling(min_prop * ncol(X))
  skip <- n_nonmiss < thresh
  if (any(skip)) {
    ids <- get_id_vec(d)
    for (j in which(skip)) logger$write(glue::glue("⚠️ Skipping participant {ids[j]} for AQ: missing items >= {100*(1-min_prop)}%"))
  }
  
  S <- matrix(0, nrow = nrow(X), ncol = ncol(X))
  for (k in seq_len(ncol(X))) {
    mx <- if (!is.null(per_item_max)) as.numeric(per_item_max$max[match(keep_norm[k], per_item_max$item_norm)]) else suppressWarnings(max(X[[k]], na.rm = TRUE))
    if (is.infinite(mx) || is.na(mx)) mx <- suppressWarnings(max(X[[k]], na.rm = TRUE))
    S[,k] <- as.numeric(X[[k]] %in% c(mx, mx-1))
  }
  sc <- rowSums(S, na.rm = TRUE)
  sc[skip] <- NA_real_
  sc
}

# simple alias scorers (inherit ... safely)
score_IDAS  <- score_sum
score_ASRS  <- score_sum
score_IUS   <- score_sum
score_APS   <- score_sum
score_TICS  <- score_sum
score_CTQ   <- score_sum
score_MAPSR <- score_sum

# BISBAS total = BAS-only (if higher-order available)
score_BISBAS_total <- function(d, items, ...) {
  if (!nrow(items_by_higher)) return(score_sum(d, items, ...))
  bas_rows <- items_by_higher %>% dplyr::filter(tolower(.data$higher_order_subscale) == "bas")
  if (!nrow(bas_rows)) return(score_sum(d, items, ...))
  bas_items <- unique(unlist(bas_rows$items, use.names = FALSE))
  score_sum(d, bas_items, ...)
}

# ===== McDonald’s omega (only omega.t) =====
compute_omega_safe <- function(items_df) {
  items_df <- items_df[, colSums(is.finite(as.matrix(items_df))) > 0, drop = FALSE]
  if (ncol(items_df) < 2L || nrow(items_df) < 10L) return(NA_real_)
  
  # numeric only
  items_df[] <- lapply(items_df, function(z) suppressWarnings(as.numeric(z)))
  if (!all(vapply(items_df, function(z) any(is.finite(z)), logical(1)))) return(NA_real_)
  
  # try fast omega without Schmid–Leiman (sl = FALSE)
  # this avoids the heavy schmid() path + those long warnings/loops
  out <- try(
    psych::omega(items_df, nfactors = 1, sl = FALSE, plot = FALSE, warnings = FALSE),
    silent = TRUE
  )
  if (inherits(out, "try-error") || is.null(out)) return(NA_real_)
  
  cand <- c("omega.t","omega.tot","omega.totale","omega.totals","omega.totalscore")
  for (nm in cand) if (!is.null(out[[nm]]) && is.numeric(out[[nm]])) return(as.numeric(out[[nm]]))
  if (!is.null(out$omega) && is.data.frame(out$omega) && "omega.tot" %in% names(out$omega)) {
    return(as.numeric(out$omega[,"omega.tot", drop = TRUE]))
  }
  NA_real_
}


omega_for <- function(d, items, min_prop = MIN_PROP_ITEMS) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (length(keep_norm) < 2L) return(NA_real_)
  orig_cols  <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  n_nonmiss <- rowSums(!is.na(as.matrix(X)))
  thresh    <- ceiling(min_prop * ncol(X))
  X <- X[n_nonmiss >= thresh, , drop = FALSE]
  if (nrow(X) < 10L) return(NA_real_)
  compute_omega_safe(X)
}

# ===== Helpers for consistent x-limits per (sub)scale =====
.col_minmax <- function(df_cols) {
  if (!ncol(df_cols)) return(data.frame(min=numeric(0), max=numeric(0)))
  mins <- vapply(df_cols, function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (all(is.na(v))) NA_real_ else suppressWarnings(min(v, na.rm = TRUE))
  }, numeric(1))
  maxs <- vapply(df_cols, function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (all(is.na(v))) NA_real_ else suppressWarnings(max(v, na.rm = TRUE))
  }, numeric(1))
  data.frame(min = mins, max = maxs)
}

score_limits <- function(d, scale_name, sub_name, items) {
  sc_lower <- tolower(scale_name)
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(c(NA_real_, NA_real_))
  
  if (sc_lower == "aq") {
    n <- length(keep_norm)
    return(c(0, n))  # dichotomous sum
  }
  
  if (sc_lower == "cape") {
    orig <- col_map$orig[match(keep_norm, col_map$item_norm)]
    nn   <- normalize_id(orig)
    is_f <- grepl("^capefreq", nn, ignore.case = TRUE)
    is_d <- grepl("^capedistr", nn, ignore.case = TRUE)
    if (!any(is_f) || !any(is_d)) return(c(NA_real_, NA_real_))
    idnum <- function(v) stringr::str_extract(v, "\\d+")
    ids_f <- idnum(nn[is_f]); ids_d <- idnum(nn[is_d])
    ok_f  <- !is.na(ids_f);    ok_d  <- !is.na(ids_d)
    ids_f <- ids_f[ok_f];      ids_d <- ids_d[ok_d]
    orig_f <- orig[is_f][ok_f]; orig_d <- orig[is_d][ok_d]
    common <- intersect(ids_f, ids_d)
    if (!length(common)) return(c(NA_real_, NA_real_))
    
    df_f <- d[, orig_f, drop=FALSE][, match(common, ids_f), drop=FALSE]
    df_d <- d[, orig_d, drop=FALSE][, match(common, ids_d), drop=FALSE]
    rf <- .col_minmax(df_f)
    rd <- .col_minmax(df_d)
    rf <- rf[seq_along(common), , drop=FALSE]
    rd <- rd[seq_along(common), , drop=FALSE]
    prod_min <- rf$min * rd$min
    prod_max <- rf$max * rd$max
    return(c(mean(prod_min, na.rm = TRUE), mean(prod_max, na.rm = TRUE)))
  }
  
  # default: infer from observed per-item ranges, then sum or mean depending on scorer family
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  R <- .col_minmax(d[, orig_cols, drop=FALSE])
  
  if (grepl("^(idas|asrs|ius|aps|tics|ctq|map[-_]?sr|bisbas)$", sc_lower)) {
    c(sum(R$min, na.rm = TRUE), sum(R$max, na.rm = TRUE))
  } else {
    c(mean(R$min, na.rm = TRUE), mean(R$max, na.rm = TRUE))
  }
}

# adaptive density smoothing from number of distinct possible values
# <=10 -> 0.50   >=50 -> 0.75   linear in between
.adjust_from_nvals <- function(n_vals) {
  if (!is.finite(n_vals) || n_vals <= 0) return(0.75)
  if (n_vals <= 10) return(0.50)
  if (n_vals >= 50) return(0.75)
  0.50 + (n_vals - 10) * (0.25 / 40)  # linear ramp
}

# cap histogram bins by # of distinct observed values (never more bins than possible values)
.bins_from_values <- function(x, max_bins = 15L) {
  n_vals <- length(unique(x[is.finite(x)]))
  bins <- min(max_bins, max(1L, n_vals))
  as.integer(bins)
}


# ===== Plotting =====
save_hist <- function(vec, out_png, title, xlab, mode_text = NA, vlines = NULL, xlim = NULL) {
  d <- tibble::tibble(x = as.numeric(vec)) %>% dplyr::filter(is.finite(x))
  if (!nrow(d)) { message("  • (hist) no data -> skipped: ", out_png); return(invisible(FALSE)) }
  subtitle <- if (!is.na(mode_text)) glue::glue("Scoring: {mode_text}") else NULL
  
  rng_emp <- range(d$x, na.rm = TRUE)
  m_emp   <- mean(d$x, na.rm = TRUE)
  n_emp   <- nrow(d)
  cap <- paste0(
    "Possible min/max: ",
    if (!is.null(xlim) && all(is.finite(xlim))) paste0("[", signif(xlim[1],4), ", ", signif(xlim[2],4), "]") else "unknown",
    " | Empirical range: [", signif(rng_emp[1],4), ", ", signif(rng_emp[2],4), "]",
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
  message("  • saved: ", out_png)
  TRUE
}



# ===== Main plot/score runner for one (sub)scale =====
score_and_plot_one <- function(d, scale_name, sub_name, items, level_name) {
  lvl <- tolower(level_name)
  sc_lower <- tolower(scale_name)
  mode_txt <- get_mode_text(scale_name, if (lvl == "subscale") sub_name else NULL)
  
  scorer <- score_mean
  vlines <- NULL
  
  if (sc_lower == "cape") {
    scorer <- score_CAPE
  } else if (sc_lower == "aq") {
    scorer <- score_AQ; vlines <- 6
  } else if (sc_lower == "idas") {
    scorer <- score_IDAS
  } else if (sc_lower == "asrs") {
    scorer <- score_ASRS
  } else if (sc_lower == "ius") {
    scorer <- score_IUS
  } else if (sc_lower == "aps") {
    scorer <- score_APS; vlines <- c(32.5, 58.5)
  } else if (sc_lower == "bisbas") {
    if (lvl == "scale") scorer <- score_BISBAS_total else scorer <- score_sum
  } else if (sc_lower == "tics") {
    scorer <- score_TICS; vlines <- c(8.5, 17.5, 26.5)
  } else if (sc_lower == "ctq") {
    scorer <- score_CTQ
    if (lvl == "subscale") vlines <- c(8.5, 12.5, 15.5, 18.5) else vlines <- c(36.5, 51.5, 68.5, 87.5)
  } else if (sc_lower == "map-sr" || sc_lower == "mapsr" || sc_lower == "map_sr") {
    scorer <- score_MAPSR
  } else if (sc_lower == "suq") {
    message("SUQ has no consistent scoring — skipped.")
    return(invisible(NA_real_))
  } else if (sc_lower == "fhsfamilytree" || sc_lower == "fhs-familytree" || sc_lower == "fhs_familytree") {
    message("FHSfamilytree has no consistent scoring — skipped.")
    return(invisible(NA_real_))
  } else {
    scorer <- score_mean
  }
  
  score <- scorer(d, items, MIN_PROP_ITEMS, scale_label = scale_name, sub_label = if (!is.null(sub_name)) sub_name else NULL)
  varname <- paste0(level_name, "_score")
  d_local <- d; d_local[[varname]] <- score
  
  # Consistent x-axis for this (sub)scale
  lims <- score_limits(d, scale_name, sub_name, items)
  if (all(is.finite(lims)) && lims[1] < lims[2]) {
    xlim_use <- lims
  } else {
    xr <- range(d_local[[varname]], na.rm = TRUE)
    pad <- if (is.finite(diff(xr))) diff(xr) * 0.02 else 0
    xlim_use <- if (is.finite(xr[1]) && is.finite(xr[2])) c(xr[1]-pad, xr[2]+pad) else NULL
  }
  
  fname_base <- if (lvl == "scale") glue::glue("scale_{safe_var(scale_name)}")
  else glue::glue("subscale_{safe_var(scale_name)}__{safe_var(sub_name)}")
  
  # Overall histogram
  fn_hist <- fs::path(DIR_PLOTS, glue::glue("{fname_base}_overall_hist.png"))
  save_hist(score, fn_hist,
            title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {scale_name}{if (lvl=='subscale') paste0(' — ', sub_name) else ''} (overall)"),
            xlab  = glue::glue("{level_name} score"),
            mode_text = mode_txt,
            vlines = vlines,
            xlim = xlim_use)
  
  # Per-project density (all)
  if (!is.null(proj_col)) {
    fn_den <- fs::path(DIR_PLOTS, glue::glue("{fname_base}_by_project_density.png"))
    plot_density_by_project(
      df = d_local, var = varname, proj_col = proj_col, out_png = fn_den,
      title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {scale_name}{if (lvl=='subscale') paste0(' — ', sub_name) else ''} (by project)"),
      xlab  = glue::glue("{level_name} score"),
      palette = vanilla_colors,
      group_filter = NULL,
      adjust = NULL,
      mode_text = mode_txt,
      xlim = xlim_use,
      vlines = vlines
    )
  }
  
  # HC / patient variants
  if (MAKE_GROUP_VARIANTS) {
    if (!("group" %in% names(d_local)) || all(is.na(d_local$group))) {
      warning("Group column missing/empty; skipping group-specific plots.", call. = FALSE)
    } else {
      for (grp in c("HC","patient")) {
        suf <- if (grp == "HC") "controls" else "patients"
        dd <- d_local[d_local$group %in% grp, , drop = FALSE]
        if (!nrow(dd)) next
        
        # overall hist by group
        fn_hg <- fs::path(DIR_PLOTS, glue::glue("{fname_base}_overall_hist_{suf}.png"))
        save_hist(dd[[varname]], fn_hg,
                  title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {scale_name}{if (lvl=='subscale') paste0(' — ', sub_name) else ''} (overall, {suf})"),
                  xlab  = glue::glue("{level_name} score"),
                  mode_text = mode_txt,
                  vlines = vlines,
                  xlim = xlim_use)
        
        # per-project density within group
        if (!is.null(proj_col)) {
          fn_pg <- fs::path(DIR_PLOTS, glue::glue("{fname_base}_by_project_density_{suf}.png"))
          plot_density_by_project(
            df = d_local, var = varname, proj_col = proj_col, out_png = fn_pg,
            title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {scale_name}{if (lvl=='subscale') paste0(' — ', sub_name) else ''} (by project, {suf})"),
            xlab  = glue::glue("{level_name} score"),
            palette = vanilla_colors,
            group_filter = grp,
            adjust = NULL,
            mode_text = mode_txt,
            xlim = xlim_use,
            vlines = vlines
          )
        }
      }
    }
  }
  
  invisible(score)
}

# ===== MAIN LOOPS =====
results <- list()
plots_made <- 0L

# ---- SCALES
if (nrow(items_by_scale)) {
  message("== Scales ==")
  for (i in seq_len(nrow(items_by_scale))) {
    sc    <- items_by_scale$scale[i]
    items <- unlist(items_by_scale$items[[i]], use.names = FALSE)
    mchk  <- match_items(items)
    message(glue::glue("[Scale {i}/{nrow(items_by_scale)}] {sc}: {mchk$n_found}/{mchk$n_in_key} items found"))
    if (mchk$n_found == 0) {
      warning(glue::glue("No matching item columns for scale '{sc}'. Skipping plots & omega."), call. = FALSE)
      next
    }
    
    score_col  <- score_and_plot_one(df, sc, NULL, items, "scale"); plots_made <- plots_made + 1L
    om_overall <- omega_for(df, items)
    
    om_by_proj <- NA_real_
    if (!is.null(proj_col)) {
      om_by_proj <- split(df, df[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items))
    }
    
    om_hc <- om_pat <- NA_real_
    if ("group" %in% names(df) && any(df$group %in% c("HC","patient"), na.rm = TRUE)) {
      om_hc  <- omega_for(dplyr::filter(df, .data$group %in% "HC"), items)
      om_pat <- omega_for(dplyr::filter(df, .data$group %in% "patient"), items)
    }
    
    om_by_proj_hc <- om_by_proj_pat <- NULL
    if (!is.null(proj_col) && "group" %in% names(df)) {
      d_hc  <- dplyr::filter(df,  .data$group %in% "HC")
      d_pat <- dplyr::filter(df,  .data$group %in% "patient")
      if (nrow(d_hc))  om_by_proj_hc  <- split(d_hc,  d_hc[[proj_col]])  |> purrr::map_dbl(~ omega_for(.x, items))
      if (nrow(d_pat)) om_by_proj_pat <- split(d_pat, d_pat[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items))
    }
    
    results[[paste0("scale__", sc)]] <- list(
      level = "scale", name = sc,
      omega_overall = om_overall,
      omega_by_project = om_by_proj,
      omega_overall_HC = om_hc,
      omega_overall_patient = om_pat,
      omega_by_project_HC = om_by_proj_hc,
      omega_by_project_patient = om_by_proj_pat
    )
  }
}

# ---- SUBSCALES
if (nrow(items_by_subscale)) {
  message("== Subscales ==")
  for (i in seq_len(nrow(items_by_subscale))) {
    sc    <- items_by_subscale$scale[i]
    ssc   <- items_by_subscale$subscale[i]
    items <- unlist(items_by_subscale$items[[i]], use.names = FALSE)
    mchk  <- match_items(items)
    message(glue::glue("[Subscale {i}/{nrow(items_by_subscale)}] {sc} — {ssc}: {mchk$n_found}/{mchk$n_in_key} items found"))
    if (mchk$n_found == 0) {
      warning(glue::glue("No matching item columns for subscale '{sc} — {ssc}'. Skipping plots & omega."), call. = FALSE)
      next
    }
    
    score_col  <- score_and_plot_one(df, sc, ssc, items, "subscale"); plots_made <- plots_made + 1L
    om_overall <- omega_for(df, items)
    
    om_by_proj <- NA_real_
    if (!is.null(proj_col)) {
      om_by_proj <- split(df, df[[proj_col]]) %>% purrr::map_dbl(~ omega_for(.x, items))
    }
    
    om_hc <- om_pat <- NA_real_
    if ("group" %in% names(df) && any(df$group %in% c("HC","patient"), na.rm = TRUE)) {
      om_hc  <- omega_for(dplyr::filter(df, .data$group %in% "HC"), items)
      om_pat <- omega_for(dplyr::filter(df, .data$group %in% "patient"), items)
    }
    
    om_by_proj_hc <- om_by_proj_pat <- NULL
    if (!is.null(proj_col) && "group" %in% names(df)) {
      d_hc  <- dplyr::filter(df,  .data$group %in% "HC")
      d_pat <- dplyr::filter(df,  .data$group %in% "patient")
      if (nrow(d_hc))  om_by_proj_hc  <- split(d_hc,  d_hc[[proj_col]])  |> purrr::map_dbl(~ omega_for(.x, items))
      if (nrow(d_pat)) om_by_proj_pat <- split(d_pat, d_pat[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items))
    }
    
    results[[paste0("subscale__", sc, "__", ssc)]] <- list(
      level = "subscale", name = paste(sc, ssc, sep = " / "),
      omega_overall = om_overall,
      omega_by_project = om_by_proj,
      omega_overall_HC = om_hc,
      omega_overall_patient = om_pat,
      omega_by_project_HC = om_by_proj_hc,
      omega_by_project_patient = om_by_proj_pat
    )
  }
}

# ===== Flatten ω to a table and save =====
flatten_named <- function(x, prefix) {
  if (is.null(x) || length(x) == 0) return(tibble::tibble())
  nm <- names(x); if (is.null(nm)) nm <- paste0("split_", seq_along(x))
  tibble::tibble(project_or_split = nm, value = as.numeric(unname(x)), kind = prefix)
}

omega_rows <- purrr::imap_dfr(results, function(res, key) {
  base <- tibble::tibble(
    level = res$level,
    name  = res$name,
    omega_overall = as.numeric(res$omega_overall),
    omega_overall_HC = as.numeric(res$omega_overall_HC),
    omega_overall_patient = as.numeric(res$omega_overall_patient)
  )
  by_proj         <- flatten_named(res$omega_by_project,        "all")
  by_proj_hc      <- flatten_named(res$omega_by_project_HC,     "HC")
  by_proj_patient <- flatten_named(res$omega_by_project_patient,"patient")
  long_by_proj <- dplyr::bind_rows(by_proj, by_proj_hc, by_proj_patient)
  
  if (!nrow(long_by_proj)) {
    base %>% dplyr::mutate(project = "OVERALL") %>%
      dplyr::relocate(project, .after = name)
  } else {
    long_by_proj %>%
      tidyr::pivot_wider(id_cols = "project_or_split", names_from = kind, values_from = value) %>%
      dplyr::rename(project = project_or_split) %>%
      dplyr::mutate(level = res$level, name = res$name, .before = 1) %>%
      dplyr::bind_rows(base %>% dplyr::mutate(project = "OVERALL")) %>%
      dplyr::relocate(project, .after = name)
  }
})

omega_path <- fs::path(OUT_BASE, glue::glue("{SAMPLE}_omega_results.csv"))
write.csv(omega_rows, omega_path, delim = DELIM)

message("== Summary ==")
message("Plots written to: ", DIR_PLOTS)
message("Omega CSV:       ", omega_path)
message("Total plot sets attempted: ", plots_made)
message("Log file:        ", log_path)

try(logger$close(), silent = TRUE)
