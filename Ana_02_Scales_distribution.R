# --- analyze_backbone_scales_hist_density.R ----------------------------------
# Fast figures only: overall histograms + per-project density overlays (+ HC/patient)
# No omega. No per-item violins.
# Includes SUQ scoring (sum) + correct SUQ x-axis limits.
# Also builds scale totals even if only subscales exist (union-of-subscales).

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
DELIM               <- ";"          # German Excel
MIN_PROP_ITEMS      <- 0.50         # 50% items required to score
MAKE_GROUP_VARIANTS <- TRUE         # skip if 'group' missing/empty

# ===== Source helpers =====
source(file.path(DIR_FUNCTIONS, "plot_density_by_project.R"))
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))
source(file.path(DIR_FUNCTIONS, "extract_date_string.R"))
source(file.path(DIR_FUNCTIONS, "latest_questionnaire_for_sample.R"))
source(file.path(DIR_FUNCTIONS, "safe_var.R"))
source(file.path(DIR_FUNCTIONS, "setup_logging.R"))

# ===== Paths =====
master_csv <- fs::path(DIR_EXPORT, SAMPLE, glue::glue("{SAMPLE}_clean_master.csv"))
keys_json  <- fs::path(DIR_KEYS,   glue::glue("{SAMPLE}_keys.json"))
stopifnot(fs::file_exists(master_csv), fs::file_exists(keys_json))

latest_scoring <- function(info_dir = DIR_INFO) {
  files <- fs::dir_ls(info_dir, glob = "*_Scoring.xlsx", fail = FALSE)
  if (!length(files)) return(NA_character_)
  dt <- stringr::str_match(basename(files), "^(\\d{4}-\\d{2}-\\d{2})_")[,2]
  files[order(as.Date(dt), decreasing = TRUE)][1]
}

q_file   <- latest_questionnaire_for_sample(SAMPLE, DIR_QUESTIONNAIRES)
date_str <- if (!is.na(q_file)) extract_date_string(q_file) else format(Sys.Date(), "%Y-%m-%d")

OUT_BASE <- fs::path(ROOT, "out", "internal_data_analysis",
                     "distribution_of_backbone_scores_and_internal_consistency",
                     paste0(date_str, "_", SAMPLE))
DIR_PLOTS <- fs::path(OUT_BASE, "plots_hist_density")

# --- robust folder clearing (Windows locks safe) ---
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
log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_data_analysis_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

# ===== Palette =====
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3","p5"="#E7298A",
  "p6 children"="#66A61E","p7"="#E6AB02","p8 children"="#000000",
  "p8 adults"="#A6761D","p9"="#7A7A7A"
)

# ===== Read master CSV (robust) =====
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

# normalize group; detect project col
if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  df$group <- factor(df$group, levels = c("HC","patient"))
}
proj_col <- get_project_col(df) # "project" or "p" or NULL

# ===== Column map for items =====
col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id(names(df)))

get_id_vec <- function(d) {
  if ("vpid" %in% names(d)) d$vpid
  else if ("vp" %in% names(d)) d$vp
  else if ("participantid" %in% names(d)) d$participantid
  else rep(NA_character_, nrow(d))
}

# ===== Keys & Scoring workbook (for subtitles/modes + AQ max if present) =====
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

# --- Build scale totals even if keys only define subscales ---
.as_chr0 <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) == 1 && is.na(x)) return(character(0))
  as.character(x)
}

scale_from_subscales <- items_by_subscale %>%
  dplyr::filter(!is.na(.data$scale)) %>%
  dplyr::group_by(.data$scale) %>%
  dplyr::summarise(items_sub = list(unique(unlist(.data$items, use.names = FALSE))),
                   .groups = "drop")

items_by_scale_all <- items_by_scale %>%
  dplyr::rename(items_scale = .data$items) %>%
  dplyr::full_join(scale_from_subscales, by = "scale") %>%
  dplyr::mutate(
    items = purrr::map2(.data$items_scale, .data$items_sub, ~{
      a <- .as_chr0(.x); b <- .as_chr0(.y)
      if (length(a)) a else b
    }),
    total_source = dplyr::case_when(
      purrr::map_int(.data$items_scale, ~ length(.as_chr0(.x))) > 0 ~ "keys_scale",
      TRUE ~ "union_subscales"
    )
  ) %>%
  dplyr::select(.data$scale, .data$items, .data$total_source) %>%
  dplyr::filter(!is.na(.data$scale))

latest_scoring_path <- latest_scoring(DIR_INFO)
scoring_df <- if (!is.na(latest_scoring_path)) {
  suppressMessages(readxl::read_excel(latest_scoring_path)) %>% janitor::clean_names()
} else tibble::tibble()

get_mode_text <- function(scale, subscale = NULL) {
  if (!nrow(scoring_df) || !"mode" %in% names(scoring_df)) return(NA_character_)
  norm <- function(x) if (is.null(x)) NA_character_ else normalize_id(as.character(x))
  s_need  <- norm(scale)
  ss_need <- norm(subscale)
  scoring_df$.scale_norm    <- norm(scoring_df$scale)
  scoring_df$.subscale_norm <- if ("subscale" %in% names(scoring_df)) norm(scoring_df$subscale) else NA_character_
  
  if (!is.null(subscale) && "subscale" %in% names(scoring_df)) {
    m <- dplyr::filter(scoring_df, .data$.scale_norm == s_need, .data$.subscale_norm == ss_need)
    if (nrow(m) && !is.na(m$mode[1])) return(as.character(m$mode[1]))
  }
  m <- dplyr::filter(scoring_df, .data$.scale_norm == s_need)
  if (nrow(m) && !is.na(m$mode[1])) return(as.character(m$mode[1]))
  NA_character_
}

# ===== Base scorers =====
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

# ===== Special scorers =====
score_CAPE <- function(d, items, min_prop = MIN_PROP_ITEMS,
                       scale_label = "CAPE", sub_label = NULL, ...) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  
  orig <- col_map$orig[match(keep_norm, col_map$item_norm)]
  nn   <- normalize_id(orig)
  
  is_freq <- grepl("^capefreq", nn, ignore.case = TRUE)
  is_dist <- grepl("^capedistr", nn, ignore.case = TRUE)
  if (!any(is_freq) || !any(is_dist)) {
    logger$write("⚠️ CAPE scoring skipped: no CAPEfreq*/CAPEdistr* columns detected.")
    return(rep(NA_real_, nrow(d)))
  }
  
  item_id <- function(v) stringr::str_extract(v, "\\d+")
  ids_freq <- item_id(nn[is_freq]); ids_dist <- item_id(nn[is_dist])
  valid_freq <- !is.na(ids_freq); valid_dist <- !is.na(ids_dist)
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
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
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
    mx <- if (!is.null(per_item_max)) {
      as.numeric(per_item_max$max[match(keep_norm[k], per_item_max$item_norm)])
    } else {
      suppressWarnings(max(X[[k]], na.rm = TRUE))
    }
    if (is.infinite(mx) || is.na(mx)) mx <- suppressWarnings(max(X[[k]], na.rm = TRUE))
    S[, k] <- as.numeric(X[[k]] %in% c(mx, mx - 1))
  }
  
  sc <- rowSums(S, na.rm = TRUE)
  sc[skip] <- NA_real_
  sc
}

# Aliases
score_IDAS  <- score_sum
score_ASRS  <- score_sum
score_IUS   <- score_sum
score_APS   <- score_sum
score_TICS  <- score_sum
score_CTQ   <- score_sum
score_MAPSR <- score_sum
score_BISBAS_total <- function(d, items, ...) score_sum(d, items, ...)

# ===== Range helpers (for consistent x-limits) =====
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
  
  # ---- SUQ special range (sum across blocks with mixed maxima) ----
  if (sc_lower == "suq") {
    m <- stringr::str_match(keep_norm, "^(.*?)(?:_)?([1-3])$")
    base <- m[,2]; q <- m[,3]
    ok <- !is.na(base) & !is.na(q)
    base <- base[ok]; q <- q[ok]
    if (!length(base)) return(c(NA_real_, NA_real_))
    
    base_max <- tapply(q, base, function(qq) {
      (("1" %in% qq) * 1) + (("2" %in% qq) * 5) + (("3" %in% qq) * 4)
    })
    return(c(0, sum(base_max)))
  }
  
  # AQ: dichotomous sum
  if (sc_lower == "aq") return(c(0, length(keep_norm)))
  
  # CAPE: product-based score; estimate possible range from item ranges
  if (sc_lower == "cape") {
    orig <- col_map$orig[match(keep_norm, col_map$item_norm)]
    nn   <- normalize_id(orig)
    is_f <- grepl("^capefreq", nn, ignore.case = TRUE)
    is_d <- grepl("^capedistr", nn, ignore.case = TRUE)
    if (!any(is_f) || !any(is_d)) return(c(NA_real_, NA_real_))
    
    idnum <- function(v) stringr::str_extract(v, "\\d+")
    ids_f <- idnum(nn[is_f]); ids_d <- idnum(nn[is_d])
    ok_f  <- !is.na(ids_f);   ok_d  <- !is.na(ids_d)
    ids_f <- ids_f[ok_f];     ids_d <- ids_d[ok_d]
    orig_f <- orig[is_f][ok_f]; orig_d <- orig[is_d][ok_d]
    common <- intersect(ids_f, ids_d)
    if (!length(common)) return(c(NA_real_, NA_real_))
    
    df_f <- d[, orig_f, drop = FALSE][, match(common, ids_f), drop = FALSE]
    df_d <- d[, orig_d, drop = FALSE][, match(common, ids_d), drop = FALSE]
    rf <- .col_minmax(df_f); rd <- .col_minmax(df_d)
    
    prod_min <- rf$min * rd$min
    prod_max <- rf$max * rd$max
    return(c(mean(prod_min, na.rm = TRUE), mean(prod_max, na.rm = TRUE)))
  }
  
  # Default: infer from observed per-item ranges
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  R <- .col_minmax(d[, orig_cols, drop = FALSE])
  
  if (grepl("^(idas|asrs|ius|aps|tics|ctq|map[-_]?sr|bisbas|suq)$", sc_lower)) {
    c(sum(R$min, na.rm = TRUE), sum(R$max, na.rm = TRUE))
  } else {
    c(mean(R$min, na.rm = TRUE), mean(R$max, na.rm = TRUE))
  }
}

# cap histogram bins by # of distinct observed values
.bins_from_values <- function(x, max_bins = 15L) {
  n_vals <- length(unique(x[is.finite(x)]))
  bins <- min(max_bins, max(1L, n_vals))
  as.integer(bins)
}

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
  TRUE
}

# ===== Plot one (sub)scale =====
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
    scorer <- score_sum
  } else if (sc_lower %in% c("fhsfamilytree", "fhs-familytree", "fhs_familytree")) {
    message("FHSfamilytree has no consistent scoring — skipped.")
    return(invisible(TRUE))
  } else {
    scorer <- score_mean
  }
  
  score <- scorer(d, items, MIN_PROP_ITEMS, scale_label = scale_name, sub_label = sub_name)
  varname <- paste0(level_name, "_score")
  d_local <- d; d_local[[varname]] <- score
  
  lims <- score_limits(d, scale_name, sub_name, items)
  if (all(is.finite(lims)) && lims[1] < lims[2]) {
    xlim_use <- lims
  } else {
    xr <- range(d_local[[varname]], na.rm = TRUE)
    pad <- if (is.finite(diff(xr))) diff(xr) * 0.02 else 0
    xlim_use <- if (is.finite(xr[1]) && is.finite(xr[2])) c(xr[1] - pad, xr[2] + pad) else NULL
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
  
  # Per-project density (area-normalized per project)
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
        
        fn_hg <- fs::path(DIR_PLOTS, glue::glue("{fname_base}_overall_hist_{suf}.png"))
        save_hist(dd[[varname]], fn_hg,
                  title = glue::glue("{SAMPLE} — {if (lvl=='scale') 'Scale' else 'Subscale'}: {scale_name}{if (lvl=='subscale') paste0(' — ', sub_name) else ''} (overall, {suf})"),
                  xlab  = glue::glue("{level_name} score"),
                  mode_text = mode_txt,
                  vlines = vlines,
                  xlim = xlim_use)
        
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
  
  invisible(TRUE)
}

# ===== MAIN LOOPS (plots only) ===============================================
plots_made <- 0L

if (nrow(items_by_scale_all)) {
  message("== Scales (including totals from union-of-subscales) ==")
  for (i in seq_len(nrow(items_by_scale_all))) {
    sc    <- items_by_scale_all$scale[i]
    items <- unlist(items_by_scale_all$items[[i]], use.names = FALSE)
    
    items_norm <- normalize_id(items)
    hit <- intersect(items_norm, col_map$item_norm)
    
    message(glue::glue("[Scale {i}/{nrow(items_by_scale_all)}] {sc}: {length(hit)}/{length(items_norm)} items found"))
    if (!length(hit)) {
      warning(glue::glue("No matching item columns for scale '{sc}'. Skipping plots."), call. = FALSE)
      next
    }
    score_and_plot_one(df, sc, NULL, items, "scale")
    plots_made <- plots_made + 1L
  }
}

if (nrow(items_by_subscale)) {
  message("== Subscales ==")
  for (i in seq_len(nrow(items_by_subscale))) {
    sc    <- items_by_subscale$scale[i]
    ssc   <- items_by_subscale$subscale[i]
    items <- unlist(items_by_subscale$items[[i]], use.names = FALSE)
    
    items_norm <- normalize_id(items)
    hit <- intersect(items_norm, col_map$item_norm)
    
    message(glue::glue("[Subscale {i}/{nrow(items_by_subscale)}] {sc} — {ssc}: {length(hit)}/{length(items_norm)} items found"))
    if (!length(hit)) {
      warning(glue::glue("No matching item columns for subscale '{sc} — {ssc}'. Skipping plots."), call. = FALSE)
      next
    }
    score_and_plot_one(df, sc, ssc, items, "subscale")
    plots_made <- plots_made + 1L
  }
}

message("== Summary ==")
message("Plots written to: ", DIR_PLOTS)
message("Total plot sets attempted: ", plots_made)
message("Log file:        ", log_path)

try(logger$close(), silent = TRUE)
