#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Add questionnaire score columns to already cleaned Backbone masters
# Authors: Saskia Wilken
# New split step: 2026-03-16
#
# Description:
# Reads item-level clean masters from 02_cleaned/<sample>/, reads keys + scoring,
# optionally removes low-loading items flagged by analyze_backbone_scales.R,
# computes score_* columns, and writes scored masters.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "readr", "writexl", "janitor", "dplyr", "stringr", "tibble",
  "purrr", "fs", "glue", "jsonlite", "rprojroot"
))

`%||%` <- function(a, b) if (!is.null(a)) a else b

CFG <- list(
  samples = c("adults", "adolescents"),
  min_prop_items_default = 0.50,
  export_filtered_scores = TRUE,
  loading_threshold = 0.30,
  combined_label = "adults_adolescents"
)

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

ROOT <- project_root()
DIR_EXPORT     <- fs::path(ROOT, "02_cleaned")
DIR_KEYS       <- fs::path(DIR_EXPORT, "keys")
DIR_INFO       <- fs::path(ROOT, "information")
DIR_LOGS       <- fs::path(ROOT, "logs")
DIR_INTCONS    <- fs::path(ROOT, "out", "internal_data_analysis", "distribution_of_backbone_scores_and_internal_consistency")

fs::dir_create(DIR_LOGS)

logfile <- fs::path(DIR_LOGS, glue::glue("add_scores_to_clean_master_{format(Sys.time(), '%Y-%m-%d_%H%M%S')}.log"))
log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- helpers -----------------------------------------------------------------
latest_file_by_pattern <- function(dir, pattern) {
  files_all <- fs::dir_ls(dir, type = "file", recurse = TRUE, fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  info <- file.info(files)
  files[order(info$mtime, decreasing = TRUE)][1]
}

latest_scoring <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(DIR_INFO, patt)
}

read_master_csv_robust <- function(master_csv, default_delim = ";") {
  first_line <- readr::read_lines(master_csv, n_max = 1)
  delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
    sub("^sep=", "", first_line, ignore.case = TRUE)
  } else default_delim
  
  skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L
  
  suppressMessages(
    readr::read_delim(
      master_csv,
      delim = delim,
      skip = skip_n,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
  ) %>% janitor::clean_names()
}

read_scoring <- function(filepath) {
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

normalize_id_local <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_replace_all("\\[|\\]", "") %>%
    stringr::str_replace_all("[^a-z0-9_]+", "")
}

safe_score_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

latest_flag_helper <- function(combined_label, threshold_tag) {
  patt <- paste0("^", combined_label, "_flagged_items_", threshold_tag, "\\.xlsx$")
  latest_file_by_pattern(DIR_INTCONS, patt)
}

read_flagged_items <- function(path, dataset_label, threshold_value) {
  if (is.na(path) || !fs::file_exists(path)) return(tibble::tibble())
  
  x <- suppressMessages(readxl::read_excel(path, sheet = "flagged_items")) %>%
    janitor::clean_names()
  
  req <- c("dataset", "level", "scale", "subscale", "item", "loading")
  if (!all(req %in% names(x))) return(tibble::tibble())
  
  x %>%
    dplyr::mutate(
      dataset = as.character(.data$dataset),
      level = as.character(.data$level),
      scale = as.character(.data$scale),
      subscale = as.character(.data$subscale),
      item = as.character(.data$item),
      loading = as.numeric(.data$loading)
    ) %>%
    dplyr::filter(.data$dataset == dataset_label, !is.na(.data$loading), abs(.data$loading) < threshold_value)
}

drop_flagged_items_from_keys <- function(keys, flagged_tbl) {
  if (is.null(flagged_tbl) || !nrow(flagged_tbl)) return(keys)
  
  flagged_norm <- normalize_id_local(flagged_tbl$item)
  
  if (!is.null(keys$items_by_scale) && nrow(keys$items_by_scale)) {
    keys$items_by_scale <- keys$items_by_scale %>%
      dplyr::mutate(
        items = purrr::map(.data$items, ~ {
          x <- as.character(.x)
          x[!(normalize_id_local(x) %in% flagged_norm)]
        })
      )
  }
  
  if (!is.null(keys$items_by_subscale) && nrow(keys$items_by_subscale)) {
    keys$items_by_subscale <- keys$items_by_subscale %>%
      dplyr::mutate(
        items = purrr::map(.data$items, ~ {
          x <- as.character(.x)
          x[!(normalize_id_local(x) %in% flagged_norm)]
        })
      )
  }
  
  if (!is.null(keys$items_by_higher_order) && nrow(keys$items_by_higher_order)) {
    keys$items_by_higher_order <- keys$items_by_higher_order %>%
      dplyr::mutate(
        items = purrr::map(.data$items, ~ {
          x <- as.character(.x)
          x[!(normalize_id_local(x) %in% flagged_norm)]
        })
      )
  }
  
  keys
}

get_scale_scoring_mode <- function(scoring_df, scale, default = "mean") {
  if (toupper(trimws(scale)) == "SUQ") return("sum")
  if (is.null(scoring_df) || !"scale" %in% names(scoring_df)) return(default)
  
  cand_cols <- intersect(
    names(scoring_df),
    c("mode","scoring","method","aggregation","score_type","score_method","compute")
  )
  if (!length(cand_cols)) return(default)
  
  sc_key <- toupper(trimws(scale))
  s_key  <- toupper(trimws(as.character(scoring_df$scale)))
  idx    <- which(s_key == sc_key)
  if (!length(idx)) return(default)
  
  for (cc in cand_cols) {
    v <- scoring_df[[cc]][idx[1]]
    if (is.na(v)) next
    v0 <- tolower(trimws(as.character(v)))
    if (grepl("sum|total", v0)) return("sum")
    if (grepl("mean|avg|average", v0)) return("mean")
  }
  
  default
}

get_scale_min_prop <- function(scoring_df, scale, default = CFG$min_prop_items_default) {
  if (is.null(scoring_df) || !"scale" %in% names(scoring_df)) return(default)
  
  cand_cols <- intersect(names(scoring_df), c("min_prop_items","min_prop","minprop","min_prop_item"))
  if (!length(cand_cols)) return(default)
  
  sc_key <- toupper(trimws(scale))
  s_key  <- toupper(trimws(as.character(scoring_df$scale)))
  idx    <- which(s_key == sc_key)
  if (!length(idx)) return(default)
  
  v <- suppressWarnings(as.numeric(scoring_df[[cand_cols[1]]][idx[1]]))
  if (is.na(v)) default else v
}

score_items_wide <- function(d, items, col_map, agg = c("mean","sum"), min_prop = CFG$min_prop_items_default) {
  agg <- match.arg(agg)
  
  items_norm <- normalize_id_local(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  orig_cols <- orig_cols[!is.na(orig_cols)]
  if (!length(orig_cols)) return(rep(NA_real_, nrow(d)))
  
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  
  n_nonmiss <- rowSums(!is.na(as.matrix(M)))
  thresh    <- ceiling(min_prop * ncol(M))
  
  out <- if (agg == "sum") rowSums(M, na.rm = TRUE) else rowMeans(M, na.rm = TRUE)
  out[n_nonmiss < thresh] <- NA_real_
  out
}

add_scale_scores <- function(df, keys, scoring_df,
                             prefix = "score_",
                             default_min_prop = CFG$min_prop_items_default,
                             exclude_scales = character(0)) {
  if (is.null(keys) || is.null(keys$items_by_scale) || !nrow(keys$items_by_scale)) return(df)
  
  col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id_local(names(df))) %>%
    dplyr::distinct(item_norm, .keep_all = TRUE)
  
  scales_tbl <- keys$items_by_scale %>%
    dplyr::filter(!is.na(scale), scale != "") %>%
    dplyr::filter(!(toupper(scale) %in% toupper(exclude_scales)))
  
  if (!nrow(scales_tbl)) return(df)
  
  for (i in seq_len(nrow(scales_tbl))) {
    sc    <- as.character(scales_tbl$scale[i])
    items <- scales_tbl$items[[i]]
    if (length(items) < 1L) next
    
    mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean")
    mp_i   <- get_scale_min_prop(scoring_df, sc, default = default_min_prop)
    
    new_col <- paste0(prefix, safe_score_name(sc))
    df[[new_col]] <- score_items_wide(df, items, col_map, agg = mode_i, min_prop = mp_i)
    
    log_msg("Scored scale '", sc, "' -> ", new_col, " (mode=", mode_i, ", min_prop=", mp_i, ")")
  }
  
  df
}

add_subscale_scores <- function(df, keys, scoring_df,
                                prefix = "score_",
                                default_min_prop = CFG$min_prop_items_default,
                                exclude_scales = character(0)) {
  if (is.null(keys) || is.null(keys$items_by_subscale) || !nrow(keys$items_by_subscale)) return(df)
  
  col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id_local(names(df))) %>%
    dplyr::distinct(item_norm, .keep_all = TRUE)
  
  subs_tbl <- keys$items_by_subscale %>%
    dplyr::filter(!is.na(scale), scale != "") %>%
    dplyr::filter(!is.na(subscale), subscale != "") %>%
    dplyr::filter(!(toupper(scale) %in% toupper(exclude_scales))) %>%
    dplyr::distinct(scale, subscale, .keep_all = TRUE)
  
  if (!nrow(subs_tbl)) return(df)
  
  for (i in seq_len(nrow(subs_tbl))) {
    sc    <- as.character(subs_tbl$scale[i])
    sub   <- as.character(subs_tbl$subscale[i])
    items <- subs_tbl$items[[i]]
    if (length(items) < 1L) next
    
    mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean")
    mp_i   <- get_scale_min_prop(scoring_df, sc, default = default_min_prop)
    
    new_col <- paste0(prefix, safe_score_name(sc), "__", safe_score_name(sub))
    df[[new_col]] <- score_items_wide(df, items, col_map, agg = mode_i, min_prop = mp_i)
    
    log_msg("Scored subscale '", sc, " / ", sub, "' -> ", new_col,
            " (mode=", mode_i, ", min_prop=", mp_i, ")")
  }
  
  df
}

write_master_variant <- function(df, sample, suffix = NULL) {
  out_dir <- fs::path(DIR_EXPORT, sample)
  fs::dir_create(out_dir)
  
  fname <- if (is.null(suffix) || !nzchar(suffix)) {
    glue::glue("{sample}_clean_master_scored.csv")
  } else {
    glue::glue("{sample}_clean_master_scored_{suffix}.csv")
  }
  
  out_path <- fs::path(out_dir, fname)
  write.csv2(df, out_path, row.names = FALSE)
  log_msg("Wrote scored master: ", out_path)
  out_path
}

process_sample <- function(sample, scoring_df, flag_helper_path = NA_character_) {
  log_msg("\n--- Scoring sample: ", sample, " ---")
  
  master_path <- fs::path(DIR_EXPORT, sample, glue::glue("{sample}_clean_master.csv"))
  keys_path   <- fs::path(DIR_KEYS, glue::glue("{sample}_keys.rds"))
  
  stopifnot(fs::file_exists(master_path), fs::file_exists(keys_path))
  
  df   <- read_master_csv_robust(master_path)
  keys <- readRDS(keys_path)
  
  # round 1: unfiltered scores
  df_scored <- df
  df_scored <- add_scale_scores(df_scored, keys, scoring_df, prefix = "score_")
  df_scored <- add_subscale_scores(df_scored, keys, scoring_df, prefix = "score_")
  write_master_variant(df_scored, sample)
  
  # round 2: filtered scores
  if (isTRUE(CFG$export_filtered_scores)) {
    flagged_tbl <- read_flagged_items(
      path = flag_helper_path,
      dataset_label = sample,
      threshold_value = CFG$loading_threshold
    )
    
    if (nrow(flagged_tbl)) {
      log_msg("Applying filtered scoring for sample '", sample, "' with ", nrow(flagged_tbl), " flagged items.")
      keys_filt <- drop_flagged_items_from_keys(keys, flagged_tbl)
      
      df_scored_f <- df
      df_scored_f <- add_scale_scores(df_scored_f, keys_filt, scoring_df, prefix = "score_")
      df_scored_f <- add_subscale_scores(df_scored_f, keys_filt, scoring_df, prefix = "score_")
      
      write_master_variant(df_scored_f, sample, suffix = make_threshold_tag(CFG$loading_threshold))
    } else {
      log_msg("No flagged items found for sample '", sample, "'. Skipping filtered scored master.")
    }
  }
  
  log_msg("--- Done scoring sample: ", sample, " ---\n")
}

# ---- Main --------------------------------------------------------------------
SCORING_PATH <- latest_scoring()
if (is.na(SCORING_PATH)) {
  stop("No scoring file found in information/.")
}
SCORING <- read_scoring(SCORING_PATH)

threshold_tag <- make_threshold_tag(CFG$loading_threshold)
FLAG_HELPER <- latest_flag_helper(CFG$combined_label, threshold_tag)

if (!is.na(FLAG_HELPER)) {
  log_msg("Using flagged-item helper: ", FLAG_HELPER)
} else {
  log_msg("No flagged-item helper found. Filtered scored masters may be skipped.")
}

purrr::walk(CFG$samples, ~ process_sample(.x, scoring_df = SCORING, flag_helper_path = FLAG_HELPER))