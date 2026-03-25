#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Export HiTOP-mapped + COMPLETE questionnaire-only inputs for factor analysis
# Date: 2026-03-16
#
# Assumes:
#   - Item-level clean master file in: ROOT/02_cleaned/<sample>/
#   - Keys in:                        ROOT/02_cleaned/keys/
#   - Item Information in:           ROOT/information/
#   - Internal consistency helper in:
#       ROOT/out/internal_data_analysis/
#         distribution_of_backbone_scores_and_internal_consistency/
#
# Exports:
#   C) <samples>_stratification_info.xlsx
#      - project, age, gender, group (from clean master)
#
#   HiTOP (Item Information-based):
#   A) <samples>_HiTOP_items.xlsx
#      - ONLY item columns that have ANY HiTOP mapping in Item Information
#   B) <samples>_HiTOP_subscales.xlsx
#      - recomputed score_* columns for the SAME HiTOP-mapped scales
#
#   COMPLETE (questionnaire-only, explicitly configured below):
#   D) <samples>_complete_items.xlsx
#      - ALL item columns belonging to CFG$questionnaire_scales
#   E) <samples>_complete_subscales.xlsx
#      - recomputed score_* columns belonging to CFG$questionnaire_scales
#
# Optional filtered exports:
#   same filenames with suffix _lt030 (or whatever threshold tag applies)
#   -> items removed based on poor factor loadings
#   -> scale/subscale scores recomputed from filtered item set
#
# Notes:
#   - COMPLETE is built first and imputed pooled across samples.
#   - HiTOP then reuses overlapping imputed values from COMPLETE where available.
#   - Filtered COMPLETE is built first, then filtered HiTOP reuses overlapping
#     values from filtered COMPLETE.
#
# Run:
#   Rscript export_hitop_analysis_inputs.R --samples=adults,adolescents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# ---- Packages ----------------------------------------------------------------
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "readr", "writexl", "janitor", "dplyr", "stringr", "tibble",
  "purrr", "fs", "glue", "rstudioapi", "jsonlite", "mice"
))

# ---- CONFIG ------------------------------------------------------------------
CFG <- list(
  samples = c("adults", "adolescents"),
  export_hitop = TRUE,
  export_complete = TRUE,
  export_loading_filtered = TRUE,
  loading_threshold = 0.30,
  
  questionnaire_scales = c("IDAS","CAPE","AQ","SUQ","ASRS-5","BISBAS","IUS","APS","TICS","CTQ","MAP-SR"),
  score_total_override = c(),
  clean_master = NULL,
  item_info = NULL,
  min_prop_items_default = 0.50,
  missing_threshold = 0.20,
  mice_m = 1,
  mice_maxit = 2,
  mice_seed = 1234,
  mice_donors = 3,
  protected_hidden_items = c("and2", "and3", "hal2", "hal3", "opi2", "opi3")
  
)

# ---- Helpers -----------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x) && !is.na(x) && nzchar(x)) x else y

script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

parse_args <- function(args) {
  out <- list()
  for (a in args) {
    if (grepl("^--", a)) {
      kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
      key <- kv[1]
      val <- if (length(kv) >= 2) kv[2] else NA_character_
      out[[key]] <- val
    }
  }
  out
}

normalize_sample_case <- function(sample) {
  stringr::str_replace_all(stringr::str_to_title(sample), "_", " ")
}

extract_date <- function(x) {
  d <- stringr::str_match(x, "(\\d{4}-\\d{2}-\\d{2})")[,2]
  suppressWarnings(as.Date(d))
}

normalize_id <- function(x) {
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

is_nonempty <- function(x) {
  x0 <- trimws(as.character(x))
  !is.na(x0) & nzchar(x0) & tolower(x0) != "na"
}

pick_first <- function(cands, nms) {
  hit <- cands[cands %in% nms]
  if (length(hit)) hit[1] else NA_character_
}

choose_id_col <- function(nms) {
  pick_first(c("vp_id","vp","vpid","participant_id","participantid","id"), nms)
}

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

sample_tag <- function(samples) {
  paste(samples, collapse = "_")
}

latest_file_by_pattern <- function(dir, pattern, recurse = FALSE) {
  files_all <- fs::dir_ls(dir, type = "file", recurse = recurse, fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  if (all(is.na(dts))) {
    info <- file.info(files)
    return(files[order(info$mtime, decreasing = TRUE)][1])
  }
  files[order(dts, decreasing = TRUE)][1]
}

latest_clean_master_for_sample <- function(root, sample) {
  folder <- fs::path(root, "02_cleaned", sample)
  cand <- fs::path(folder, paste0(sample, "_clean_master.csv"))
  if (fs::file_exists(cand)) return(cand)
  
  files <- fs::dir_ls(folder, regexp = "clean_master\\.csv$", type = "file", fail = FALSE)
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  files[order(dts, decreasing = TRUE)][1]
}

latest_iteminfo_for_sample <- function(root, sample) {
  info_dir <- fs::path(root, "information")
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(info_dir, patt)
}

latest_scoring <- function(root) {
  info_dir <- fs::path(root, "information")
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(info_dir, patt)
}

resolve_iteminfo_with_fallback <- function(root, sample, fallback_sample = "adults") {
  ii_path <- latest_iteminfo_for_sample(root, sample)
  if (!is.na(ii_path) && fs::file_exists(ii_path)) return(ii_path)
  
  fb <- latest_iteminfo_for_sample(root, fallback_sample)
  if (!is.na(fb) && fs::file_exists(fb)) {
    message("ItemInfo missing for sample '", sample, "'. Falling back to '", fallback_sample, "': ", fb)
    return(fb)
  }
  NA_character_
}

read_master_csv_robust <- function(master_csv, default_delim = ";") {
  first_line <- readr::read_lines(master_csv, n_max = 1)
  delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
    sub("^sep=", "", first_line, ignore.case = TRUE)
  } else default_delim
  
  skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L
  
  suppressMessages(
    readr::read_delim(
      master_csv, delim = delim, skip = skip_n, show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
  ) %>% tibble::as_tibble()
}

read_scoring <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

# ---- Main paths / logging ----------------------------------------------------
ROOT <- script_dir()
ARGS <- parse_args(commandArgs(trailingOnly = TRUE))

DIR_LOGS <- fs::path(ROOT, "logs")
OUT_DIR  <- fs::path(ROOT, "03_analysis_input")

fs::dir_create(DIR_LOGS)
fs::dir_create(OUT_DIR)

logfile <- fs::path(
  DIR_LOGS,
  glue::glue("export_hitop_analysis_inputs_{format(Sys.time(), '%Y-%m-%d_%H%M%S')}.log")
)

log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- Keys / scoring helpers --------------------------------------------------
fill_hidden_items_for_adolescents <- function(df, sample, items = CFG$protected_hidden_items, fill_value = 0) {
  if (tolower(sample) != "adolescents") return(df)
  
  item_map <- tibble::tibble(
    orig = names(df),
    norm = normalize_id(names(df))
  ) %>% dplyr::distinct(.data$norm, .keep_all = TRUE)
  
  for (it in normalize_id(items)) {
    hit <- item_map$orig[item_map$norm == it]
    
    if (!length(hit)) {
      df[[it]] <- fill_value
      log_msg("Adolescent hidden-item export fix: created missing column '", it,
              "' and filled with ", fill_value, ".")
    } else {
      cc <- hit[1]
      df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
      n_na <- sum(is.na(df[[cc]]))
      if (n_na > 0) {
        df[[cc]][is.na(df[[cc]])] <- fill_value
      }
      log_msg("Adolescent hidden-item export fix: filled ", n_na,
              " NA values in column '", cc, "' with ", fill_value, ".")
    }
  }
  
  names(df) <- normalize_id(names(df))
  df
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
  
  items_norm <- normalize_id(items)
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
  
  col_map <- tibble::tibble(
    orig      = names(df),
    item_norm = normalize_id(names(df))
  ) %>% dplyr::distinct(item_norm, .keep_all = TRUE)
  
  scales_tbl <- keys$items_by_scale %>%
    dplyr::filter(!is.na(scale), scale != "") %>%
    dplyr::filter(!(toupper(scale) %in% toupper(exclude_scales)))
  
  if (!nrow(scales_tbl)) return(df)
  
  for (i in seq_len(nrow(scales_tbl))) {
    sc    <- as.character(scales_tbl$scale[i])
    items <- scales_tbl$items[[i]]
    if (!length(items)) next
    
    mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean")
    mp_i   <- get_scale_min_prop(scoring_df, sc, default = default_min_prop)
    
    new_col <- paste0(prefix, safe_score_name(sc))
    df[[new_col]] <- score_items_wide(df, items, col_map, agg = mode_i, min_prop = mp_i)
  }
  
  df
}

add_subscale_scores <- function(df, keys, scoring_df,
                                prefix = "score_",
                                default_min_prop = CFG$min_prop_items_default,
                                exclude_scales = character(0)) {
  if (is.null(keys) || is.null(keys$items_by_subscale) || !nrow(keys$items_by_subscale)) return(df)
  
  col_map <- tibble::tibble(
    orig      = names(df),
    item_norm = normalize_id(names(df))
  ) %>% dplyr::distinct(item_norm, .keep_all = TRUE)
  
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
    if (!length(items)) next
    
    mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean")
    mp_i   <- get_scale_min_prop(scoring_df, sc, default = default_min_prop)
    
    new_col <- paste0(prefix, safe_score_name(sc), "__", safe_score_name(sub))
    df[[new_col]] <- score_items_wide(df, items, col_map, agg = mode_i, min_prop = mp_i)
  }
  
  df
}

drop_flagged_items_from_keys <- function(keys, flagged_tbl) {
  if (is.null(flagged_tbl) || !nrow(flagged_tbl)) return(keys)
  
  protected_norm <- normalize_id(CFG$protected_hidden_items)
  flagged_norm <- setdiff(unique(normalize_id(flagged_tbl$item)), protected_norm)
  
  prune_vec <- function(x) {
    x <- as.character(x)
    x[!(normalize_id(x) %in% flagged_norm)]
  }
  
  if (!is.null(keys$items_by_scale) && nrow(keys$items_by_scale)) {
    keys$items_by_scale <- keys$items_by_scale %>%
      dplyr::mutate(items = purrr::map(.data$items, prune_vec))
  }
  
  if (!is.null(keys$items_by_subscale) && nrow(keys$items_by_subscale)) {
    keys$items_by_subscale <- keys$items_by_subscale %>%
      dplyr::mutate(items = purrr::map(.data$items, prune_vec))
  }
  
  if (!is.null(keys$items_by_higher_order) && nrow(keys$items_by_higher_order)) {
    keys$items_by_higher_order <- keys$items_by_higher_order %>%
      dplyr::mutate(items = purrr::map(.data$items, prune_vec))
  }
  
  if (!is.null(keys$nested) && nrow(keys$nested)) {
    keys$nested <- keys$nested %>%
      dplyr::mutate(
        higher_order = purrr::map(.data$higher_order, function(ho_tbl) {
          if (is.null(ho_tbl) || !nrow(ho_tbl)) return(ho_tbl)
          ho_tbl %>%
            dplyr::mutate(
              subscales = purrr::map(.data$subscales, function(sub_tbl) {
                if (is.null(sub_tbl) || !nrow(sub_tbl)) return(sub_tbl)
                sub_tbl %>%
                  dplyr::mutate(items = purrr::map(.data$items, prune_vec))
              })
            )
        })
      )
  }
  
  keys
}

# ---- Flag-helper handling ----------------------------------------------------
latest_flag_helper <- function(root, combined_label, threshold_tag) {
  base_dir <- fs::path(
    root, "out", "internal_data_analysis",
    "distribution_of_backbone_scores_and_internal_consistency"
  )
  patt <- paste0("^", combined_label, "_flagged_items_", threshold_tag, "\\.xlsx$")
  latest_file_by_pattern(base_dir, patt, recurse = TRUE)
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
      item    = as.character(.data$item),
      loading = suppressWarnings(as.numeric(.data$loading))
    ) %>%
    dplyr::filter(
      .data$dataset == dataset_label,
      !is.na(.data$loading),
      abs(.data$loading) < threshold_value
    )
}

# ---- Data helpers ------------------------------------------------------------
dedupe_by_sample_id <- function(df, label = "table") {
  if (is.null(df) || !nrow(df)) return(df)
  
  id_col <- choose_id_col(names(df))
  if (is.na(id_col) || !"sample" %in% names(df)) return(df)
  
  dup_idx <- duplicated(df[, c("sample", id_col), drop = FALSE])
  if (any(dup_idx)) {
    log_msg("Detected ", sum(dup_idx), " duplicate rows in ", label,
            " by sample + id. Keeping first occurrence.")
    df <- df[!dup_idx, , drop = FALSE]
  }
  df
}

export_items_tbl <- function(d, id_col, ii_rows) {
  col_map <- tibble::tibble(orig = names(d), item_norm = normalize_id(names(d))) %>%
    dplyr::distinct(.data$item_norm, .keep_all = TRUE)
  
  items_keep_norm <- unique(normalize_id(ii_rows$item))
  item_cols <- col_map %>%
    dplyr::filter(.data$item_norm %in% items_keep_norm) %>%
    dplyr::pull(.data$orig) %>%
    unique()
  
  if (!length(item_cols)) return(NULL)
  
  d %>%
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))
    )
}

recompute_scores_tbl <- function(d, id_col, keys_obj, scales, scoring_df, total_override = c()) {
  scales <- unique(as.character(scales))
  scales <- scales[is_nonempty(scales)]
  if (!length(scales)) return(NULL)
  
  d_scored <- d
  keys_use <- keys_obj
  
  d_scored <- add_scale_scores(
    d_scored,
    keys = keys_use$items_by_scale %>%
      { list(items_by_scale = dplyr::filter(., .data$scale %in% scales),
             items_by_subscale = tibble::tibble()) },
    scoring_df = scoring_df,
    prefix = "score_"
  )
  
  if (!is.null(keys_use$items_by_subscale) && nrow(keys_use$items_by_subscale)) {
    keys_sub <- list(
      items_by_scale = tibble::tibble(),
      items_by_subscale = dplyr::filter(keys_use$items_by_subscale, .data$scale %in% scales)
    )
    d_scored <- add_subscale_scores(
      d_scored,
      keys = keys_sub,
      scoring_df = scoring_df,
      prefix = "score_"
    )
  }
  
  sub_tbl <- ii %>%
    dplyr::filter(.data$scale %in% scales) %>%
    dplyr::filter(is_nonempty(.data$subscale)) %>%
    dplyr::distinct(.data$scale, .data$subscale)
  
  wanted <- purrr::map_dfr(scales, function(sc) {
    subs <- sub_tbl %>%
      dplyr::filter(.data$scale == sc) %>%
      dplyr::pull(.data$subscale) %>%
      as.character() %>%
      unique()
    
    sc_safe <- safe_score_name(sc)
    
    if (length(subs)) {
      tibble::tibble(
        scale = sc,
        subscale = subs,
        col = paste0("score_", sc_safe, "__", safe_score_name(subs))
      )
    } else {
      if (length(total_override) && sc %in% names(total_override)) {
        tibble::tibble(scale = sc, subscale = NA_character_, col = unname(total_override[[sc]]))
      } else {
        tibble::tibble(scale = sc, subscale = NA_character_, col = paste0("score_", sc_safe))
      }
    }
  })
  
  present_scores <- intersect(wanted$col, names(d_scored))
  if (!length(present_scores)) return(NULL)
  
  d_scored %>%
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(present_scores), ~ suppressWarnings(as.numeric(.x)))
    )
}

impute_complete_pooled <- function(df_combined,
                                   item_cols,
                                   missing_threshold = CFG$missing_threshold,
                                   sample_col = "sample") {
  item_cols <- intersect(item_cols, names(df_combined))
  if (!length(item_cols)) {
    stop("No COMPLETE item columns found for pooled imputation.", call. = FALSE)
  }
  
  id_col <- choose_id_col(names(df_combined))
  if (is.na(id_col)) {
    stop("Could not find ID column in combined COMPLETE dataset.", call. = FALSE)
  }
  
  df_combined <- dedupe_by_sample_id(df_combined, label = "COMPLETE pooled input")
  
  X <- df_combined[, item_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  row_missing <- rowMeans(is.na(X))
  drop_idx <- row_missing > missing_threshold
  
  if (any(drop_idx)) {
    dropped <- df_combined[drop_idx, c(sample_col, id_col), drop = FALSE]
    dropped$missing_prop <- row_missing[drop_idx]
    
    log_msg("Dropping ", nrow(dropped), " participants with >", missing_threshold * 100,
            "% missing COMPLETE item data before pooled imputation.")
    
    for (i in seq_len(nrow(dropped))) {
      log_msg("Dropped participant: sample=", dropped[[sample_col]][i],
              ", id=", dropped[[id_col]][i],
              ", missing_prop=", round(dropped$missing_prop[i], 3))
    }
  } else {
    log_msg("No participants exceeded the >", missing_threshold * 100,
            "% missing COMPLETE threshold.")
  }
  
  df_keep <- df_combined[!drop_idx, , drop = FALSE]
  X <- df_keep[, item_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  if (!anyNA(X)) {
    log_msg("No missing COMPLETE item data to impute after exclusion.")
    return(df_keep)
  }
  
  miss_cols_initial <- names(X)[colSums(is.na(X)) > 0]
  obs_cols <- setdiff(names(X), miss_cols_initial)
  
  fully_missing_cols <- miss_cols_initial[colSums(!is.na(X[, miss_cols_initial, drop = FALSE])) == 0]
  if (length(fully_missing_cols)) {
    stop(
      paste0(
        "Some COMPLETE item columns are fully missing even in pooled data and cannot be imputed: ",
        paste(fully_missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  one_value_cols <- miss_cols_initial[
    vapply(X[, miss_cols_initial, drop = FALSE], function(z) {
      length(unique(z[!is.na(z)])) == 1
    }, logical(1))
  ]
  
  if (length(one_value_cols)) {
    for (cc in one_value_cols) {
      fill_val <- unique(X[[cc]][!is.na(X[[cc]])])[1]
      X[[cc]][is.na(X[[cc]])] <- fill_val
    }
    log_msg("Filled ", length(one_value_cols),
            " COMPLETE columns deterministically because they had exactly one observed value.")
  }
  
  miss_cols_after_constant_fill <- names(X)[colSums(is.na(X)) > 0]
  if (!length(miss_cols_after_constant_fill)) {
    df_keep[, item_cols] <- X
    log_msg("All remaining COMPLETE missingness resolved by deterministic single-value fill.")
    return(df_keep)
  }
  
  imp_data <- X[, c(miss_cols_after_constant_fill, obs_cols), drop = FALSE]
  
  pred <- mice::quickpred(
    imp_data,
    mincor = 0.10,
    minpuc = 0.25,
    include = obs_cols
  )
  
  meth <- rep("", ncol(imp_data))
  names(meth) <- names(imp_data)
  meth[miss_cols_after_constant_fill] <- "pmm"
  
  if (length(obs_cols)) {
    pred[obs_cols, ] <- 0
  }
  
  log_msg("COMPLETE pooled item columns total: ", length(item_cols),
          " | with missingness before constant fill: ", length(miss_cols_initial),
          " | imputed with mice: ", length(miss_cols_after_constant_fill),
          " | fully observed: ", length(obs_cols))
  
  log_msg("Running pooled mice imputation for COMPLETE items ...")
  
  suppressMessages({
    imp <- mice::mice(
      data = imp_data,
      m = CFG$mice_m,
      maxit = CFG$mice_maxit,
      method = meth,
      predictorMatrix = pred,
      donors = CFG$mice_donors,
      seed = CFG$mice_seed,
      printFlag = FALSE
    )
  })
  
  completed <- mice::complete(imp, 1)
  X[, names(completed)] <- completed
  
  # final fallback so COMPLETE output is fully filled
  remaining_cols <- names(X)[colSums(is.na(X)) > 0]
  if (length(remaining_cols)) {
    for (cc in remaining_cols) {
      z <- suppressWarnings(as.numeric(X[[cc]]))
      fill_val <- suppressWarnings(stats::median(z, na.rm = TRUE))
      if (!is.finite(fill_val)) fill_val <- unique(z[!is.na(z)])[1] %||% 0
      X[[cc]][is.na(X[[cc]])] <- fill_val
    }
    log_msg("Applied final deterministic fallback fill to ", length(remaining_cols),
            " COMPLETE columns still containing NA after mice.")
  }
  
  df_keep[, item_cols] <- X
  
  remaining_na <- sum(is.na(df_keep[, item_cols, drop = FALSE]))
  log_msg("Pooled COMPLETE imputation finished. Remaining COMPLETE item NAs: ", remaining_na)
  
  df_keep
}

merge_from_complete_items <- function(hitop_tbl, complete_tbl) {
  if (is.null(hitop_tbl) || !nrow(hitop_tbl)) return(hitop_tbl)
  if (is.null(complete_tbl) || !nrow(complete_tbl)) return(hitop_tbl)
  
  id_col_hitop <- choose_id_col(names(hitop_tbl))
  id_col_comp  <- choose_id_col(names(complete_tbl))
  
  if (is.na(id_col_hitop) || is.na(id_col_comp)) {
    stop("Could not find ID column when merging COMPLETE values into HiTOP.", call. = FALSE)
  }
  
  complete_tbl <- dedupe_by_sample_id(complete_tbl, label = "COMPLETE items for HiTOP merge")
  
  comp_item_cols <- setdiff(names(complete_tbl), c("sample", id_col_comp))
  hitop_item_cols <- setdiff(names(hitop_tbl), c("sample", id_col_hitop))
  overlap_item_cols <- intersect(hitop_item_cols, comp_item_cols)
  
  if (!length(overlap_item_cols)) return(hitop_tbl)
  
  comp_sub <- complete_tbl %>%
    dplyr::select(dplyr::all_of(c("sample", id_col_comp, overlap_item_cols)))
  
  names(comp_sub)[names(comp_sub) == id_col_comp] <- id_col_hitop
  names(comp_sub)[names(comp_sub) %in% overlap_item_cols] <- paste0(overlap_item_cols, "__from_complete")
  
  out <- hitop_tbl %>%
    dplyr::left_join(comp_sub, by = c("sample", id_col_hitop), relationship = "many-to-one")
  
  for (cc in overlap_item_cols) {
    src <- paste0(cc, "__from_complete")
    out[[cc]] <- dplyr::coalesce(
      suppressWarnings(as.numeric(out[[src]])),
      suppressWarnings(as.numeric(out[[cc]]))
    )
  }
  
  out %>%
    dplyr::select(-dplyr::matches("__from_complete$"))
}

# ---- Load inputs -------------------------------------------------------------
if (is.null(ARGS$samples) && !is.null(ARGS$sample) && nzchar(ARGS$sample)) {
  ARGS$samples <- ARGS$sample
}

samples_cli <- ARGS$samples %||% NA_character_
samples <- if (!is.na(samples_cli) && nzchar(samples_cli)) {
  strsplit(samples_cli, ",", fixed = TRUE)[[1]] |> trimws() |> tolower()
} else {
  CFG$samples |> tolower()
}

combined_label <- sample_tag(samples)
threshold_tag  <- make_threshold_tag(CFG$loading_threshold)

message("Samples:    ", paste(samples, collapse = ", "))
message("Sample tag: ", combined_label)
message("Output dir: ", OUT_DIR)
message("COMPLETE questionnaire scales: ", paste(CFG$questionnaire_scales, collapse = ", "))

ITEM_INFO <- (ARGS$item_info %||% CFG$item_info) %||%
  resolve_iteminfo_with_fallback(ROOT, "adults", fallback_sample = "adults")
stopifnot(!is.na(ITEM_INFO), fs::file_exists(ITEM_INFO))
message("ItemInfo:   ", ITEM_INFO)

SCORING_PATH <- latest_scoring(ROOT)
stopifnot(!is.na(SCORING_PATH), fs::file_exists(SCORING_PATH))
message("Scoring:    ", SCORING_PATH)

ii <- readxl::read_excel(ITEM_INFO) %>%
  janitor::clean_names()

if (!"item" %in% names(ii))  stop("Item Information is missing an 'item' column after clean_names().")
if (!"scale" %in% names(ii)) stop("Item Information is missing a 'scale' column after clean_names().")
if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_

ii <- ii %>%
  dplyr::mutate(
    item     = normalize_id(.data$item),
    scale    = as.character(.data$scale),
    subscale = as.character(.data$subscale)
  ) %>%
  dplyr::filter(is_nonempty(.data$item), is_nonempty(.data$scale))

scoring_df <- read_scoring(SCORING_PATH)

# ---- Per-sample loader -------------------------------------------------------
read_one_sample_master <- function(sample) {
  cm <- (ARGS$clean_master %||% CFG$clean_master)
  master_path <- if (!is.null(cm) && !is.na(cm) && nzchar(cm)) cm else latest_clean_master_for_sample(ROOT, sample)
  if (is.na(master_path) || !fs::file_exists(master_path)) {
    stop("Missing clean master for sample '", sample, "'. Looked for: ", master_path, call. = FALSE)
  }
  
  keys_path <- fs::path(ROOT, "02_cleaned", "keys", paste0(sample, "_keys.rds"))
  if (!fs::file_exists(keys_path)) {
    stop("Missing keys for sample '", sample, "'. Looked for: ", keys_path, call. = FALSE)
  }
  
  d <- read_master_csv_robust(master_path)
  
  id_col_orig <- choose_id_col(names(d))
  if (is.na(id_col_orig)) {
    stop("Could not find an ID column in master for sample '", sample, "'.", call. = FALSE)
  }
  
  names(d) <- normalize_id(names(d))
  id_col <- normalize_id(id_col_orig)
  
  d <- fill_hidden_items_for_adolescents(d, sample)
  
  list(
    sample = sample,
    master_path = master_path,
    keys_path = keys_path,
    d = d,
    id_col = id_col,
    keys = readRDS(keys_path)
  )
}

masters <- purrr::map(samples, read_one_sample_master)

# ---- Stratification ----------------------------------------------------------
make_strat <- function(obj) {
  d <- obj$d; id_col <- obj$id_col; sample <- obj$sample
  proj_col   <- pick_first(c("project", "p"), names(d))
  age_col    <- pick_first(c("age_years", "age"), names(d))
  gender_col <- pick_first(c("gender"), names(d))
  group_col  <- pick_first(c("group"), names(d))
  
  tibble::tibble(
    sample  = sample,
    !!id_col := as.character(d[[id_col]]),
    project = if (!is.na(proj_col))   as.character(d[[proj_col]]) else NA_character_,
    age     = if (!is.na(age_col))    suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
    gender  = if (!is.na(gender_col)) as.character(d[[gender_col]]) else NA_character_,
    group   = if (!is.na(group_col))  as.character(d[[group_col]]) else NA_character_
  )
}

strat_combined <- purrr::map(masters, make_strat) %>% dplyr::bind_rows()
out_strat <- fs::path(OUT_DIR, glue::glue("{combined_label}_stratification_info.xlsx"))
writexl::write_xlsx(list(strat_all = strat_combined), out_strat)
message("Wrote: ", out_strat)
log_msg("Wrote: ", out_strat)

# ---- HiTOP / COMPLETE selection ---------------------------------------------
hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)

ii_hitop <- NULL
if (isTRUE(CFG$export_hitop) && length(hitop_cols)) {
  mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
  ii_hitop <- ii[mapped_rows, , drop = FALSE]
}

ii_complete <- NULL
if (isTRUE(CFG$export_complete)) {
  ii_complete <- ii %>% dplyr::filter(.data$scale %in% CFG$questionnaire_scales)
  if (!nrow(ii_complete)) {
    stop("COMPLETE export: No Item Information rows match CFG$questionnaire_scales.", call. = FALSE)
  }
}

# ---- Unfiltered COMPLETE -----------------------------------------------------
complete_items_combined <- NULL
complete_scores_combined <- NULL

if (isTRUE(CFG$export_complete) && !is.null(ii_complete) && nrow(ii_complete)) {
  scales_all <- unique(ii_complete$scale)
  
  complete_items_list <- list()
  for (obj in masters) {
    tab_items <- export_items_tbl(obj$d, obj$id_col, ii_complete)
    if (!is.null(tab_items)) {
      tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
      complete_items_list[[obj$sample]] <- tab_items
    }
  }
  
  complete_items_combined_raw <- dplyr::bind_rows(complete_items_list)
  
  complete_item_cols <- setdiff(
    names(complete_items_combined_raw),
    c("sample", choose_id_col(names(complete_items_combined_raw)))
  )
  
  complete_items_combined <- impute_complete_pooled(
    df_combined = complete_items_combined_raw,
    item_cols = complete_item_cols,
    missing_threshold = CFG$missing_threshold,
    sample_col = "sample"
  )
  
  complete_scores_list <- list()
  for (obj in masters) {
    d_imp_sample <- complete_items_combined %>%
      dplyr::filter(.data$sample == obj$sample) %>%
      dplyr::select(-.data$sample)
    
    tab_scores <- recompute_scores_tbl(
      d_imp_sample, obj$id_col, obj$keys, scales_all, scoring_df,
      total_override = CFG$score_total_override
    )
    
    if (!is.null(tab_scores)) {
      tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
      complete_scores_list[[obj$sample]] <- tab_scores
    }
  }
  
  complete_scores_combined <- dplyr::bind_rows(complete_scores_list)
  
  out_items_all <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_items.xlsx"))
  writexl::write_xlsx(list(complete_items = complete_items_combined), out_items_all)
  message("Wrote: ", out_items_all)
  log_msg("Wrote: ", out_items_all)
  
  out_scores_all <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_subscales.xlsx"))
  writexl::write_xlsx(list(complete_subscales = complete_scores_combined), out_scores_all)
  message("Wrote: ", out_scores_all)
  log_msg("Wrote: ", out_scores_all)
}

# ---- Unfiltered HiTOP --------------------------------------------------------
if (isTRUE(CFG$export_hitop) && !is.null(ii_hitop) && nrow(ii_hitop)) {
  hitop_items_list  <- list()
  hitop_scores_list <- list()
  scales_hitop <- unique(ii_hitop$scale)
  
  for (obj in masters) {
    tab_items_raw <- export_items_tbl(obj$d, obj$id_col, ii_hitop)
    if (!is.null(tab_items_raw)) {
      tab_items_raw <- tab_items_raw %>% dplyr::mutate(sample = obj$sample, .before = 1)
    }
    
    if (!is.null(complete_items_combined) && nrow(complete_items_combined) && !is.null(tab_items_raw)) {
      comp_sample <- complete_items_combined %>%
        dplyr::filter(.data$sample == obj$sample)
      tab_items <- merge_from_complete_items(tab_items_raw, comp_sample)
    } else {
      tab_items <- tab_items_raw
    }
    
    tab_scores <- if (!is.null(tab_items)) {
      recompute_scores_tbl(
        d = tab_items %>% dplyr::select(-dplyr::any_of("sample")),
        id_col = obj$id_col,
        keys_obj = obj$keys,
        scales = scales_hitop,
        scoring_df = scoring_df,
        total_override = CFG$score_total_override
      )
    } else NULL
    
    if (!is.null(tab_items)) {
      hitop_items_list[[obj$sample]] <- tab_items
    }
    if (!is.null(tab_scores)) {
      tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
      hitop_scores_list[[obj$sample]] <- tab_scores
    }
  }
  
  hitop_items_combined  <- dplyr::bind_rows(hitop_items_list)
  hitop_scores_combined <- dplyr::bind_rows(hitop_scores_list)
  
  out_items <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_items.xlsx"))
  writexl::write_xlsx(list(hitop_items = hitop_items_combined), out_items)
  message("Wrote: ", out_items)
  log_msg("Wrote: ", out_items)
  
  out_scores <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_subscales.xlsx"))
  writexl::write_xlsx(list(hitop_subscales = hitop_scores_combined), out_scores)
  message("Wrote: ", out_scores)
  log_msg("Wrote: ", out_scores)
}

# ---- Filtered COMPLETE + Filtered HiTOP -------------------------------------
if (isTRUE(CFG$export_loading_filtered)) {
  flag_helper <- latest_flag_helper(ROOT, combined_label, threshold_tag)
  
  if (is.na(flag_helper) || !fs::file_exists(flag_helper)) {
    message("No flagged-item helper found for filtered exports. Skipping filtered outputs.")
    log_msg("No flagged-item helper found for filtered exports. Skipping filtered outputs.")
  } else {
    message("Flag helper: ", flag_helper)
    log_msg("Flag helper: ", flag_helper)
    
    flagged_tbl <- read_flagged_items(
      flag_helper,
      dataset_label = combined_label,
      threshold_value = CFG$loading_threshold
    )
    
    # ---- Filtered COMPLETE ----------------------------------------------------
    complete_items_combined_f <- NULL
    complete_scores_combined_f <- NULL
    
    if (isTRUE(CFG$export_complete) && !is.null(ii_complete) && nrow(ii_complete)) {
      protected_norm <- normalize_id(CFG$protected_hidden_items)
      flagged_norm_complete <- setdiff(unique(normalize_id(flagged_tbl$item)), protected_norm)
      
      ii_complete_filt <- if (length(flagged_norm_complete)) {
        ii_complete %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm_complete))
      } else {
        ii_complete
      }
      
      scales_all_filt <- unique(ii_complete_filt$scale)
      
      complete_items_list_f <- list()
      for (obj in masters) {
        tab_items <- export_items_tbl(obj$d, obj$id_col, ii_complete_filt)
        if (!is.null(tab_items)) {
          tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
          complete_items_list_f[[obj$sample]] <- tab_items
        }
      }
      
      complete_items_combined_f_raw <- dplyr::bind_rows(complete_items_list_f)
      
      complete_item_cols_f <- setdiff(
        names(complete_items_combined_f_raw),
        c("sample", choose_id_col(names(complete_items_combined_f_raw)))
      )
      
      complete_items_combined_f <- impute_complete_pooled(
        df_combined = complete_items_combined_f_raw,
        item_cols = complete_item_cols_f,
        missing_threshold = CFG$missing_threshold,
        sample_col = "sample"
      )
      
      complete_scores_list_f <- list()
      for (obj in masters) {
        keys_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl)
        
        d_imp_sample <- complete_items_combined_f %>%
          dplyr::filter(.data$sample == obj$sample) %>%
          dplyr::select(-.data$sample)
        
        tab_scores <- recompute_scores_tbl(
          d_imp_sample, obj$id_col, keys_filt, scales_all_filt, scoring_df,
          total_override = CFG$score_total_override
        )
        
        if (!is.null(tab_scores)) {
          tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
          complete_scores_list_f[[obj$sample]] <- tab_scores
        }
      }
      
      complete_scores_combined_f <- dplyr::bind_rows(complete_scores_list_f)
      
      out_items_all_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_items_{threshold_tag}.xlsx"))
      writexl::write_xlsx(list(complete_items = complete_items_combined_f), out_items_all_f)
      message("Wrote: ", out_items_all_f)
      log_msg("Wrote: ", out_items_all_f)
      
      out_scores_all_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_subscales_{threshold_tag}.xlsx"))
      writexl::write_xlsx(list(complete_subscales = complete_scores_combined_f), out_scores_all_f)
      message("Wrote: ", out_scores_all_f)
      log_msg("Wrote: ", out_scores_all_f)
    }
    
    # ---- Filtered HiTOP -------------------------------------------------------
    if (isTRUE(CFG$export_hitop) && !is.null(ii_hitop) && nrow(ii_hitop)) {
      protected_norm <- normalize_id(CFG$protected_hidden_items)
      flagged_norm_hitop <- setdiff(unique(normalize_id(flagged_tbl$item)), protected_norm)
      ii_hitop_filt <- if (length(flagged_norm_hitop)) {
        ii_hitop %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm_hitop))
      } else {
        ii_hitop
      }
      
      hitop_items_list_f  <- list()
      hitop_scores_list_f <- list()
      scales_hitop_f <- unique(ii_hitop_filt$scale)
      
      for (obj in masters) {
        keys_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl)
        
        tab_items_raw <- export_items_tbl(obj$d, obj$id_col, ii_hitop_filt)
        if (!is.null(tab_items_raw)) {
          tab_items_raw <- tab_items_raw %>% dplyr::mutate(sample = obj$sample, .before = 1)
        }
        
        if (!is.null(complete_items_combined_f) && nrow(complete_items_combined_f) && !is.null(tab_items_raw)) {
          comp_sample <- complete_items_combined_f %>%
            dplyr::filter(.data$sample == obj$sample)
          tab_items <- merge_from_complete_items(tab_items_raw, comp_sample)
        } else {
          tab_items <- tab_items_raw
        }
        
        tab_scores <- if (!is.null(tab_items)) {
          recompute_scores_tbl(
            d = tab_items %>% dplyr::select(-dplyr::any_of("sample")),
            id_col = obj$id_col,
            keys_obj = keys_filt,
            scales = scales_hitop_f,
            scoring_df = scoring_df,
            total_override = CFG$score_total_override
          )
        } else NULL
        
        if (!is.null(tab_items)) {
          hitop_items_list_f[[obj$sample]] <- tab_items
        }
        if (!is.null(tab_scores)) {
          tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
          hitop_scores_list_f[[obj$sample]] <- tab_scores
        }
      }
      
      hitop_items_combined_f  <- dplyr::bind_rows(hitop_items_list_f)
      hitop_scores_combined_f <- dplyr::bind_rows(hitop_scores_list_f)
      
      out_items_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_items_{threshold_tag}.xlsx"))
      writexl::write_xlsx(list(hitop_items = hitop_items_combined_f), out_items_f)
      message("Wrote: ", out_items_f)
      log_msg("Wrote: ", out_items_f)
      
      out_scores_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_subscales_{threshold_tag}.xlsx"))
      writexl::write_xlsx(list(hitop_subscales = hitop_scores_combined_f), out_scores_f)
      message("Wrote: ", out_scores_f)
      log_msg("Wrote: ", out_scores_f)
    }
    miss_after <- names(tab_items)[colSums(is.na(tab_items)) > 0]
    log_msg("missing HiTOP cols after merge: ", length(miss_after))
  }
}