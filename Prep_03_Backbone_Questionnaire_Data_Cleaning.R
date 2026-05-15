#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Separate Backbone Data by Project — ITEM-LEVEL Cleaning & Export Pipeline
# Authors: Saskia Wilken
# Split version: 2026-03-16
#
# Description:
# Reads questionnaire data, filters bad rows, reverse-codes items, applies
# questionnaire-specific item-level preprocessing, enriches demographics/health,
# writes item-level clean masters, per-project exports, sanity logs, and keys.
#
# IMPORTANT:
# - This script does NOT compute score_* columns.
# - Scoring is moved to a later script so internal consistency can run in between.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# Clean up R environment -------------------------------------------------------
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "janitor", "stringr", "dplyr", 
  "purrr", "lubridate", "tibble", "glue", "fs", "jsonlite", "readr",
  "forcats"
))

# ---- Paths & Logging ---------------------------------------------------------
script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

ROOT <- script_dir()

DIR_QUESTIONNAIRES  <- fs::path(ROOT, "01_project_data","all_projects_backbone", "questionnaires")
DIR_INFO            <- fs::path(ROOT, "information")
DIR_EXPORT          <- fs::path(ROOT, "02_cleaned")
DIR_KEYS            <- fs::path(DIR_EXPORT, "keys")
DIR_LOGS            <- fs::path(ROOT, "logs")
DIR_PRIVATE         <- fs::path(ROOT, "private_information")
SANITY_LOG_FILE     <- fs::path(DIR_LOGS, "sanity_check_backbone_data_log.txt")
DIR_FUNCTIONS       <- fs::path(ROOT, "functions")

fs::dir_create(DIR_EXPORT)
fs::dir_create(DIR_KEYS)
fs::dir_create(DIR_LOGS)

logfile <- fs::path(DIR_LOGS, glue::glue("clean_questionnaires_items_only_{format(Sys.time(), '%Y-%m-%d_%H%M%S')}.log"))

log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- Source required functions -----------------------------------------------
source(file.path(DIR_FUNCTIONS, "write_excel_friendly_csv.R"))
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))

# --- Normalizers --------------------------------------------------------------
normalize_project_key <- function(x) {
  x0  <- x %>% as.character() %>% stringr::str_trim() %>% stringr::str_to_lower()
  num <- stringr::str_match(x0, "p?\\s*(\\d+)")[,2]
  ifelse(is.na(num), NA_character_, paste0("p", num))
}

norm_token <- function(x) gsub("[^a-z]", "", tolower(paste(x)))

normalize_sample_case <- function(sample) {
  stringr::str_replace_all(stringr::str_to_title(sample), "_", " ")
}

extract_date <- function(x) {
  d <- stringr::str_match(x, "(\\d{4}-\\d{2}-\\d{2})")[,2]
  suppressWarnings(lubridate::ymd(d))
}

latest_file_by_pattern <- function(dir, pattern) {
  files_all <- fs::dir_ls(dir, type = "file", fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  files[order(dts, decreasing = TRUE)][1]
}

latest_questionnaire_for_sample_local <- function(sample) {
  patt <- glue::glue("^ALL_\\d{{4}}-\\d{{2}}-\\d{{2}}_{sample}_questionnaire\\.xlsx$")
  latest_file_by_pattern(DIR_QUESTIONNAIRES, patt)
}

latest_iteminfo_for_sample <- function(sample) {
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_scoring <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_groupings_file <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_groupings\\.xlsx$"
  latest_file_by_pattern(DIR_PRIVATE, patt)
}

normalize_vpid <- function(x) {
  x %>% as.character() %>% stringr::str_to_lower() %>% stringr::str_replace_all("[^a-z0-9]", "")
}

# ---- IO helpers --------------------------------------------------------------
read_questionnaire <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading questionnaire: ", filepath)
  df <- suppressMessages(readxl::read_excel(filepath))
  df <- janitor::remove_empty(df, which = c("rows", "cols"))
  tibble::as_tibble(df)
}

read_item_info <- function(filepath) {
  if (is.null(filepath) || is.na(filepath)) {
    log_msg("Item Information file not found for this sample — continuing without it.")
    return(NULL)
  }
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading Item Information: ", filepath)
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(
      item_norm = normalize_id(.data$item),
      reverse_coded = as.logical(.data$reverse_coded)
    )
}

read_scoring <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading Scoring file: ", filepath)
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

# ---- Groupings helpers -------------------------------------------------------
read_groupings_for_sample <- function(sample, filepath = NULL) {
  if (is.null(filepath)) filepath <- latest_groupings_file()
  if (is.null(filepath) || is.na(filepath) || !fs::file_exists(filepath)) {
    log_msg("No groupings file found — skipping group mapping for sample '", sample, "'.")
    return(NULL)
  }
  log_msg("Reading groupings workbook: ", filepath)
  
  sheets <- readxl::excel_sheets(filepath)
  
  parsed <- purrr::map_dfr(sheets, function(sh) {
    sh_l <- tolower(sh)
    m <- stringr::str_match(sh_l, "^\\s*(p?\\s*(\\d+))\\s*([\\s_-].*)?$")
    if (all(is.na(m))) return(NULL)
    num <- m[,3]
    suf <- m[,4]
    tibble::tibble(
      sheet = sh,
      .p_join = paste0("p", num),
      suffix_norm = norm_token(ifelse(is.na(suf), "", suf))
    )
  })
  
  if (!nrow(parsed)) {
    log_msg("Groupings workbook has no 'p#' sheets — skipping.")
    return(NULL)
  }
  
  sample_norm <- norm_token(sample)
  parsed <- parsed %>%
    dplyr::mutate(
      score = dplyr::case_when(
        suffix_norm == "" ~ 1L,
        grepl(sample_norm, suffix_norm, fixed = TRUE) ~ 2L,
        TRUE ~ 0L
      )
    )
  
  id_candidates    <- c("vp","vp id","vpid","vp_id","vp-id","participant","participant id",
                        "participant_id","participantid","subject","subject id","subject_id","vpid_clean")
  group_candidates <- c("group","grp","status","diagnosis","dx","health_status","arm")
  
  standardize_group <- function(x) {
    x0 <- x %>% as.character() %>% stringr::str_trim() %>% stringr::str_to_lower()
    dplyr::case_when(
      x0 %in% c("hc","healthy","control","healthy control","healthy_control","healthy-control","controls") ~ "HC",
      x0 %in% c("patient","pt","case","clinical","patients") ~ "patient",
      TRUE ~ NA_character_
    )
  }
  
  all_rows <- purrr::map_dfr(parsed$sheet, function(sh) {
    meta <- parsed[parsed$sheet == sh, , drop = FALSE]
    df_sh <- suppressMessages(readxl::read_excel(filepath, sheet = sh)) %>% janitor::clean_names()
    
    id_col  <- intersect(names(df_sh), janitor::make_clean_names(id_candidates)) |> purrr::pluck(1, .default = NA_character_)
    grp_col <- intersect(names(df_sh), janitor::make_clean_names(group_candidates)) |> purrr::pluck(1, .default = NA_character_)
    
    if (!is.na(id_col) && !is.na(grp_col)) {
      return(
        df_sh %>%
          dplyr::transmute(
            .p_join = meta$.p_join[1],
            vp_join = normalize_vpid(.data[[id_col]]),
            group   = standardize_group(.data[[grp_col]]),
            .score  = meta$score[1]
          ) %>%
          dplyr::filter(!is.na(vp_join) & !is.na(group))
      )
    }
    
    pat_col <- intersect(names(df_sh), c("patient","patients")) |> purrr::pluck(1, .default = NA_character_)
    ctr_col <- intersect(names(df_sh), c("control","controls","hc")) |> purrr::pluck(1, .default = NA_character_)
    if (!is.na(pat_col) || !is.na(ctr_col)) {
      rows_pat <- if (!is.na(pat_col)) tibble::tibble(vp_join = normalize_vpid(df_sh[[pat_col]]), group = "patient") else tibble::tibble()
      rows_ctr <- if (!is.na(ctr_col)) tibble::tibble(vp_join = normalize_vpid(df_sh[[ctr_col]]), group = "HC") else tibble::tibble()
      bind <- dplyr::bind_rows(rows_pat, rows_ctr) %>%
        dplyr::filter(!is.na(vp_join) & vp_join != "")
      if (nrow(bind)) {
        bind$.p_join <- meta$.p_join[1]
        bind$.score  <- meta$score[1]
      }
      return(bind)
    }
    
    log_msg("Sheet '", sh, "' has no recognizable ID/group nor Patient/Control columns — skipped.")
    NULL
  })
  
  if (!nrow(all_rows)) {
    log_msg("Groupings for '", sample, "' contained no usable rows after cleaning.")
    return(NULL)
  }
  
  out <- all_rows %>%
    dplyr::arrange(dplyr::desc(.score)) %>%
    dplyr::distinct(.p_join, vp_join, .keep_all = TRUE) %>%
    dplyr::select(.p_join, vp_join, group)
  
  log_msg("Loaded ", nrow(out), " group mappings for sample '", sample, "'.")
  out
}

attach_groupings <- function(df, sample,
                             id_guess = c("vp","vp id","vpid","vp_id","vp-id",
                                          "participant","participant id","participant_id","participantid",
                                          "subject","subject_id","subject id")) {
  proj_col <- get_project_col(df)
  if (is.null(proj_col)) {
    log_msg("No project column ('project' or 'p') in data — cannot attach groupings. Returning original df.")
    if (!"group" %in% names(df)) df$group <- NA_character_
    return(df)
  }
  
  grp <- read_groupings_for_sample(sample)
  if (is.null(grp)) {
    if (!"group" %in% names(df)) df$group <- NA_character_
    return(df)
  }
  
  df$.p_join <- normalize_project_key(df[[proj_col]])
  
  df_nms_norm <- gsub("[^a-z0-9]+", "", tolower(names(df)))
  cand_norm   <- gsub("[^a-z0-9]+", "", tolower(id_guess))
  id_in_df    <- names(df)[df_nms_norm %in% cand_norm]
  
  best <- NULL; best_n <- -1; best_id <- NA_character_
  
  for (id_col in id_in_df) {
    tmp <- df %>%
      dplyr::mutate(.vp_join = normalize_vpid(.data[[id_col]])) %>%
      dplyr::left_join(grp, by = c(".p_join", ".vp_join" = "vp_join"))
    nmatch <- sum(!is.na(tmp$group))
    if (nmatch > best_n) {
      best <- tmp
      best_n <- nmatch
      best_id <- id_col
    }
  }
  
  df_out <- if (!is.null(best)) {
    best
  } else {
    df %>% dplyr::left_join(grp %>% dplyr::distinct(.p_join, group), by = ".p_join")
  }
  
  if (!"group" %in% names(df_out) || all(is.na(df_out$group))) {
    df_out <- df_out %>%
      dplyr::left_join(grp %>% dplyr::distinct(.p_join, group), by = ".p_join")
  }
  
  if (!"group" %in% names(df_out)) df_out$group <- NA_character_
  df_out <- df_out %>%
    dplyr::mutate(group = as.character(.data$group)) %>%
    dplyr::select(-dplyr::any_of(c(".p_join", ".vp_join")))
  
  matched <- sum(!is.na(df_out$group))
  log_msg(glue::glue("Grouping for '{sample}': matched {matched}/{nrow(df_out)} rows ",
                     if (!is.na(best_id)) glue::glue("(via '{best_id}')") else "(project-only)"))
  df_out
}

# ---- Validation & keys -------------------------------------------------------
check_header_match <- function(q_df, item_info) {
  q_norm  <- normalize_id(names(q_df))
  ii_norm <- unique(item_info$item_norm)
  
  not_in_q  <- setdiff(ii_norm, q_norm)
  not_in_ii <- setdiff(q_norm, ii_norm)
  
  exempt <- c("p", "project", "rushingflag", "rushing_flag", "rushingmethod",
              "participant", "participantid", "vp", "vpid", "id", "submitdate",
              "startdate", "datestamp", "end", "consent", "startlanguage",
              "seed", "refurl", "lastpage")
  not_in_ii <- setdiff(not_in_ii, exempt)
  
  if (length(not_in_q) || length(not_in_ii)) {
    msg <- paste0(
      "⚠️  Header / Item Information mismatch detected.\n",
      if (length(not_in_q))
        paste0("  • Items expected but NOT found in questionnaire: ",
               paste(not_in_q, collapse = ", "), "\n") else "",
      if (length(not_in_ii))
        paste0("  • Columns in questionnaire NOT present in Item Information: ",
               paste(not_in_ii, collapse = ", "), "\n") else ""
    )
    warning(msg, call. = FALSE)
    log_msg(msg)
  } else {
    log_msg("Header check: questionnaire columns align with Item Information (filtered to scored scales).")
  }
  invisible(TRUE)
}

build_keys <- function(item_info) {
  ii <- item_info
  if (!"item_norm" %in% names(ii)) ii$item_norm <- normalize_id(ii$item)
  if (!"scale" %in% names(ii)) ii$scale <- NA_character_
  if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_
  if (!"higher_order_subscale" %in% names(ii)) ii$higher_order_subscale <- NA_character_
  
  ii <- ii %>%
    dplyr::mutate(
      scale = as.character(scale),
      subscale = as.character(subscale),
      higher_order_subscale = as.character(higher_order_subscale)
    )
  
  items_by_scale <- ii %>% dplyr::group_by(scale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  items_by_sub <- ii %>% dplyr::group_by(scale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  items_by_ho <- ii %>% dplyr::group_by(higher_order_subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  leaf <- ii %>% dplyr::group_by(scale, higher_order_subscale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  ho_level <- leaf %>% dplyr::group_by(scale, higher_order_subscale) %>%
    dplyr::summarise(
      subscales = list(tibble::tibble(subscale = subscale, items = items)),
      .groups = "drop"
    )
  
  nested <- ho_level %>% dplyr::group_by(scale) %>%
    dplyr::summarise(
      higher_order = list(tibble::tibble(
        higher_order_subscale = higher_order_subscale,
        subscales = subscales
      )),
      .groups = "drop"
    )
  
  list(
    items_by_scale = items_by_scale,
    items_by_subscale = items_by_sub,
    items_by_higher_order = items_by_ho,
    nested = nested
  )
}

# ---- Row filtering -----------------------------------------------------------
remove_flagged_rows <- function(q_df, sample) {
  flag_col <- intersect(names(q_df), c("rushing_flag", "rushingflag")) |> purrr::pluck(1, .default = NA_character_)
  if (is.na(flag_col)) {
    log_msg("No rushing flag column found; 0 rows removed.")
    return(list(clean = q_df, discarded = tibble::tibble()))
  }
  flag_val <- q_df[[flag_col]]
  flag_log <- dplyr::case_when(
    is.logical(flag_val) ~ flag_val,
    is.numeric(flag_val) ~ flag_val != 0,
    TRUE ~ tolower(as.character(flag_val)) %in% c("true","t","1","yes","y")
  )
  discarded <- q_df %>% dplyr::filter(flag_log %in% TRUE)
  clean     <- q_df %>% dplyr::filter(!(flag_log %in% TRUE))
  log_msg(glue::glue("Sample '{sample}': removed {nrow(discarded)} rows due to rushing flag."))
  list(clean = clean, discarded = discarded)
}

# ---- Wide-only preprocessing helpers -----------------------------------------
split_id_and_item_columns <- function(q_df, item_info, scoring) {
  valid_scales <- unique(toupper(as.character(scoring$scale)))
  
  item_info_valid <- item_info %>%
    dplyr::filter(!is.na(scale) & scale != "") %>%
    dplyr::filter(toupper(.data$scale) %in% valid_scales | toupper(.data$scale) == "SUQ") %>%
    dplyr::mutate(item_norm = normalize_id(item))
  
  valid_ids <- unique(item_info_valid$item_norm)
  col_norm  <- normalize_id(names(q_df))
  is_item   <- col_norm %in% valid_ids
  
  id_cols   <- names(q_df)[!is_item]
  item_cols <- names(q_df)[is_item]
  
  log_msg("Using items whose Scale is present in the Scoring sheet (+ always SUQ).")
  log_msg("Detected ", length(item_cols), " item columns and ",
          length(id_cols), " meta/ID columns for this sample.")
  
  list(
    id_cols   = id_cols,
    item_cols = item_cols,
    item_info_valid = item_info_valid
  )
}

reverse_code_wide <- function(df, item_cols, col_meta) {
  min_by_col <- col_meta$min; names(min_by_col) <- col_meta$orig
  max_by_col <- col_meta$max; names(max_by_col) <- col_meta$orig
  rev_by_col <- col_meta$reverse_coded; names(rev_by_col) <- col_meta$orig
  
  df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ {
          val <- .x
          col <- dplyr::cur_column()
          if (isTRUE(rev_by_col[[col]])) min_by_col[[col]] + max_by_col[[col]] - val else val
        }
      )
    )
}

NON_SCORABLE_SCALES <- c(
  "FHSfamilytree", "health", "demographics", "times",
  "date", "id", "project"
)

normalize_scale_chr <- function(x) toupper(trimws(as.character(x)))

get_item_col_map <- function(df, item_info) {
  ii <- item_info
  if (!"item_norm" %in% names(ii)) {
    ii <- ii %>% dplyr::mutate(item_norm = normalize_id(.data$item))
  }
  
  tibble::tibble(
    orig      = names(df),
    item_norm = normalize_id(names(df))
  ) %>%
    dplyr::distinct(.data$item_norm, .keep_all = TRUE) %>%
    dplyr::inner_join(
      ii %>%
        dplyr::mutate(
          item = as.character(.data$item),
          scale = as.character(.data$scale),
          subscale = as.character(.data$subscale),
          item_norm = normalize_id(.data$item)
        ) %>%
        dplyr::select(item, scale, subscale, item_norm),
      by = "item_norm"
    )
}

ensure_suq_columns <- function(df, item_info) {
  if (is.null(item_info) || !all(c("scale", "item") %in% names(item_info))) return(df)
  
  ii <- item_info
  if (!"item_norm" %in% names(ii)) {
    ii <- ii %>% dplyr::mutate(item_norm = normalize_id(.data$item))
  }
  
  ii_suq <- ii %>%
    dplyr::mutate(
      scale_chr = normalize_scale_chr(.data$scale),
      item_chr  = trimws(as.character(.data$item)),
      item_norm = normalize_id(.data$item)
    ) %>%
    dplyr::filter(.data$scale_chr == "SUQ", .data$item_chr != "", .data$item_norm != "") %>%
    dplyr::distinct(.data$item_chr, .data$item_norm)
  
  if (!nrow(ii_suq)) return(df)
  
  col_norm <- normalize_id(names(df))
  n_created <- 0L
  
  for (i in seq_len(nrow(ii_suq))) {
    if (!(ii_suq$item_norm[i] %in% col_norm)) {
      df[[ii_suq$item_chr[i]]] <- NA_real_
      n_created <- n_created + 1L
    }
  }
  
  if (n_created > 0L) {
    log_msg("SUQ preprocessing: created ", n_created, " absent SUQ columns as NA before branching cleanup.")
  }
  
  df
}

recode_cape_range_to_1_4 <- function(df, item_info, scoring_df) {
  if (is.null(item_info) || !all(c("scale", "item") %in% names(item_info))) return(df)
  
  cmap <- get_item_col_map(df, item_info) %>%
    dplyr::filter(normalize_scale_chr(.data$scale) == "CAPE")
  
  if (!nrow(cmap)) return(df)
  
  cape_scoring <- scoring_df %>%
    dplyr::filter(normalize_scale_chr(.data$scale) == "CAPE") %>%
    dplyr::slice(1)
  
  scoring_says_0_3 <- nrow(cape_scoring) &&
    !is.na(cape_scoring$min) && !is.na(cape_scoring$max) &&
    cape_scoring$min == 0 && cape_scoring$max == 3
  
  cape_vals <- unlist(lapply(cmap$orig, function(cc) suppressWarnings(as.numeric(df[[cc]]))), use.names = FALSE)
  data_has_zero <- any(cape_vals == 0, na.rm = TRUE)
  
  if (!isTRUE(scoring_says_0_3) && !isTRUE(data_has_zero)) {
    log_msg("CAPE range recode: skipped because CAPE does not look like 0–3.")
    return(df)
  }
  
  for (cc in cmap$orig) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    idx <- !is.na(x)
    x[idx] <- x[idx] + 1
    df[[cc]] <- x
  }
  
  log_msg("CAPE range recode: shifted ", length(cmap$orig), " CAPE item columns from 0–3 to 1–4.")
  df
}

impute_cape_distress_from_frequency <- function(df,
                                                item_info,
                                                scoring_df,
                                                distress_missing_value = 0) {
  if (is.null(item_info) || !all(c("scale", "subscale", "item") %in% names(item_info))) return(df)
  
  cmap <- get_item_col_map(df, item_info) %>%
    dplyr::filter(normalize_scale_chr(.data$scale) == "CAPE")
  
  if (!nrow(cmap)) return(df)
  
  freq_tbl <- cmap %>%
    dplyr::filter(tolower(as.character(.data$subscale)) == "frequency") %>%
    dplyr::mutate(code = stringr::str_extract(.data$item, "\\[(\\d+)\\]"))
  
  distr_tbl <- cmap %>%
    dplyr::filter(tolower(as.character(.data$subscale)) == "distress") %>%
    dplyr::mutate(code = stringr::str_extract(.data$item, "\\[(\\d+)\\]"))
  
  pairs <- freq_tbl %>%
    dplyr::select(code, freq_col = orig) %>%
    dplyr::inner_join(
      distr_tbl %>% dplyr::select(code, distr_col = orig),
      by = "code"
    )
  
  if (!nrow(pairs)) return(df)
  
  n_filled_missing <- 0L
  n_left_missing <- 0L
  
  for (i in seq_len(nrow(pairs))) {
    fcol <- pairs$freq_col[i]
    dcol <- pairs$distr_col[i]
    
    df[[fcol]] <- suppressWarnings(as.numeric(df[[fcol]]))
    df[[dcol]] <- suppressWarnings(as.numeric(df[[dcol]]))
    
    idx_freq_present_distress_missing <- !is.na(df[[fcol]]) & is.na(df[[dcol]])
    
    if (any(idx_freq_present_distress_missing)) {
      df[[dcol]][idx_freq_present_distress_missing] <- distress_missing_value
      n_filled_missing <- n_filled_missing + sum(idx_freq_present_distress_missing)
      log_msg(
        "CAPE: filled ", sum(idx_freq_present_distress_missing),
        " missing distress values in '", dcol,
        "' with ", distress_missing_value,
        " because matching frequency item '", fcol,
        "' was present."
      )
    }
    
    idx_freq_missing_distress_missing <- is.na(df[[fcol]]) & is.na(df[[dcol]])
    n_left_missing <- n_left_missing + sum(idx_freq_missing_distress_missing)
  }
  
  log_msg(
    "CAPE: filled ", n_filled_missing,
    " missing distress values with ", distress_missing_value,
    " where matching frequency was present; left ",
    n_left_missing,
    " distress values missing because matching frequency was also missing."
  )
  
  df
}

preprocess_suq_wide <- function(df, item_info) {
  if (is.null(item_info) || !"scale" %in% names(item_info)) return(df)
  
  if (!"item_norm" %in% names(item_info)) {
    item_info <- item_info %>% dplyr::mutate(item_norm = normalize_id(.data$item))
  }
  if (!"subscale" %in% names(item_info) || !"item" %in% names(item_info)) return(df)
  
  ii_suq <- item_info %>%
    dplyr::filter(toupper(.data$scale) == "SUQ") %>%
    dplyr::mutate(
      subscale = as.character(.data$subscale),
      item_num = suppressWarnings(as.integer(stringr::str_extract(.data$item, "\\d+$"))),
      item_norm = normalize_id(.data$item)
    ) %>%
    dplyr::filter(!is.na(.data$item_num), .data$item_num %in% c(1L, 2L, 3L),
                  !is.na(.data$subscale), .data$subscale != "")
  
  if (!nrow(ii_suq)) return(df)
  
  col_map <- tibble::tibble(
    orig      = names(df),
    item_norm = normalize_id(names(df))
  )
  
  mapped <- ii_suq %>%
    dplyr::inner_join(col_map, by = "item_norm") %>%
    dplyr::select(subscale, item_num, col = orig) %>%
    dplyr::distinct()
  
  wide_map <- mapped %>%
    dplyr::group_by(.data$subscale) %>%
    dplyr::summarise(
      q1 = dplyr::first(.data$col[.data$item_num == 1]),
      q2 = dplyr::first(.data$col[.data$item_num == 2]),
      q3 = dplyr::first(.data$col[.data$item_num == 3]),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data$q1), !is.na(.data$q2), !is.na(.data$q3))
  
  if (!nrow(wide_map)) return(df)
  
  n_q2_imputed_yes <- 0L
  n_q3_forced_zero <- 0L
  n_remaining_na <- 0L
  
  for (i in seq_len(nrow(wide_map))) {
    c1 <- wide_map$q1[i]
    c2 <- wide_map$q2[i]
    c3 <- wide_map$q3[i]
    
    q1 <- suppressWarnings(as.numeric(df[[c1]]))
    q2 <- suppressWarnings(as.numeric(df[[c2]]))
    q3 <- suppressWarnings(as.numeric(df[[c3]]))
    
    idx_yes_q2_missing <- !is.na(q1) & q1 == 1 & is.na(q2)
    if (any(idx_yes_q2_missing)) {
      q2[idx_yes_q2_missing] <- 1
      n_q2_imputed_yes <- n_q2_imputed_yes + sum(idx_yes_q2_missing)
    }
    
    idx_no <- !is.na(q1) & q1 == 0
    q2[idx_no] <- 0
    q3[idx_no] <- 0
    
    idx_yes_q2_one <- !is.na(q1) & q1 == 1 & !is.na(q2) & q2 == 1
    if (any(idx_yes_q2_one)) {
      n_q3_forced_zero <- n_q3_forced_zero + sum(is.na(q3[idx_yes_q2_one]) | q3[idx_yes_q2_one] != 0)
      q3[idx_yes_q2_one] <- 0
    }
    
    n_remaining_na <- n_remaining_na + sum(is.na(q1)) + sum(is.na(q2)) + sum(is.na(q3))
    q1[is.na(q1)] <- 0
    q2[is.na(q2)] <- 0
    q3[is.na(q3)] <- 0
    
    df[[c1]] <- q1
    df[[c2]] <- q2
    df[[c3]] <- q3
  }
  
  log_msg("SUQ preprocessing: filled ", n_q2_imputed_yes,
          " missing Q2 cells with 1 where Q1 == 1; forced ",
          n_q3_forced_zero,
          " Q3 cells to 0 where Q1 == 1 and Q2 == 1; filled ",
          n_remaining_na, " remaining missing SUQ cells with 0.")
  
  df
}

filter_and_impute_global_scored_items <- function(df,
                                                  item_info,
                                                  scoring_df,
                                                  sample,
                                                  min_prop_items = 0.50,
                                                  exclude_scales = NON_SCORABLE_SCALES) {
  if (is.null(item_info) || is.null(scoring_df)) {
    return(list(clean = df, discarded = tibble::tibble(), imputation_log = tibble::tibble()))
  }
  
  scoring_scales <- unique(normalize_scale_chr(scoring_df$scale))
  exclude_scales_norm <- normalize_scale_chr(exclude_scales)
  
  cmap <- get_item_col_map(df, item_info) %>%
    dplyr::mutate(scale_norm = normalize_scale_chr(.data$scale)) %>%
    dplyr::filter(
      .data$scale_norm %in% scoring_scales | .data$scale_norm == "SUQ",
      !(.data$scale_norm %in% exclude_scales_norm)
    ) %>%
    dplyr::filter(!(.data$scale_norm == "CAPE" & tolower(as.character(.data$subscale)) == "distress")) %>%
    dplyr::distinct(.data$orig, .keep_all = TRUE)
  
  item_cols <- cmap$orig
  if (!length(item_cols)) {
    log_msg("Global missingness filter: no scorable item columns found for sample '", sample, "'.")
    return(list(clean = df, discarded = tibble::tibble(), imputation_log = tibble::tibble()))
  }
  
  df[item_cols] <- lapply(df[item_cols], function(z) suppressWarnings(as.numeric(z)))
  
  M <- as.matrix(df[, item_cols, drop = FALSE])
  n_nonmiss <- rowSums(!is.na(M))
  n_total <- length(item_cols)
  min_required <- ceiling(min_prop_items * n_total)
  
  keep <- n_nonmiss >= min_required
  
  discarded <- df[!keep, , drop = FALSE]
  if (nrow(discarded)) {
    discarded$.__reason__ <- paste0(
      "global_missingness: fewer than ", min_required, "/",
      n_total, " scorable items present"
    )
  }
  
  clean <- df[keep, , drop = FALSE]
  
  log_msg("Global missingness filter for sample '", sample, "': kept ",
          nrow(clean), "/", nrow(df), " participants; threshold = ",
          min_required, "/", n_total, " scorable items.")
  
  if (!nrow(clean)) {
    stop("Global missingness filter removed all rows for sample '", sample, "'.")
  }
  
  imputation_log <- purrr::map_dfr(item_cols, function(cc) {
    x <- suppressWarnings(as.numeric(clean[[cc]]))
    n_missing <- sum(is.na(x))
    
    if (n_missing == 0L) {
      clean[[cc]] <<- x
      return(tibble::tibble(item = cc, n_imputed = 0L, impute_value = NA_real_))
    }
    
    imp <- suppressWarnings(stats::median(x, na.rm = TRUE))
    if (is.na(imp)) {
      stop("Cannot impute item column '", cc, "' because all retained values are missing.")
    }
    
    x[is.na(x)] <- imp
    clean[[cc]] <<- x
    
    tibble::tibble(item = cc, n_imputed = n_missing, impute_value = imp)
  })
  
  n_imp <- sum(imputation_log$n_imputed, na.rm = TRUE)
  log_msg("Global item imputation for sample '", sample, "': imputed ",
          n_imp, " remaining missing scorable item cells by item median.")
  
  list(clean = clean, discarded = discarded, imputation_log = imputation_log)
}

# ---- Export helpers ----------------------------------------------------------
normalize_master_item_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_replace_all("\\[|\\]", "") %>%
    stringr::str_replace_all("[^a-z0-9_]+", "")
}

is_item_like_col <- function(nm) {
  grepl("\\[\\d+\\]$", nm)
}

rename_master_item_columns_inplace <- function(df) {
  old <- names(df)
  new <- old
  idx <- vapply(old, is_item_like_col, logical(1))
  new[idx] <- vapply(old[idx], normalize_master_item_id, character(1))
  new <- make.unique(new, sep = "_dup")
  map_tbl <- tibble::tibble(old = old, new = new, renamed = old != new)
  names(df) <- new
  list(df = df, map = map_tbl)
}

export_per_project <- function(df_clean, df_discard, sample, delim = ";") {
  out_dir <- fs::path(DIR_EXPORT, sample)
  fs::dir_create(out_dir)
  
  ren <- rename_master_item_columns_inplace(df_clean)
  df_clean <- ren$df
  map_tbl  <- ren$map
  
  ts <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  map_path <- fs::path(out_dir, glue::glue("rename_map_master_items_{sample}_{ts}.csv"))
  readr::write_csv(map_tbl, map_path)
  log_msg("Wrote rename map: ", map_path, "  (renamed n=", sum(map_tbl$renamed), ")")
  
  master_path <- fs::path(out_dir, glue::glue("{sample}_clean_master.csv"))
  disc_path   <- fs::path(out_dir, glue::glue("{sample}_discarded.csv"))
  
  write.csv2(df_clean, master_path, row.names = FALSE)
  write.csv2(df_discard, disc_path, row.names = FALSE)
  
  log_msg("Wrote master (items only, no score columns): ", master_path)
  log_msg("Wrote discarded: ", disc_path)
  
  proj_col <- get_project_col(df_clean)
  if (is.null(proj_col)) {
    log_msg("No project column ('project' or 'p') — skipping per-project split.")
    return(invisible(NULL))
  }
  
  df_split <- split(df_clean, df_clean[[proj_col]])
  purrr::iwalk(df_split, function(dd, proj) {
    safe_proj <- gsub("[^A-Za-z0-9_-]+", "_", proj)
    filepath  <- fs::path(out_dir, glue::glue("{sample}_project-{safe_proj}_clean.csv"))
    write_excel_friendly_csv(dd, filepath, delim)
  })
  
  log_msg(glue::glue("Exported {length(df_split)} CSVs with BOM + sep='{delim}' for '{sample}'."))
}

save_keys <- function(keys, sample) {
  path_rds  <- fs::path(DIR_KEYS, glue::glue("{sample}_keys.rds"))
  path_json <- fs::path(DIR_KEYS, glue::glue("{sample}_keys.json"))
  saveRDS(keys, path_rds)
  jsonlite::write_json(keys, path_json, pretty = TRUE, auto_unbox = TRUE)
  log_msg(glue::glue("Saved keys for '{sample}' to: {path_rds} and {path_json}"))
}

# ---- Demographics / health ---------------------------------------------------
label_demographics_health <- function(df) {
  out <- df
  
  gender_cols <- intersect(
    c("gender",
      grep("^parentsgender|^siblingsgender|^cildrengender|^childrengender|^children(gender)?",
           names(out), value = TRUE, ignore.case = TRUE)),
    names(out)
  )
  if (length(gender_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(gender_cols),
          ~ factor(as.character(.x),
                   levels = c("1","2","3","0","-oth-"),
                   labels = c("female","male","nonbinary","no gender","other"))
        )
      )
  }
  
  if ("education" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        education = factor(as.character(education),
                           levels = c("0","1","2","3","4","5","6","7","8","9","10","-oth-"),
                           labels = c("no school diploma",
                                      "primary school",
                                      "Lower Secondary School Certificate",
                                      "Intermediate Secondary School Certificate",
                                      "General Qualification for University Entrance",
                                      "apprentice",
                                      "applied science (Fachhochschulabschluss)",
                                      "diploma","bachelor","master","PhD","other"))
      ) %>%
      dplyr::mutate(
        education = forcats::fct_relevel(
          education,
          "no school diploma","primary school",
          "Lower Secondary School Certificate","Intermediate Secondary School Certificate",
          "General Qualification for University Entrance","apprentice",
          "applied science (Fachhochschulabschluss)",
          "bachelor","diploma","master","PhD","other"
        ),
        education = factor(education, levels = levels(education), ordered = TRUE)
      )
  }
  
  if ("maritalstat" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        maritalstat = factor(as.character(maritalstat),
                             levels = c("0","1","2","3","4"),
                             labels = c("single","relationship","married","divorced","widowed"))
      )
  }
  
  sens_cols <- intersect(c("eyesight","hearing"), names(out))
  if (length(sens_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(sens_cols),
          ~ factor(as.character(.x),
                   levels = c("0","1","2"),
                   labels = c("normal","corrected","not corrected"))
        )
      )
  }
  
  bin_candidates <- c("psychomedication","othermedication","ownpsychdisorder")
  bin_candidates <- union(bin_candidates, grep("contact", names(out), value = TRUE))
  bin_cols <- intersect(bin_candidates, names(out))
  if (length(bin_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(bin_cols),
                      ~ factor(as.character(.x), levels = c("0","1"), labels = c("no","yes")))
      )
  }
  
  out
}

log_sanity <- function(sample, df_rows, var, rule_desc) {
  if (nrow(df_rows) == 0) return(invisible(NULL))
  apply(df_rows, 1, function(r) {
    msg <- glue::glue("🔺 vpid {r[['vpid']]} (p={r[['p']]}, sample={sample}) has {var}={r[['value']]} — {rule_desc}.")
    log_msg(msg)
    cat(paste0(msg, "\n"), file = SANITY_LOG_FILE, append = TRUE)
  })
  invisible(TRUE)
}

enrich_demographics_and_health_fields <- function(df, sample) {
  d <- label_demographics_health(df)
  
  parse_date_safely <- function(x) suppressWarnings(as.Date(x))
  get_start_like_date <- function(z) {
    cand <- intersect(c("startdate", "datestamp"), names(z))
    if (!length(cand)) return(rep(as.Date(NA), nrow(z)))
    parse_date_safely(z[[cand[1]]])
  }
  looks_like_years_vec <- function(x) {
    xn <- suppressWarnings(as.numeric(x))
    if (all(is.na(xn))) return(FALSE)
    stats::median(xn, na.rm = TRUE) <= 120
  }
  
  dob   <- if ("age" %in% names(d)) parse_date_safely(d$age) else as.Date(NA)
  start <- get_start_like_date(d)
  age_num_raw <- if ("age" %in% names(d)) suppressWarnings(as.numeric(d$age)) else rep(NA_real_, nrow(d))
  treat_as_years <- looks_like_years_vec(d$age)
  
  d <- d %>%
    dplyr::mutate(
      age_years = dplyr::case_when(
        !is.na(dob) & !is.na(start) ~ as.integer(floor(lubridate::interval(dob, start) / lubridate::years(1))),
        treat_as_years ~ as.integer(floor(age_num_raw)),
        TRUE ~ NA_integer_
      ),
      height = suppressWarnings(as.numeric(.data[["height"]])),
      weight = suppressWarnings(as.numeric(.data[["weight"]]))
    )
  
  get_id_vec <- function(z) {
    if ("vpid" %in% names(z)) z$vpid
    else if ("vp" %in% names(z)) z$vp
    else if ("participantid" %in% names(z)) z$participantid
    else rep(NA_character_, nrow(z))
  }
  proj_col <- get_project_col(d)
  id_vec <- get_id_vec(d)
  
  rep_rows <- function(idx, var, values, rule) {
    if (any(idx, na.rm = TRUE)) {
      tibble::tibble(
        vpid = id_vec[idx],
        p    = if (!is.null(proj_col)) d[[proj_col]][idx] else NA_character_,
        value = values[idx]
      ) %>% log_sanity(sample, ., var, rule)
    }
  }
  
  if (tolower(sample) == "adults") {
    age_bad    <- !is.na(d$age_years) & d$age_years < 18L
    height_bad <- !is.na(d$height)    & d$height > 250
    weight_bad <- !is.na(d$weight)    & d$weight < 35
    
    rep_rows(age_bad,    "age",    d$age_years, "below sensible threshold 18 years (adults)")
    rep_rows(height_bad, "height", d$height,    "above sensible threshold 250 cm")
    rep_rows(weight_bad, "weight", d$weight,    "below sensible threshold 35 kg")
    
    d <- d %>%
      dplyr::mutate(
        age_years = dplyr::if_else(age_bad, as.integer(NA), age_years),
        height    = dplyr::if_else(height_bad, NA_real_, height),
        weight    = dplyr::if_else(weight_bad, NA_real_, weight)
      )
  } else if (tolower(sample) == "adolescents") {
    age_bad    <- !is.na(d$age_years) & d$age_years < 5L
    height_bad <- !is.na(d$height)    & d$height > 250
    weight_bad <- !is.na(d$weight)    & d$weight < 35
    
    rep_rows(age_bad,    "age",    d$age_years, "below sensible threshold 5 years")
    rep_rows(height_bad, "height", d$height,    "above sensible threshold 250 cm")
    rep_rows(weight_bad, "weight", d$weight,    "below sensible threshold 35 kg")
    
    d <- d %>%
      dplyr::mutate(
        age_years = dplyr::if_else(age_bad, as.integer(NA), age_years),
        height    = dplyr::if_else(height_bad, NA_real_, height),
        weight    = dplyr::if_else(weight_bad, NA_real_, weight)
      )
  }
  
  if (any(!is.na(id_vec))) {
    idx_manual <- as.character(id_vec) %in% c("30102")
    if (any(idx_manual, na.rm = TRUE)) {
      tibble::tibble(
        vpid = id_vec[idx_manual],
        p    = if (!is.null(proj_col)) d[[proj_col]][idx_manual] else NA_character_,
        value = d$age_years[idx_manual]
      ) %>% log_sanity(sample, ., "age", "manually set to NA due to implausible DOB (would be ~5 years in adults)")
      d$age_years[idx_manual] <- NA_integer_
    }
  }
  
  if ("gender" %in% names(d)) {
    g <- as.character(d$gender)
    g[!(g %in% c("female","male"))] <- NA_character_
    d$gender <- factor(g, levels = c("female","male"))
  }
  if ("education" %in% names(d)) {
    e <- as.character(d$education)
    e[e %in% c("other","missing")] <- NA_character_
    lev <- setdiff(levels(d$education), c("other","missing"))
    d$education <- factor(e, levels = lev, ordered = TRUE)
  }
  if ("maritalstat" %in% names(d)) {
    m <- as.character(d$maritalstat)
    ord <- c("single","relationship","married","divorced","widowed")
    d$maritalstat <- factor(m, levels = ord, ordered = TRUE)
  }
  
  d
}

# ---- One-sample driver -------------------------------------------------------
process_sample <- function(sample,
                           questionnaire_path = NULL,
                           iteminfo_path = NULL,
                           scoring_df = NULL,
                           drop_criteria_fun = remove_flagged_rows) {
  log_msg("\n--- Processing sample: ", sample, " ---")
  
  if (is.null(questionnaire_path)) questionnaire_path <- latest_questionnaire_for_sample_local(sample)
  if (is.na(questionnaire_path)) {
    log_msg("No questionnaire file found for sample '", sample, "'. Skipping.")
    return(invisible(NULL))
  }
  if (is.null(iteminfo_path)) iteminfo_path <- latest_iteminfo_for_sample(sample)
  
  if (is.null(iteminfo_path) || is.na(iteminfo_path) || !fs::file_exists(iteminfo_path)) {
    fallback <- latest_iteminfo_for_sample("adults")
    if (!is.na(fallback) && fs::file_exists(fallback)) {
      log_msg("Item Information missing for sample '", sample,
              "'. Falling back to adults Item Information: ", fallback)
      iteminfo_path <- fallback
    }
  }
  
  q_raw  <- read_questionnaire(questionnaire_path)
  ii_all <- read_item_info(iteminfo_path)
  
  if (is.null(ii_all)) {
    log_msg("Item Information missing even after fallback; cannot proceed. Skipping sample '", sample, "'.")
    return(invisible(NULL))
  }
  
  dropped   <- drop_criteria_fun(q_raw, sample)
  q_clean0  <- dropped$clean
  q_discard <- dropped$discarded
  
  q_clean0 <- ensure_suq_columns(q_clean0, ii_all)
  
  parts   <- split_id_and_item_columns(q_clean0, ii_all, scoring_df)
  id_cols <- parts$id_cols
  ii      <- parts$item_info_valid
  
  check_header_match(q_clean0, ii)
  
  col_map <- tibble::tibble(
    orig      = names(q_clean0),
    item_norm = normalize_id(names(q_clean0))
  ) %>%
    dplyr::inner_join(ii %>% dplyr::select(item_norm, scale, reverse_coded), by = "item_norm") %>%
    dplyr::distinct(orig, .keep_all = TRUE)
  
  minmax <- scoring_df %>% dplyr::select(scale, min, max)
  col_meta <- col_map %>%
    dplyr::left_join(minmax, by = "scale") %>%
    dplyr::select(orig, reverse_coded, min, max)
  
  q_final <- reverse_code_wide(q_clean0, unique(col_map$orig), col_meta)
  q_final <- recode_cape_range_to_1_4(q_final, ii, scoring_df)
  q_final <- impute_cape_distress_from_frequency(q_final, ii, scoring_df, distress_missing_value = 0)
  q_final <- preprocess_suq_wide(q_final, ii_all)
  global_miss <- filter_and_impute_global_scored_items(
    df = q_final,
    item_info = ii_all,
    scoring_df = scoring_df,
    sample = sample,
    min_prop_items = 0.50,
    exclude_scales = NON_SCORABLE_SCALES
  )
  
  q_final <- global_miss$clean
  
  if (nrow(global_miss$discarded)) {
    q_discard <- dplyr::bind_rows(
      q_discard %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
      global_miss$discarded %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    )
  }
  q_final <- q_final %>% dplyr::select(dplyr::any_of(id_cols), dplyr::everything())
  q_final <- attach_groupings(q_final, sample)
  
  q_ready <- enrich_demographics_and_health_fields(q_final, sample)
  
  keys <- build_keys(ii)
  save_keys(keys, sample)
  
  export_per_project(q_ready, q_discard, sample)
  
  log_msg("--- Done sample: ", sample, " ---\n")
  invisible(list(
    data_clean = q_ready,
    data_discarded = q_discard,
    keys = keys
  ))
}

# ---- Main --------------------------------------------------------------------
SAMPLES_TO_PROCESS <- c("adults", "adolescents")

SCORING_PATH <- latest_scoring()
if (is.na(SCORING_PATH)) {
  stop(glue::glue(
    "No scoring file found in '{DIR_INFO}'. Expected something like 'YYYY-MM-DD_Scoring.xlsx'."
  ))
}
SCORING <- read_scoring(SCORING_PATH)

results <- purrr::map(SAMPLES_TO_PROCESS, ~ process_sample(.x, scoring_df = SCORING))