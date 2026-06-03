# --- prep06_export_analyze-ready_sheets.R -----------------------------------------------
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
#      - existing score_* columns created by prep05 for the SAME HiTOP-mapped scales
#
#   COMPLETE (questionnaire-only, explicitly configured below):
#   D) <samples>_complete_items.xlsx
#      - ALL item columns belonging to CFG$questionnaire_scales
#   E) <samples>_complete_subscales.xlsx
#      - existing score_* columns created by prep05 belonging to CFG$questionnaire_scales
#
# Optional filtered exports:
#   same filenames with suffix _lt020 (or whatever threshold tag applies)
#   -> items removed based on poor factor loadings
#   -> scale/subscale scores recomputed from filtered item set
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
  "purrr", "fs", "glue", "rstudioapi", "jsonlite"
))

# ---- CONFIG ------------------------------------------------------------------
CFG <- list(
  samples = c("adults", "adolescents"),
  export_hitop = TRUE,
  export_complete = TRUE,
  export_loading_filtered = TRUE,
  export_enriched = TRUE,
  loading_threshold = 0.30,   # make more aggressive by increasing this, e.g. 0.30 / 0.35
  
  questionnaire_scales = c("IDAS","CAPE","AQ","SUQ","ASRS","BISBAS","IUS","APS","TICS","CTQ","MAP-SR"),
  enrichment_demographics_scale = "demographics",
  enrichment_date_candidates = c("date", "datum", "startdate", "start_date", "submit_date", "submitted_at", "enddate", "end_date"),
  enrichment_group_candidates = c("group", "paper_group", "clinical_group", "participant_group", "case_control", "patient_hc"),
  clean_master = NULL,
  item_info = NULL,
  score_total_override = c()
)

# ---- Helpers -----------------------------------------------------------------
add_z_score_columns <- function(df,
                                score_pattern = "^score_",
                                z_prefix = "z_") {
  score_cols <- grep(score_pattern, names(df), value = TRUE)
  score_cols <- score_cols[!startsWith(score_cols, z_prefix)]
  
  if (!length(score_cols)) return(df)
  
  for (cc in score_cols) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    
    z_col <- paste0(z_prefix, cc)
    df[[z_col]] <- if (is.finite(s) && s > 0) {
      (x - m) / s
    } else {
      NA_real_
    }
  }
  
  df
}

make_export_sheets <- function(x, combined_sheet = "combined", z_scores = FALSE) {
  x <- x[!vapply(x, is.null, logical(1))]
  
  if (isTRUE(z_scores)) {
    x <- purrr::map(x, add_z_score_columns)
  }
  
  combined <- dplyr::bind_rows(x)
  
  if (isTRUE(z_scores)) {
    combined <- add_z_score_columns(combined)
  }
  
  x[[combined_sheet]] <- combined
  x
}

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

latest_scored_master_for_sample <- function(root, sample, suffix = NULL) {
  folder <- fs::path(root, "02_cleaned", sample)
  
  fname <- if (is.null(suffix) || !nzchar(suffix)) {
    paste0(sample, "_clean_master_scored.csv")
  } else {
    paste0(sample, "_clean_master_scored_", suffix, ".csv")
  }
  
  cand <- fs::path(folder, fname)
  if (fs::file_exists(cand)) return(cand)
  
  NA_character_
}

latest_iteminfo_for_sample <- function(root, sample) {
  info_dir <- fs::path(root, "information")
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(info_dir, patt)
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

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

sample_tag <- function(samples) {
  paste(samples, collapse = "_")
}

read_master_csv_robust <- function(master_csv, default_delim = ";") {
  first_line <- readr::read_lines(master_csv, n_max = 1)
  
  delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
    sub("^sep=", "", first_line, ignore.case = TRUE)
  } else {
    default_delim
  }
  
  skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L
  
  suppressMessages(
    readr::read_delim(
      master_csv,
      delim = delim,
      skip = skip_n,
      show_col_types = FALSE,
      locale = readr::locale(
        encoding = "UTF-8",
        decimal_mark = if (identical(delim, ";")) "," else ".",
        grouping_mark = if (identical(delim, ";")) "." else ","
      )
    )
  ) %>%
    tibble::as_tibble()
}

# ---- Keys / scoring helpers --------------------------------------------------

drop_flagged_items_from_keys <- function(keys, flagged_tbl) {
  if (is.null(flagged_tbl) || !nrow(flagged_tbl)) return(keys)
  
  flagged_norm <- unique(normalize_id(flagged_tbl$item))
  
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
  
  to_logical_flag <- function(z) {
    if (is.logical(z)) return(z)
    if (is.numeric(z)) return(z != 0)
    z0 <- tolower(trimws(as.character(z)))
    z0 %in% c("true", "t", "1", "yes", "y")
  }
  
  x <- x %>%
    dplyr::mutate(
      dataset = as.character(.data$dataset),
      level = as.character(.data$level),
      scale = as.character(.data$scale),
      subscale = as.character(.data$subscale),
      item = as.character(.data$item),
      loading = suppressWarnings(as.numeric(.data$loading))
    )
  
  if ("flagged_for_removal" %in% names(x)) {
    x$flagged_for_removal <- to_logical_flag(x$flagged_for_removal)
  } else {
    x$flagged_for_removal <- !is.na(x$loading) & abs(x$loading) < threshold_value
  }
  
  x %>%
    dplyr::filter(
      .data$dataset == dataset_label,
      .data$flagged_for_removal %in% TRUE
    ) %>%
    dplyr::distinct(.data$dataset, .data$level, .data$scale, .data$subscale, .data$item, .keep_all = TRUE)
}

# ---- Main --------------------------------------------------------------------
ROOT <- script_dir()
ARGS <- parse_args(commandArgs(trailingOnly = TRUE))

if (is.null(ARGS$samples) && !is.null(ARGS$sample) && nzchar(ARGS$sample)) {
  ARGS$samples <- ARGS$sample
}

OUT_DIR <- fs::path(ROOT, "03_analysis_input")
fs::dir_create(OUT_DIR)

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

ii <- readxl::read_excel(ITEM_INFO) |>
  janitor::clean_names()

if (!"item" %in% names(ii))  stop("Item Information is missing an 'item' column after clean_names().")
if (!"scale" %in% names(ii)) stop("Item Information is missing a 'scale' column after clean_names().")
if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_

ii <- ii |>
  dplyr::mutate(
    item     = as.character(.data$item),
    scale    = as.character(.data$scale),
    subscale = as.character(.data$subscale)
  ) |>
  dplyr::filter(is_nonempty(.data$item), is_nonempty(.data$scale))

# -----------------------------
# Per-sample loader
# -----------------------------
read_one_sample_master <- function(sample) {
  cm <- (ARGS$clean_master %||% CFG$clean_master)
  master_path <- if (!is.null(cm) && !is.na(cm) && nzchar(cm)) {
    cm
  } else {
    latest_clean_master_for_sample(ROOT, sample)
  }
  
  if (is.na(master_path) || !fs::file_exists(master_path)) {
    stop("Missing clean master for sample '", sample, "'. Looked for: ", master_path, call. = FALSE)
  }
  
  keys_path <- fs::path(ROOT, "02_cleaned", "keys", paste0(sample, "_keys.rds"))
  if (!fs::file_exists(keys_path)) {
    stop("Missing keys for sample '", sample, "'. Looked for: ", keys_path, call. = FALSE)
  }
  
  d <- read_master_csv_robust(master_path)
  
  scored_full_path <- latest_scored_master_for_sample(ROOT, sample)
  if (is.na(scored_full_path) || !fs::file_exists(scored_full_path)) {
    stop("Missing unfiltered scored master for sample '", sample, "'. Run Step 3 first.", call. = FALSE)
  }
  
  scored_filtered_path <- latest_scored_master_for_sample(
    ROOT,
    sample,
    suffix = threshold_tag
  )
  
  scored_combined_suffix <- paste0(threshold_tag, "_combined")
  scored_combined_path <- latest_scored_master_for_sample(
    ROOT,
    sample,
    suffix = scored_combined_suffix
  )
  
  d_scored_full <- read_master_csv_robust(scored_full_path)
  
  d_scored_filtered <- if (!is.na(scored_filtered_path) && fs::file_exists(scored_filtered_path)) {
    read_master_csv_robust(scored_filtered_path)
  } else {
    NULL
  }
  
  d_scored_combined <- if (!is.na(scored_combined_path) && fs::file_exists(scored_combined_path)) {
    read_master_csv_robust(scored_combined_path)
  } else {
    NULL
  }
  
  id_col <- pick_first(c("vp_id", "vp", "vpid", "participant_id", "participantid", "id"), names(d))
  if (is.na(id_col)) {
    stop("Could not find an ID column in master for sample '", sample, "'.", call. = FALSE)
  }
  
  list(
    sample = sample,
    master_path = master_path,
    keys_path = keys_path,
    d = d,
    id_col = id_col,
    keys = readRDS(keys_path),
    d_scored_full = d_scored_full,
    d_scored_filtered = d_scored_filtered,
    d_scored_combined = d_scored_combined,
    scored_full_path = scored_full_path,
    scored_filtered_path = scored_filtered_path,
    scored_combined_path = scored_combined_path
  )
}

masters <- purrr::map(samples, read_one_sample_master)

# -----------------------------
# Build stratification tables
# -----------------------------
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

strat_list <- purrr::map(masters, make_strat)
strat_combined <- dplyr::bind_rows(strat_list)

out_strat <- fs::path(OUT_DIR, glue::glue("{combined_label}_stratification_info.xlsx"))
writexl::write_xlsx(list(strat_all = strat_combined), out_strat)
message("Wrote: ", out_strat)

# -----------------------------
# Export builders
# -----------------------------
export_items_tbl <- function(d, id_col, ii_rows) {
  col_map <- tibble::tibble(orig = names(d), item_norm = normalize_id(names(d))) |>
    dplyr::distinct(.data$item_norm, .keep_all = TRUE)
  
  items_keep_norm <- unique(normalize_id(ii_rows$item))
  item_cols <- col_map |>
    dplyr::filter(.data$item_norm %in% items_keep_norm) |>
    dplyr::pull(.data$orig) |>
    unique()
  
  if (!length(item_cols)) return(NULL)
  
  d |>
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))
    )
}

expected_score_cols_from_keys <- function(keys_obj, scales, total_override = c()) {
  scales <- unique(as.character(scales))
  scales <- scales[is_nonempty(scales)]
  
  expected_total <- tibble::tibble()
  if (!is.null(keys_obj$items_by_scale) && nrow(keys_obj$items_by_scale)) {
    expected_total <- keys_obj$items_by_scale %>%
      dplyr::filter(.data$scale %in% scales) %>%
      dplyr::mutate(
        level = "scale",
        subscale = NA_character_,
        col = if (length(total_override)) {
          dplyr::if_else(
            .data$scale %in% names(total_override),
            unname(total_override[.data$scale]),
            paste0("score_", safe_score_name(.data$scale))
          )
        } else {
          paste0("score_", safe_score_name(.data$scale))
        }
      ) %>%
      dplyr::select(.data$level, .data$scale, .data$subscale, .data$col)
  }
  
  expected_sub <- tibble::tibble()
  if (!is.null(keys_obj$items_by_subscale) && nrow(keys_obj$items_by_subscale)) {
    expected_sub <- keys_obj$items_by_subscale %>%
      dplyr::filter(.data$scale %in% scales) %>%
      dplyr::filter(is_nonempty(.data$subscale)) %>%
      dplyr::mutate(
        level = "subscale",
        col = paste0("score_", safe_score_name(.data$scale), "__", safe_score_name(.data$subscale))
      ) %>%
      dplyr::select(.data$level, .data$scale, .data$subscale, .data$col) %>%
      dplyr::distinct()
  }
  
  dplyr::bind_rows(expected_total, expected_sub) %>%
    dplyr::distinct()
}

export_existing_scores_tbl <- function(d_scored, id_col, keys_obj, scales,
                                       total_override = c(),
                                       dataset_label = NA_character_) {
  if (is.null(d_scored)) {
    stop("Score export requested, but scored data is NULL", call. = FALSE)
  }
  
  expected <- expected_score_cols_from_keys(
    keys_obj = keys_obj,
    scales = scales,
    total_override = total_override
  )
  
  if (!nrow(expected)) return(NULL)
  
  present <- intersect(expected$col, names(d_scored))
  
  missing_not_created <- expected %>%
    dplyr::filter(!(.data$col %in% present))
  
  if (nrow(missing_not_created)) {
    for (i in seq_len(nrow(missing_not_created))) {
      lbl <- if (missing_not_created$level[i] == "scale") {
        as.character(missing_not_created$scale[i])
      } else {
        paste0(missing_not_created$scale[i], " / ", missing_not_created$subscale[i])
      }
      message(
        "Score export: expected ", missing_not_created$level[i], " '", lbl,
        "' was not present in the scored master and will be omitted",
        if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
        "."
      )
    }
  }
  
  present_nonempty <- character(0)
  for (cc in present) {
    z <- suppressWarnings(as.numeric(d_scored[[cc]]))
    if (!all(is.na(z))) present_nonempty <- c(present_nonempty, cc)
  }
  
  present_nonempty <- unique(present_nonempty)
  
  if (!length(present_nonempty)) {
    message(
      "Score export: no non-missing score columns remained",
      if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
      "."
    )
    return(
      d_scored %>%
        dplyr::transmute(!!id_col := as.character(.data[[id_col]]))
    )
  }
  
  d_scored %>%
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(present_nonempty), ~ suppressWarnings(as.numeric(.x)))
    )
}


# -----------------------------
# Enrichment helpers
# -----------------------------
add_enriched_suffix <- function(path) {
  sub("\\.xlsx$", "_enriched.xlsx", path)
}

pick_first_col_norm <- function(cands, nms) {
  cand_norm <- normalize_id(cands)
  nms_norm <- normalize_id(nms)
  idx <- match(cand_norm, nms_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx)) nms[idx[1]] else NA_character_
}

normalize_group_patient_hc <- function(x) {
  x_chr <- as.character(x)
  z <- x_chr %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
  
  dplyr::case_when(
    z %in% c("hc", "healthy_control", "healthy_controls", "control", "controls") ~ "hc",
    z %in% c("patient", "patients", "pat", "clinical", "case", "cases", "pp", "chr", "ar", "at_risk") ~ "patient",
    TRUE ~ x_chr
  )
}

make_enrichment_tbl <- function(obj, ii_rows,
                                demographics_scale = CFG$enrichment_demographics_scale) {
  d <- obj$d
  id_col <- obj$id_col
  nms <- names(d)
  col_map <- tibble::tibble(orig = nms, item_norm = normalize_id(nms)) %>%
    dplyr::distinct(.data$item_norm, .keep_all = TRUE)
  
  demo_items_norm <- ii_rows %>%
    dplyr::filter(tolower(trimws(as.character(.data$scale))) == tolower(trimws(demographics_scale))) %>%
    dplyr::pull(.data$item) %>%
    normalize_id() %>%
    unique()
  
  demo_cols <- col_map %>%
    dplyr::filter(.data$item_norm %in% demo_items_norm) %>%
    dplyr::pull(.data$orig) %>%
    unique()
  
  age_col <- pick_first_col_norm(c("age_years", "age"), nms)
  date_col <- pick_first_col_norm(CFG$enrichment_date_candidates, nms)
  project_col <- pick_first_col_norm(c("project", "p"), nms)
  group_col <- pick_first_col_norm(CFG$enrichment_group_candidates, nms)
  
  out <- tibble::tibble(
    !!id_col := as.character(d[[id_col]]),
    age_years = if (!is.na(age_col)) suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
    date = if (!is.na(date_col)) as.character(d[[date_col]]) else NA_character_,
    project = if (!is.na(project_col)) as.character(d[[project_col]]) else NA_character_,
    group = if (!is.na(group_col)) normalize_group_patient_hc(d[[group_col]]) else NA_character_
  )
  
  # Keep demographics item columns from Item Information, but avoid duplicate canonical fields.
  blocked_norm <- normalize_id(c(id_col, "age", "age_years", "date", "datum", "project", "p", "group"))
  demo_cols <- demo_cols[!(normalize_id(demo_cols) %in% blocked_norm)]
  demo_cols <- setdiff(demo_cols, names(out))
  
  if (length(demo_cols)) {
    out <- dplyr::bind_cols(
      out,
      d %>% dplyr::select(dplyr::all_of(demo_cols))
    )
  }
  
  out
}

add_enrichment_to_tbl <- function(tab, obj, ii_rows) {
  if (is.null(tab) || !nrow(tab)) return(tab)
  enrich <- make_enrichment_tbl(obj, ii_rows)
  by_col <- obj$id_col
  
  if (!(by_col %in% names(tab))) {
    stop("Cannot enrich table for sample '", obj$sample, "': ID column '", by_col, "' is missing.", call. = FALSE)
  }
  
  # Do not create .x/.y duplicates if an export already contains one of these columns.
  duplicate_cols <- setdiff(intersect(names(enrich), names(tab)), by_col)
  if (length(duplicate_cols)) {
    enrich <- enrich %>% dplyr::select(-dplyr::all_of(duplicate_cols))
  }
  
  tab %>% dplyr::left_join(enrich, by = by_col)
}

make_enriched_sample_sheets <- function(x, masters, ii_rows,
                                        combined_sheet = "combined",
                                        z_scores = FALSE) {
  if (!isTRUE(CFG$export_enriched)) return(NULL)
  
  enriched <- list()
  for (obj in masters) {
    if (!is.null(x[[obj$sample]])) {
      enriched[[obj$sample]] <- add_enrichment_to_tbl(x[[obj$sample]], obj, ii_rows)
    }
  }
  
  make_export_sheets(enriched, combined_sheet = combined_sheet, z_scores = z_scores)
}

make_enriched_existing_sheets <- function(x, masters, ii_rows,
                                          combined_sheet = "combined") {
  if (!isTRUE(CFG$export_enriched)) return(NULL)
  
  out <- list()
  for (obj in masters) {
    if (!is.null(x[[obj$sample]])) {
      out[[obj$sample]] <- add_enrichment_to_tbl(x[[obj$sample]], obj, ii_rows)
    }
  }
  
  if (!is.null(x[[combined_sheet]])) {
    combined_parts <- list()
    for (obj in masters) {
      tab <- x[[combined_sheet]]
      if ("sample" %in% names(tab)) {
        tab <- tab %>% dplyr::filter(.data$sample == obj$sample)
      }
      combined_parts[[obj$sample]] <- add_enrichment_to_tbl(tab, obj, ii_rows)
    }
    out[[combined_sheet]] <- dplyr::bind_rows(combined_parts)
  }
  
  out
}

# -----------------------------
# HiTOP selection
# -----------------------------
hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)
ii_hitop <- NULL
if (isTRUE(CFG$export_hitop) && length(hitop_cols)) {
  mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
  ii_hitop <- ii[mapped_rows, , drop = FALSE]
}

# -----------------------------
# COMPLETE selection
# -----------------------------
ii_complete <- NULL
if (isTRUE(CFG$export_complete)) {
  ii_complete <- ii |> dplyr::filter(.data$scale %in% CFG$questionnaire_scales)
  if (!nrow(ii_complete)) {
    stop("COMPLETE export: No Item Information rows match CFG$questionnaire_scales.", call. = FALSE)
  }
}

# -----------------------------
# Unfiltered exports
# -----------------------------
if (isTRUE(CFG$export_hitop) && !is.null(ii_hitop) && nrow(ii_hitop)) {
  items_list  <- list()
  scores_list <- list()
  scales_hitop <- unique(ii_hitop$scale)
  
  for (obj in masters) {
    tab_items  <- export_items_tbl(obj$d, obj$id_col, ii_hitop)
    tab_scores <- export_existing_scores_tbl(
      obj$d_scored_full, obj$id_col, obj$keys, scales_hitop,
      total_override = CFG$score_total_override,
      dataset_label = paste0(obj$sample, " HiTOP")
    )
    
    if (!is.null(tab_items)) {
      tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
      items_list[[obj$sample]] <- tab_items
    }
    if (!is.null(tab_scores)) {
      tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
      scores_list[[obj$sample]] <- tab_scores
    }
  }
  
  out_items <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_items.xlsx"))
  writexl::write_xlsx(make_export_sheets(items_list), out_items)
  message("Wrote: ", out_items)
  
  if (isTRUE(CFG$export_enriched)) {
    out_items_enriched <- add_enriched_suffix(out_items)
    writexl::write_xlsx(make_enriched_sample_sheets(items_list, masters, ii), out_items_enriched)
    message("Wrote: ", out_items_enriched)
  }
  
  out_scores <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_subscales.xlsx"))
  writexl::write_xlsx(make_export_sheets(scores_list, z_scores = TRUE), out_scores)
  message("Wrote: ", out_scores)
  
  if (isTRUE(CFG$export_enriched)) {
    out_scores_enriched <- add_enriched_suffix(out_scores)
    writexl::write_xlsx(make_enriched_sample_sheets(scores_list, masters, ii, z_scores = TRUE), out_scores_enriched)
    message("Wrote: ", out_scores_enriched)
  }
}

if (isTRUE(CFG$export_complete) && !is.null(ii_complete) && nrow(ii_complete)) {
  items_list  <- list()
  scores_list <- list()
  scales_all <- unique(ii_complete$scale)
  
  for (obj in masters) {
    tab_items <- export_items_tbl(obj$d, obj$id_col, ii_complete)
    tab_scores <- export_existing_scores_tbl(
      obj$d_scored_full,
      obj$id_col,
      obj$keys,
      scales_all,
      total_override = CFG$score_total_override,
      dataset_label = paste0(obj$sample, " COMPLETE")
    )
    
    if (!is.null(tab_items)) {
      tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
      items_list[[obj$sample]] <- tab_items
    }
    if (!is.null(tab_scores)) {
      tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
      scores_list[[obj$sample]] <- tab_scores
    }
  }
  
  out_items_all <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_items.xlsx"))
  writexl::write_xlsx(make_export_sheets(items_list), out_items_all)
  message("Wrote: ", out_items_all)
  
  if (isTRUE(CFG$export_enriched)) {
    out_items_all_enriched <- add_enriched_suffix(out_items_all)
    writexl::write_xlsx(make_enriched_sample_sheets(items_list, masters, ii), out_items_all_enriched)
    message("Wrote: ", out_items_all_enriched)
  }
  
  out_scores_all <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_subscales.xlsx"))
  writexl::write_xlsx(make_export_sheets(scores_list, z_scores = TRUE), out_scores_all)
  message("Wrote: ", out_scores_all)
  
  if (isTRUE(CFG$export_enriched)) {
    out_scores_all_enriched <- add_enriched_suffix(out_scores_all)
    writexl::write_xlsx(make_enriched_sample_sheets(scores_list, masters, ii, z_scores = TRUE), out_scores_all_enriched)
    message("Wrote: ", out_scores_all_enriched)
  }
}

# -----------------------------
# Filtered exports:
# - sample sheets use sample-specific flags + sample-specific filtered scored masters
# - combined sheet uses combined flags + combined-filtered scored masters
# -----------------------------
if (isTRUE(CFG$export_loading_filtered)) {
  flag_helper <- latest_flag_helper(ROOT, combined_label, threshold_tag)
  
  if (is.na(flag_helper) || !fs::file_exists(flag_helper)) {
    message("No flagged-item helper found for filtered exports. Skipping filtered outputs.")
  } else {
    message("Flag helper: ", flag_helper)
    
    # -------------------------------------------------------------------------
    # HiTOP filtered exports
    # -------------------------------------------------------------------------
    if (isTRUE(CFG$export_hitop) && !is.null(ii_hitop) && nrow(ii_hitop)) {
      items_list  <- list()
      scores_list <- list()
      scales_hitop <- unique(ii_hitop$scale)
      
      # sample-specific sheets
      for (obj in masters) {
        if (is.null(obj$d_scored_filtered)) {
          stop(
            "Missing sample-specific filtered scored master for sample '",
            obj$sample, "'. Run Step 3 with export_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl <- read_flagged_items(
          flag_helper,
          dataset_label = obj$sample,
          threshold_value = CFG$loading_threshold
        )
        
        keys_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl)
        
        flagged_norm <- unique(normalize_id(flagged_tbl$item))
        ii_hitop_filt <- if (length(flagged_norm)) {
          ii_hitop %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm))
        } else {
          ii_hitop
        }
        
        tab_items <- export_items_tbl(obj$d, obj$id_col, ii_hitop_filt)
        tab_scores <- export_existing_scores_tbl(
          obj$d_scored_filtered,
          obj$id_col,
          keys_filt,
          scales_hitop,
          total_override = CFG$score_total_override,
          dataset_label = paste0(obj$sample, " HiTOP_", threshold_tag)
        )
        
        if (!is.null(tab_items)) {
          items_list[[obj$sample]] <- tab_items %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores)) {
          scores_list[[obj$sample]] <- tab_scores %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      # combined sheet
      combined_items_parts <- list()
      combined_scores_parts <- list()
      
      for (obj in masters) {
        if (is.null(obj$d_scored_combined)) {
          stop(
            "Missing combined-filtered scored master for sample '",
            obj$sample, "'. Run Step 3 with export_combined_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl_combined <- read_flagged_items(
          flag_helper,
          dataset_label = combined_label,
          threshold_value = CFG$loading_threshold
        )
        
        keys_combined_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl_combined)
        
        flagged_norm_combined <- unique(normalize_id(flagged_tbl_combined$item))
        ii_hitop_combined_filt <- if (length(flagged_norm_combined)) {
          ii_hitop %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm_combined))
        } else {
          ii_hitop
        }
        
        tab_items_combined <- export_items_tbl(obj$d, obj$id_col, ii_hitop_combined_filt)
        tab_scores_combined <- export_existing_scores_tbl(
          obj$d_scored_combined,
          obj$id_col,
          keys_combined_filt,
          scales_hitop,
          total_override = CFG$score_total_override,
          dataset_label = paste0(combined_label, " HiTOP_", threshold_tag)
        )
        
        if (!is.null(tab_items_combined)) {
          combined_items_parts[[obj$sample]] <- tab_items_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores_combined)) {
          combined_scores_parts[[obj$sample]] <- tab_scores_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      items_list[["combined"]]  <- dplyr::bind_rows(combined_items_parts)
      scores_list[["combined"]] <- dplyr::bind_rows(combined_scores_parts)
      
      out_items_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_items_{threshold_tag}.xlsx"))
      writexl::write_xlsx(items_list, out_items_f)
      message("Wrote: ", out_items_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_items_f_enriched <- add_enriched_suffix(out_items_f)
        writexl::write_xlsx(make_enriched_existing_sheets(items_list, masters, ii), out_items_f_enriched)
        message("Wrote: ", out_items_f_enriched)
      }
      
      out_scores_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_subscales_{threshold_tag}.xlsx"))
      scores_list <- purrr::map(scores_list, add_z_score_columns)
      writexl::write_xlsx(scores_list, out_scores_f)
      message("Wrote: ", out_scores_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_scores_f_enriched <- add_enriched_suffix(out_scores_f)
        writexl::write_xlsx(make_enriched_existing_sheets(scores_list, masters, ii), out_scores_f_enriched)
        message("Wrote: ", out_scores_f_enriched)
      }
    }
    
    # -------------------------------------------------------------------------
    # COMPLETE filtered exports
    # -------------------------------------------------------------------------
    if (isTRUE(CFG$export_complete) && !is.null(ii_complete) && nrow(ii_complete)) {
      items_list  <- list()
      scores_list <- list()
      scales_all <- unique(ii_complete$scale)
      
      # sample-specific sheets
      for (obj in masters) {
        if (is.null(obj$d_scored_filtered)) {
          stop(
            "Missing sample-specific filtered scored master for sample '",
            obj$sample, "'. Run Step 3 with export_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl <- read_flagged_items(
          flag_helper,
          dataset_label = obj$sample,
          threshold_value = CFG$loading_threshold
        )
        
        keys_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl)
        
        flagged_norm <- unique(normalize_id(flagged_tbl$item))
        ii_complete_filt <- if (length(flagged_norm)) {
          ii_complete %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm))
        } else {
          ii_complete
        }
        
        tab_items <- export_items_tbl(obj$d, obj$id_col, ii_complete_filt)
        tab_scores <- export_existing_scores_tbl(
          obj$d_scored_filtered,
          obj$id_col,
          keys_filt,
          scales_all,
          total_override = CFG$score_total_override,
          dataset_label = paste0(obj$sample, " COMPLETE_", threshold_tag)
        )
        
        if (!is.null(tab_items)) {
          items_list[[obj$sample]] <- tab_items %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores)) {
          scores_list[[obj$sample]] <- tab_scores %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      # combined sheet
      combined_items_parts <- list()
      combined_scores_parts <- list()
      
      for (obj in masters) {
        if (is.null(obj$d_scored_combined)) {
          stop(
            "Missing combined-filtered scored master for sample '",
            obj$sample, "'. Run Step 3 with export_combined_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl_combined <- read_flagged_items(
          flag_helper,
          dataset_label = combined_label,
          threshold_value = CFG$loading_threshold
        )
        
        keys_combined_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl_combined)
        
        flagged_norm_combined <- unique(normalize_id(flagged_tbl_combined$item))
        ii_complete_combined_filt <- if (length(flagged_norm_combined)) {
          ii_complete %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm_combined))
        } else {
          ii_complete
        }
        
        tab_items_combined <- export_items_tbl(obj$d, obj$id_col, ii_complete_combined_filt)
        tab_scores_combined <- export_existing_scores_tbl(
          obj$d_scored_combined,
          obj$id_col,
          keys_combined_filt,
          scales_all,
          total_override = CFG$score_total_override,
          dataset_label = paste0(combined_label, " COMPLETE_", threshold_tag)
        )
        
        if (!is.null(tab_items_combined)) {
          combined_items_parts[[obj$sample]] <- tab_items_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores_combined)) {
          combined_scores_parts[[obj$sample]] <- tab_scores_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      items_list[["combined"]]  <- dplyr::bind_rows(combined_items_parts)
      scores_list[["combined"]] <- dplyr::bind_rows(combined_scores_parts)
      
      out_items_all_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_items_{threshold_tag}.xlsx"))
      writexl::write_xlsx(items_list, out_items_all_f)
      message("Wrote: ", out_items_all_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_items_all_f_enriched <- add_enriched_suffix(out_items_all_f)
        writexl::write_xlsx(make_enriched_existing_sheets(items_list, masters, ii), out_items_all_f_enriched)
        message("Wrote: ", out_items_all_f_enriched)
      }
      
      out_scores_all_f <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_subscales_{threshold_tag}.xlsx"))
      scores_list <- purrr::map(scores_list, add_z_score_columns)
      writexl::write_xlsx(scores_list, out_scores_all_f)
      message("Wrote: ", out_scores_all_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_scores_all_f_enriched <- add_enriched_suffix(out_scores_all_f)
        writexl::write_xlsx(make_enriched_existing_sheets(scores_list, masters, ii), out_scores_all_f_enriched)
        message("Wrote: ", out_scores_all_f_enriched)
      }
    }
  }
}