#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Export HiTOP-mapped + COMPLETE questionnaire-only inputs for factor analysis
# Date: 2026-02-12
#
# Assumes:
#   - Master file in:        ROOT/02_cleaned/<sample>/
#   - Item Information in:   ROOT/information/
#   - Output files to:       ROOT/03_analysis_input/
#
# Exports:
#   C) stratification_info.xlsx
#      - project, age, gender, group (from master)
#
#   HiTOP (Item Information-based):
#   A) <sample>_HiTOP_items.xlsx
#      - ONLY item columns that have ANY HiTOP mapping in Item Information
#   B) <sample>_HiTOP_subscales.xlsx
#      - score_* columns for the SAME HiTOP-mapped scales
#
#   COMPLETE (questionnaire-only, explicitly configured below):
#   D) <sample>_complete_items.xlsx
#      - ALL item columns belonging to CFG$questionnaire_scales
#   E) <sample>_complete_subscales.xlsx
#      - ALL score_* columns (subscales if present else total scale)
#        belonging to CFG$questionnaire_scales
#
# Exported questionnaire scales (COMPLETE):
#   - IDAS, CAPE, AQ, SUQ, ASRS-5, BISBAS, IUS, APS, TICS, CTQ, MAP-SR
#
# Run:
#   Rscript export_hitop_analysis_inputs.R --sample=adults
#   (or run in RStudio; recommended: source the file, not line-by-line)
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
  "readxl", "readr", "writexl", "janitor", "dplyr", "stringr", "tibble", "purrr", "fs", "glue", "rstudioapi"
))

# ---- CONFIG ------------------------------------------------------------------
CFG <- list(
  sample = c("adolescents"),
  
  # Toggle exports
  export_hitop    = TRUE,
  export_complete = TRUE,
  
  # COMPLETE: only these questionnaire scales (explicit allow-list)
  questionnaire_scales = c(
    "IDAS","CAPE","AQ","SUQ","ASRS-5","BISBAS","IUS","APS","TICS","CTQ","MAP-SR"
  ),
  
  # Optional: handle exceptional score column names if your master deviates
  # Names = scale in Item Information, values = explicit score column to use for TOTAL scale only.
  # Example: c("ASRS-5" = "score_asrs5")  # if your master uses score_asrs5 instead of score_asrs_5
  score_total_override = c(),
  
  # Paths (usually auto-resolved)
  clean_master = NULL,
  item_info    = NULL
)

# ---- Helpers -----------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x) && !is.na(x) && nzchar(x)) x else y

script_dir <- function() {
  # Prefer RStudio active document path (prevents "C:/Users/.../Documents" surprises)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  # Command line --file=
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

latest_file_by_pattern <- function(dir, pattern) {
  files_all <- fs::dir_ls(dir, type = "file", fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  files <- files[order(dts, decreasing = TRUE)]
  files[1]
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

normalize_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_replace_all("\\[|\\]", "") %>%  # remove brackets to match master
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

# ---- Main --------------------------------------------------------------------
ROOT <- script_dir()
ARGS <- parse_args(commandArgs(trailingOnly = TRUE))

sample <- (ARGS$sample %||% CFG$sample) |> tolower()
CLEAN_MASTER <- (ARGS$clean_master %||% CFG$clean_master) %||% latest_clean_master_for_sample(ROOT, sample)
ITEM_INFO    <- (ARGS$item_info    %||% CFG$item_info)    %||% latest_iteminfo_for_sample(ROOT, sample)

OUT_DIR <- fs::path(ROOT, "03_analysis_input")
fs::dir_create(OUT_DIR)

stopifnot(!is.na(CLEAN_MASTER), fs::file_exists(CLEAN_MASTER))
stopifnot(!is.na(ITEM_INFO), fs::file_exists(ITEM_INFO))

message("Sample:     ", sample)
message("Master:     ", CLEAN_MASTER)
message("ItemInfo:   ", ITEM_INFO)
message("Output dir: ", OUT_DIR)
message("COMPLETE questionnaire scales: ", paste(CFG$questionnaire_scales, collapse = ", "))

# Read master (CSV2 => semicolon separated)
d <- readr::read_csv2(CLEAN_MASTER, show_col_types = FALSE, progress = FALSE) |> tibble::as_tibble()

# ---- ID column (needed for EFA split-half alignment) -------------------------
# Prefer 'vp_id' exactly, but accept common alternatives
id_col <- pick_first(
  c("vp_id","vp","vpid","participant_id","participantid","id"),
  names(d)
)

if (is.na(id_col)) {
  stop(
    "Could not find an ID column in master. Expected one of: vp_id, vp, vpid, participant_id, participantid, id",
    call. = FALSE
  )
}

message("Using ID column: ", id_col)

# -----------------------------
# OUTPUT C: STRATIFICATION INFO
# -----------------------------
proj_col   <- pick_first(c("project", "p"), names(d))
age_col    <- pick_first(c("age_years", "age"), names(d))
gender_col <- pick_first(c("gender"), names(d))
group_col  <- pick_first(c("group"), names(d))

missing_needed <- c(
  if (is.na(proj_col))   "project/p" else NULL,
  if (is.na(age_col))    "age_years/age" else NULL,
  if (is.na(gender_col)) "gender" else NULL,
  if (is.na(group_col))  "group" else NULL
)

if (length(missing_needed)) {
  warning("Stratification export: missing columns in master: ",
          paste(missing_needed, collapse = ", "),
          call. = FALSE)
}

stratification_info <- tibble::tibble(
  !!id_col := as.character(d[[id_col]]),
  project  = if (!is.na(proj_col))   as.character(d[[proj_col]]) else NA_character_,
  age      = if (!is.na(age_col))    suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
  gender   = if (!is.na(gender_col)) as.character(d[[gender_col]]) else NA_character_,
  group    = if (!is.na(group_col))  as.character(d[[group_col]]) else NA_character_
)


out_strat <- fs::path(OUT_DIR, "stratification_info.xlsx")
writexl::write_xlsx(list(stratification_info = stratification_info), out_strat)
message("Wrote: ", out_strat, "  (", ncol(stratification_info), " columns)")

# -----------------------------
# Read Item Information
# -----------------------------
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

# Build map master colname -> normalized
col_map <- tibble::tibble(
  orig = names(d),
  item_norm = normalize_id(names(d))
) |>
  dplyr::distinct(.data$item_norm, .keep_all = TRUE)

# -----------------------------
# Export helpers
# -----------------------------
export_items_xlsx <- function(ii_rows, out_path, sheet_name = "items") {
  items_keep_norm <- unique(normalize_id(ii_rows$item))
  item_cols <- col_map |>
    dplyr::filter(.data$item_norm %in% items_keep_norm) |>
    dplyr::pull(.data$orig) |>
    unique()
  
  if (!length(item_cols)) {
    stop("No requested items were found as columns in the master file for: ", out_path, call. = FALSE)
  }
  
  # prepend ID column (kept with original name, e.g. vp_id)
  d_items <- d |>
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))
    )
  
  writexl::write_xlsx(setNames(list(d_items), sheet_name), out_path)
  message("Wrote: ", out_path, "  (", ncol(d_items), " columns incl. ID)")
}


export_scores_xlsx <- function(scales, out_path, sheet_name = "scores", total_override = c()) {
  scales <- unique(as.character(scales))
  scales <- scales[is_nonempty(scales)]
  if (!length(scales)) {
    warning("No scales provided for score export: ", out_path, call. = FALSE)
    return(invisible(NULL))
  }
  
  sub_tbl <- ii |>
    dplyr::filter(.data$scale %in% scales) |>
    dplyr::filter(is_nonempty(.data$subscale)) |>
    dplyr::distinct(.data$scale, .data$subscale)
  
  wanted <- purrr::map_dfr(scales, function(sc) {
    subs <- sub_tbl |>
      dplyr::filter(.data$scale == sc) |>
      dplyr::pull(.data$subscale) |>
      as.character() |>
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
  }) |>
    dplyr::arrange(.data$scale, dplyr::if_else(is.na(.data$subscale), "", .data$subscale))
  
  missing_scores <- setdiff(wanted$col, names(d))
  if (length(missing_scores)) {
    warning(
      "These requested score columns were not found in the master file and will be skipped:\n  - ",
      paste(missing_scores, collapse = "\n  - "),
      call. = FALSE
    )
  }
  
  present_scores <- intersect(wanted$col, names(d))
  if (!length(present_scores)) {
    warning("None of the requested score columns were found; not writing: ", out_path, call. = FALSE)
    return(invisible(NULL))
  }
  
  # prepend ID column (kept with original name, e.g. vp_id)
  d_sub <- d |>
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(present_scores), ~ suppressWarnings(as.numeric(.x)))
    )
  
  writexl::write_xlsx(setNames(list(d_sub), sheet_name), out_path)
  message("Wrote: ", out_path, "  (", ncol(d_sub), " columns incl. ID)")
}


# -----------------------------
# HiTOP exports (as before)
# -----------------------------
if (isTRUE(CFG$export_hitop)) {
  hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)
  
  if (!length(hitop_cols)) {
    warning(
      "No HiTOP mapping columns found in Item Information; HiTOP outputs will be skipped. ",
      "Columns I see are: ", paste(names(ii), collapse = ", "),
      call. = FALSE
    )
  } else {
    mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
    ii_hitop <- ii[mapped_rows, , drop = FALSE]
    
    if (nrow(ii_hitop)) {
      out_items <- fs::path(OUT_DIR, paste0(sample, "_HiTOP_items.xlsx"))
      export_items_xlsx(ii_hitop, out_items, sheet_name = "hitop_items")
      
      scales_hitop <- unique(ii_hitop$scale)
      out_sub <- fs::path(OUT_DIR, paste0(sample, "_HiTOP_subscales.xlsx"))
      export_scores_xlsx(scales_hitop, out_sub, sheet_name = "hitop_subscales", total_override = CFG$score_total_override)
    } else {
      warning("No HiTOP-mapped items found after filtering; HiTOP outputs skipped.", call. = FALSE)
    }
  }
}

# -----------------------------
# COMPLETE exports (questionnaire-only, explicit allow-list)
# -----------------------------
if (isTRUE(CFG$export_complete)) {
  ii_complete <- ii |>
    dplyr::filter(.data$scale %in% CFG$questionnaire_scales)
  
  if (!nrow(ii_complete)) {
    stop("COMPLETE export: No Item Information rows match CFG$questionnaire_scales. Check spelling/case.", call. = FALSE)
  }
  
  out_items_all <- fs::path(OUT_DIR, paste0(sample, "_complete_items.xlsx"))
  export_items_xlsx(ii_complete, out_items_all, sheet_name = "complete_items")
  
  scales_all <- unique(ii_complete$scale)
  out_scores_all <- fs::path(OUT_DIR, paste0(sample, "_complete_subscales.xlsx"))
  export_scores_xlsx(scales_all, out_scores_all, sheet_name = "complete_subscales", total_override = CFG$score_total_override)
}

message("Done.")
