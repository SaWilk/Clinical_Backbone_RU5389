#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Export HiTOP-mapped items + (sub)scale score columns for factor analysis
# Date: 2025-12-17
#
# Assumes:
#   - Master file in:        ROOT/02_cleaned/<sample>/
#   - Item Information in:   ROOT/information/
#   - Output files to:       ROOT/03_analysis_input/
#
# Outputs:
#   A) <sample>_HiTOP_items.xlsx
#        - ONLY item columns that have ANY HiTOP mapping in Item Information
#        - No IDs/meta columns, no score columns
#   B) <sample>_HiTOP_subscales.xlsx
#        - For the SAME HiTOP-mapped scales as (A):
#            * if the scale has subscales -> include score_<scale>__<subscale> columns
#            * if the scale has zero subscales -> include score_<scale> (total scale) column
#        - Uses existing score_* columns already present in the master file
#
# Run:
#   Rscript export_hitop_analysis_inputs.R --sample=adults
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
  "readxl", "readr", "writexl", "janitor", "dplyr", "stringr", "tibble", "purrr", "fs", "glue"
))

# ---- Helpers -----------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x) && !is.na(x) && nzchar(x)) x else y

script_dir <- function() {
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
    stringr::str_replace_all("[^a-z0-9\\[\\]_]+", "")
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

# ---- Main --------------------------------------------------------------------
ROOT <- script_dir()
ARGS <- parse_args(commandArgs(trailingOnly = TRUE))

sample <- (ARGS$sample %||% "adults") |> tolower()
CLEAN_MASTER <- ARGS$clean_master %||% latest_clean_master_for_sample(ROOT, sample)
ITEM_INFO    <- ARGS$item_info    %||% latest_iteminfo_for_sample(ROOT, sample)

# Fixed output directory as requested
OUT_DIR <- fs::path(ROOT, "03_analysis_input")
fs::dir_create(OUT_DIR)

stopifnot(!is.na(CLEAN_MASTER), fs::file_exists(CLEAN_MASTER))
stopifnot(!is.na(ITEM_INFO), fs::file_exists(ITEM_INFO))

message("Sample: ", sample)
message("Master: ", CLEAN_MASTER)
message("ItemInfo: ", ITEM_INFO)
message("Output dir: ", OUT_DIR)

# Read master (your pipeline writes CSV2 => semicolon separated)
d <- readr::read_csv2(CLEAN_MASTER, show_col_types = FALSE, progress = FALSE) |> tibble::as_tibble()

# -----------------------------
# OUTPUT C: STRATIFICATION INFO
# -----------------------------
pick_first <- function(cands, nms) {
  hit <- cands[cands %in% nms]
  if (length(hit)) hit[1] else NA_character_
}

proj_col  <- pick_first(c("project", "p"), names(d))
age_col   <- pick_first(c("age_years", "age"), names(d))
gender_col<- pick_first(c("gender"), names(d))
group_col <- pick_first(c("group"), names(d))

missing_needed <- c(
  if (is.na(proj_col))  "project/p" else NULL,
  if (is.na(age_col))   "age_years/age" else NULL,
  if (is.na(gender_col))"gender" else NULL,
  if (is.na(group_col)) "group" else NULL
)

if (length(missing_needed)) {
  warning("Stratification export: missing columns in master: ",
          paste(missing_needed, collapse = ", "),
          call. = FALSE)
}

stratification_info <- tibble::tibble(
  project = if (!is.na(proj_col))  as.character(d[[proj_col]])  else NA_character_,
  age     = if (!is.na(age_col))   suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
  gender  = if (!is.na(gender_col))as.character(d[[gender_col]]) else NA_character_,
  group   = if (!is.na(group_col)) as.character(d[[group_col]]) else NA_character_
)

out_strat <- fs::path(OUT_DIR, "stratification_info.xlsx")
writexl::write_xlsx(list(stratification_info = stratification_info), out_strat)
message("Wrote: ", out_strat, "  (", ncol(stratification_info), " columns)")


# Read item info (robust to missing subscale column)
ii <- readxl::read_excel(ITEM_INFO) |>
  janitor::clean_names()

# make sure required columns exist
if (!"item" %in% names(ii))  stop("Item Information is missing an 'item' column after clean_names().")
if (!"scale" %in% names(ii)) stop("Item Information is missing a 'scale' column after clean_names().")
if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_

ii <- ii |>
  dplyr::mutate(
    item     = as.character(.data$item),
    scale    = as.character(.data$scale),
    subscale = as.character(.data$subscale)
  )

# Identify HiTOP mapping columns (after clean_names())
hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)

if (!length(hitop_cols)) {
  stop(
    "No HiTOP mapping columns found in Item Information. ",
    "Columns I see are: ", paste(names(ii), collapse = ", ")
  )
}


# Keep rows where ANY HiTOP column is non-empty
mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
ii_hitop <- ii[mapped_rows, , drop = FALSE] |>
  dplyr::filter(is_nonempty(.data$scale))

if (!nrow(ii_hitop)) stop("No HiTOP-mapped items found after filtering Item Information.")

# Build map master colname -> normalized
col_map <- tibble::tibble(
  orig = names(d),
  item_norm = normalize_id(names(d))
) |>
  dplyr::distinct(.data$item_norm, .keep_all = TRUE)

# --------------------
# OUTPUT A: ITEMS ONLY
# --------------------
items_keep_norm <- unique(normalize_id(ii_hitop$item))
item_cols <- col_map |>
  dplyr::filter(.data$item_norm %in% items_keep_norm) |>
  dplyr::pull(.data$orig) |>
  unique()

if (!length(item_cols)) stop("None of the HiTOP-mapped items were found as columns in the master file.")

d_items <- d |>
  dplyr::select(dplyr::all_of(item_cols)) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))

out_items <- fs::path(OUT_DIR, paste0(sample, "_HiTOP_items.xlsx"))
writexl::write_xlsx(list(hitop_items = d_items), out_items)
message("Wrote: ", out_items, "  (", ncol(d_items), " item columns)")

# ------------------------
# OUTPUT B: SUBSCALES ONLY
# ------------------------
scales_hitop <- ii_hitop |>
  dplyr::pull(.data$scale) |>
  as.character() |>
  unique()

# Determine subscales per scale using the full item info (not just HiTOP rows)
sub_tbl <- ii |>
  dplyr::filter(.data$scale %in% scales_hitop) |>
  dplyr::filter(is_nonempty(.data$subscale)) |>
  dplyr::distinct(.data$scale, .data$subscale)

wanted <- purrr::map_dfr(scales_hitop, function(sc) {
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
    tibble::tibble(
      scale = sc,
      subscale = NA_character_,
      col = paste0("score_", sc_safe)
    )
  }
}) |>
  dplyr::arrange(.data$scale, dplyr::if_else(is.na(.data$subscale), "", .data$subscale))

missing_scores <- setdiff(wanted$col, names(d))
if (length(missing_scores)) {
  warning(
    "These required score columns were not found in the master file and will be skipped:\n  - ",
    paste(missing_scores, collapse = "\n  - "),
    call. = FALSE
  )
}

present_scores <- intersect(wanted$col, names(d))
if (!length(present_scores)) {
  warning(
    "None of the requested score columns were found in the master file, so no subscale sheet will be written.",
    call. = FALSE
  )
} else {
  d_sub <- d |>
    dplyr::select(dplyr::all_of(present_scores)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  
  out_sub <- fs::path(OUT_DIR, paste0(sample, "_HiTOP_subscales.xlsx"))
  writexl::write_xlsx(list(hitop_subscales = d_sub), out_sub)
  message("Wrote: ", out_sub, "  (", ncol(d_sub), " score columns)")
}
