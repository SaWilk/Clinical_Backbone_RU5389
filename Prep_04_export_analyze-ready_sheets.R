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
#   Rscript export_hitop_analysis_inputs.R --sample=c(adults, adolescents)
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
  samples = c("adults", "adolescents"),
  export_hitop = TRUE,
  export_complete = TRUE,
  questionnaire_scales = c("IDAS","CAPE","AQ","SUQ","ASRS-5","BISBAS","IUS","APS","TICS","CTQ","MAP-SR"),
  score_total_override = c(),
  clean_master = NULL,
  item_info = NULL
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

resolve_iteminfo_with_fallback <- function(root, sample, fallback_sample = "adults") {
  ii_path <- latest_iteminfo_for_sample(root, sample)
  if (!is.na(ii_path) && fs::file_exists(ii_path)) return(ii_path)
  
  fb <- latest_iteminfo_for_sample(root, fallback_sample)
  if (!is.na(fb) && fs::file_exists(fb)) {
    message("ItemInfo missing for sample '", sample, "'. Falling back to '", fallback_sample, "': ", fb)
    return(fb)
  }
  return(NA_character_)
}

# ---- Main --------------------------------------------------------------------
ROOT <- script_dir()
ARGS <- parse_args(commandArgs(trailingOnly = TRUE))

# accept legacy --sample=adults as well
if (is.null(ARGS$samples) && !is.null(ARGS$sample) && nzchar(ARGS$sample)) {
  ARGS$samples <- ARGS$sample
}

OUT_DIR <- fs::path(ROOT, "03_analysis_input")
fs::dir_create(OUT_DIR)

# Samples: allow CLI override --samples=adults,adolescents (optional)
samples_cli <- ARGS$samples %||% NA_character_
samples <- if (!is.na(samples_cli) && nzchar(samples_cli)) {
  strsplit(samples_cli, ",", fixed = TRUE)[[1]] |> trimws() |> tolower()
} else {
  CFG$samples |> tolower()
}

message("Samples:    ", paste(samples, collapse = ", "))
message("Output dir: ", OUT_DIR)
message("COMPLETE questionnaire scales: ", paste(CFG$questionnaire_scales, collapse = ", "))

# Resolve ItemInfo ONCE (use adults fallback if needed)
ITEM_INFO <- (ARGS$item_info %||% CFG$item_info) %||% resolve_iteminfo_with_fallback(ROOT, "adults", fallback_sample = "adults")
stopifnot(!is.na(ITEM_INFO), fs::file_exists(ITEM_INFO))
message("ItemInfo:   ", ITEM_INFO)

# Read Item Information
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
  master_path <- if (!is.null(cm) && !is.na(cm) && nzchar(cm)) cm else latest_clean_master_for_sample(ROOT, sample)
  if (is.na(master_path) || !fs::file_exists(master_path)) {
    stop("Missing clean master for sample '", sample, "'. Looked for: ", master_path, call. = FALSE)
  }
  d <- readr::read_csv2(master_path, show_col_types = FALSE, progress = FALSE) |> tibble::as_tibble()
  
  # ID column
  id_col <- pick_first(c("vp_id","vp","vpid","participant_id","participantid","id"), names(d))
  if (is.na(id_col)) {
    stop("Could not find an ID column in master for sample '", sample, "'.", call. = FALSE)
  }
  
  list(sample = sample, master_path = master_path, d = d, id_col = id_col)
}

masters <- purrr::map(samples, read_one_sample_master)

# -----------------------------
# Build stratification tables (one per sample + combined)
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

# Write stratification workbook with per-sample sheets + combined
out_strat <- fs::path(OUT_DIR, "stratification_info.xlsx")
writexl::write_xlsx(list(strat_all = strat_combined), out_strat)
message("Wrote: ", out_strat)

# -----------------------------
# Export builders (per sample)
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

export_scores_tbl <- function(d, id_col, scales, total_override = c()) {
  scales <- unique(as.character(scales))
  scales <- scales[is_nonempty(scales)]
  if (!length(scales)) return(NULL)
  
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
      tibble::tibble(scale = sc, subscale = subs, col = paste0("score_", sc_safe, "__", safe_score_name(subs)))
    } else {
      if (length(total_override) && sc %in% names(total_override)) {
        tibble::tibble(scale = sc, subscale = NA_character_, col = unname(total_override[[sc]]))
      } else {
        tibble::tibble(scale = sc, subscale = NA_character_, col = paste0("score_", sc_safe))
      }
    }
  })
  
  present_scores <- intersect(wanted$col, names(d))
  if (!length(present_scores)) return(NULL)
  
  d |>
    dplyr::transmute(
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(present_scores), ~ suppressWarnings(as.numeric(.x)))
    )
}

# -----------------------------
# HiTOP selection (from Item Info) once
# -----------------------------
hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)
ii_hitop <- NULL
if (isTRUE(CFG$export_hitop) && length(hitop_cols)) {
  mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
  ii_hitop <- ii[mapped_rows, , drop = FALSE]
}

# -----------------------------
# COMPLETE selection once
# -----------------------------
ii_complete <- NULL
if (isTRUE(CFG$export_complete)) {
  ii_complete <- ii |> dplyr::filter(.data$scale %in% CFG$questionnaire_scales)
  if (!nrow(ii_complete)) stop("COMPLETE export: No Item Information rows match CFG$questionnaire_scales.", call. = FALSE)
}

# -----------------------------
# Build multi-sheet workbooks
# -----------------------------
# Build per-sample tables then bind into ONE combined sheet each
items_list  <- list()
scores_list <- list()
scales_hitop <- unique(ii_hitop$scale)

for (obj in masters) {
  tab_items  <- export_items_tbl(obj$d, obj$id_col, ii_hitop)
  tab_scores <- export_scores_tbl(obj$d, obj$id_col, scales_hitop, total_override = CFG$score_total_override)
  
  if (!is.null(tab_items)) {
    tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
    items_list[[obj$sample]] <- tab_items
  }
  if (!is.null(tab_scores)) {
    tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
    scores_list[[obj$sample]] <- tab_scores
  }
}

hitop_items_combined  <- dplyr::bind_rows(items_list)
hitop_scores_combined <- dplyr::bind_rows(scores_list)

out_items <- fs::path(OUT_DIR, "HiTOP_items.xlsx")
writexl::write_xlsx(list(hitop_items = hitop_items_combined), out_items)
message("Wrote: ", out_items)

out_scores <- fs::path(OUT_DIR, "HiTOP_subscales.xlsx")
writexl::write_xlsx(list(hitop_subscales = hitop_scores_combined), out_scores)
message("Wrote: ", out_scores)

# COMPLETE items + scores
items_list  <- list()
scores_list <- list()
scales_all <- unique(ii_complete$scale)

for (obj in masters) {
  tab_items  <- export_items_tbl(obj$d, obj$id_col, ii_complete)
  tab_scores <- export_scores_tbl(obj$d, obj$id_col, scales_all, total_override = CFG$score_total_override)
  
  if (!is.null(tab_items)) {
    tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
    items_list[[obj$sample]] <- tab_items
  }
  if (!is.null(tab_scores)) {
    tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
    scores_list[[obj$sample]] <- tab_scores
  }
}

complete_items_combined  <- dplyr::bind_rows(items_list)
complete_scores_combined <- dplyr::bind_rows(scores_list)

out_items_all <- fs::path(OUT_DIR, "complete_items.xlsx")
writexl::write_xlsx(list(complete_items = complete_items_combined), out_items_all)
message("Wrote: ", out_items_all)

out_scores_all <- fs::path(OUT_DIR, "complete_subscales.xlsx")
writexl::write_xlsx(list(complete_subscales = complete_scores_combined), out_scores_all)
message("Wrote: ", out_scores_all)
