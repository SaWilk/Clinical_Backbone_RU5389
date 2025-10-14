#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: translate remids
# Author: Saskia Wilken (saskia.wilken@uni-hamburg.de) 
# Creation date: 2025-08-26
#
# Notes on ID rules (doc-only updates per request):
# - remid: primary Remote_ID as entered by participant.
# - remidcheck: confirmation field; must match remid, EXCEPT when remid == "XXXXX".
# - "XXXXX" means remid was not entered manually (fallback to remidcheck for lookup).
# - Valid Remote_ID formats: numeric (e.g., 80519) OR "R" + digits (e.g., R70999).
# - Leading "R/r" is treated as OPTIONAL everywhere (data + mapping) via canonicalization.
# - Special-case mapping (kept as-is): Remote_ID in {"99","999","9999","99999","999999",
#   "9999999","R70999","10"} all map to Participant_ID "99999". This is intentional
#   and preserved; documented here for clarity.
#
# Description:
# Translates Remote_IDs in the children/parents and adults datasets
# into Participant_IDs using combined mapping tables from Project 8 and 9.
# Performs consistency checks, fixes known entry errors, applies mappings,
# logs “XXXXX” fallbacks, and exports cleaned CSVs for analysis.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean up R environment (safe in interactive sessions)
rm(list = ls())
if (interactive()) cat("\014")  # clear console only in interactive contexts

# install & load required packages --------------------------------------------
pkg <- c("dplyr", "readxl", "rstudioapi")
to_install <- pkg[!sapply(pkg, require, character.only = TRUE)]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkg, library, character.only = TRUE))

# number display (scoped) ------------------------------------------------------
.old_opts <- options(scipen = 999)
on.exit(options(.old_opts), add = TRUE)

# paths ------------------------------------------------------------------------
# Allow overriding script_dir via CLI: Rscript script.R /path/to/project
script_dir <- NULL
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  proposed <- args[1]
  if (dir.exists(proposed)) script_dir <- normalizePath(proposed)
}

if (is.null(script_dir)) {
  script_dir <- tryCatch(
    dirname(rstudioapi::getSourceEditorContext()$path),
    error = function(e) getwd()
  )
}

in_path      <- file.path(script_dir, "raw_data")
info_path    <- file.path(script_dir, "private_information")

# optional log dir for Project 8 special-case notes
log_dir <- file.path(script_dir, "01_project_data", "logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

file_children_parents <- "results-survey798916.csv"
file_adults           <- "results-survey564757.csv"

out_children_parents  <- "results-survey798916_remids_translated.csv"
out_adults            <- "results-survey564757_remids_translated.csv"

xlsx_file_p8   <- "2025-08-20_Remote-IDs_Projekt_8_Extended_SW.xlsx"
xlsx_file_p9   <- "2025-10-02_Remote-IDs_Projekt_9.xlsx"
xlsx_sheet     <- "Combined"

# utilities --------------------------------------------------------------------
is_blank <- function(x) {
  y <- trimws(as.character(x))
  is.na(y) | y == ""
}

# Canonicalize Remote_ID by stripping an optional single leading R/r
canon_remote <- function(x) {
  y <- toupper(trimws(as.character(x)))
  sub("^R([0-9]+)$", "\\1", y)
}

require_cols <- function(df, cols, nm) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(sprintf("[%s] Missing required columns: %s", nm, paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

# load data --------------------------------------------------------------------
dat_children_parents <- read.csv(file.path(in_path, file_children_parents),
                                 sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
dat_adults <- read.csv(file.path(in_path, file_adults),
                       sep = ";", stringsAsFactors = FALSE, check.names = FALSE)

# enforce required columns (Points 1 & 9) --------------------------------------
require_cols(dat_children_parents, c("remid", "remidcheck"), "children/parents")
require_cols(dat_adults,           c("remid", "remidcheck"), "adults")

# ---- LOAD BOTH MAPPING FILES (Project 8 + Project 9) ----
load_mapping <- function(xlsx_path, sheet) {
  if (!file.exists(xlsx_path)) stop("Mapping file not found: ", xlsx_path)
  sheets_available <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character(0))
  if (!(sheet %in% sheets_available)) {
    stop(sprintf("Mapping sheet '%s' not found in '%s'. Sheets present: %s",
                 sheet, xlsx_path, paste(sheets_available, collapse = ", ")))
  }
  readxl::read_excel(xlsx_path, sheet = sheet) |>
    dplyr::select(Participant_ID, Remote_ID) |>
    dplyr::mutate(
      Participant_ID = as.character(Participant_ID),
      Remote_ID      = as.character(Remote_ID)
    )
}

map_p8 <- load_mapping(file.path(info_path, xlsx_file_p8), xlsx_sheet)
map_p9 <- load_mapping(file.path(info_path, xlsx_file_p9), xlsx_sheet)

# combine, remove duplicates across both mappings
map_ids <- dplyr::bind_rows(map_p8, map_p9) |>
  dplyr::distinct(Remote_ID, .keep_all = TRUE)

# fix known data-entry errors --------------------------------------------------
# 1) Extend mapping with special cases (all map to vpid 99999)
#    (Doc-only clarification per Point 15; logic intentionally unchanged.)
special_map <- data.frame(
  Participant_ID = rep("99999", 8),
  Remote_ID      = c("99", "999", "9999", "99999", "999999", "9999999", "R70999", "10"),
  stringsAsFactors = FALSE
)
map_ids <- dplyr::bind_rows(map_ids, special_map) |>
  dplyr::distinct(Remote_ID, .keep_all = TRUE)

# ---- NORMALIZE MAPPING (optional leading R/r) & guard duplicates post-normalization
map_ids <- map_ids |>
  dplyr::mutate(Remote_ID_canon = canon_remote(Remote_ID))

dups <- map_ids |>
  dplyr::filter(!is_blank(Remote_ID_canon)) |>
  dplyr::count(Remote_ID_canon, name = "n") |>
  dplyr::filter(n > 1)

if (nrow(dups) > 0) {
  stop("Duplicate Remote_IDs after normalization (leading 'R' stripped). Please fix mapping: ",
       paste(dups$Remote_ID_canon, collapse = ", "))
}

# 2) Correct mis-typed remid (and remidcheck) in raw survey data (805199 → 80519)
fix_mistyped_both <- function(df) {
  if ("remid" %in% names(df)) {
    if (is.numeric(df$remid)) {
      df$remid[df$remid == 805199] <- 80519
    } else {
      df$remid[df$remid == "805199"] <- "80519"
    }
  }
  if ("remidcheck" %in% names(df)) {
    if (is.numeric(df$remidcheck)) {
      df$remidcheck[df$remidcheck == 805199] <- 80519
    } else {
      df$remidcheck[df$remidcheck == "805199"] <- "80519"
    }
  }
  df
}
dat_children_parents <- fix_mistyped_both(dat_children_parents)
dat_adults           <- fix_mistyped_both(dat_adults)

# Identity check before any mapping (except for the "XXXXX" rows) --------------
flag_and_report_mismatches <- function(df, dataset_name) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  
  xmask <- !is.na(remid_chr) & toupper(trimws(remid_chr)) == "XXXXX"
  both_present <- !is_blank(remid_chr) & !is_blank(check_chr)
  mismatch <- both_present & (trimws(remid_chr) != trimws(check_chr)) & !xmask
  
  if (any(mismatch)) {
    idx <- which(mismatch)
    if (interactive()) {
      cat("\n⚠️ [", dataset_name, "] remid vs remidcheck mismatch rows (", length(idx), "):\n", sep = "")
      for (i in idx) {
        cat("   ⚠️ row ", i, ": remid='", remid_chr[i], "', remidcheck='", check_chr[i], "' -> MANUAL CHECK NEEDED (skipped)\n", sep = "")
      }
    } else {
      message(sprintf("[ %s ] remid vs remidcheck mismatches: %s", dataset_name, paste(idx, collapse = ", ")))
    }
  }
  
  eligible <- !mismatch
  eligible
}

# Unmapped check (Point 5 + canonical compare) ---------------------------------
find_unmapped <- function(df, remote_ids_canon, eligible_rows) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  
  xmask      <- !is.na(remid_chr) & toupper(trimws(remid_chr)) == "XXXXX"
  key        <- ifelse(xmask & !is_blank(check_chr), check_chr, remid_chr)
  
  use_mask   <- eligible_rows & !is_blank(key)
  candidates <- trimws(key[use_mask])
  
  # Accept: digits OR optional leading R/r + digits
  valid_pat  <- grepl("^[0-9]+$", candidates) | grepl("^[Rr][0-9]+$", candidates)
  candidates <- candidates[valid_pat]
  
  # Compare canonically (strip optional 'R')
  canon <- canon_remote(candidates)
  unmapped_canon <- setdiff(canon, as.character(remote_ids_canon))
  
  # Return the ORIGINAL candidate strings whose canonical form is unmapped
  candidates[canon %in% unmapped_canon]
}

# translate + log function (canonical join + richer logs) ----------------------
translate_and_log <- function(df, map_tbl, eligible_rows, dataset_name) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  xmask      <- !is.na(remid_chr) & toupper(trimws(remid_chr)) == "XXXXX"
  
  key <- ifelse(xmask & !is_blank(check_chr), check_chr, remid_chr)
  df$key_for_mapping        <- key
  df$key_for_mapping_canon  <- canon_remote(key)
  
  # Join on canonical key
  df <- df |>
    dplyr::left_join(
      map_tbl |> dplyr::select(Participant_ID, Remote_ID_canon),
      by = c("key_for_mapping_canon" = "Remote_ID_canon")
    )
  
  can_translate <- eligible_rows & !is_blank(df$key_for_mapping) & !is.na(df$Participant_ID)
  
  df$vpid[can_translate] <- df$Participant_ID[can_translate]
  
  df$remid[can_translate]      <- NA
  df$remidcheck[can_translate] <- NA
  
  # richer log for 'XXXXX' fallbacks
  x_rows <- which(xmask & eligible_rows)
  log_df <- NULL
  if (length(x_rows) > 0) {
    id_cols_pref <- c("ResponseId","Response ID","id","ID","StartDate","EndDate","record_id")
    have_ids <- intersect(id_cols_pref, names(df))
    extra_ids <- if (length(have_ids)) df[x_rows, have_ids, drop = FALSE] else NULL
    
    base_log <- data.frame(
      dataset                      = dataset_name,
      row_index                    = x_rows,
      entered_key                  = df$key_for_mapping[x_rows],
      canonical_key_used_for_match = df$key_for_mapping_canon[x_rows],
      mapped_participant_ID        = df$Participant_ID[x_rows],
      stringsAsFactors = FALSE
    )
    log_df <- if (!is.null(extra_ids)) cbind(base_log, extra_ids) else base_log
  }
  
  df <- dplyr::select(df, -key_for_mapping, -key_for_mapping_canon, -Participant_ID)
  list(df = df, log = log_df)
}

# PROCESS: children/parents ----------------------------------------------------
eligible_cp <- flag_and_report_mismatches(dat_children_parents, "children/parents")

unmapped_children <- find_unmapped(dat_children_parents, map_ids$Remote_ID_canon, eligible_cp)
if (length(unmapped_children) > 0) {
  if (interactive()) {
    cat("\n⚠️ [children/parents] remids without mapping (n=", length(unmapped_children), "):\n", sep = "")
    print(unmapped_children)
  } else {
    message(sprintf("[children/parents] remids without mapping (n=%d).", length(unmapped_children)))
  }
  stop("⚠️ Unmapped remids detected (accepts digits or optional leading 'R'; uses remidcheck when 'XXXXX'). Please update the Excel mappings and re-run.")
}

res_cp <- translate_and_log(dat_children_parents, map_ids, eligible_cp, "children_parents")
dat_children_parents_out <- res_cp$df
log_cp <- res_cp$log

# PROCESS: adults --------------------------------------------------------------
eligible_ad <- flag_and_report_mismatches(dat_adults, "adults")

unmapped_adults <- find_unmapped(dat_adults, map_ids$Remote_ID_canon, eligible_ad)
if (length(unmapped_adults) > 0) {
  if (interactive()) {
    cat("\n⚠️ [adults] remids without mapping (n=", length(unmapped_adults), "):\n", sep = "")
    print(unmapped_adults)
  } else {
    message(sprintf("[adults] remids without mapping (n=%d).", length(unmapped_adults)))
  }
  stop("⚠️ Unmapped remids detected (accepts digits or optional leading 'R'; uses remidcheck when 'XXXXX'). Please update the Excel mappings and re-run.")
}

res_ad <- translate_and_log(dat_adults, map_ids, eligible_ad, "adults")
dat_adults_out <- res_ad$df
log_ad <- res_ad$log

# Write Project 8/9 log for 'XXXXX' fallbacks ----------------------------------
log_all <- dplyr::bind_rows(log_cp, log_ad)
if (!is.null(log_all) && nrow(log_all) > 0) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0("project8_xxxxx_fallback_log_", ts, ".csv"))
  write.table(log_all, file = log_file, sep = ";", dec = ".", row.names = FALSE, qmethod = "double")
  if (interactive()) {
    cat("\n⚠️ Project 8/9 'XXXXX' fallback log written to:\n", log_file, "\n")
  } else {
    message("Project 8/9 'XXXXX' fallback log written to: ", log_file)
  }
}

# save results -----------------------------------------------------------------
write.table(dat_children_parents_out,
            file = file.path(in_path, out_children_parents),
            sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

write.table(dat_adults_out,
            file = file.path(in_path, out_adults),
            sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

if (interactive()) {
  cat("\n✅ Saved translated data to:\n",
      file.path(in_path, out_children_parents), "\n",
      file.path(in_path, out_adults), "\n", sep = "")
} else {
  message("Saved translated data to: ",
          file.path(in_path, out_children_parents), " ; ",
          file.path(in_path, out_adults))
}
