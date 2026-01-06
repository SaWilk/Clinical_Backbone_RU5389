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
# Also writes UNMAPPED-REMID warnings to the shared action log
# (logs/all_action_points.log), so the first script can later split them
# into project-specific log files.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean up R environment (safe in interactive sessions)
rm(list = ls())
if (interactive()) cat("\014")  # clear console only in interactive contexts

# install & load required packages --------------------------------------------
pkg <- c("dplyr", "readxl", "rstudioapi", "readr")
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

# === Shared logging (same as first script) ====================================
# We append to logs/all_action_points.log (root). The first script later
# removes *.log in project folders (NOT this file) and then splits this root log.
log_root_dir <- file.path(script_dir, "logs")
if (!dir.exists(log_root_dir)) dir.create(log_root_dir, recursive = TRUE, showWarnings = FALSE)
shared_log_path <- file.path(log_root_dir, "all_action_points.log")

# Try to use the same logger implementation as the first script.
# Fallback to simple line-append if the function file isn't available.
functions_dir <- file.path(script_dir, "functions")
logger <- NULL
try({
  if (file.exists(file.path(functions_dir, "setup_logging.R"))) {
    source(file.path(functions_dir, "setup_logging.R"))
    logger <- setup_logging(shared_log_path)
  }
}, silent = TRUE)

# Minimal fallback logger if setup_logging.R not available ---------------------
.log_line <- function(text) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", text)
  cat(line, "\n", file = shared_log_path, append = TRUE)
}

log_warn_unmapped <- function(project, sample, remid_raw, remid_canon, dataset_name, row_index, note = NULL) {
  msg <- sprintf(
    "UNMAPPED_REMID | project=%s | sample=%s | dataset=%s | row=%s | remid='%s' | canon='%s'%s",
    as.character(project %||% ""), as.character(sample %||% ""), as.character(dataset_name %||% ""),
    as.character(row_index %||% ""), as.character(remid_raw %||% ""), as.character(remid_canon %||% ""),
    if (!is.null(note) && nzchar(note)) paste0(" | note=", note) else ""
  )
  if (!is.null(logger) && is.list(logger) && !is.null(logger$log)) {
    # Best-effort call into your logger. Signature may differ, so we keep the message consolidated.
    # Many implementations accept: logger$log(level, project, sample, data_type, message)
    try(logger$log(level = "WARN", project = as.character(project %||% ""),
                   sample = as.character(sample %||% ""), data_type = "translate_remids", message = msg),
        silent = TRUE)
  } else {
    .log_line(msg)
  }
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ===== (kept) project8/9 special CSV log for "XXXXX" fallbacks ================
# (This stays where it was; it’s an additional, more detailed CSV log.)
p8p9_csv_log_dir <- file.path(script_dir, "01_project_data", "logs")
if (!dir.exists(p8p9_csv_log_dir)) dir.create(p8p9_csv_log_dir, recursive = TRUE, showWarnings = FALSE)

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
  # Strip ALL leading R/r characters (one or more)
  y <- gsub("R+", "", y)
  y
}

# ---- remidcheck sanity: only trust 5-digit numeric IDs (10000..99999) --------
numeric_part_remote <- function(x) {
  y <- canon_remote(x)               # strips leading R/r, trims, uppercases
  y <- trimws(as.character(y))
  ok <- grepl("^[0-9]+$", y)
  out <- suppressWarnings(as.numeric(y))
  out[!ok] <- NA_real_
  out
}

is_valid_5digit_remote <- function(x) {
  n <- numeric_part_remote(x)
  !is.na(n) & n >= 10000 & n <= 99999
}

# Choose the key for mapping:
# - default: remid
# - only if remid == "XXXXX" AND remidcheck is a valid 5-digit numeric ID -> use remidcheck
pick_key_for_mapping <- function(remid, remidcheck) {
  remid_chr <- as.character(remid)
  check_chr <- as.character(remidcheck)
  
  xmask <- !is.na(remid_chr) & toupper(trimws(remid_chr)) == "XXXXX"
  use_check <- xmask & !is_blank(check_chr) & is_valid_5digit_remote(check_chr)
  
  ifelse(use_check, check_chr, remid_chr)
}


require_cols <- function(df, cols, nm) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(sprintf("[%s] Missing required columns: %s", nm, paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

# Detect a project column if present (robust; mirrors style used elsewhere) ----
detect_project_col <- function(df) {
  cand <- c("project", "Projekt...Project", "Projekt.", "Projekt", "projekt", "p", "proj", "Proj")
  idx <- match(tolower(cand), tolower(names(df)))
  idx <- idx[!is.na(idx)]
  if (length(idx)) names(df)[idx[1]] else NULL
}

# Extract a best-effort project value for a given row --------------------------
# If no explicit project column exists, returns NA (still logs; split later may not attach).
get_project_value <- function(df, row_idx) {
  pc <- detect_project_col(df)
  if (is.null(pc)) return(NA)
  val <- df[[pc]][row_idx]
  if (is.null(val) || (length(val) == 1 && is.na(val))) return(NA)
  # Try to keep a simple "first-digit" or cleaned scalar
  v <- as.character(val)
  v <- trimws(v)
  if (grepl("^[0-9]+$", v)) return(v)
  # Common variants like "Project 9", "9.", "P9"
  if (grepl("([0-9]+)", v)) {
    return(sub(".*?([0-9]+).*", "\\1", v))
  }
  v
}

# load data --------------------------------------------------------------------
dat_children_parents <- readr::read_delim(
  file.path(in_path, file_children_parents),
  delim = ";",
  show_col_types = FALSE,
  locale = readr::locale(decimal_mark = ".")
)

dat_adults <- readr::read_delim(
  file.path(in_path, file_adults),
  delim = ";",
  show_col_types = FALSE,
  locale = readr::locale(decimal_mark = ".")
)
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
  remid_chr <- as.character(df$remid)
  check_chr <- as.character(df$remidcheck)
  
  # canonicalize BOTH sides (strip all r/R)
  remid_can  <- canon_remote(remid_chr)
  check_can  <- canon_remote(check_chr)
  
  xmask <- !is.na(remid_chr) & toupper(trimws(remid_chr)) == "XXXXX"
  both_present <- !is_blank(remid_chr) & !is_blank(check_chr)
  
  # NEW: only treat as a "real" mismatch if remidcheck looks like a plausible ID (5 digits)
  check_valid <- is_valid_5digit_remote(check_chr)
  
  # mismatch only if canonical values differ AND remidcheck is valid AND not XXXXX-row
  mismatch <- both_present & (trimws(remid_can) != trimws(check_can)) & !xmask & check_valid
  
  # NEW: log rows where remidcheck is present but invalid/out-of-range (does NOT block translation)
  invalid_check <- both_present & !xmask & !check_valid
  if (any(invalid_check)) {
    idx_bad <- which(invalid_check)
    for (i in idx_bad) {
      proj <- get_project_value(df, i)
      log_warn_unmapped(
        project      = proj,
        sample       = dataset_name,         # keep consistent with how you label logs elsewhere
        remid_raw    = check_chr[i],
        remid_canon  = canon_remote(check_chr[i]),
        dataset_name = dataset_name,
        row_index    = i,
        note         = sprintf("remidcheck invalid/out-of-range; ignored for mismatch gate. remid='%s' used.", remid_chr[i])
      )
    }
  }
  
  if (any(mismatch)) {
    idx <- which(mismatch)
    if (interactive()) {
      cat("\n⚠️ [", dataset_name, "] canonical remid vs remidcheck mismatches (", length(idx), "):\n", sep = "")
      for (i in idx) {
        cat("   ⚠️ row ", i,
            ": remid='", remid_chr[i], "' (→ ", remid_can[i], ")",
            ", remidcheck='", check_chr[i], "' (→ ", check_can[i], ") -> MANUAL CHECK NEEDED (skipped)\n", sep = "")
      }
    } else {
      message(sprintf("[ %s ] canonical mismatches: %s", dataset_name, paste(idx, collapse = ", ")))
    }
  }
  
  eligible <- !mismatch
  eligible
}


# Unmapped check (returns rows + values; also logs into shared log) ------------
# (Point 5 + canonical compare, extended to gather indices & project/sample)
find_unmapped_rows <- function(df, remote_ids_canon, eligible_rows, dataset_name, sample_label) {
  remid_chr <- as.character(df$remid)
  check_chr <- as.character(df$remidcheck)
  
  key <- pick_key_for_mapping(remid_chr, check_chr)
  
  use_mask   <- eligible_rows & !is_blank(key)
  candidates <- trimws(key)
  
  # validate AFTER stripping all r/R: require digits-only canonical form
  canon_all  <- canon_remote(candidates)
  keep       <- grepl("^[0-9]+$", canon_all)
  
  # row indices that are candidates
  idx_all <- which(use_mask & keep)
  
  # which canonical forms are NOT in the mapping
  canon_kept <- canon_all[idx_all]
  unmapped_mask <- !canon_kept %in% as.character(remote_ids_canon)
  idx_unmapped  <- idx_all[unmapped_mask]
  
  if (length(idx_unmapped) > 0) {
    for (i in idx_unmapped) {
      proj <- get_project_value(df, i)
      log_warn_unmapped(
        project    = proj,
        sample     = sample_label,
        remid_raw  = candidates[i],
        remid_canon= canon_remote(candidates[i]),
        dataset_name = dataset_name,
        row_index  = i,
        note       = "No mapping for canonical Remote_ID (digits only)."
      )
    }
  }
  
  # Return a data.frame with details for error reporting if needed
  if (length(idx_unmapped) == 0) {
    return(data.frame(row_index=integer(0), entered=character(0), canonical=character(0), project=character(0)))
  }
  data.frame(
    row_index = idx_unmapped,
    entered   = candidates[idx_unmapped],
    canonical = canon_remote(candidates[idx_unmapped]),
    project   = vapply(idx_unmapped, function(i) as.character(get_project_value(df, i) %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
}

# translate + log function (canonical join + richer logs) ----------------------
translate_and_log <- function(df, map_tbl, eligible_rows, dataset_name) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  xmask <- !is.na(remid_chr) & toupper(trimws(remid_chr)) == "XXXXX"
  
  # Use remidcheck only if it's a valid 5-digit numeric ID; otherwise stick with remid
  key <- pick_key_for_mapping(remid_chr, check_chr)
  df$key_for_mapping        <- key
  df$key_for_mapping_canon  <- canon_remote(key)
  
  # Join on canonical key
  df <- df |>
    dplyr::left_join(
      map_tbl |> dplyr::select(Participant_ID, Remote_ID_canon),
      by = c("key_for_mapping_canon" = "Remote_ID_canon"), 
      multiple = "all"
    )
  
  can_translate <- eligible_rows & !is_blank(df$key_for_mapping) & !is.na(df$Participant_ID)
  
  df$vpid[can_translate] <- df$Participant_ID[can_translate]
  
  df$remid[can_translate]      <- NA
  df$remidcheck[can_translate] <- NA
  
  # log for 'XXXXX' fallbacks (kept: CSV in 01_project_data/logs)
  used_check <- xmask & !is_blank(check_chr) & is_valid_5digit_remote(check_chr)
  used_check <- xmask & !is_blank(check_chr) & is_valid_5digit_remote(check_chr)
  x_rows <- which(used_check & eligible_rows)
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

unmapped_rows_children <- find_unmapped_rows(
  dat_children_parents, map_ids$Remote_ID_canon, eligible_cp,
  dataset_name = "children/parents", sample_label = "children_parents"
)

if (nrow(unmapped_rows_children) > 0) {
  if (interactive()) {
    cat("\n⚠️ [children/parents] remids without mapping (n=", nrow(unmapped_rows_children), "):\n", sep = "")
    print(unmapped_rows_children)
  } else {
    message(sprintf("[children/parents] remids without mapping (n=%d).", nrow(unmapped_rows_children)))
  }
  stop("⚠️ Unmapped remids detected (accepts digits or optional leading 'R'; uses remidcheck when 'XXXXX'). Please update the Excel mappings and re-run.")
}

res_cp <- translate_and_log(dat_children_parents, map_ids, eligible_cp, "children_parents")
dat_children_parents_out <- res_cp$df
log_cp <- res_cp$log

# PROCESS: adults --------------------------------------------------------------
eligible_ad <- flag_and_report_mismatches(dat_adults, "adults")

unmapped_rows_adults <- find_unmapped_rows(
  dat_adults, map_ids$Remote_ID_canon, eligible_ad,
  dataset_name = "adults", sample_label = "adults"
)

if (nrow(unmapped_rows_adults) > 0) {
  if (interactive()) {
    cat("\n⚠️ [adults] remids without mapping (n=", nrow(unmapped_rows_adults), "):\n", sep = "")
    print(unmapped_rows_adults)
  } else {
    message(sprintf("[adults] remids without mapping (n=%d).", nrow(unmapped_rows_adults)))
  }
  stop("⚠️ Unmapped remids detected (accepts digits or optional leading 'R'; uses remidcheck when 'XXXXX'). Please update the Excel mappings and re-run.")
}

res_ad <- translate_and_log(dat_adults, map_ids, eligible_ad, "adults")
dat_adults_out <- res_ad$df
log_ad <- res_ad$log

# Write Project 8/9 log for 'XXXXX' fallbacks (CSV; unchanged behavior) --------
log_all <- dplyr::bind_rows(log_cp, log_ad)
if (!is.null(log_all) && nrow(log_all) > 0) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(p8p9_csv_log_dir, paste0("project8_xxxxx_fallback_log_", ts, ".csv"))
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

# Close logger if available (harmless otherwise) -------------------------------
try({ if (!is.null(logger) && !is.null(logger$close)) logger$close() }, silent = TRUE)

if (interactive()) {
  cat("\n✅ Saved translated data to:\n",
      file.path(in_path, out_children_parents), "\n",
      file.path(in_path, out_adults), "\n", sep = "")
} else {
  message("Saved translated data to: ",
          file.path(in_path, out_children_parents), " ; ",
          file.path(in_path, out_adults))
}
