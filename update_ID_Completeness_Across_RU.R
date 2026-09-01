# ==============================================================================
# Update "ID Completeness Across RU.xlsx"
# ==============================================================================
#
# Purpose
# -------
# This script updates only QUEST and COG-TEST plus their related OLD-ID and TIME
# columns in the "Completeness Overview" sheet. It automatically uses the
# newest dated *_id_completeness_report.xlsx in "ids_in_all_projects".
#
# Processing rules
# ----------------
# * complete              -> QUEST = TRUE,  COG-TEST = TRUE
# * missing_questionnaire -> QUEST = FALSE, COG-TEST = TRUE
# * missing_cogtest       -> QUEST = TRUE,  COG-TEST = FALSE
# * Project 6 is excluded completely.
# * IDs from 80500 through 89999 are excluded completely.
# * Existing rows from these child samples are removed from the Overview and
#   never appear in MANUAL_CHECKS.
# * ID absent from the filtered report -> QUEST = FALSE, COG-TEST = FALSE
# * New report ID -> new row; other actively maintained modality columns are
#   initialized to FALSE
# * TIME is populated only for problematic IDs. An ID is problematic when
#   QUEST or COG-TEST is missing or ambiguous, when either modality was renamed,
#   or when Change Audit indicates a duplicate history.
# * TIME is stored at day level (yyyy-mm-dd), matching the existing Overview.
#   Change Audit is the primary source. If no matching audit row exists, the
#   unique report submitdate is used only where the established fallback rules
#   allow it.
# * Ambiguous values are never guessed. The field remains blank and the case is
#   described in MANUAL_CHECKS.
#
# The script creates a backup before every successful replacement.
# The Excel workbook must be closed while the script runs.
# ==============================================================================

# ---- Configuration -----------------------------------------------------------

ROOT_DIR <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389"

TARGET_FILE <- file.path(
  ROOT_DIR,
  "private_information",
  "ID Completeness Across RU.xlsx"
)

REPORT_DIR <- file.path(
  ROOT_DIR,
  "private_information",
  "ids_in_all_projects"
)

BACKUP_DIR <- file.path(
  ROOT_DIR,
  "private_information",
  "ID_Completeness_backups"
)

OVERVIEW_SHEET_WANTED <- "Completeness Overview"
AUDIT_SHEET_WANTED <- "Change Audit"
MANUAL_SHEET <- "MANUAL_CHECKS"

# Child samples that do not belong in this Overview.
EXCLUDED_PROJECTS <- 6L
PROJECT_8_CHILD_MIN_ID <- 80500
PROJECT_8_CHILD_MAX_ID <- 89999

# This identifier is printed at startup so the executed version is explicit.
SCRIPT_REVISION <- "2026-09-01_duplicate-time-hard-skip-v3"

# The 1.29 series supports the modern/threaded Excel comments and workbook
# relationships used by this workbook.
MIN_OPENXLSX2_VERSION <- "1.29.0"


# ---- Helper functions --------------------------------------------------------

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

require_package <- function(package, minimum_version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf(
      "Package '%s' is missing. Run once: install.packages(\"%s\")",
      package,
      package
    )
  }

  if (!is.null(minimum_version) &&
      utils::packageVersion(package) < numeric_version(minimum_version)) {
    stopf(
      paste0(
        "Package '%s' is too old (installed: %s; required: >= %s). ",
        "Update it once with: install.packages(\"%s\")"
      ),
      package,
      as.character(utils::packageVersion(package)),
      minimum_version,
      package
    )
  }
}

is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

normalize_token <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- gsub("[^a-z0-9]+", "_", out)
  gsub("^_+|_+$", "", out)
}

normalize_id <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }

  out <- rep(NA_character_, length(x))
  numeric_input <- is.numeric(x)

  if (any(numeric_input & !is.na(x))) {
    out[numeric_input & !is.na(x)] <- format(
      x[numeric_input & !is.na(x)],
      scientific = FALSE,
      trim = TRUE,
      digits = 15
    )
  }

  if (any(!numeric_input & !is.na(x))) {
    out[!numeric_input & !is.na(x)] <- trimws(as.character(x[!numeric_input & !is.na(x)]))
  }

  out <- sub("\\.0+$", "", out)
  out[is.na(out) | out == ""] <- NA_character_
  out
}

effective_project <- function(ids, projects) {
  ids <- normalize_id(ids)
  out <- suppressWarnings(as.integer(as.numeric(as.character(projects))))

  # If P is blank, the first digit of a numeric ID is a safety fallback.
  # An explicit project value always takes precedence.
  fallback <- rep(NA_integer_, length(ids))
  valid_id <- !is.na(ids) & grepl("^[0-9]+$", ids)
  fallback[valid_id] <- suppressWarnings(as.integer(substr(ids[valid_id], 1L, 1L)))
  out[is.na(out)] <- fallback[is.na(out)]
  out
}

is_excluded_id_range <- function(ids) {
  ids <- normalize_id(ids)
  id_number <- suppressWarnings(as.numeric(ids))

  # ID ranges are intentionally checked independently of P so excluded child
  # samples remain excluded even if P is blank or incorrect.
  project_6_id <- !is.na(ids) & grepl("^6[0-9]*$", ids)
  project_8_child_id <- !is.na(id_number) &
    id_number >= PROJECT_8_CHILD_MIN_ID &
    id_number <= PROJECT_8_CHILD_MAX_ID

  out <- project_6_id | project_8_child_id
  out[is.na(out)] <- FALSE
  out
}

is_excluded_population <- function(ids, projects) {
  ids <- normalize_id(ids)
  project <- effective_project(ids, projects)
  id_number <- suppressWarnings(as.numeric(ids))

  out <- !is.na(ids) & (
    project %in% EXCLUDED_PROJECTS |
      (project == 8L & !is.na(id_number) & id_number >= PROJECT_8_CHILD_MIN_ID) |
      is_excluded_id_range(ids)
  )
  out[is.na(out)] <- FALSE
  out
}

normalize_bool <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  z <- toupper(trimws(as.character(x)))
  out <- rep(NA, length(z))
  out[z %in% c("TRUE", "1")] <- TRUE
  out[z %in% c("FALSE", "0")] <- FALSE
  out
}

parse_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  z <- trimws(as.character(x))
  z[is.na(x) | z == ""] <- NA_character_

  out <- as.Date(rep(NA_character_, length(z)))
  formats <- c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%m/%d/%Y")

  # This Overview needs only the date component of datetime strings.
  z10 <- ifelse(is.na(z), NA_character_, substr(z, 1L, 10L))
  for (fmt in formats) {
    parsed <- suppressWarnings(as.Date(z10, format = fmt))
    take <- is.na(out) & !is.na(parsed)
    out[take] <- parsed[take]
  }

  # Also convert Excel serial dates if readxl returns them as text.
  numeric_text <- !is.na(z) & grepl("^[0-9]+(\\.[0-9]+)?$", z)
  take <- is.na(out) & numeric_text
  if (any(take)) {
    out[take] <- as.Date(
      suppressWarnings(as.numeric(z[take])),
      origin = "1899-12-30"
    )
  }

  out
}

first_non_missing_date <- function(...) {
  candidates <- list(...)
  if (length(candidates) == 0L) {
    return(as.Date(character()))
  }

  parsed <- lapply(candidates, parse_date_safe)
  out <- as.Date(rep(NA_character_, length(parsed[[1L]])))
  for (x in parsed) {
    take <- is.na(out) & !is.na(x)
    out[take] <- x[take]
  }
  out
}

unique_non_missing <- function(x) {
  unique(x[!is.na(x) & trimws(as.character(x)) != ""])
}

unique_dates <- function(x) {
  x <- parse_date_safe(x)
  vals <- unique(as.character(x[!is.na(x)]))
  as.Date(vals)
}

sort_ids <- function(ids, project_lookup = NULL) {
  ids <- unique(ids[!is.na(ids)])
  if (length(ids) <= 1L) {
    return(ids)
  }

  numeric_ids <- suppressWarnings(as.numeric(ids))
  numeric_ids[is.na(numeric_ids)] <- Inf

  if (is.null(project_lookup)) {
    return(ids[order(numeric_ids, ids)])
  }

  projects <- vapply(
    ids,
    function(id) {
      val <- project_lookup[[id]]
      if (is.null(val) || length(val) != 1L || is.na(val)) Inf else as.numeric(val)
    },
    numeric(1L)
  )

  ids[order(projects, numeric_ids, ids)]
}

resolve_name <- function(actual_names, wanted, what) {
  hit <- which(normalize_token(actual_names) == normalize_token(wanted))
  if (length(hit) != 1L) {
    stopf(
      "%s '%s' was not found uniquely. Available: %s",
      what,
      wanted,
      paste(actual_names, collapse = ", ")
    )
  }
  actual_names[hit]
}

resolve_columns <- function(actual_names, wanted, context) {
  actual_norm <- normalize_token(actual_names)
  wanted_norm <- normalize_token(wanted)
  idx <- match(wanted_norm, actual_norm)

  if (anyNA(idx)) {
    stopf(
      "Required columns are missing in %s: %s. Available: %s",
      context,
      paste(wanted[is.na(idx)], collapse = ", "),
      paste(actual_names, collapse = ", ")
    )
  }

  if (anyDuplicated(actual_norm[idx])) {
    stopf("Columns in %s are not named uniquely.", context)
  }

  stats::setNames(idx, wanted)
}

select_latest_report <- function(report_dir) {
  if (!dir.exists(report_dir)) {
    stopf("Report directory not found: %s", report_dir)
  }

  files <- list.files(
    report_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  files <- files[!grepl("^~\\$", basename(files))]
  files <- files[grepl(
    "id_completeness_report\\.xlsx$",
    basename(files),
    ignore.case = TRUE
  )]

  if (length(files) == 0L) {
    stopf(
      "No dated *_id_completeness_report.xlsx was found in %s.",
      report_dir
    )
  }

  tags <- regmatches(
    basename(files),
    regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", basename(files))
  )
  dates <- suppressWarnings(as.Date(tags, format = "%Y-%m-%d"))

  # Undated copies are intentionally ignored. Selection is based only on the
  # YYYY-MM-DD tag in the filename.
  dated <- !is.na(dates)
  files <- files[dated]
  dates <- dates[dated]

  if (length(files) == 0L) {
    stopf(
      "Report files exist, but none has a valid YYYY-MM-DD filename tag in %s.",
      report_dir
    )
  }

  latest_date <- max(dates)
  latest <- files[dates == latest_date]

  if (length(latest) != 1L) {
    stopf(
      paste0(
        "Several reports share the newest date tag %s. ",
        "Select the correct report manually: %s"
      ),
      as.character(latest_date),
      paste(basename(latest), collapse = ", ")
    )
  }

  normalizePath(latest, winslash = "/", mustWork = TRUE)
}

read_report_sheet <- function(report_file, sheet_name) {
  out <- tryCatch(
    readxl::read_excel(
      report_file,
      sheet = sheet_name,
      col_types = "text",
      na = c("", "NA", "N/A")
    ),
    error = function(e) {
      stopf(
        "Report sheet '%s' could not be read from %s: %s",
        sheet_name,
        report_file,
        conditionMessage(e)
      )
    }
  )

  wanted <- c("sample", "project", "id", "submitdate")
  idx <- resolve_columns(names(out), wanted, paste0("report sheet '", sheet_name, "'"))
  out <- as.data.frame(out[, unname(idx), drop = FALSE], stringsAsFactors = FALSE)
  names(out) <- wanted

  out$id <- normalize_id(out$id)
  out$sample <- trimws(as.character(out$sample))
  out$project <- suppressWarnings(as.integer(out$project))
  out$submitdate <- parse_date_safe(out$submitdate)
  out <- out[!is.na(out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

as_text_df <- function(x) {
  out <- lapply(x, function(col) {
    if (inherits(col, "POSIXt")) {
      ans <- format(col, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else if (inherits(col, "Date")) {
      ans <- as.character(col)
    } else {
      ans <- as.character(col)
    }
    ans[is.na(col)] <- "<NA>"
    ans
  })
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

write_vector <- function(wb, sheet, col, start_row, values) {
  if (length(values) == 0L) {
    return(wb)
  }

  x <- data.frame(value = values, check.names = FALSE)
  openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = x,
    dims = paste0(col, start_row),
    col_names = FALSE,
    row_names = FALSE,
    apply_cell_style = FALSE,
    remove_cell_style = FALSE,
    na = "_openxlsx_NULL"
  )
}

write_id_vector <- function(wb, sheet, col, start_row, values) {
  values <- normalize_id(values)
  numeric_mask <- !is.na(values) &
    grepl("^[0-9]{1,15}$", values) &
    !grepl("^0[0-9]+$", values)

  numeric_values <- rep(NA_real_, length(values))
  numeric_values[numeric_mask] <- as.numeric(values[numeric_mask])
  wb <- write_vector(wb, sheet, col, start_row, numeric_values)

  text_rows <- which(!is.na(values) & !numeric_mask)
  for (k in text_rows) {
    wb <- write_vector(
      wb,
      sheet,
      col,
      start_row + k - 1L,
      values[k]
    )
  }

  wb
}


# ---- Main program ------------------------------------------------------------

main <- function() {
  require_package("openxlsx2", MIN_OPENXLSX2_VERSION)
  require_package("readxl")

  options(
    openxlsx2.dateFormat = "yyyy-mm-dd",
    openxlsx2.datetimeFormat = "yyyy-mm-dd hh:mm:ss"
  )

  boundary_ids <- c("8518", "80000", "80499", "80500", "89999", "90000")
  boundary_expected <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  if (!identical(is_excluded_id_range(boundary_ids), boundary_expected)) {
    stopf("Internal error in the ID exclusion boundaries; no changes were made.")
  }

  if (!file.exists(TARGET_FILE)) {
    stopf("Target file not found: %s", TARGET_FILE)
  }

  report_file <- select_latest_report(REPORT_DIR)
  message("Script revision: ", SCRIPT_REVISION)
  message(
    "Hard exclusion active: Project 6 completely; IDs ",
    PROJECT_8_CHILD_MIN_ID, "-", PROJECT_8_CHILD_MAX_ID, " completely."
  )
  message("Report used: ", report_file)

  # The report is intentionally read with readxl so drawing-only or dangling
  # Excel relationships do not interfere with its data tables.
  report_sheets_actual <- readxl::excel_sheets(report_file)
  report_sheet_complete <- resolve_name(
    report_sheets_actual,
    "complete",
    "report sheet"
  )
  report_sheet_missing_q <- resolve_name(
    report_sheets_actual,
    "missing_questionnaire",
    "report sheet"
  )
  report_sheet_missing_c <- resolve_name(
    report_sheets_actual,
    "missing_cogtest",
    "report sheet"
  )

  report_complete <- read_report_sheet(report_file, report_sheet_complete)
  report_missing_q <- read_report_sheet(report_file, report_sheet_missing_q)
  report_missing_c <- read_report_sheet(report_file, report_sheet_missing_c)

  report_complete$source_sheet <- "complete"
  report_complete$quest_present <- TRUE
  report_complete$cog_present <- TRUE

  report_missing_q$source_sheet <- "missing_questionnaire"
  report_missing_q$quest_present <- FALSE
  report_missing_q$cog_present <- TRUE

  report_missing_c$source_sheet <- "missing_cogtest"
  report_missing_c$quest_present <- TRUE
  report_missing_c$cog_present <- FALSE

  report <- rbind(report_complete, report_missing_q, report_missing_c)
  report <- report[!is.na(report$id), , drop = FALSE]
  rownames(report) <- NULL

  # Excluded child samples are removed before status calculation, ID insertion,
  # or manual-check creation. They cannot be re-added or reported as missing.
  report_project_effective <- effective_project(report$id, report$project)
  report_id_number <- suppressWarnings(as.numeric(report$id))
  report_excluded_mask <- is_excluded_population(report$id, report$project)
  report_excluded_ids <- unique(report$id[report_excluded_mask])
  report_excluded_p6_ids <- unique(
    report$id[report_excluded_mask & report_project_effective == 6L]
  )
  report_excluded_p8_ids <- unique(
    report$id[report_excluded_mask & is_excluded_id_range(report$id) &
      report_id_number >= PROJECT_8_CHILD_MIN_ID]
  )
  report <- report[!report_excluded_mask, , drop = FALSE]
  rownames(report) <- NULL

  if (nrow(report) == 0L) {
    stopf(
      paste0(
        "After excluding Project 6 and IDs from 80500 through 89999, ",
        "the newest report contains no eligible IDs."
      )
    )
  }

  wb <- tryCatch(
    openxlsx2::wb_load(TARGET_FILE),
    error = function(e) {
      stopf(
        "The target file could not be loaded. Is it still open in Excel? %s",
        conditionMessage(e)
      )
    }
  )

  sheet_names <- openxlsx2::wb_get_sheet_names(wb)
  overview_sheet <- resolve_name(sheet_names, OVERVIEW_SHEET_WANTED, "worksheet")
  audit_sheet <- resolve_name(sheet_names, AUDIT_SHEET_WANTED, "worksheet")

  overview_raw <- openxlsx2::wb_to_df(
    wb,
    sheet = overview_sheet,
    cols = 1:15,
    col_names = TRUE,
    skip_empty_rows = FALSE,
    skip_empty_cols = FALSE,
    detect_dates = TRUE,
    check_names = FALSE
  )

  overview_wanted <- c(
    "ID", "P", "TASK", "TASK TIME", "ECG", "ECG_comments", "ECG TIME",
    "PUPIL", "PUPIL TIME", "QUEST", "QUEST TIME", "OLD QUEST ID",
    "COG-TEST", "COG-TEST TIME", "OLD COG-TEST ID"
  )
  overview_idx <- resolve_columns(
    names(overview_raw),
    overview_wanted,
    paste0("sheet '", overview_sheet, "'")
  )
  overview <- as.data.frame(
    overview_raw[, unname(overview_idx), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(overview) <- overview_wanted

  existing_ids_all <- normalize_id(overview$ID)
  nonblank_id_rows <- which(!is.na(existing_ids_all))
  if (length(nonblank_id_rows) == 0L) {
    stopf("No IDs were found in the Overview.")
  }

  # Work only through the last populated ID. Internal blank rows keep their
  # original positions.
  original_existing_data_rows <- max(nonblank_id_rows)
  overview <- overview[seq_len(original_existing_data_rows), , drop = FALSE]

  # Remove any excluded child-sample rows left by an older script or a manual
  # edit, then compact the retained rows without changing their order.
  overview_excluded_mask <- is_excluded_population(overview$ID, overview$P)
  excluded_overview_ids <- unique(normalize_id(overview$ID[overview_excluded_mask]))
  excluded_overview_ids <- excluded_overview_ids[!is.na(excluded_overview_ids)]
  excluded_overview_excel_rows <- which(overview_excluded_mask) + 1L
  overview <- overview[!overview_excluded_mask, , drop = FALSE]
  rownames(overview) <- NULL

  retained_nonblank_id_rows <- which(!is.na(normalize_id(overview$ID)))
  if (length(retained_nonblank_id_rows) > 0L) {
    overview <- overview[seq_len(max(retained_nonblank_id_rows)), , drop = FALSE]
  } else {
    overview <- overview[FALSE, , drop = FALSE]
  }

  existing_data_rows <- nrow(overview)
  existing_ids <- normalize_id(overview$ID)

  audit_raw <- openxlsx2::wb_to_df(
    wb,
    sheet = audit_sheet,
    col_names = TRUE,
    skip_empty_rows = FALSE,
    skip_empty_cols = FALSE,
    detect_dates = TRUE,
    check_names = FALSE
  )

  audit_wanted <- c(
    "action", "project", "sample", "data_type", "id_col", "old_id",
    "new_id", "criterion", "limesurvey_id", "startdate", "submitdate",
    "datestamp", "TIME_start", "TIME_end", "gender", "age_years"
  )
  audit_idx <- resolve_columns(
    names(audit_raw),
    audit_wanted,
    paste0("sheet '", audit_sheet, "'")
  )
  audit <- as.data.frame(
    audit_raw[, unname(audit_idx), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(audit) <- audit_wanted
  audit_before <- as_text_df(audit)

  audit$action_norm <- normalize_token(audit$action)
  audit$data_type_norm <- normalize_token(audit$data_type)
  audit$criterion_norm <- tolower(trimws(as.character(audit$criterion)))
  audit$old_id_norm <- normalize_id(audit$old_id)
  audit$new_id_norm <- normalize_id(audit$new_id)
  audit$current_id <- ifelse(
    !is.na(audit$new_id_norm),
    audit$new_id_norm,
    audit$old_id_norm
  )
  audit$modality <- ifelse(
    grepl("question", audit$data_type_norm),
    "QUEST",
    ifelse(grepl("experiment|cog", audit$data_type_norm), "COG-TEST", NA_character_)
  )
  audit$quest_day <- first_non_missing_date(
    audit$submitdate,
    audit$datestamp,
    audit$startdate
  )
  audit$cog_day <- first_non_missing_date(audit$TIME_start, audit$TIME_end)
  audit$duplicate_related <- grepl(
    "duplicat",
    paste(audit$action_norm, audit$criterion_norm),
    ignore.case = TRUE
  )
  audit$duplicate_related[is.na(audit$duplicate_related)] <- FALSE

  # Keep all report information together by ID so field-level conflicts remain
  # detectable.
  report_by_id <- split(report, report$id, drop = TRUE)
  report_info <- lapply(report_by_id, function(x) {
    q_values <- unique(x$quest_present)
    c_values <- unique(x$cog_present)
    projects <- unique_non_missing(x$project)
    samples <- unique_non_missing(x$sample)

    q_dates <- unique_dates(x$submitdate[x$quest_present %in% TRUE])
    c_report_dates <- unique_dates(
      x$submitdate[x$quest_present %in% FALSE & x$cog_present %in% TRUE]
    )

    row_details <- paste(
      paste0(
        x$source_sheet,
        " / sample=", x$sample,
        " / project=", x$project,
        " / date=", as.character(x$submitdate)
      ),
      collapse = " | "
    )

    list(
      quest = if (length(q_values) == 1L) q_values else NA,
      cog = if (length(c_values) == 1L) c_values else NA,
      quest_candidates = q_values,
      cog_candidates = c_values,
      projects = projects,
      samples = samples,
      quest_dates = q_dates,
      cog_report_dates = c_report_dates,
      details = row_details
    )
  })

  report_ids <- names(report_info)
  existing_unique_ids <- unique(existing_ids[!is.na(existing_ids)])

  project_lookup <- lapply(report_info, function(info) {
    if (length(info$projects) == 1L) info$projects else NA_integer_
  })

  new_ids <- setdiff(report_ids, existing_unique_ids)
  new_ids <- sort_ids(new_ids, project_lookup)

  all_row_ids <- c(existing_ids, new_ids)
  total_data_rows <- length(all_row_ids)
  excel_rows <- seq_len(total_data_rows) + 1L
  rows_by_id <- split(excel_rows[!is.na(all_row_ids)], all_row_ids[!is.na(all_row_ids)])
  unique_ids_all <- unique(all_row_ids[!is.na(all_row_ids)])

  quest_status <- rep(NA, total_data_rows)
  cog_status <- rep(NA, total_data_rows)

  for (i in seq_along(all_row_ids)) {
    id <- all_row_ids[i]
    if (is.na(id)) {
      next
    }

    info <- report_info[[id]]
    if (is.null(info)) {
      # The report is the union of available QUEST and COG data. If an Overview
      # ID is absent from it, both modalities are missing.
      quest_status[i] <- FALSE
      cog_status[i] <- FALSE
    } else {
      quest_status[i] <- info$quest
      cog_status[i] <- info$cog
    }
  }

  # Named maps for values later written to every target row of the same ID.
  old_quest_by_id <- setNames(rep(NA_character_, length(unique_ids_all)), unique_ids_all)
  old_cog_by_id <- old_quest_by_id
  quest_time_by_id <- setNames(as.Date(rep(NA_character_, length(unique_ids_all))), unique_ids_all)
  cog_time_by_id <- quest_time_by_id

  checks <- list()
  add_check <- function(id, field, problem, details = "", source = "") {
    # Final hard boundary: excluded IDs may never enter the check list, even
    # temporarily.
    if (length(id) == 1L && !is.na(id) && isTRUE(is_excluded_id_range(id))) {
      return(invisible(NULL))
    }

    target_rows <- rows_by_id[[id]]
    target_rows_text <- if (is.null(target_rows)) "no target row yet" else paste(target_rows, collapse = ", ")

    checks[[length(checks) + 1L]] <<- data.frame(
      ID = ifelse(is.na(id), "<missing>", id),
      Field = field,
      Problem = problem,
      Candidate_Details = details,
      Overview_Rows = target_rows_text,
      Source = source,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # IDs already duplicated in the Overview are not deleted. Safe values are
  # written to every matching row while manual consolidation remains visible.
  existing_counts <- table(existing_ids[!is.na(existing_ids)])
  duplicate_ids <- names(existing_counts[existing_counts > 1L])
  for (id in duplicate_ids) {
    add_check(
      id,
      "ID",
      "ID appears more than once in Completeness Overview.",
      paste0("Rows: ", paste(rows_by_id[[id]], collapse = ", ")),
      overview_sheet
    )
  }

  # Record report conflicts and project differences.
  for (id in report_ids) {
    info <- report_info[[id]]

    if (length(info$quest_candidates) != 1L) {
      add_check(
        id,
        "QUEST",
        "Conflicting QUEST status in the report; field remains blank.",
        info$details,
        basename(report_file)
      )
    }

    if (length(info$cog_candidates) != 1L) {
      add_check(
        id,
        "COG-TEST",
        "Conflicting COG-TEST status in the report; field remains blank.",
        info$details,
        basename(report_file)
      )
    }

    if (length(info$samples) > 1L) {
      add_check(
        id,
        "sample",
        "The same ID appears in multiple report samples; the Overview has no sample column.",
        paste(info$samples, collapse = " | "),
        basename(report_file)
      )
    }

    if (length(info$projects) != 1L) {
      add_check(
        id,
        "P",
        "Project assignment is ambiguous in the report.",
        paste(info$projects, collapse = " | "),
        basename(report_file)
      )
    }
  }

  # Project differences in existing rows are reported but not changed.
  existing_projects <- suppressWarnings(as.integer(overview$P))
  for (id in intersect(existing_unique_ids, report_ids)) {
    info <- report_info[[id]]
    if (length(info$projects) != 1L) {
      next
    }

    idx <- which(existing_ids == id)
    target_projects <- unique_non_missing(existing_projects[idx])
    if (length(target_projects) > 1L ||
        (length(target_projects) == 1L && target_projects != info$projects)) {
      add_check(
        id,
        "P",
        "The Overview project does not match the report uniquely; P remains unchanged.",
        paste0(
          "Overview: ", paste(target_projects, collapse = " | "),
          "; Report: ", paste(info$projects, collapse = " | ")
        ),
        paste(overview_sheet, basename(report_file), sep = " + ")
      )
    }
  }

  renamed_rows <- audit[
    !is.na(audit$action_norm) &
      audit$action_norm == "id_changed" &
      !is.na(audit$current_id) &
      !is.na(audit$modality),
    ,
    drop = FALSE
  ]
  renamed_ids <- unique(renamed_rows$current_id)

  duplicate_history_rows <- audit[
    audit$duplicate_related &
      !is.na(audit$current_id) &
      !is.na(audit$modality),
    ,
    drop = FALSE
  ]
  duplicate_history_ids <- unique(duplicate_history_rows$current_id)
  duplicate_history_ids <- duplicate_history_ids[
    !is_excluded_id_range(duplicate_history_ids)
  ]

  problem_by_id <- setNames(rep(FALSE, length(unique_ids_all)), unique_ids_all)

  for (id in unique_ids_all) {
    row_idx <- which(all_row_ids == id)
    q_values <- unique(quest_status[row_idx])
    c_values <- unique(cog_status[row_idx])
    q <- if (length(q_values) == 1L) q_values else NA
    c <- if (length(c_values) == 1L) c_values else NA

    problematic <- is.na(q) || is.na(c) || !isTRUE(q) || !isTRUE(c) ||
      id %in% renamed_ids || id %in% duplicate_history_ids
    problem_by_id[id] <- problematic

    if (!problematic) {
      next
    }

    # OLD IDs come only from genuine id_changed rows. Multiple distinct
    # candidates are intentionally not collapsed into an automatic value.
    old_q <- unique_non_missing(
      renamed_rows$old_id_norm[
        renamed_rows$current_id == id & renamed_rows$modality == "QUEST"
      ]
    )
    if (length(old_q) == 1L) {
      old_quest_by_id[id] <- old_q
    } else if (length(old_q) > 1L) {
      add_check(
        id,
        "OLD QUEST ID",
        "Multiple old questionnaire IDs map to the same current ID; field remains blank.",
        paste(old_q, collapse = " | "),
        audit_sheet
      )
    }

    old_c <- unique_non_missing(
      renamed_rows$old_id_norm[
        renamed_rows$current_id == id & renamed_rows$modality == "COG-TEST"
      ]
    )
    if (length(old_c) == 1L) {
      old_cog_by_id[id] <- old_c
    } else if (length(old_c) > 1L) {
      add_check(
        id,
        "OLD COG-TEST ID",
        "Multiple old cog-test IDs map to the same current ID; field remains blank.",
        paste(old_c, collapse = " | "),
        audit_sheet
      )
    }

    info <- report_info[[id]]

    # QUEST TIME: Change Audit is the primary source. The report submitdate is
    # used only when no non-deleted questionnaire row exists for the current ID.
    # Multiple audit dates remain ambiguous and are never hidden by a single
    # report row.
    if (isTRUE(q)) {
      audit_rows_for_q <- audit[
        !is.na(audit$current_id) &
          audit$current_id == id &
          !is.na(audit$modality) &
          audit$modality == "QUEST" &
          !is.na(audit$action_norm) &
          audit$action_norm != "deleted",
        ,
        drop = FALSE
      ]

      if (nrow(audit_rows_for_q) > 0L) {
        q_dates <- unique_dates(audit_rows_for_q$quest_day)
        q_source <- audit_sheet
      } else {
        q_dates <- if (is.null(info)) as.Date(character()) else info$quest_dates
        q_source <- basename(report_file)
      }

      if (length(q_dates) == 1L) {
        quest_time_by_id[id] <- q_dates
      } else if (length(q_dates) > 1L) {
        add_check(
          id,
          "QUEST TIME",
          "Multiple questionnaire dates are possible; field remains blank.",
          paste(as.character(q_dates), collapse = " | "),
          q_source
        )
      } else {
        add_check(
          id,
          "QUEST TIME",
          "QUEST is present, but no unique time source exists; field remains blank.",
          "",
          q_source
        )
      }
    }

    # COG-TEST TIME: Change Audit is also primary here. If there is no
    # non-deleted cog-test audit row and QUEST is missing, the date falls back
    # to submitdate from the missing_questionnaire report row.
    if (isTRUE(c)) {
      audit_rows_for_id <- audit[
        !is.na(audit$current_id) &
          audit$current_id == id &
          !is.na(audit$modality) &
          audit$modality == "COG-TEST" &
          !is.na(audit$action_norm) &
          audit$action_norm != "deleted",
        ,
        drop = FALSE
      ]

      if (nrow(audit_rows_for_id) > 0L) {
        c_dates <- unique_dates(audit_rows_for_id$cog_day)
        c_source <- audit_sheet
      } else if (isFALSE(q) && !is.null(info)) {
        c_dates <- info$cog_report_dates
        c_source <- basename(report_file)
      } else {
        c_dates <- as.Date(character())
        c_source <- audit_sheet
      }

      if (length(c_dates) == 1L) {
        cog_time_by_id[id] <- c_dates
      } else if (length(c_dates) > 1L) {
        add_check(
          id,
          "COG-TEST TIME",
          "Multiple cog-test dates are possible; field remains blank.",
          paste(as.character(c_dates), collapse = " | "),
          c_source
        )
      } else {
        add_check(
          id,
          "COG-TEST TIME",
          "COG-TEST is present, but no unique time source exists; field remains blank.",
          "",
          c_source
        )
      }
    }
  }

  # Output vectors by table row.
  old_quest_out <- rep(NA_character_, total_data_rows)
  old_cog_out <- rep(NA_character_, total_data_rows)
  quest_time_out <- as.Date(rep(NA_character_, total_data_rows))
  cog_time_out <- as.Date(rep(NA_character_, total_data_rows))

  for (i in seq_along(all_row_ids)) {
    id <- all_row_ids[i]
    if (is.na(id) || !isTRUE(problem_by_id[id])) {
      next
    }
    old_quest_out[i] <- old_quest_by_id[id]
    old_cog_out[i] <- old_cog_by_id[id]
    quest_time_out[i] <- quest_time_by_id[id]
    cog_time_out[i] <- cog_time_by_id[id]
  }

  # New non-numeric IDs are written as text. Add the exception now so it is
  # included in the MANUAL_CHECKS sheet created below.
  nonnumeric_new_ids <- new_ids[
    !grepl("^[0-9]{1,15}$", new_ids) | grepl("^0[0-9]+$", new_ids)
  ]
  for (id in nonnumeric_new_ids) {
    add_check(
      id,
      "ID",
      "New ID cannot be written numerically without loss and is stored as text.",
      id,
      basename(report_file)
    )
  }

  # Deduplicate checks, enforce exclusions again, and sort stably. The second
  # exclusion is intentionally redundant so future check rules can never expose
  # excluded IDs.
  if (length(checks) > 0L) {
    checks_df <- unique(do.call(rbind, checks))
    checks_df <- checks_df[
      !is_excluded_id_range(checks_df$ID),
      ,
      drop = FALSE
    ]
  } else {
    checks_df <- data.frame()
  }

  if (nrow(checks_df) > 0L) {
    check_numeric_id <- suppressWarnings(as.numeric(checks_df$ID))
    check_numeric_id[is.na(check_numeric_id)] <- Inf
    checks_df <- checks_df[order(check_numeric_id, checks_df$ID, checks_df$Field), , drop = FALSE]
    rownames(checks_df) <- NULL
  } else {
    checks_df <- data.frame(
      ID = "",
      Field = "",
      Problem = "No cases requiring manual review were detected.",
      Candidate_Details = "",
      Overview_Rows = "",
      Source = basename(report_file),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # ---- Update the workbook ---------------------------------------------------

  col_letter <- function(header) {
    openxlsx2::int2col(unname(overview_idx[header]))
  }

  excluded_overview_count <- length(excluded_overview_excel_rows)
  if (excluded_overview_count > 0L) {
    first_overview_col <- openxlsx2::int2col(min(unname(overview_idx)))
    last_overview_col <- openxlsx2::int2col(max(unname(overview_idx)))
    original_last_excel_row <- original_existing_data_rows + 1L

    # Clear values while preserving styles and comments, then rewrite all
    # retained rows compactly in their original order.
    wb <- openxlsx2::wb_clean_sheet(
      wb,
      sheet = overview_sheet,
      dims = paste0(
        first_overview_col, "2:", last_overview_col, original_last_excel_row
      ),
      numbers = TRUE,
      characters = TRUE,
      styles = FALSE,
      merged_cells = FALSE,
      hyperlinks = FALSE
    )

    identifier_headers <- c("ID", "OLD QUEST ID", "OLD COG-TEST ID")
    for (header in overview_wanted) {
      if (header %in% identifier_headers) {
        wb <- write_id_vector(
          wb,
          overview_sheet,
          col_letter(header),
          2L,
          overview[[header]]
        )
      } else {
        wb <- write_vector(
          wb,
          overview_sheet,
          col_letter(header),
          2L,
          overview[[header]]
        )
      }
    }
  }

  old_last_excel_row <- existing_data_rows + 1L
  style_source_excel_row <- if (existing_data_rows > 0L) old_last_excel_row else 2L
  new_count <- length(new_ids)

  if (new_count > 0L) {
    new_start_excel_row <- old_last_excel_row + 1L
    new_end_excel_row <- old_last_excel_row + new_count

    # Copy each column's style from the last existing data row.
    for (j in seq_along(overview_wanted)) {
      col <- openxlsx2::int2col(unname(overview_idx[j]))
      style <- openxlsx2::wb_get_cell_style(
        wb,
        sheet = overview_sheet,
        dims = paste0(col, style_source_excel_row)
      )
      wb <- openxlsx2::wb_set_cell_style(
        wb,
        sheet = overview_sheet,
        dims = paste0(col, new_start_excel_row, ":", col, new_end_excel_row),
        style = unname(style[1L])
      )
    }

    # Write IDs numerically where safe. Non-numeric IDs remain genuine text IDs
    # and are recorded as a visible manual exception.
    wb <- write_id_vector(
      wb,
      overview_sheet,
      col_letter("ID"),
      new_start_excel_row,
      new_ids
    )

    new_projects <- vapply(
      new_ids,
      function(id) {
        val <- project_lookup[[id]]
        if (is.null(val) || length(val) != 1L || is.na(val)) NA_integer_ else as.integer(val)
      },
      integer(1L)
    )
    wb <- write_vector(
      wb,
      overview_sheet,
      col_letter("P"),
      new_start_excel_row,
      new_projects
    )

    # For new IDs, set only other status columns already maintained in the
    # existing Overview to FALSE. Entirely unused columns remain blank.
    other_modalities <- c("TASK", "ECG", "PUPIL")
    active_other_modalities <- other_modalities[vapply(
      overview[other_modalities],
      function(x) any(!is_blank(x)),
      logical(1L)
    )]
    for (header in active_other_modalities) {
      wb <- write_vector(
        wb,
        overview_sheet,
        col_letter(header),
        new_start_excel_row,
        rep(FALSE, new_count)
      )
    }
  }

  # Rewrite QUEST, COG, and their four related fields entirely from the rules
  # above. Logical values remain genuine Excel booleans.
  wb <- write_vector(wb, overview_sheet, col_letter("QUEST"), 2L, quest_status)
  wb <- write_vector(wb, overview_sheet, col_letter("COG-TEST"), 2L, cog_status)
  wb <- write_vector(wb, overview_sheet, col_letter("QUEST TIME"), 2L, quest_time_out)
  wb <- write_id_vector(wb, overview_sheet, col_letter("OLD QUEST ID"), 2L, old_quest_out)
  wb <- write_vector(wb, overview_sheet, col_letter("COG-TEST TIME"), 2L, cog_time_out)
  wb <- write_id_vector(wb, overview_sheet, col_letter("OLD COG-TEST ID"), 2L, old_cog_out)

  last_excel_row <- total_data_rows + 1L
  wb <- openxlsx2::wb_add_numfmt(
    wb,
    sheet = overview_sheet,
    dims = paste0(col_letter("QUEST TIME"), "2:", col_letter("QUEST TIME"), last_excel_row),
    numfmt = "yyyy-mm-dd"
  )
  wb <- openxlsx2::wb_add_numfmt(
    wb,
    sheet = overview_sheet,
    dims = paste0(col_letter("COG-TEST TIME"), "2:", col_letter("COG-TEST TIME"), last_excel_row),
    numfmt = "yyyy-mm-dd"
  )

  # Recreate MANUAL_CHECKS on every run. Change Audit remains the second sheet
  # and its content is never modified.
  current_sheets <- openxlsx2::wb_get_sheet_names(wb)
  existing_manual <- which(normalize_token(current_sheets) == normalize_token(MANUAL_SHEET))
  if (length(existing_manual) > 0L) {
    wb <- openxlsx2::wb_remove_worksheet(wb, sheet = current_sheets[existing_manual[1L]])
  }
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = MANUAL_SHEET)
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = MANUAL_SHEET,
    x = checks_df,
    dims = "A1",
    col_names = TRUE,
    row_names = FALSE,
    with_filter = TRUE,
    na = "_openxlsx_NULL"
  )
  wb <- openxlsx2::wb_freeze_pane(wb, sheet = MANUAL_SHEET, first_row = TRUE)
  wb <- openxlsx2::wb_set_col_widths(
    wb,
    sheet = MANUAL_SHEET,
    cols = 1:6,
    widths = c(14, 22, 48, 70, 22, 38)
  )

  # ---- Save, reload, and validate --------------------------------------------

  dir.create(BACKUP_DIR, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  backup_file <- file.path(
    BACKUP_DIR,
    paste0("ID Completeness Across RU_before_", timestamp, ".xlsx")
  )

  if (!file.copy(TARGET_FILE, backup_file, overwrite = FALSE)) {
    stopf("Backup could not be created: %s", backup_file)
  }

  temp_output <- tempfile(
    pattern = "ID_Completeness_updated_",
    tmpdir = dirname(TARGET_FILE),
    fileext = ".xlsx"
  )
  on.exit(unlink(temp_output, force = TRUE), add = TRUE)

  openxlsx2::wb_save(wb, temp_output, overwrite = TRUE)

  check_wb <- tryCatch(
    openxlsx2::wb_load(temp_output),
    error = function(e) {
      stopf("The generated Excel file could not be reloaded: %s", conditionMessage(e))
    }
  )

  check_sheets <- openxlsx2::wb_get_sheet_names(check_wb)
  if (!all(c(overview_sheet, audit_sheet, MANUAL_SHEET) %in% check_sheets)) {
    stopf("Validation failed: expected worksheets are missing.")
  }

  check_manual_raw <- openxlsx2::wb_to_df(
    check_wb,
    sheet = MANUAL_SHEET,
    col_names = TRUE,
    skip_empty_rows = FALSE,
    skip_empty_cols = FALSE,
    detect_dates = TRUE,
    check_names = FALSE
  )
  check_manual_idx <- resolve_columns(
    names(check_manual_raw),
    c("ID", "Field", "Problem", "Candidate_Details", "Overview_Rows", "Source"),
    "MANUAL_CHECKS validation"
  )
  check_manual <- as.data.frame(
    check_manual_raw[, unname(check_manual_idx), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(check_manual) <- names(check_manual_idx)

  manual_excluded_mask <- is_excluded_id_range(check_manual$ID)
  if (any(manual_excluded_mask)) {
    stopf(
      paste0(
        "Validation failed: excluded IDs appear in ",
        "MANUAL_CHECKS: %s"
      ),
      paste(unique(normalize_id(check_manual$ID[manual_excluded_mask])), collapse = ", ")
    )
  }

  check_overview_raw <- openxlsx2::wb_to_df(
    check_wb,
    sheet = overview_sheet,
    cols = 1:15,
    col_names = TRUE,
    skip_empty_rows = FALSE,
    skip_empty_cols = FALSE,
    detect_dates = TRUE,
    check_names = FALSE
  )
  check_idx <- resolve_columns(
    names(check_overview_raw),
    overview_wanted,
    "updated Overview validation"
  )
  check_overview <- as.data.frame(
    check_overview_raw[, unname(check_idx), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(check_overview) <- overview_wanted

  check_ids <- normalize_id(check_overview$ID)
  check_q <- normalize_bool(check_overview$QUEST)
  check_c <- normalize_bool(check_overview[["COG-TEST"]])
  check_project <- effective_project(check_overview$ID, check_overview$P)

  check_excluded_mask <- is_excluded_population(check_overview$ID, check_overview$P)
  if (any(check_excluded_mask)) {
    remaining_excluded <- unique(check_ids[check_excluded_mask])
    remaining_excluded <- remaining_excluded[!is.na(remaining_excluded)]
    stopf(
      paste0(
        "Validation failed: excluded child-sample IDs remain in the Overview: %s"
      ),
      paste(remaining_excluded, collapse = ", ")
    )
  }

  missing_new <- setdiff(new_ids, check_ids)
  if (length(missing_new) > 0L) {
    stopf(
      "Validation failed: new IDs are missing after saving: %s",
      paste(missing_new, collapse = ", ")
    )
  }

  for (id in unique_ids_all) {
    expected_rows <- which(all_row_ids == id)
    actual_rows <- which(check_ids == id)
    if (length(actual_rows) != length(expected_rows)) {
      stopf(
        "Validation failed: ID %s has %d rows instead of the expected %d.",
        id,
        length(actual_rows),
        length(expected_rows)
      )
    }

    expected_q <- unique(quest_status[expected_rows])
    expected_c <- unique(cog_status[expected_rows])

    if (length(expected_q) == 1L) {
      if (is.na(expected_q)) {
        if (any(!is.na(check_q[actual_rows]))) {
          stopf("Validation failed: QUEST for ID %s should be blank.", id)
        }
      } else if (any(is.na(check_q[actual_rows])) ||
                 any(check_q[actual_rows] != expected_q)) {
        stopf("Validation failed: QUEST status for ID %s does not match.", id)
      }
    }

    if (length(expected_c) == 1L) {
      if (is.na(expected_c)) {
        if (any(!is.na(check_c[actual_rows]))) {
          stopf("Validation failed: COG-TEST for ID %s should be blank.", id)
        }
      } else if (any(is.na(check_c[actual_rows])) ||
                 any(check_c[actual_rows] != expected_c)) {
        stopf("Validation failed: COG-TEST status for ID %s does not match.", id)
      }
    }
  }

  check_audit_raw <- openxlsx2::wb_to_df(
    check_wb,
    sheet = audit_sheet,
    col_names = TRUE,
    skip_empty_rows = FALSE,
    skip_empty_cols = FALSE,
    detect_dates = TRUE,
    check_names = FALSE
  )
  check_audit_idx <- resolve_columns(
    names(check_audit_raw),
    audit_wanted,
    "Change Audit validation"
  )
  check_audit <- as.data.frame(
    check_audit_raw[, unname(check_audit_idx), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(check_audit) <- audit_wanted

  if (!identical(audit_before, as_text_df(check_audit))) {
    stopf("Validation failed: the content of 'Change Audit' changed.")
  }

  # Replace the target file only after validation succeeds.
  copied <- file.copy(temp_output, TARGET_FILE, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stopf(
      paste0(
        "The validated file could not be written to '%s'. ",
        "Check whether the workbook is still open in Excel. ",
        "The backup is available at: %s"
      ),
      TARGET_FILE,
      backup_file
    )
  }

  if (!identical(unname(tools::md5sum(temp_output)), unname(tools::md5sum(TARGET_FILE)))) {
    # Restore the original state from the validated backup if the copy is ever
    # incomplete.
    file.copy(backup_file, TARGET_FILE, overwrite = TRUE)
    stopf(
      "The copied file was not byte-identical; the backup was restored: %s",
      backup_file
    )
  }

  manual_count <- if (
    nrow(checks_df) == 1L &&
      identical(checks_df$Problem[1L], "No cases requiring manual review were detected.")
  ) 0L else nrow(checks_df)

  message("")
  message("Update completed successfully.")
  message("  Existing data rows:      ", existing_data_rows)
  message("  Removed from Overview:   ", excluded_overview_count)
  message(
    "  Report IDs excluded:      ", length(report_excluded_ids),
    " (P6: ", length(report_excluded_p6_ids),
    "; IDs 80500-89999: ", length(report_excluded_p8_ids), ")"
  )
  message("  New IDs added:            ", new_count)
  message("  QUEST = TRUE/FALSE:      ", sum(quest_status %in% TRUE, na.rm = TRUE),
          " / ", sum(quest_status %in% FALSE, na.rm = TRUE))
  message("  COG = TRUE/FALSE:        ", sum(cog_status %in% TRUE, na.rm = TRUE),
          " / ", sum(cog_status %in% FALSE, na.rm = TRUE))
  message(
    "  IDs with duplicate history: ",
    length(intersect(unique_ids_all, duplicate_history_ids))
  )
  project_counts <- vapply(
    2:9,
    function(project_number) {
      keep <- !is.na(check_ids) & !is.na(check_project) &
        check_project == project_number
      length(unique(check_ids[keep]))
    },
    integer(1L)
  )
  message(
    "  Unique IDs by project:     ",
    paste0("P", 2:9, "=", project_counts, collapse = "; ")
  )
  message("  Manual review cases:     ", manual_count)
  message("  Updated file:            ", TARGET_FILE)
  message("  Backup:                  ", backup_file)

  if (manual_count > 0L) {
    message("")
    warning(
      paste0(
        manual_count,
        " ambiguous or manually resolvable cases are listed in sheet '",
        MANUAL_SHEET,
        "'."
      ),
      call. = FALSE,
      immediate. = TRUE
    )
    print(checks_df, row.names = FALSE)
  }

  invisible(list(
    target_file = TARGET_FILE,
    report_file = report_file,
    backup_file = backup_file,
    excluded_report_ids = report_excluded_ids,
    removed_overview_ids = excluded_overview_ids,
    new_ids = new_ids,
    duplicate_history_ids = intersect(unique_ids_all, duplicate_history_ids),
    manual_checks = checks_df
  ))
}


main()
