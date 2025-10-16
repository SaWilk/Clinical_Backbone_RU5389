# -------------------------------------------------------------------------
# correct_child_vpids()
#
# Purpose:
#   Correct wrongly entered VP-IDs for a specific project using a mapping file.
#   Now supports:
#     - Customizable vpid/project column names
#     - Excluding rows from remapping based on questionnaire start date and vpid
#
# Exclusion rule:
#   If startdate > 2025-08-15 AND vpid > 80500 (and in target_project),
#   then do NOT remap that row and do NOT list it in the "no mapping found" message.
#
# Inputs:
#   dat             : data.frame with ID, project, and optionally start date columns
#   mapping_file    : path to Excel mapping
#   sheet           : sheet index or name
#   vpid_col        : name of the VP-ID column (default: "vpid")
#   project_col     : name of the project column (default: "project")
#   startdate_col   : name of the questionnaire start date column (default: "startdate")
#   target_project  : project code to correct (default: 8)
#   cutoff_date     : date threshold for exclusion (default: as.Date("2025-08-15"))
#   vpid_threshold  : vpid threshold for exclusion (default: 80500)
#
# Output:
#   Updated data.frame. Messages about duplicates, drops, unmapped, and skips.
#
# Example:
#   dat <- correct_child_vpids(
#     dat, vpid_col = "id", project_col = "p", startdate_col = "started_at"
#   )
# -------------------------------------------------------------------------

library(readxl)

correct_child_vpids <- function(
    dat,
    mapping_file = "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx",
    sheet = 1,
    vpid_col = "vpid",
    project_col = "project",
    startdate_col = "startdate",
    target_project = 8,
    cutoff_date = as.Date("2025-08-15"),
    vpid_threshold = 80500
) {
  # ---- Validate core columns ----
  required_data_cols <- c(vpid_col, project_col)
  missing_data_cols <- setdiff(required_data_cols, names(dat))
  if (length(missing_data_cols) > 0) {
    stop("Data is missing required column(s): ",
         paste(missing_data_cols, collapse = ", "),
         ". Expected columns for ID and project.")
  }
  
  # ---- Read & validate mapping ----
  map_raw <- read_excel(mapping_file, sheet = sheet, col_names = TRUE)
  required_cols <- c("Original VP-ID", "Neue Child VP-ID")
  missing_cols <- setdiff(required_cols, names(map_raw))
  if (length(missing_cols) > 0) {
    stop("Mapping file is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Expected columns: 'Original VP-ID' and 'Neue Child VP-ID'.")
  }
  
  map_df <- map_raw[, required_cols]
  names(map_df) <- c("original", "new")
  
  # Normalize mapping types
  map_df$original <- trimws(as.character(map_df$original))
  map_df$new      <- trimws(as.character(map_df$new))
  
  # Drop empty rows
  na_rows <- is.na(map_df$original) | map_df$original == "" |
    is.na(map_df$new)      | map_df$new == ""
  if (any(na_rows)) {
    warning("Dropped ", sum(na_rows), " mapping row(s) with empty/NA values.")
    map_df <- map_df[!na_rows, , drop = FALSE]
  }
  
  # Handle duplicate keys (keep last)
  if (any(duplicated(map_df$original))) {
    dup_keys <- unique(map_df$original[duplicated(map_df$original)])
    warning("Duplicate 'Original VP-ID' entries found for: ",
            paste(dup_keys, collapse = ", "),
            ". Using the last occurrence.")
    map_df <- map_df[rev(!duplicated(rev(map_df$original))), , drop = FALSE]
  }
  
  # Lookup vector
  map_vec <- setNames(map_df$new, map_df$original)
  
  # ---- Target project subset ----
  is_target <- dat[[project_col]] == target_project
  if (!any(is_target, na.rm = TRUE)) {
    message("No rows with ", project_col, " == ", target_project, "; nothing to update.")
    return(dat)
  }
  
  # ---- Build exclusion mask based on startdate & vpid ----
  exclude_by_rule <- rep(FALSE, nrow(dat))
  
  # Try to compute only if startdate_col exists
  if (startdate_col %in% names(dat)) {
    start_vals <- dat[[startdate_col]]
    
    # Coerce to Date safely
    start_dates <- tryCatch({
      if (inherits(start_vals, "Date")) {
        start_vals
      } else if (inherits(start_vals, "POSIXt")) {
        as.Date(start_vals)
      } else {
        as.Date(start_vals)  # attempt character/numeric coercion
      }
    }, warning = function(w) as.Date(start_vals, origin = "1970-01-01"),
    error   = function(e) rep(NA_real_, length(start_vals)))
    
    # Coerce vpid to numeric for comparison (NA if not numeric)
    vpid_num <- suppressWarnings(as.numeric(as.character(dat[[vpid_col]])))
    
    after_cutoff <- !is.na(start_dates) & (start_dates > as.Date(cutoff_date))
    over_thresh  <- !is.na(vpid_num)    & (vpid_num > vpid_threshold)
    
    exclude_by_rule <- is_target & after_cutoff & over_thresh
  } else {
    message("Column '", startdate_col, "' not found; skipping start-date/vpid threshold exclusion rule.")
  }
  
  # ---- Determine which rows are eligible for remapping ----
  eligible <- is_target & !exclude_by_rule
  
  vpid_chr <- as.character(dat[[vpid_col]])
  has_map  <- eligible & vpid_chr %in% names(map_vec)
  
  # ---- Apply replacements (eligible rows only) ----
  vpid_chr[has_map] <- unname(map_vec[vpid_chr[has_map]])
  
  # Restore type
  if (is.numeric(dat[[vpid_col]])) {
    dat[[vpid_col]] <- as.numeric(vpid_chr)
  } else {
    dat[[vpid_col]] <- vpid_chr
  }
  
  # ---- Reporting ----
  # Unmapped among eligible rows only (exclude rows skipped by rule)
  untouched <- unique(vpid_chr[eligible & !has_map])
  
  skipped_n <- sum(exclude_by_rule, na.rm = TRUE)
  if (skipped_n > 0) {
    message(
      "Skipped ", skipped_n,
      " row(s) from remapping because ", startdate_col, " > ",
      format(as.Date(cutoff_date), "%Y-%m-%d"),
      " and ", vpid_col, " > ", vpid_threshold, "."
    )
  }
  
  if (length(untouched) > 0) {
    message("These ", project_col, " == ", target_project,
            " VP-IDs were not altered (no mapping found): ",
            paste(untouched, collapse = ", "))
  } else {
    message("All eligible ", project_col, " == ", target_project, " VP-IDs were updated.")
  }
  
  dat
}
