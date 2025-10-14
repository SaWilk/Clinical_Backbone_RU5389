# -------------------------------------------------------------------------
# remove_test_rows()
#
# Purpose:
#   Remove clear test/invalid rows from a dataset by:
#     (a) excluding participants flagged as "test" in a cross-dataset map,
#     (b) dropping records whose ID is all 9s/1s (≥3 digits) or in a fixed
#         blacklist, and
#     (c) filtering out rows with a start date older than a cutoff.
#
# Behavior:
#   - Participant ID detection:
#       • Uses the first matching column among:
#         {"vpid","Versuchspersonennummer.","Versuchspersonen.ID...Participant.ID","id","vpid_1"}.
#   - Cross-dataset “test” removal (optional):
#       • If `dat_general` is provided and has {vpid, project, datacollection},
#         removes rows whose (vpid, project) pair is marked as "test"
#         in `dat_general` (case-insensitive).
#       • If the current data lacks a project column, falls back to vpid-only match
#         and warns.
#   - ID heuristics:
#       • Removes IDs that are all the same digit (1 or 9) with length ≥3.
#       • Removes explicit test IDs in `bad_ids`.
#   - Date filter:
#       • If a date column is found among {"startdate","TIME_start"}, keep rows
#         where the parsed timestamp is NA (conservative keep) or ≥ cutoff
#         (default: "2024-08-01 00:00:00" UTC).
#       • Supports "YYYY-MM-DD HH:MM:SS" and "YYYY-MM-DD-HH-MM" formats.
#       • Warns if no date column is found.
#   - Messaging:
#       • Emits `message()` about how many rows were removed per step and in total.
#       • Emits `warning()` when required columns are missing for cross-checks.
#
# Input:
#   df           : data frame to clean.
#   name         : string, dataset label used in messages.
#   dat_general  : optional data frame with at least {vpid, project, datacollection}
#                  for cross-dataset "test" detection (case-insensitive).
#
# Output:
#   Returns:
#     data frame after removing rows by the rules above.
#
# Notes:
#   - Candidate project columns in `df` are one of {"project","Project","projekt","Projekt","p"}.
#   - Date parsing assumes UTC if no timezone present.
#   - Conservative choice for date parsing: rows with unparseable dates are kept.
#
# Example:
#   dat_clean <- remove_test_rows(df = dat_adults, name = "adults", dat_general = general_map)
#
# -------------------------------------------------------------------------

remove_test_rows <- function(df, name, dat_general = NULL) {
  # List of explicit test IDs to remove
  bad_ids <- c("123456", "99998", "79999")
  
  before <- nrow(df)
  
  # helper: detect IDs that are all the same digit (≥3 digits)
  all_same_digit <- function(x, digit) {
    s <- trimws(as.character(x))
    grepl(paste0("^[", digit, "]{3,}$"), s)
  }
  
  # Candidate participant ID column names (in order of preference)
  id_cols <- c(
    "vpid",
    "Versuchspersonennummer.",
    "Versuchspersonen.ID...Participant.ID",
    "id",
    "vpid_1"
  )
  
  # Find the first matching ID column in df
  id_col <- intersect(id_cols, names(df))[1]
  
  # --- Cross-check with dat_general (vpid + project where datacollection == "test") ---
  if (!is.null(dat_general)) {
    gnames <- names(dat_general)
    g_vpid_col <- gnames[tolower(gnames) == "vpid"][1]
    g_proj_col <- gnames[tolower(gnames) == "project"][1]
    g_dc_col   <- gnames[tolower(gnames) == "datacollection"][1]
    
    if (is.na(g_vpid_col) || is.na(g_proj_col) || is.na(g_dc_col)) {
      warning(name, ": dat_general missing one of required columns {vpid, project, datacollection}. Skipping dat_general check.")
    } else if (is.na(id_col)) {
      warning(name, ": No matching participant ID column found in df. Skipping dat_general check.")
    } else {
      test_map <- dat_general %>%
        dplyr::mutate(
          vpid_chr    = trimws(as.character(.data[[g_vpid_col]])),
          project_chr = trimws(as.character(.data[[g_proj_col]])),
          dc_chr      = tolower(trimws(as.character(.data[[g_dc_col]])))
        ) %>%
        dplyr::group_by(vpid_chr, project_chr) %>%
        dplyr::summarise(is_test = any(dc_chr == "test", na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(is_test) %>%
        dplyr::select(vpid_chr, project_chr) %>%
        dplyr::distinct()
      
      # Candidate project columns in df
      proj_candidates <- c("project", "Project", "projekt", "Projekt", "p")
      proj_col <- intersect(proj_candidates, names(df))[1]
      
      before_general <- nrow(df)
      if (!is.na(proj_col)) {
        # Strict match: vpid + project/p
        df <- df %>%
          dplyr::mutate(
            .join_vpid    = trimws(as.character(.data[[id_col]])),
            .join_project = trimws(as.character(.data[[proj_col]]))
          ) %>%
          dplyr::anti_join(test_map,
                           by = c(".join_vpid" = "vpid_chr", ".join_project" = "project_chr")) %>%
          dplyr::select(-.join_vpid, -.join_project)
      } else {
        # Fallback: match on vpid only
        warning(name, ": No 'project' or 'p' column found in df; falling back to vpid-only match against dat_general.")
        test_ids <- dplyr::distinct(test_map, vpid_chr)
        df <- df %>%
          dplyr::mutate(.join_vpid = trimws(as.character(.data[[id_col]]))) %>%
          dplyr::anti_join(test_ids, by = c(".join_vpid" = "vpid_chr")) %>%
          dplyr::select(-.join_vpid)
      }
      removed_dat_general <- before_general - nrow(df)
      if (removed_dat_general > 0) {
        message(name, ": Removed ", removed_dat_general, " observation(s) flagged as 'test' via dat_general.")
      }
    }
  }
  # --- END dat_general cross-check ---
  
  # If we have an ID column, remove all-9s/all-1s and explicit bad ids
  if (!is.na(id_col)) {
    df <- df %>%
      dplyr::filter(
        !all_same_digit(.data[[id_col]], 9),
        !all_same_digit(.data[[id_col]], 1),
        !trimws(as.character(.data[[id_col]])) %in% bad_ids
      )
  } else {
    warning(name, ": No matching participant ID column found. No rows removed for test IDs.")
  }
  
  # --- NEW: Remove rows with start date before 2024-08-01 ---
  date_candidates <- c("startdate", "TIME_start")
  date_col <- intersect(date_candidates, names(df))[1]
  cutoff <- as.POSIXct("2024-08-01 00:00:00", tz = "UTC")
  
  if (!is.na(date_col)) {
    # parse helper supporting:
    #  - "YYYY-MM-DD HH:MM:SS"
    #  - "YYYY-MM-DD-HH-MM"  -> convert to "YYYY-MM-DD HH:MM"
    raw <- trimws(as.character(df[[date_col]]))
    
    # try ymd_hms first
    parsed <- suppressWarnings(lubridate::ymd_hms(raw, quiet = TRUE, tz = "UTC"))
    
    # where NA, try the "YYYY-MM-DD-HH-MM" pattern
    need_alt <- is.na(parsed) & grepl("^\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}$", raw)
    if (any(need_alt)) {
      alt <- sub("^(\\d{4}-\\d{2}-\\d{2})-(\\d{2})-(\\d{2})$", "\\1 \\2:\\3", raw[need_alt])
      parsed[need_alt] <- suppressWarnings(lubridate::ymd_hm(alt, quiet = TRUE, tz = "UTC"))
    }
    
    # Keep rows with NA dates (can't judge) or >= cutoff
    before_date <- nrow(df)
    df <- df[ is.na(parsed) | parsed >= cutoff, , drop = FALSE ]
    removed_date <- before_date - nrow(df)
    if (removed_date > 0) {
      message(name, ": Removed ", removed_date, " observation(s) with start date before 2024-08-01 (using column '", date_col, "').")
    }
  } else {
    warning("\u26A0\uFE0F ", name, ": No 'startdate' or 'TIME_start' column found. Skipping date-based removal.")
  }
  # --- END date filter ---
  
  after <- nrow(df)
  message(name, ": Removed ", before - after, " observation(s) in total (dat_general tests + ID-based tests + date filter).")
  return(df)
}
