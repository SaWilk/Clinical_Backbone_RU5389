# -------------------------------------------------------------------------
# remove_test_rows()
#
# Purpose:
#   Removes non-main data collection rows and known "test" participant IDs
#   from a dataset.
#
# Behavior:
#   - If the column `Datenerhebung...Data.collection.` exists, only rows
#     with the value `"HaupterhebungMain data collection"` are kept.
#   - Identifies the participant ID column to use in a hierarchical order:
#       • vpid
#       • Versuchspersonennummer.
#       • Versuchspersonen.ID...Participant.ID
#       • id
#       • vpid_1
#   - Using the first available column from that list, the function removes:
#       • Rows with IDs consisting of only 9’s (≥3 digits).
#       • Rows with IDs consisting of only 1’s (≥3 digits).
#       • Rows where the ID matches one of the explicit "bad IDs" defined
#         at the top of the function (e.g., 123456, 99998, 79999).
#   - If no participant ID column is found, no rows are removed and a warning
#     is issued.
#
# Input:
#   df   : data frame to clean.
#   name : string, name of the dataset (used in messages).
#
# Output:
#   - Returns the input data frame with test/non-main rows removed.
#   - Prints a message with the number of rows removed.
#
# Example:
#   dat_adults <- remove_test_rows(dat_adults, "adults")
#
# -------------------------------------------------------------------------

remove_test_rows <- function(df, name, dat_general = NULL) {
  # List of explicit test IDs to remove
  bad_ids <- c("123456", "99998", "79999")
  
  before <- nrow(df)
  removed_dat_general <- 0L
  
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
  
  after <- nrow(df)
  message(name, ": Removed ", before - after, " observation(s) in total (dat_general tests + ID-based tests).")
  return(df)
}
