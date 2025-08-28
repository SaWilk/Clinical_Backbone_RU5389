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

remove_test_rows <- function(df, name) {
  
  # List of explicit test IDs to remove
  bad_ids <- c(
    "123456", 
    "99998", 
    "79999"
  )
  
  before <- nrow(df)
  
  # helper: detect IDs that are all the same digit (≥3 digits)
  all_same_digit <- function(x, digit) {
    s <- trimws(as.character(x))
    grepl(paste0("^[", digit, "]{3,}$"), s)
  }
  
  # If data collection column exists, keep only main data collection rows
  if ("Datenerhebung...Data.collection." %in% names(df)) {
    df <- df %>%
      dplyr::filter(.data[["Datenerhebung...Data.collection."]] == "HaupterhebungMain data collection")
  }
  
  # Candidate participant ID column names (in order of preference)
  id_cols <- c(
    "vpid", 
    "Versuchspersonennummer.",
    "Versuchspersonen.ID...Participant.ID",
    "id",
    "vpid_1"
  )
  
  # Find the first matching column in df
  id_col <- intersect(id_cols, names(df))[1]
  
  if (!is.na(id_col)) {
    df <- df %>%
      dplyr::filter(
        !all_same_digit(.data[[id_col]], 9),                           # Remove IDs with all 9's
        !all_same_digit(.data[[id_col]], 1),                           # Remove IDs with all 1's
        !trimws(as.character(.data[[id_col]])) %in% bad_ids            # Remove explicit bad IDs
      )
  } else {
    warning(name, ": No matching participant ID column found. No rows removed for test IDs.")
  }
  
  after <- nrow(df)
  message(name, ": Removed ", before - after, " observations (non-main data + test IDs).")
  return(df)
}
