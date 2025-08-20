# Remove test observations and filter by main data collection if applicable
remove_test_rows <- function(df, name) {
  before <- nrow(df)
  
  # helper: detect IDs that are all the same digit (≥3 digits)
  all_same_digit <- function(x, digit) {
    s <- trimws(as.character(x))
    grepl(paste0("^[", digit, "]{3,}$"), s)
  }
  
  # List of explicit test IDs to remove
  bad_ids <- c("123456", "99998")
  
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
