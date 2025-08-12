# Remove test observations and filter by main data collection if applicable
remove_test_rows <- function(df, name) {
  before <- nrow(df)
  
  # helper: detect IDs that are all 9s (≥3 digits)
  all_nines <- function(x) {
    s <- trimws(as.character(x))
    grepl("^[9]{3,}$", s)
  }
  
  # If data collection column exists, keep only main data collection rows
  if ("Datenerhebung...Data.collection." %in% names(df)) {
    df <- df %>%
      dplyr::filter(.data[["Datenerhebung...Data.collection."]] == "HaupterhebungMain data collection")
  }
  
  # Candidate participant ID column names (in order of preference)
  id_cols <- c(
    "Versuchspersonennummer.",
    "Versuchspersonen.ID...Participant.ID",
    "id",
    "vpid_1"
  )
  
  # Find the first matching column in df
  id_col <- intersect(id_cols, names(df))[1]
  
  if (!is.na(id_col)) {
    df <- df %>% dplyr::filter(!all_nines(.data[[id_col]]))
  } else {
    warning(name, ": No matching participant ID column found. No rows removed for test IDs.")
  }
  
  after <- nrow(df)
  message(name, ": Removed ", before - after, " observations (non-main data + test IDs).")
  return(df)
}
