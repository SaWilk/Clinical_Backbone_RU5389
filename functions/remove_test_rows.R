# Remove test observations and filter by main data collection if applicable
remove_test_rows <- function(df, name) {
  before <- nrow(df)
  
  # If data collection column exists, keep only main data collection rows
  if ("Datenerhebung...Data.collection." %in% names(df)) {
    df <- df %>%
      filter(.data[["Datenerhebung...Data.collection."]] == "HaupterhebungMain data collection")
  }
  
  # Remove test participants from the appropriate ID column
  if ("Versuchspersonennummer." %in% names(df)) {
    df <- df %>% filter(.data[["Versuchspersonennummer."]] != 99999)
  } else if ("Versuchspersonen.ID...Participant.ID" %in% names(df)) {
    df <- df %>% filter(.data[["Versuchspersonen.ID...Participant.ID"]] != 99999)
  } else {
    warning(name, ": No matching participant ID column found. No rows removed for test IDs.")
  }
  
  after <- nrow(df)
  message(name, ": Removed ", before - after, " observations (non-main data + test IDs).")
  return(df)
}
