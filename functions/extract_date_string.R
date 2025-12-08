extract_date_string <- function(path) {
  b <- basename(path)
  d <- stringr::str_match(b, "ALL_(\\d{4}-\\d{2}-\\d{2})_")[,2]
  ifelse(is.na(d), format(Sys.Date(), "%Y-%m-%d"), d)
}