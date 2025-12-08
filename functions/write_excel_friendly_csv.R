# Excel-friendly CSV writer (BOM + sep= + CRLF)
write_excel_friendly_csv <- function(df, path, delim = ";") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) file.remove(path)
  con <- file(path, open = "wb"); on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\xEF\xBB\xBF"), con)
  writeBin(charToRaw(paste0("sep=", delim, "\r\n")), con)
  write.table(df, file = con, sep = delim, row.names = FALSE, col.names = TRUE,
              na = "", qmethod = "double", eol = "\r\n", dec = ".")
}