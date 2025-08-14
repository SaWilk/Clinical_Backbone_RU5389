# Special splitter for cognitive tests (column: "Projekt...Project")
separate_by_project_cog <- function(df, out_path = NULL, export_csv = FALSE) {
  # pick up global 'out_path' only if not provided
  if (is.null(out_path)) out_path <- get0("out_path", inherits = TRUE)
  if (is.null(out_path)) stop("Please define 'out_path' in your environment or pass it as an argument.")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  if (!"Projekt...Project" %in% names(df)) {
    stop("Input data frame must contain a column named 'Projekt...Project'.")
  }
  
  # Remove empty/testing project entries
  df <- df[!(df[["Projekt...Project"]] %in% c("", "Testing mode(No actual data collection)")), , drop = FALSE]
  
  # Split by the cognitive project label
  parts <- split(df, df[["Projekt...Project"]], drop = TRUE)
  proj_labels <- names(parts)
  
  # helper: define “empty” (0 rows OR all columns NA)
  is_empty_df <- function(x) {
    is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  }
  
  # derive a compact project id (strip leading "P" if present)
  get_pid <- function(lbl) {
    m <- regmatches(lbl, regexpr("^\\s*P(\\d+)", lbl, perl = TRUE))
    if (!is.na(m) && nzchar(m)) {
      return(sub("^P", "", m)) # remove leading "P"
    }
    # fallback: sanitize full label
    gsub("[^A-Za-z0-9]+", "_", lbl)
  }
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  sample_tag <- "general_info"
  
  for (lbl in proj_labels) {
    d <- parts[[lbl]]
    pid <- get_pid(lbl)
    
    # Only create env var if the split is not empty
    if (!is_empty_df(d)) {
      varname <- sprintf("data_%s_p_%s", sample_tag, pid)
      assign(varname, d, envir = .GlobalEnv)
      
      # Save to disk
      base <- sprintf("%s_%s_%s", pid, date_str, sample_tag)
      if (export_csv) {
        utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
      }
      writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
    }
  }
  
  invisible(TRUE)
}
