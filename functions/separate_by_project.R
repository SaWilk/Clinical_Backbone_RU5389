separate_by_project <- function(df, out_path = NULL) {
  # pick up global 'out_path' only if not provided
  if (is.null(out_path)) out_path <- get0("out_path", inherits = TRUE)
  
  if (is.null(out_path)) {
    stop("Please define 'out_path' in your environment or pass it as an argument.")
  }
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  if (!"Projekt." %in% names(df)) {
    stop("Input data frame must contain a column named 'Projekt.'.")
  }
  
  # infer sample from object name, e.g. dat_adults -> "adults"
  obj_name   <- deparse(substitute(df))
  sample_tag <- tolower(gsub("^dat_", "", obj_name))
  
  # split by project (only projects present in data will appear)
  parts <- split(df, df$Projekt., drop = TRUE)
  proj_ids <- names(parts)
  
  # helper: define “empty” (0 rows OR all columns NA)
  is_empty_df <- function(x) {
    is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  }
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  for (pid in proj_ids) {
    d <- parts[[pid]]
    
    # Always create the environment variable, even for 0/99 and even if empty
    varname <- sprintf("data_%s_p_%s", sample_tag, as.character(pid))
    assign(varname, d, envir = .GlobalEnv)
    
    # Skip file writing for test projects 0 and 99
    if (pid %in% c("0", "99")) next
    
    # Save only if non-empty
    if (!is_empty_df(d)) {
      base <- sprintf("%s_%s_%s", as.character(pid), date_str, sample_tag)
      utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
    }
  }
  
  invisible(TRUE)
}
