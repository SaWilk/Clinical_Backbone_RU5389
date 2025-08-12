separate_by_project <- function(df, out_path = NULL, export_csv = FALSE) {
  # pick up global 'out_path' only if not provided
  if (is.null(out_path)) out_path <- get0("out_path", inherits = TRUE)
  if (is.null(out_path)) {
    stop("Please define 'out_path' in your environment or pass it as an argument.")
  }
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  
  # detect the project column: prefer 'Projekt.'; otherwise accept 'p'
  proj_col <- NULL
  var_prefix_extra <- ""
  if ("Projekt." %in% names(df)) {
    proj_col <- "Projekt."
  } else if ("p" %in% names(df)) {
    proj_col <- "p"
    var_prefix_extra <- "_psytool"
  } else {
    stop("Input data frame must contain a column named 'Projekt.' or 'p'.")
  }
  
  # normalize project IDs to character for consistent splitting/naming
  proj_vals <- df[[proj_col]]
  if (is.factor(proj_vals)) proj_vals <- as.character(proj_vals)
  if (is.numeric(proj_vals) || is.integer(proj_vals)) proj_vals <- as.character(proj_vals)
  
  # infer sample from object name, e.g. dat_adults -> "adults"
  obj_name   <- deparse(substitute(df))
  sample_tag <- tolower(gsub("^dat_", "", obj_name))
  
  # split by project (only projects present in data will appear)
  parts <- split(df, proj_vals, drop = TRUE)
  proj_ids <- names(parts)
  
  # helper: define “empty” (0 rows OR all columns NA)
  is_empty_df <- function(x) {
    is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  }
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  for (pid in proj_ids) {
    d <- parts[[pid]]
    
    # Create the environment variable name — add _psytool if using 'p'
    varname <- sprintf("data_%s%s_p_%s", sample_tag, var_prefix_extra, as.character(pid))
    assign(varname, d, envir = .GlobalEnv)
    
    # Skip file writing for test projects 0 and 99
    if (pid %in% c("0", "99")) next
    
    # Save only if non-empty
    if (!is_empty_df(d)) {
      base <- sprintf("%s_%s_%s", as.character(pid), date_str, sample_tag)
      if (export_csv) {
        utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
      }
      writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
    }
  }
  
  invisible(TRUE)
}
