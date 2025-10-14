# -------------------------------------------------------------------------
# collect_ids_to_excel()
#
# Purpose:
#   Collect unique IDs from one or more data frames and export them as
#   columns in a single Excel sheet. Optionally split each data frame’s IDs
#   by project, and stamp the file name with a retrieval date derived from
#   `meta_data[[ctime_col]]`.
#
# Behavior:
#   - Validates inputs:
#       * `meta_data` must be a data.frame and contain `ctime_col`.
#       * `...` must include at least one data frame; non–data-frame args
#         cause an error listing the offending arguments.
#   - Derives a retrieval date:
#       * Parses `meta_data[[ctime_col]]` with tolerant date parsing
#         (supports "YYYY-MM-DD", "DD.MM.YYYY", "MM/DD/YYYY", "YYYY/MM/DD",
#         otherwise falls back to `as.Date()` on the date portion).
#       * Picks the most frequent date (mode); on ties, uses the earliest.
#       * The formatted date (YYYY-MM-DD) is used in the output file name.
#   - For each data frame in `...`:
#       * If `id_col` is missing, it is skipped with a warning.
#       * Drops NA IDs and de-duplicates, treating IDs as numeric values.
#       * If `project_col` is NULL or absent: creates one output column with
#         all unique IDs for that data frame.
#       * Else: splits by `project_col` (dropping NA projects) and creates
#         one output column per project (column name: "<df>_<project>").
#   - Builds a rectangular data frame by padding shorter ID columns with NA.
#   - Writes an Excel file to "<out_dir>/<DATE>_<out_basename>_<data_type>.xlsx"
#       * One sheet named "ids", frozen header row, auto column widths.
#       * Requires package `openxlsx`.
#
# Inputs:
#   meta_data     : data.frame containing at least `ctime_col`.
#   ...           : one or more data frames to harvest IDs from.
#   id_col        : character scalar, name of the ID column (default: "vpid").
#   project_col   : character scalar or NULL; name of a project column used
#                   to split IDs into separate columns (default: "project").
#   ctime_col     : character scalar; name of the date/time column in
#                   `meta_data` used to derive the retrieval date (default: "ctime").
#   data_type     : character scalar used in the output file name
#                   (default: "questionnaire").
#   out_basename  : character scalar base name for the output file
#                   (default: "ids_in_all_projects").
#   out_dir       : character scalar directory to write the Excel file to.
#                   Defaults to "<script_dir>/private_information" to match
#                   previous behavior when NULL.
#
# Output:
#   - Invisibly returns the output file path (character).
#   - Writes the Excel file to disk.
#   - Emits messages/warnings for skipped data frames, missing columns, and
#     the final file path.
#
# Notes:
#   - Requires R ≥ 4.1 for the base pipe (`|>`). Replace pipes with nested
#     calls for older R versions.
#   - IDs are treated as numeric; non-numeric values become NA and are dropped.
#
# Example:
#   file <- collect_ids_to_excel(
#     meta_data = meta_df,
#     df_a, df_b,           # multiple data frames via `...`
#     id_col = "vpid",
#     project_col = "project",
#     ctime_col = "ctime",
#     data_type = "questionnaire",
#     out_basename = "ids_in_all_projects",
#     out_dir = NULL
#   )
# -------------------------------------------------------------------------

collect_ids_to_excel <- function(
    meta_data,
    ...,
    id_col = "vpid",
    project_col = "project",      # can be NULL
    ctime_col = "ctime",
    data_type = "questionnaire",  # goes into file name
    out_basename = "ids_in_all_projects",
    out_dir = NULL                # defaults to "<script_dir>/private_information"
) {
  # --- helpers ---------------------------------------------------------------
  get_script_dir <- function() {
    path1 <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = FALSE), error = function(e) "")
    if (nzchar(path1)) return(dirname(path1))
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
      p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
      if (nzchar(p)) return(dirname(p))
    }
    getwd()
  }
  
  safe_as_date <- function(x) {
    x_chr <- as.character(x)
    date_part <- sub("[ T].*$", "", x_chr)
    for (fmt in c("%Y-%m-%d", "%d.%m.%Y", "%m/%d/%Y", "%Y/%m/%d")) {
      d <- as.Date(date_part, format = fmt)
      if (any(!is.na(d))) {
        nas <- is.na(d)
        if (any(nas)) d[nas] <- as.Date(date_part[nas])
        return(d)
      }
    }
    as.Date(date_part)
  }
  
  decide_retrieval_date <- function(dates) {
    dates <- dates[!is.na(dates)]
    if (!length(dates)) stop("Could not parse any valid dates.", call. = FALSE)
    tab <- sort(table(dates), decreasing = TRUE)
    top_counts <- tab[tab == max(tab)]
    if (length(top_counts) == 1) as.Date(names(top_counts))
    else min(as.Date(names(top_counts)), na.rm = TRUE)
  }
  
  # --- validate meta_data/date ----------------------------------------------
  if (!is.data.frame(meta_data)) stop("meta_data must be a data.frame.", call. = FALSE)
  if (!ctime_col %in% names(meta_data)) {
    stop(sprintf("Column '%s' not found in meta_data.", ctime_col), call. = FALSE)
  }
  retrieval_date <- meta_data[[ctime_col]] |> safe_as_date() |> decide_retrieval_date()
  date_tag <- format(retrieval_date, "%Y-%m-%d")
  
  # --- grab the supplied data frames ----------------------------------------
  dots <- list(...)
  if (!length(dots)) stop("Please pass at least one data frame via '...'.", call. = FALSE)
  
  arg_exprs <- as.list(substitute(list(...)))[-1L]
  arg_names <- names(dots)
  if (is.null(arg_names)) arg_names <- rep("", length(dots))
  
  for (i in seq_along(dots)) {
    if (!nzchar(arg_names[i])) {
      nm <- tryCatch(paste0(deparse(arg_exprs[[i]]), collapse = ""), error = function(e) paste0("df", i))
      arg_names[i] <- nm
    }
  }
  
  bad <- which(!vapply(dots, is.data.frame, logical(1)))
  if (length(bad)) {
    stop(sprintf("These arguments are not data frames: %s", paste(arg_names[bad], collapse = ", ")), call. = FALSE)
  }
  
  # --- build columns ---------------------------------------------------------
  col_list <- list()
  
  for (i in seq_along(dots)) {
    nm <- arg_names[i]
    df <- dots[[i]]
    
    if (!id_col %in% names(df)) {
      warning(sprintf("Skipping %s: missing id column '%s'", nm, id_col))
      next
    }
    
    # convert IDs to numeric and remove NA IDs
    ids_raw <- suppressWarnings(as.numeric(df[[id_col]]))
    df2 <- df[!is.na(ids_raw), , drop = FALSE]
    ids_numeric <- suppressWarnings(as.numeric(df2[[id_col]]))
    if (!nrow(df2) || all(is.na(ids_numeric))) next
    
    if (is.null(project_col) || !project_col %in% names(df2)) {
      # no project column → one column with all unique numeric IDs
      ids <- sort(unique(ids_numeric), na.last = NA)
      col_list[[nm]] <- ids
    } else {
      # split by project
      df2 <- df2[!is.na(df2[[project_col]]), , drop = FALSE]
      proj_vals <- sort(unique(df2[[project_col]]))
      for (pj in proj_vals) {
        rows <- df2[[project_col]] == pj
        ids <- sort(unique(suppressWarnings(as.numeric(df2[rows, id_col, drop = TRUE]))), na.last = NA)
        if (!length(ids)) next
        colname <- paste0(nm, "_", pj)
        col_list[[colname]] <- ids
      }
    }
  }
  
  if (!length(col_list)) stop("No valid columns to export.", call. = FALSE)
  
  max_len <- max(vapply(col_list, length, integer(1)))
  pad_to <- function(x, n) { length(x) <- n; x }
  out_df <- as.data.frame(
    lapply(col_list, pad_to, n = max_len),
    check.names = FALSE,
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  
  # --- write XLSX ------------------------------------------------------------
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install with install.packages('openxlsx').", call. = FALSE)
  }
  
  # compute default out_dir if not supplied
  if (is.null(out_dir)) {
    base_dir <- get_script_dir()
    out_dir <- file.path(base_dir, "private_information")
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  file_name <- paste0(date_tag, "_", out_basename, "_", data_type, ".xlsx")
  file_path <- file.path(out_dir, file_name)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "ids")
  openxlsx::writeData(wb, "ids", out_df)
  openxlsx::freezePane(wb, "ids", firstActiveRow = 2, firstActiveCol = 1)
  openxlsx::setColWidths(wb, "ids", cols = 1:ncol(out_df), widths = "auto")
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  message(sprintf("✅ Wrote file: %s", file_path))
  invisible(file_path)
}
