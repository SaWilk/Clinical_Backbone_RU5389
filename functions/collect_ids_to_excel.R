collect_ids_to_excel <- function(
    meta_data,
    ...,
    id_col = "vpid",
    project_col = "project",      # can be NULL
    ctime_col = "ctime",
    data_type = "questionnaire",  # goes into file name
    out_basename = "ids_in_all_projects"
) {
  # --- helpers ---------------------------------------------------------------
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a) && nzchar(a)) a else b
  
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
    if (!length(dates)) stop("Could not parse any valid dates from meta_data[[ctime_col]].", call. = FALSE)
    tab <- sort(table(dates), decreasing = TRUE)
    top_counts <- tab[tab == max(tab)]
    if (length(top_counts) == 1) as.Date(names(top_counts))
    else min(as.Date(names(top_counts)), na.rm = TRUE)
  }
  
  # --- validate meta_data/date ----------------------------------------------
  if (!is.data.frame(meta_data)) stop("meta_data must be a data.frame.", call. = FALSE)
  if (!ctime_col %in% names(meta_data)) stop(sprintf("Column '%s' not found in meta_data.", ctime_col), call. = FALSE)
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
    
    # remove NA IDs
    df2 <- df[!is.na(df[[id_col]]), , drop = FALSE]
    if (!nrow(df2)) next
    
    if (is.null(project_col) || !project_col %in% names(df2)) {
      # no project column → one column with all unique IDs
      ids <- sort(unique(as.character(df2[[id_col]])))
      col_list[[nm]] <- ids
    } else {
      # split by project
      df2 <- df2[!is.na(df2[[project_col]]), , drop = FALSE]
      proj_vals <- sort(unique(as.character(df2[[project_col]])))
      for (pj in proj_vals) {
        ids <- sort(unique(as.character(df2[df2[[project_col]] == pj, id_col, drop = TRUE])))
        if (!length(ids)) next
        colname <- paste0(nm, "_", pj)
        col_list[[colname]] <- ids
      }
    }
  }
  
  if (!length(col_list)) stop("No valid columns to export.", call. = FALSE)
  
  max_len <- max(vapply(col_list, length, integer(1)))
  pad_to <- function(x, n) { length(x) <- n; x }
  out_df <- as.data.frame(lapply(col_list, pad_to, n = max_len),
                          check.names = FALSE, optional = TRUE, stringsAsFactors = FALSE)
  
  # --- write XLSX ------------------------------------------------------------
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install with install.packages('openxlsx').", call. = FALSE)
  }
  
  base_dir <- get_script_dir()
  target_dir <- file.path(base_dir, "private_information")
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  
  file_name <- paste0(date_tag, "_", out_basename, "_", data_type, ".xlsx")
  file_path <- file.path(target_dir, file_name)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "ids")
  openxlsx::writeData(wb, "ids", out_df)
  openxlsx::freezePane(wb, "ids", firstActiveRow = 2, firstActiveCol = 1)
  openxlsx::setColWidths(wb, "ids", cols = 1:ncol(out_df), widths = "auto")
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  message(sprintf("✅ Wrote file: %s", file_path))
  invisible(file_path)
}
