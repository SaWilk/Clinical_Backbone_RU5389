separate_by_project <- function(
    df,
    out_path = NULL,
    sample = NULL,
    export_csv = FALSE,
    data_type = c("questionnaires", "experiment_data"),
    metadata_info = NULL, # optional
    dry_run = FALSE,
    verbose = TRUE
) {
  data_type <- match.arg(data_type)
  export_csv <- isTRUE(export_csv)
  
  # ----- resolve base directory -----
  detect_script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    idx <- grep("^--file=", args)
    if (length(idx)) {
      p <- sub("^--file=", "", args[idx[length(idx)]])
      if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
    }
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
      if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
    }
    if (requireNamespace("knitr", quietly = TRUE)) {
      p <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) "")
      if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
    }
    ""
  }
  sanitize_base <- function(p) {
    if (is.null(p)) return(NULL)
    p2 <- normalizePath(p, winslash = "/", mustWork = FALSE)
    tail <- tolower(basename(p2))
    if (tail %in% c("experiment_data", "questionnaires")) dirname(p2) else p2
  }
  base_dir <- sanitize_base(out_path)
  if (is.null(base_dir) || !nzchar(base_dir)) {
    base_dir <- detect_script_dir()
    if (!nzchar(base_dir)) {
      base_dir <- getwd()
      if (verbose) warning("Could not detect script directory. Using getwd(): ", base_dir)
    }
  }
  if (!dir.exists(base_dir) && !dry_run) dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  if (verbose) message("Base dir: ", base_dir)
  
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  
  # ----- locate or make project column -----
  proj_cols <- c("project", "Projekt...Project", "Projekt.", "Projekt", "projekt", "p", "proj", "Proj")
  lower_names <- tolower(names(df))
  match_idx <- which(tolower(proj_cols) %in% lower_names)[1]
  if (is.na(match_idx)) {
    warning("No project column found. Assigning all rows to default project '6'.")
    proj_col <- "Projekt."
    df[[proj_col]] <- "6"
  } else {
    proj_col <- names(df)[match(tolower(proj_cols[match_idx]), lower_names)]
  }
  if (verbose) message("Project column: '", proj_col, "'")
  
  # ----- remove test participants -----
  if ("Versuchspersonennummer." %in% names(df)) {
    n0 <- nrow(df)
    keep <- !(df$Versuchspersonennummer. == 99999 | as.character(df$Versuchspersonennummer.) == "99999")
    keep[is.na(keep)] <- TRUE
    df <- df[keep, , drop = FALSE]
    n1 <- nrow(df)
    if (n1 < n0 && verbose) message("Removed ", n0 - n1, " test rows.")
  }
  
  # ----- normalize labels -----
  df[[proj_col]] <- trimws(as.character(df[[proj_col]]))
  df <- df[!(df[[proj_col]] %in% c("", "Testing mode(No actual data collection)")), , drop = FALSE]
  
  # ----- sample detection -----
  normalize_sample <- function(x) {
    x <- tolower(x)
    if (grepl("parents_p6", x)) return("parents_p6")
    if (grepl("children_p6", x)) return("children_p6")
    if (grepl("children_parents", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(children)(?:$|[_\\.-])", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(adolescents)(?:$|[_\\.-])", x)) return("adolescents")
    if (grepl("(?:^|[_\\.-])(adults)(?:$|[_\\.-])", x)) return("adults")
    "unknown"
  }
  infer_sample_from_call <- function() {
    txt1 <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
    s1 <- normalize_sample(txt1); if (s1 != "unknown") return(s1)
    mc <- tryCatch(match.call(expand.dots = FALSE)$df, error = function(e) NULL)
    txt2 <- tryCatch(if (!is.null(mc)) paste(deparse(mc), collapse = "") else "", error = function(e) "")
    s2 <- normalize_sample(txt2); if (s2 != "unknown") return(s2)
    pf <- parent.frame()
    nms <- ls(envir = pf, all.names = TRUE)
    cand <- nms[grepl("adults|adolescents|children_parents|children|parents_p6|children_p6", tolower(nms))]
    for (nm in cand) {
      s3 <- normalize_sample(nm)
      if (s3 != "unknown") return(s3)
    }
    "unknown"
  }
  sample_name <- if (!is.null(sample)) normalize_sample(sample) else infer_sample_from_call()
  if (verbose) message("Sample: ", sample_name)
  
  # ----- metadata (optional) -----
  date_str <- "unknown_date"
  if (is.null(metadata_info)) {
    warning("No 'metadata_info' provided; using 'unknown_date' in filenames.")
  } else {
    load_metadata <- function(meta) {
      if (is.data.frame(meta)) return(meta)
      if (is.character(meta) && length(meta) == 1L && nzchar(meta)) {
        ext <- tolower(tools::file_ext(meta))
        if (ext == "csv") {
          return(utils::read.csv(meta, stringsAsFactors = FALSE))
        } else if (ext %in% c("xlsx", "xls")) {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("To read '", ext, "' metadata files, install.packages('readxl').")
          }
          return(as.data.frame(readxl::read_excel(meta)))
        } else stop("Unsupported metadata file extension: .", ext)
      }
      stop("metadata_info must be a data.frame or a path to a .csv/.xlsx file.")
    }
    ok <- TRUE
    meta_df <- tryCatch(load_metadata(metadata_info), error = function(e) { warning(conditionMessage(e)); return(NULL) })
    if (is.null(meta_df)) {
      ok <- FALSE; warning("Could not load metadata; using 'unknown_date'.")
    } else if (!all(c("sample", "ctime") %in% names(meta_df))) {
      ok <- FALSE; warning("Metadata missing 'sample'/'ctime'; using 'unknown_date'.")
    }
    if (ok) {
      ms <- tolower(as.character(meta_df$sample))
      idx <- which(ms == tolower(sample_name))
      if (length(idx) == 0) {
        warning("No metadata match for sample='", sample_name, "'; using 'unknown_date'.")
      } else {
        ctimes <- meta_df$ctime[idx]
        first_non_na <- which(!is.na(ctimes))[1]
        if (!is.na(first_non_na)) {
          parsed_date <- tryCatch(as.Date(ctimes[first_non_na]), error = function(e) NA)
          if (!is.na(parsed_date)) {
            date_str <- format(parsed_date, "%Y-%m-%d")
            if (verbose) message("Using ctime from metadata: ", date_str)
          } else {
            warning("Could not parse ctime for sample='", sample_name, "'; using 'unknown_date'.")
          }
        } else {
          warning("All ctime NA for sample='", sample_name, "'; using 'unknown_date'.")
        }
      }
    }
  }
  
  # ----- suffixes -----
  file_suffix <- if (data_type == "experiment_data") "cogtests" else "questionnaire"
  env_suffix  <- if (data_type == "experiment_data") "cogtest" else "questionnaire"
  subfolder   <- if (data_type == "experiment_data") "experiment_data" else "questionnaires"
  
  # ----- project parsing/helpers -----
  parse_project <- function(lbl) {
    s <- trimws(as.character(lbl))
    digits <- gsub("\\D+", "", s)
    list(pid = ifelse(nzchar(digits), digits, "unknown"))
  }
  is_empty_df <- function(x) is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  ensure_dir <- function(path) if (!dir.exists(path) && !dry_run) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  # ====== NEW: collect output directories ======
  collected_dirs <- character(0)
  
  # ----- split & write -----
  parts <- split(df, df[[proj_col]], drop = TRUE)
  for (lbl in names(parts)) {
    d <- parts[[lbl]]
    pid_clean <- parse_project(lbl)$pid
    varname <- sprintf("data_%s_p_%s_%s", sample_name, pid_clean, env_suffix)
    assign(varname, d, envir = .GlobalEnv)
    
    if (pid_clean %in% c("0", "99") || is_empty_df(d)) next
    
    pid_dir <- file.path(base_dir, sprintf("%s_backbone", pid_clean), subfolder)
    ensure_dir(pid_dir)
    base <- sprintf("%s_%s_%s_%s", pid_clean, date_str, sample_name, file_suffix)
    
    if (!dry_run) {
      if (export_csv) utils::write.csv(d, file.path(pid_dir, paste0(base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(d, file.path(pid_dir, paste0(base, ".xlsx")))
    }
    if (verbose) message("Saved: ", file.path(pid_dir, paste0(base, ".xlsx")), " | env: ", varname)
    
    # ====== NEW: record the directory ======
    collected_dirs <- c(collected_dirs, pid_dir)
  }
  
  # Return unique paths as a character vector (named by project id for convenience)
  collected_dirs <- unique(collected_dirs)
  names(collected_dirs) <- basename(dirname(collected_dirs)) # e.g., "123_backbone"
  
  return(collected_dirs)
}
