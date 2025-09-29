separate_by_project <- function(df,
                                out_path = NULL,
                                sample = NULL,
                                export_csv = FALSE,
                                data_type = c("questionnaires", "experiment_data"),
                                dry_run = FALSE,
                                verbose = TRUE) {
  data_type  <- match.arg(data_type)
  export_csv <- isTRUE(export_csv)  # ensure logical
  
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
    warning("Could not find a project column. Assigning all rows to default project '6'. ",
            "Tried: ", paste(proj_cols, collapse = ", "), ".")
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
    if (n1 < n0 && verbose) message("Removed ", n0 - n1, " test observation(s) with Versuchspersonennummer. == 99999.")
  }
  
  # ----- clean labels -----
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
  if (sample_name == "unknown" && verbose) {
    warning("Could not infer sample. Consider passing sample = 'adults'/'adolescents'/'children_parents'/'parents_p6'/'children_p6'.")
  }
  if (verbose) message("Sample: '", sample_name, "'")
  
  # choose suffixes
  file_suffix <- if (data_type == "experiment_data") "cogtests" else "questionnaire"  # for filenames
  env_suffix  <- if (data_type == "experiment_data") "cogtest"  else "questionnaire"  # for env vars
  subfolder   <- if (data_type == "experiment_data") "experiment_data" else "questionnaires"
  if (verbose) message("Saving under subfolder: ", subfolder, " | file suffix: ", file_suffix, " | env suffix: ", env_suffix)
  
  # ----- project parsing -----
  parse_project <- function(lbl) {
    if (is.na(lbl)) return(list(type = "other", pid = NA_character_))
    s <- trimws(as.character(lbl))
    m1 <- regexec("(?i)^\\s*P\\s*(\\d+)", s, perl = TRUE)
    r1 <- regmatches(s, m1)[[1]]
    if (length(r1) >= 2 && nzchar(r1[2])) return(list(type = "general_info", pid = r1[2]))
    m2 <- regexec("^\\s*(\\d+)\\s*$", s, perl = TRUE)
    r2 <- regmatches(s, m2)[[1]]
    if (length(r2) >= 2 && nzchar(r2[2])) return(list(type = "number", pid = r2[2]))
    digits <- gsub("\\D+", "", s)
    if (nzchar(digits)) return(list(type = "number", pid = digits))
    list(type = "other", pid = NA_character_)
  }
  
  # helpers
  is_empty_df <- function(x) is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  ensure_dir  <- function(path) if (!dir.exists(path) && !dry_run) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  date_str    <- format(Sys.Date(), "%Y-%m-%d")
  
  # split
  parts <- split(df, df[[proj_col]], drop = TRUE)
  
  for (lbl in names(parts)) {
    d <- parts[[lbl]]
    parsed <- parse_project(lbl)
    pid <- parsed$pid
    kind <- parsed$type
    
    pid_clean <- if (!is.na(pid) && nzchar(pid)) pid else "unknown"
    pid_dir <- file.path(base_dir, sprintf("%s_backbone", pid_clean), subfolder)
    ensure_dir(pid_dir)
    
    # ----- environment variable names now include data-type suffix -----
    if (kind == "general_info") {
      varname <- sprintf("data_general_info_%s_%s", sample_name, env_suffix)
    } else {
      varname <- sprintf("data_%s_p_%s_%s", sample_name, pid_clean, env_suffix)
    }
    assign(varname, d, envir = .GlobalEnv)
    
    # ----- skip saves for test or empty -----
    if (pid_clean %in% c("0", "99")) { if (verbose) message("Skipping save for test project ", pid_clean, " (env: ", varname, ")"); next }
    if (is_empty_df(d))            { if (verbose) message("Skipping save for empty project ", pid_clean, " (env: ", varname, ")");   next }
    
    # filenames keep sample tag + dataset file suffix
    if (kind == "general_info") {
      base <- sprintf("general_info_%s_%s_%s", date_str, sample_name, file_suffix)
    } else {
      base <- sprintf("%s_%s_%s_%s", pid_clean, date_str, sample_name, file_suffix)
    }
    
    if (!dry_run) {
      if (export_csv) utils::write.csv(d, file.path(pid_dir, paste0(base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(d, file.path(pid_dir, paste0(base, ".xlsx")))
    }
    
    if (verbose) message("Saved: ", file.path(pid_dir, paste0(base, ".xlsx")), " | env: ", varname)
  }
  
  invisible(TRUE)
}
