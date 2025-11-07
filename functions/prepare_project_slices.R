prepare_project_slices <- function(
    df,
    out_path = NULL,
    sample = NULL,
    data_type = c("questionnaires", "experiment_data"),
    metadata_info = NULL,
    pilot_mode = FALSE,
    verbose = TRUE
) {
  data_type <- match.arg(data_type)
  
  # ----- base dir: you pass <root>/01_project_data here -----
  base_dir <- if (is.null(out_path)) {
    p <- .detect_script_dir(); if (!nzchar(p)) p <- getwd(); normalizePath(p, winslash = "/", mustWork = FALSE)
  } else normalizePath(out_path, winslash = "/", mustWork = FALSE)
  if (verbose) message("Base dir (root/01_project_data): ", base_dir)
  
  # ===== MISSING BEFORE: locate or make project column =====
  proj_cols <- c("project","Projekt...Project","Projekt.","Projekt","projekt","p","proj","Proj")
  lower_names <- tolower(names(df))
  hit <- which(tolower(proj_cols) %in% lower_names)[1]
  if (is.na(hit)) {
    warning("No project column found. Assigning all rows to default project '6'.")
    proj_col <- "Projekt."
    df[[proj_col]] <- "6"
  } else {
    # map the alias back to the actual column name present in df
    proj_col <- names(df)[match(tolower(proj_cols[hit]), lower_names)]
  }
  if (verbose) message("Project column: '", proj_col, "'")
  
  # ----- remove test participants -----
  if ("Versuchspersonennummer." %in% names(df)) {
    n0 <- nrow(df)
    keep <- !(df$Versuchspersonennummer. == 99999 | as.character(df$Versuchspersonennummer.) == "99999")
    keep[is.na(keep)] <- TRUE
    df <- df[keep, , drop = FALSE]
    if (verbose && nrow(df) < n0) message("Removed ", n0 - nrow(df), " test rows.")
  }
  
  # ----- normalize labels & drop Testing mode -----
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
    pf <- parent.frame()
    nms <- ls(envir = pf, all.names = TRUE)
    cand <- nms[grepl("adults|adolescents|children_parents|children|parents_p6|children_p6", tolower(nms))]
    for (nm in cand) { s3 <- normalize_sample(nm); if (s3 != "unknown") return(s3) }
    "unknown"
  }
  sample_name <- if (!is.null(sample)) normalize_sample(sample) else infer_sample_from_call()
  if (verbose) message("Sample: ", sample_name)
  
  # ----- METADATA: use ADOLESCENTS ctime (latest) regardless of current sample -----
  # Rationale: adolescents export is the only one that doesn't require REMID translation,
  # therefore it contains the creation date of the actual download.
  date_str <- "unknown_date"
  if (is.null(metadata_info)) {
    warning("No 'metadata_info' provided; using 'unknown_date'.")
  } else {
    load_metadata <- function(meta) {
      if (is.data.frame(meta)) return(meta)
      if (is.character(meta) && length(meta) == 1L && nzchar(meta)) {
        ext <- tolower(tools::file_ext(meta))
        if (ext == "csv")  return(utils::read.csv(meta, stringsAsFactors = FALSE))
        if (ext %in% c("xlsx","xls")) {
          if (!requireNamespace("readxl", quietly = TRUE)) stop("Install 'readxl' to read xlsx/xls.")
          return(as.data.frame(readxl::read_excel(meta)))
        }
      }
      stop("metadata_info must be a data.frame or a path to .csv/.xlsx")
    }
    meta_df <- tryCatch(load_metadata(metadata_info), error = function(e) { warning(conditionMessage(e)); NULL })
    if (!is.null(meta_df) && all(c("sample","ctime") %in% names(meta_df))) {
      smp <- tolower(as.character(meta_df$sample))
      idx <- which(smp == "adolescents")
      if (length(idx) > 0) {
        valid_dates <- suppressWarnings(as.Date(meta_df$ctime[idx]))
        if (any(!is.na(valid_dates))) {
          date_str <- format(max(valid_dates, na.rm = TRUE), "%Y-%m-%d")
          if (verbose) message("Using adolescents ctime (latest) from metadata: ", date_str)
        } else warning("Adolescents rows have no valid ctime; using 'unknown_date'.")
      } else warning("No 'adolescents' row found in metadata; using 'unknown_date'.")
    } else warning("Metadata missing 'sample'/'ctime'; using 'unknown_date'.")
  }
  
  # ----- suffixes and subpaths -----
  file_suffix    <- if (data_type == "experiment_data") "cogtests"        else "questionnaire"
  env_suffix     <- if (data_type == "experiment_data") "cogtest"         else "questionnaire"
  subfolder_main <- if (data_type == "experiment_data") "experiment_data" else "questionnaires"
  subpath_parts  <- if (pilot_mode) c("pilot_data", subfolder_main) else c(subfolder_main)
  
  # ----- split df, build split_list & assign to .GlobalEnv -----
  parse_pid <- function(lbl) { s <- trimws(as.character(lbl)); d <- gsub("\\D+", "", s); if (nzchar(d)) d else "unknown" }
  parts <- split(df, df[[proj_col]], drop = TRUE)
  split_list <- list()
  for (lbl in names(parts)) {
    d <- parts[[lbl]]
    pid <- parse_pid(lbl)
    varname <- sprintf("data_%s_p_%s_%s", sample_name, pid, env_suffix)
    assign(varname, d, envir = .GlobalEnv)
    
    # paths under <root>/01_project_data/<PID>_backbone/<subfolder>
    project_dir <- file.path(base_dir, sprintf("%s_backbone", pid))
    pid_dir <- do.call(file.path, as.list(c(project_dir, subpath_parts)))
    
    base_name <- paste(c(pid, date_str, if (pilot_mode) "PILOT" else NULL, sample_name, file_suffix), collapse = "_")
    split_list[[length(split_list)+1]] <- list(
      pid = pid, label = lbl, varname = varname, df = d,
      project_dir = project_dir, pid_dir = pid_dir, file_base = base_name
    )
  }
  
  # composite in memory (+ assign)
  comp_df <- do.call(rbind, lapply(split_list, function(x){ d <- x$df; if (!is.null(d)) { d$.pid <- x$pid; d } }))
  comp_var <- sprintf("data_%s_ALL_%s", sample_name, env_suffix)
  assign(comp_var, comp_df, envir = .GlobalEnv)
  
  structure(list(
    base_dir = base_dir,
    sample_name = sample_name,
    date_str = date_str,
    data_type = data_type,
    env_suffix = env_suffix,
    file_suffix = file_suffix,
    subfolder_main = subfolder_main,
    subpath_parts = subpath_parts,
    pilot_mode = pilot_mode,
    verbose = verbose,
    proj_col = proj_col,
    split_list = split_list,
    composite_var = comp_var
  ), class = "project_prep")
}
