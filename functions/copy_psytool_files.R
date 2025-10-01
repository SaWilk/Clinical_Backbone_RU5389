copy_psytool_files <- function(env_objects = NULL,
                               cogtest_out_path = NULL,
                               meta_env_name = "cogtest_info") {
  base_dir <- normalizePath(getwd(), winslash = "\\", mustWork = TRUE)
  
  if (is.null(cogtest_out_path) || !nzchar(cogtest_out_path)) {
    cogtest_out_path <- file.path(base_dir, "01_project_data")
  }
  if (!dir.exists(cogtest_out_path)) {
    dir.create(cogtest_out_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  valid_samples <- c("adults", "adolescents", "children", "children_parents", "adults_remote")
  sample_root <- function(sample) file.path(base_dir, "raw_data", "psytoolkit", sample, "experiment_data")
  
  # -------- metadata helpers --------------------------
  meta_df <- NULL
  if (!is.null(meta_env_name) && nzchar(meta_env_name) &&
      exists(meta_env_name, envir = .GlobalEnv, inherits = FALSE)) {
    maybe_df <- get(meta_env_name, envir = .GlobalEnv)
    if (is.data.frame(maybe_df)) meta_df <- maybe_df
  }
  
  norm_tok <- function(x) tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
  
  # Robustly normalize anything resembling a date to "YYYY-MM-DD"
  extract_ymd <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
    v <- x[[1L]]
    
    # If it's already a Date
    if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
    
    # If it's POSIXt (POSIXct / POSIXlt)
    if (inherits(v, "POSIXt")) return(format(as.POSIXct(v, tz = "UTC"), "%Y-%m-%d"))
    
    # If it's numeric: try Unix epoch (sec or ms)
    if (is.numeric(v) && is.finite(v)) {
      num <- as.numeric(v)
      # Heuristic: ms if very large; convert to seconds
      if (num > 1e12) num <- num / 1000
      if (num > 6e8 && num < 4e9) { # ~1989–2096 in seconds
        ts <- as.POSIXct(num, origin = "1970-01-01", tz = "UTC")
        if (!is.na(ts)) return(format(ts, "%Y-%m-%d"))
      }
    }
    
    # Character parsing
    s <- trimws(as.character(v))
    if (!nzchar(s)) return(NA_character_)
    
    # 1) Fast path: already contains an ISO-like date (keeps YYYY-MM-DD)
    m <- regexpr("\\b\\d{4}-\\d{2}-\\d{2}\\b", s)
    if (m[1] != -1) return(substr(s, m[1], m[1] + attr(m, "match.length") - 1))
    
    # 2) Flexible capture: YYYY[-./]M[-./]D → normalize with zero-padding
    flex <- regexec("\\b(\\d{4})[-./](\\d{1,2})[-./](\\d{1,2})\\b", s)
    parts <- regmatches(s, flex)[[1]]
    if (length(parts) == 4) {
      yyyy <- as.integer(parts[2]); mm <- as.integer(parts[3]); dd <- as.integer(parts[4])
      if (is.finite(yyyy) && is.finite(mm) && is.finite(dd)) {
        # validate by constructing a Date
        suppressWarnings({
          dt <- try(as.Date(sprintf("%04d-%02d-%02d", yyyy, mm, dd)), silent = TRUE)
        })
        if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m-%d"))
      }
    }
    
    # 3) Try common timestamp formats (ISO with space/T, with or w/o seconds)
    fmts <- c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      "%Y.%m.%d %H:%M:%OS",
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%Y.%m.%d"
    )
    for (f in fmts) {
      suppressWarnings({
        ts <- try(as.POSIXct(s, format = f, tz = "UTC"), silent = TRUE)
        if (!inherits(ts, "try-error") && !is.na(ts)) return(format(ts, "%Y-%m-%d"))
        dt <- try(as.Date(s, format = f), silent = TRUE)
        if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m-%d"))
      })
    }
    
    # nothing worked
    NA_character_
  }
  
  
  # 🔧 Enhanced version with warnings:
  date_for_sample <- function(sample_name) {
    # case 1: metadata not available
    if (is.null(meta_df)) {
      warning(sprintf(
        "No metadata dataframe found for sample '%s' — using 'unknown_date'. Check that '%s' exists and is a data.frame.",
        sample_name, meta_env_name
      ))
      return("unknown_date")
    }
    
    needed_cols <- c("sample", "ctime")
    # case 2: missing required columns
    if (!all(needed_cols %in% names(meta_df))) {
      warning(sprintf(
        "Metadata dataframe for '%s' is missing required columns (%s) — using 'unknown_date'.",
        sample_name, paste(setdiff(needed_cols, names(meta_df)), collapse = ", ")
      ))
      return("unknown_date")
    }
    
    meta_sample_norm <- norm_tok(meta_df$sample)
    needle <- norm_tok(sample_name)
    rows <- which(meta_sample_norm == needle)
    
    # case 3: no match at all
    if (length(rows) == 0) {
      rows <- which(grepl(needle, meta_sample_norm, fixed = TRUE))
      if (length(rows) == 0) {
        warning(sprintf(
          "No matching rows found in metadata for sample '%s' — using 'unknown_date'.",
          sample_name
        ))
        return("unknown_date")
      }
    }
    
    ctimes <- meta_df$ctime[rows]
    for (ct in ctimes) {
      ymd <- extract_ymd(ct)
      if (!is.na(ymd) && nzchar(ymd)) return(ymd)
    }
    
    # case 4: rows found but no parsable date
    warning(sprintf(
      "Metadata found for sample '%s' but could not extract a valid date from column 'ctime' — using 'unknown_date'.",
      sample_name
    ))
    "unknown_date"
  }
  # --------------------------------------------------------------------------
  
  test_cols <- c("WCST_1", "LNS_1", "BACS_1")
  pick_id_col <- function(df) {
    for (nm in c("Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "id", "vpid_1")) {
      if (nm %in% names(df)) return(nm)
    }
    return(NA_character_)
  }
  
  if (is.null(env_objects)) env_objects <- ls(envir = .GlobalEnv)
  env_objects <- as.character(env_objects)
  
  log_rows <- list()
  
  for (obj in env_objects) {
    if (!exists(obj, envir = .GlobalEnv, inherits = FALSE)) next
    
    m <- regexec("^data_(.+)_p_([0-9]+)_cogtest$", obj)
    parts <- regmatches(obj, m)[[1]]
    if (length(parts) < 3) next
    
    sample_tag <- parts[2]
    project    <- parts[3]
    
    sample <- NA_character_
    for (s in valid_samples) {
      if (grepl(paste0("(^|_)", s, "(_|$)"), sample_tag, ignore.case = TRUE)) { sample <- s; break }
    }
    if (is.na(sample)) next
    if (!(project %in% as.character(1:9))) next
    
    src_root <- sample_root(sample)
    if (!dir.exists(src_root)) next
    
    df <- get(obj, envir = .GlobalEnv)
    if (!is.data.frame(df)) next
    
    # 🔧 this may now trigger a warning if unknown_date used
    date_tag <- date_for_sample(sample)
    
    project_block  <- sprintf("%s_backbone", project)
    experiment_dir <- file.path(cogtest_out_path, project_block, "experiment_data")
    
    dated_folder_name <- sprintf("%s_%s_%s_cogtest_data", project, date_tag, sample)
    dest_dir <- file.path(experiment_dir, dated_folder_name)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    id_col <- pick_id_col(df)
    
    for (i in seq_len(nrow(df))) {
      pid <- if (!is.na(id_col)) df[[id_col]][i] else NA
      
      for (test in test_cols) {
        if (!test %in% names(df)) next
        rel <- df[[test]][i]
        
        if (is.null(rel) || is.na(rel) || !nzchar(as.character(rel))) {
          log_rows[[length(log_rows) + 1]] <- data.frame(
            env_object = obj, project = project, sample = sample, dest_dir_used = dest_dir,
            participant_id = pid, test = test,
            source = NA_character_, destination = NA_character_,
            status = "empty_path", stringsAsFactors = FALSE
          )
          next
        }
        
        rel_chr <- as.character(rel)
        src <- normalizePath(file.path(src_root, rel_chr), winslash = "\\", mustWork = FALSE)
        
        if (!file.exists(src)) {
          log_rows[[length(log_rows) + 1]] <- data.frame(
            env_object = obj, project = project, sample = sample, dest_dir_used = dest_dir,
            participant_id = pid, test = test,
            source = src, destination = file.path(dest_dir, basename(rel_chr)),
            status = "missing_source", stringsAsFactors = FALSE
          )
          next
        }
        
        dest <- file.path(dest_dir, basename(src))
        ok <- file.copy(src, dest, overwrite = FALSE, copy.date = TRUE)
        status <- if (ok) "copied" else if (file.exists(dest)) "exists" else "copy_failed"
        
        log_rows[[length(log_rows) + 1]] <- data.frame(
          env_object = obj, project = project, sample = sample, dest_dir_used = dest_dir,
          participant_id = pid, test = test,
          source = src, destination = dest, status = status,
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (!dir.exists(experiment_dir)) {
      dir.create(experiment_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  log_df <- if (length(log_rows)) do.call(rbind, log_rows) else data.frame()
  if (!nrow(log_df)) message("No files processed — check env_objects and source data.")
  invisible(log_df)
}
