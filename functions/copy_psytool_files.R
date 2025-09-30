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
  
  # Added "children_parents"
  valid_samples <- c("adults", "adolescents", "children", "children_parents", "adults_remote")
  sample_root <- function(sample) file.path(base_dir, "raw_data", "psytoolkit", sample, "experiment_data")
  
  # -------- metadata helpers (more robust matching) --------------------------
  meta_df <- NULL
  if (!is.null(meta_env_name) && nzchar(meta_env_name) &&
      exists(meta_env_name, envir = .GlobalEnv, inherits = FALSE)) {
    maybe_df <- get(meta_env_name, envir = .GlobalEnv)
    if (is.data.frame(maybe_df)) meta_df <- maybe_df
  }
  
  # Normalize tokens: lowercase, trim, remove separators so "children-parents"
  # equals "children_parents" equals "children parents"
  norm_tok <- function(x) tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
  
  extract_ymd <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
    x_chr <- as.character(x[1])
    m <- regexpr("\\d{4}-\\d{2}-\\d{2}", x_chr)
    if (m[1] != -1) return(substr(x_chr, m[1], m[1] + attr(m, "match.length") - 1))
    suppressWarnings({
      ts <- try(as.POSIXct(x_chr, tz = "UTC"), silent = TRUE)
      if (!inherits(ts, "try-error") && !is.na(ts)) return(format(ts, "%Y-%m-%d"))
      d  <- try(as.Date(x_chr), silent = TRUE)
      if (!inherits(d, "try-error") && !is.na(d)) return(format(d, "%Y-%m-%d"))
    })
    NA_character_
  }
  
  date_for_sample <- function(sample_name) {
    if (is.null(meta_df)) return("unknown_date")
    needed_cols <- c("sample", "ctime")
    if (!all(needed_cols %in% names(meta_df))) return("unknown_date")
    
    # Normalize both sides
    meta_sample_norm <- norm_tok(meta_df$sample)
    needle <- norm_tok(sample_name)
    
    rows <- which(meta_sample_norm == needle)
    
    # If no exact normalized match, try partial match (e.g., "adults" within "adultsremote")
    if (length(rows) == 0) {
      rows <- which(grepl(needle, meta_sample_norm, fixed = TRUE))
    }
    if (length(rows) == 0) return("unknown_date")
    
    ctimes <- meta_df$ctime[rows]
    for (ct in ctimes) {
      ymd <- extract_ymd(ct)
      if (!is.na(ymd) && nzchar(ymd)) return(ymd)
    }
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
    
    # Identify sample from tag
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
    
    # Use metadata-derived date (or "unknown_date")
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
