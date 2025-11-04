# -------------------------------------------------------------------------
# copy_psytool_files()
#
# Purpose:
#   Collects and copies raw PsyToolkit experiment files for specific
#   samples/projects from standardized paths into a dated, project-scoped
#   output tree. Also builds a per-file log (returned invisibly).
#
# Behavior:
#   - Establishes a base directory as the current working directory.
#   - Determines the output root:
#       • If `cogtest_out_path` is empty: "<base>/01_project_data".
#       • Ensures the directory exists (recursive).
#   - Discovers candidate data frames in the global environment whose names
#     match:  data_<sample_tag>_p_<project>_cogtest
#       • <project> must be in `allowed_projects` (default digits "1"–"9").
#       • <sample_tag> is matched to a known sample.
#   - Resolves the sample from <sample_tag> against:
#       • "adults", "adolescents", "children", "children_parents", "adults_remote".
#   - Computes the source root for each sample:
#       • "<base>/raw_data/psytoolkit/<sample>/experiment_data"
#   - Derives a date tag for folder naming via `meta_env_name` (default "cogtest_info"):
#       • Expects a data.frame with columns: sample, ctime.
#       • Extracts/normalizes a date from `ctime` (Date, POSIXt, numeric epoch,
#         or common string formats) to "YYYY-MM-DD".
#       • Falls back to "unknown_date" with a warning if unavailable.
#   - Builds the destination folder:
#       • "<out>/<project>_backbone/experiment_data/<project>_<YYYY-MM-DD>_<sample>_cogtest_data"
#       • Ensures it exists (recursive).
#   - For each row in each candidate data frame:
#       • Chooses a participant ID column for logging only (first present among
#         "Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "id", "vpid_1").
#       • For each test in `test_cols` (default c("WCST_1","LNS_1","BACS_1")):
#           – If the path cell is empty -> log "empty_path".
#           – If the source file is missing -> log "missing_source".
#           – Otherwise copy the file to the destination:
#               · overwrite = FALSE, preserve timestamps (copy.date = TRUE).
#               · Log status "copied", "exists", or "copy_failed".
#   - Aggregates a row-wise log across all processed objects and returns it
#     (invisibly). Prints a message if no files were processed.
#
# Input:
#   env_objects      : character vector of object names to consider (default: all
#                      objects in the global environment).
#   cogtest_out_path : output directory for copied files; created if missing.
#   meta_env_name    : name of a data.frame in the global env providing metadata
#                      with columns `sample` and `ctime` (default "cogtest_info").
#   test_cols        : character vector of test/path column names to check
#                      (default c("WCST_1","LNS_1","BACS_1")).   [Fix 3]
#   allowed_projects : character vector of allowed project tags (default
#                      as.character(1:9)).                        [Fix 3]
#
# Output:
#   - Returns (invisibly) a data.frame log with columns including:
#       env_object, project, sample, dest_dir_used, participant_id, test,
#       source, destination, status.
#   - Copies files into the dated destination tree as described above.
#   - Issues warnings when metadata is missing/invalid; otherwise runs quietly.
#
# Notes:
#   - [Fix 1] Removed a redundant `dir.create(experiment_dir)` — the tree is
#     already created when `dest_dir` is ensured.
#   - [Fix 2] Logging for `missing_source` now uses `basename(src)` for the
#     `destination` field, consistent with the copy branch.
#   - [Fix 3] `test_cols` and `allowed_projects` are now parameters.
#   - [Fix 4 — docs only] The ID column selection order is fixed in code:
#     "Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "id", "vpid_1".
#     If your data vary, consider exposing a `preferred_id_cols` parameter.
#   - [Fix 6 — docs only] The function returns the log **invisibly**. Callers
#     can wrap in `print()` or assign to inspect.
#
# Example:
#   log <- copy_psytool_files(
#     env_objects      = c("data_adults_p_1_cogtest", "data_children_p_3_cogtest"),
#     cogtest_out_path = "01_project_data",
#     meta_env_name    = "cogtest_info",
#     test_cols        = c("WCST_1","LNS_1","BACS_1"),
#     allowed_projects = as.character(1:9)
#   )
# -------------------------------------------------------------------------

copy_psytool_files <- function(
    env_objects      = NULL,
    cogtest_out_path = NULL,
    meta_env_name    = "cogtest_info",
    test_cols        = c("WCST_1", "LNS_1", "BACS_1"),          
    allowed_projects = as.character(2:9),
    middle_subdir    = NULL
) {
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
  
  norm_tok <- function(x) {
    tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
  }
  
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
    
    # 1) Already ISO-like date (YYYY-MM-DD)
    m <- regexpr("\\b\\d{4}-\\d{2}-\\d{2}\\b", s)
    if (m[1] != -1) return(substr(s, m[1], m[1] + attr(m, "match.length") - 1))
    
    # 2) Flexible capture: YYYY[-./]M[-./]D → normalize with zero-padding
    flex <- regexec("\\b(\\d{4})[-./](\\d{1,2})[-./](\\d{1,2})\\b", s)
    parts <- regmatches(s, flex)[[1]]
    if (length(parts) == 4) {
      yyyy <- as.integer(parts[2]); mm <- as.integer(parts[3]); dd <- as.integer(parts[4])
      if (is.finite(yyyy) && is.finite(mm) && is.finite(dd)) {
        suppressWarnings({
          dt <- try(as.Date(sprintf("%04d-%02d-%02d", yyyy, mm, dd)), silent = TRUE)
        })
        if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m-%d"))
      }
    }
    
    # 3) Try common timestamp formats
    fmts <- c(
      "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS", "%Y.%m.%d %H:%M:%OS",
      "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d"
    )
    for (f in fmts) {
      suppressWarnings({
        ts <- try(as.POSIXct(s, format = f, tz = "UTC"), silent = TRUE)
        if (!inherits(ts, "try-error") && !is.na(ts)) return(format(ts, "%Y-%m-%d"))
        dt <- try(as.Date(s, format = f), silent = TRUE)
        if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m-%d"))
      })
    }
    
    NA_character_
  }
  
  # Enhanced version with warnings:
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
      if (grepl(paste0("(^|_)", s, "(_|$)"), sample_tag, ignore.case = TRUE)) {
        sample <- s
        break
      }
    }
    if (is.na(sample)) next
    if (!(project %in% allowed_projects)) next  # [Fix 3]
    
    src_root <- sample_root(sample)
    if (!dir.exists(src_root)) next
    
    df <- get(obj, envir = .GlobalEnv)
    if (!is.data.frame(df)) next
    
    date_tag <- date_for_sample(sample)
    
    project_block  <- sprintf("%s_backbone", project)
    experiment_dir <- if (is.null(middle_subdir) || !nzchar(middle_subdir)) {
      file.path(cogtest_out_path, project_block, "experiment_data")
    } else {
      file.path(cogtest_out_path, project_block, middle_subdir, "experiment_data")
    }
    
    dated_folder_name <- sprintf("%s_%s_%s_cogtest_data", project, date_tag, sample)
    dest_dir <- file.path(experiment_dir, dated_folder_name)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    id_col <- pick_id_col(df)
    
    for (i in seq_len(nrow(df))) {
      pid <- if (!is.na(id_col)) df[[id_col]][i] else NA
      
      for (test in test_cols) {  # [Fix 3]
        if (!test %in% names(df)) next
        rel <- df[[test]][i]
        
        if (is.null(rel) || is.na(rel) || !nzchar(as.character(rel))) {
          log_rows[[length(log_rows) + 1]] <- data.frame(
            env_object = obj, project = project, sample = sample, dest_dir_used = dest_dir,
            participant_id = pid, test = test,
            source = NA_character_,
            destination = file.path(dest_dir, NA_character_),
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
            source = src,
            destination = file.path(dest_dir, basename(src)),  # [Fix 2] consistent basename
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
    
    # [Fix 1] Removed redundant dir.create(experiment_dir) here — already ensured via dest_dir
  }
  
  log_df <- if (length(log_rows)) do.call(rbind, log_rows) else data.frame()
  if (!nrow(log_df)) message("No files processed — check env_objects and source data.")
  invisible(log_df)  # [Fix 6 — docs clarify invisibility]
}
