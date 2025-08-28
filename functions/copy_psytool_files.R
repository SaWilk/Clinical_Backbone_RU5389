# -------------------------------------------------------------------------
# copy_psytool_files()
#
# Purpose:
#   Collects cognitive test data files (WCST_1, LNS_1, BACS_1) referenced in
#   psytoolkit environment data frames and copies them into a standardized
#   folder structure grouped by project and sample.
#
# Behavior:
#   - Looks for environment objects named like:
#       data_<sampleTag>_psytool_p_<project>
#     where <sampleTag> contains one of:
#       • adults
#       • adolescents
#       • children
#       • adults_remote
#   - Parses the project number (1–9 only; 0/99 are skipped).
#   - Resolves relative paths in columns WCST_1, LNS_1, BACS_1 against the
#     sample’s raw data folder, e.g.:
#       <working_dir>/raw_data/psytoolkit/adults/experiment_data
#   - Copies each referenced file into:
#       <cogtest_out_path>/<project>/<sample>/
#     preserving the original filenames (flat layout).
#   - Skips files that are missing or already exist at the destination.
#   - Creates log entries for every file processed with status:
#       • copied
#       • exists
#       • missing_source
#       • empty_path
#       • copy_failed
#   - At the end, writes a CSV log named:
#       psytool_copy_log_<YYYY-MM-DD>.csv
#     into <cogtest_out_path>.
#
# Input:
#   env_objects     : character vector of environment object names to process.
#                     If NULL (default), scans all objects in .GlobalEnv.
#   cogtest_out_path: optional string, base output folder. If not provided,
#                     defaults to <working_dir>/01_project_data/experiment_data.
#
# Output:
#   - Returns invisibly a data frame of log entries.
#   - Writes a CSV log file summarizing copy results.
#   - Prints a message with the log path.
#
# Example:
#   # Collect all env objects for adults and process:
#   adults_objs <- ls(pattern = "^data_.*adults.*_psytool_p_[0-9]+$")
#   copy_psytool_files(adults_objs)
#
#   # Or simply run on everything in environment:
#   copy_psytool_files()
#
# -------------------------------------------------------------------------
copy_psytool_files <- function(env_objects = NULL, cogtest_out_path = NULL) {
  # dynamic base dir from working directory
  base_dir <- normalizePath(getwd(), winslash = "\\", mustWork = TRUE)
  
  # if cogtest_out_path not given, derive from base_dir
  if (is.null(cogtest_out_path) || !nzchar(cogtest_out_path)) {
    cogtest_out_path <- file.path(base_dir, "01_project_data", "experiment_data")
  }
  if (!dir.exists(cogtest_out_path)) dir.create(cogtest_out_path, recursive = TRUE, showWarnings = FALSE)
  
  valid_samples <- c("adults", "adolescents", "children", "adults_remote")
  sample_root <- function(sample) file.path(base_dir, "raw_data", "psytoolkit", sample, "experiment_data")
  
  test_cols <- c("WCST_1", "LNS_1", "BACS_1")
  pick_id_col <- function(df) {
    for (nm in c("Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "id", "vpid_1")) {
      if (nm %in% names(df)) return(nm)
    }
    return(NA_character_)
  }
  
  # if env_objects not passed, scan everything
  if (is.null(env_objects)) {
    env_objects <- ls(envir = .GlobalEnv)
  }
  env_objects <- as.character(env_objects)
  
  log <- list()
  
  for (obj in env_objects) {
    if (!exists(obj, envir = .GlobalEnv, inherits = FALSE)) next
    
    # strictly match "data_<sampleTag>_psytool_p_<project>"
    m <- regexec("^data_(.+)_psytool_p_([0-9]+)$", obj)
    parts <- regmatches(obj, m)[[1]]
    if (length(parts) < 3) next
    
    sample_tag <- parts[2]
    project    <- parts[3]
    
    # find which valid sample is inside sample_tag
    sample <- NA_character_
    for (s in valid_samples) {
      if (grepl(paste0("(^|_)", s, "(_|$)"), sample_tag, ignore.case = TRUE)) {
        sample <- s
        break
      }
    }
    if (is.na(sample)) {
      warning("Unknown sample '", sample_tag, "' in ", obj, " — skipping.")
      next
    }
    
    if (!(project %in% as.character(1:9))) next  # skip test projects
    
    src_root <- sample_root(sample)
    if (!dir.exists(src_root)) {
      warning("Source folder missing for sample '", sample, "': ", src_root, " — skipping.")
      next
    }
    
    df <- get(obj, envir = .GlobalEnv)
    if (!is.data.frame(df)) next
    
    dest_dir <- file.path(cogtest_out_path, project, sample)
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    
    id_col <- pick_id_col(df)
    
    for (i in seq_len(nrow(df))) {
      pid <- if (!is.na(id_col)) df[[id_col]][i] else NA
      
      for (test in test_cols) {
        if (!test %in% names(df)) next
        
        rel <- df[[test]][i]
        if (is.null(rel) || is.na(rel) || !nzchar(as.character(rel))) {
          log[[length(log) + 1]] <- data.frame(
            env_object = obj, project = project, sample = sample,
            participant_id = pid, test = test,
            source = NA_character_, destination = NA_character_,
            status = "empty_path", stringsAsFactors = FALSE
          )
          next
        }
        
        rel_chr <- as.character(rel)
        src <- normalizePath(file.path(src_root, rel_chr), winslash = "\\", mustWork = FALSE)
        
        if (!file.exists(src)) {
          log[[length(log) + 1]] <- data.frame(
            env_object = obj, project = project, sample = sample,
            participant_id = pid, test = test,
            source = src, destination = file.path(dest_dir, basename(rel_chr)),
            status = "missing_source", stringsAsFactors = FALSE
          )
          next
        }
        
        dest <- file.path(dest_dir, basename(src))
        ok <- file.copy(src, dest, overwrite = FALSE, copy.date = TRUE)
        status <- if (ok) "copied" else if (file.exists(dest)) "exists" else "copy_failed"
        
        log[[length(log) + 1]] <- data.frame(
          env_object = obj, project = project, sample = sample,
          participant_id = pid, test = test,
          source = src, destination = dest, status = status,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  log_df <- if (length(log)) do.call(rbind, log) else data.frame(
    env_object = character(), project = character(), sample = character(),
    participant_id = character(), test = character(),
    source = character(), destination = character(), status = character(),
    stringsAsFactors = FALSE
  )
  
  log_file <- file.path(
    cogtest_out_path,
    sprintf("psytool_copy_log_%s.csv", format(Sys.Date(), "%Y-%m-%d"))
  )
  utils::write.csv(log_df, log_file, row.names = FALSE, na = "")
  message("Done. Log written to: ", log_file)
  
  invisible(log_df)
}
