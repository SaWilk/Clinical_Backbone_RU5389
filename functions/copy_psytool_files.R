# -------------------------------------------------------------------------
# copy_psytool_files_verbose()
#
# Purpose:
#   Collects cognitive test data files (WCST_1, LNS_1, BACS_1) referenced in
#   psytoolkit environment data frames and copies them into a standardized
#   folder structure grouped by project and sample. In addition, for each
#   project–sample combination, writes a human-readable info file and a CSV
#   log alongside the copied data.
#
# Behavior:
#   - Looks for environment objects named like (NEW):
#       data_<sampleTag>_p_<project>_cogtest
#     where <sampleTag> contains one of:
#       • adults • adolescents • children • adults_remote
#   - Parses the project number (1–9 only; 0/99 are skipped).
#   - Resolves relative paths in columns WCST_1, LNS_1, BACS_1 against the
#     sample’s raw data folder, e.g.:
#       <working_dir>/raw_data/psytoolkit/<sample>/experiment_data
#   - Copies each referenced file into (created if missing):
#       <out_root>/<project>_backbone/experiment_data/
#         <project>_<YYYY-MM-DD>_<sample>_cogtest_data/
#   - Skips files that are missing or already exist at the destination.
#   - For each project–sample:
#       • Creates a text info file named:
#           <project>_<YYYY-MM-DD>_<sample>
#       • Creates a detailed CSV log:
#           <project>_<YYYY-MM-DD>_<sample>_log.csv
#     (both written into the experiment_data folder).
#
# Input:
#   env_objects      : character vector of environment object names to process;
#                      if NULL, scans all objects in .GlobalEnv.
#   cogtest_out_path : base output folder (points to 01_project_data).
#
# Output:
#   - Returns invisibly a data frame of log entries.
#   - Copies test data files into standardized locations (creating folders).
#   - Writes per-project/sample info + CSV log files under experiment_data.
#   - Prints a summary of statuses and paths.
#
# Example:
#   copy_psytool_files_verbose(
#     cogtest_out_path = file.path(getwd(), "01_project_data")
#   )
#
# -------------------------------------------------------------------------
copy_psytool_files <- function(env_objects = NULL,
                                       cogtest_out_path = NULL) {
  base_dir <- normalizePath(getwd(), winslash = "\\", mustWork = TRUE)
  
  # Base output path (should be .../01_project_data)
  if (is.null(cogtest_out_path) || !nzchar(cogtest_out_path)) {
    cogtest_out_path <- file.path(base_dir, "01_project_data")
  }
  if (!dir.exists(cogtest_out_path)) {
    dir.create(cogtest_out_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  valid_samples <- c("adults", "adolescents", "children", "adults_remote")
  sample_root <- function(sample) file.path(base_dir, "raw_data", "psytoolkit", sample, "experiment_data")
  
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
  today <- format(Sys.Date(), "%Y-%m-%d")
  
  for (obj in env_objects) {
    if (!exists(obj, envir = .GlobalEnv, inherits = FALSE)) next
    
    # NEW NAME PATTERN: data_<sampleTag>_p_<project>_cogtest
    m <- regexec("^data_(.+)_p_([0-9]+)_cogtest$", obj)
    parts <- regmatches(obj, m)[[1]]
    if (length(parts) < 3) next
    
    sample_tag <- parts[2]
    project    <- parts[3]
    
    # identify sample
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
    
    # Paths under out_root
    project_block  <- sprintf("%s_backbone", project)
    experiment_dir <- file.path(cogtest_out_path, project_block, "experiment_data")
    
    # Dated destination folder (create if missing)
    dated_folder_name <- sprintf("%s_%s_%s_cogtest_data", project, today, sample)
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
    
    # Ensure experiment_data exists (for info/log files)
    if (!dir.exists(experiment_dir)) {
      dir.create(experiment_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Combine log and write per-project/sample info+csv into experiment_data
  log_df <- if (length(log_rows)) do.call(rbind, log_rows) else data.frame()
  
  if (nrow(log_df)) {
    combo <- unique(log_df[c("project","sample","dest_dir_used")])
    
    for (k in seq_len(nrow(combo))) {
      pj <- combo$project[k]
      sm <- combo$sample[k]
      dest_dir <- combo$dest_dir_used[k]
      exp_dir <- dirname(dest_dir)  # experiment_data
      
      stem <- sprintf("%s_%s_%s", pj, today, sm)
      info_path <- file.path(exp_dir, stem)
      csv_path  <- file.path(exp_dir, paste0(stem, "_log.csv"))
      
      sub <- subset(log_df, project == pj & sample == sm)
      counts <- sort(table(sub$status), decreasing = TRUE)
      counts_line <- if (length(counts)) paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = " | ") else "none"
      
      info_lines <- c(
        paste0("project: ", pj),
        paste0("sample: ", sm),
        paste0("date: ", today),
        paste0("dest_dir: ", dest_dir),
        paste0("total_rows: ", nrow(sub)),
        paste0("status_counts: ", counts_line)
      )
      writeLines(info_lines, con = info_path, useBytes = TRUE)
      utils::write.csv(sub, csv_path, row.names = FALSE, na = "")
      
      message("Wrote info file: ", info_path)
      message("Wrote log file : ", csv_path)
      message("Data folder   : ", dest_dir)
    }
  } else {
    message("No files processed — check env_objects and source data.")
  }
  
  invisible(log_df)
}
