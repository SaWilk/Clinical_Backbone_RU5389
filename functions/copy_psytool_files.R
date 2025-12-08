# -------------------------------------------------------------------------
# copy_psytool_files() — progress + batched copies + SAFE one-time purges
# -------------------------------------------------------------------------

copy_psytool_files <- function(
    env_objects        = NULL,
    cogtest_out_path   = NULL,
    meta_env_name      = "cogtest_info",
    test_cols          = NULL,                      # NULL => auto-detect
    allowed_projects   = as.character(2:9),
    middle_subdir      = NULL,
    purge_old_dated    = TRUE,                      # delete old *_cogtest_data folders
    write_all_projects = TRUE,                      # also mirror under ALL_..._cogtest_data
    progress           = TRUE,                      # show a compact progress bar
    progress_every     = 1000,                      # emit a message every N files if no bar
    chunk_size         = 500                        # copy this many files per batch
) {
  base_dir <- normalizePath(getwd(), winslash = "\\", mustWork = TRUE)
  if (is.null(cogtest_out_path) || !nzchar(cogtest_out_path)) {
    cogtest_out_path <- file.path(base_dir, "01_project_data")
  }
  if (!dir.exists(cogtest_out_path)) {
    dir.create(cogtest_out_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  valid_samples <- c("adults", "adolescents", "children", "children_parents", "adults_remote")
  sample_root <- function(sample) {
    root <- file.path(base_dir, "raw_data", "psytoolkit")
    # prefer timestamped folders like PsyToolkitData_RU5389_BB_adults_YYYY_MM_DD_HH_MM
    pat  <- paste0("^PsyToolkitData_.*_", sample, "_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$")
    hits <- list.dirs(root, full.names = FALSE, recursive = FALSE)
    hits <- hits[grepl(pat, hits, ignore.case = TRUE)]
    if (length(hits)) {
      # pick the lexicographically last (newest) folder
      best <- hits[order(hits, decreasing = TRUE)][1]
      return(file.path(root, best, "experiment_data"))
    }
    # fallback to old static layout if the timestamped one doesn’t exist
    file.path(root, sample, "experiment_data")
  }
  
  # ---------- metadata ----------
  meta_df <- NULL
  if (!is.null(meta_env_name) && nzchar(meta_env_name) &&
      exists(meta_env_name, envir = .GlobalEnv, inherits = FALSE)) {
    maybe_df <- get(meta_env_name, envir = .GlobalEnv)
    if (is.data.frame(maybe_df)) meta_df <- maybe_df
  }
  norm_tok <- function(x) tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
  extract_ymd <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
    v <- x[[1L]]
    if (inherits(v, "Date"))   return(format(v, "%Y-%m-%d"))
    if (inherits(v, "POSIXt")) return(format(as.POSIXct(v, tz = "UTC"), "%Y-%m-%d"))
    if (is.numeric(v) && is.finite(v)) {
      num <- as.numeric(v); if (num > 1e12) num <- num / 1000
      if (num > 6e8 && num < 4e9) {
        ts <- as.POSIXct(num, origin = "1970-01-01", tz = "UTC")
        if (!is.na(ts)) return(format(ts, "%Y-%m-%d"))
      }
    }
    s <- trimws(as.character(v)); if (!nzchar(s)) return(NA_character_)
    m <- regexpr("\\b\\d{4}-\\d{2}-\\d{2}\\b", s)
    if (m[1] != -1) return(substr(s, m[1], m[1] + attr(m, "match.length") - 1))
    flex <- regexec("\\b(\\d{4})[-./](\\d{1,2})[-./](\\d{1,2})\\b", s)
    parts <- regmatches(s, flex)[[1]]
    if (length(parts) == 4) {
      yyyy <- as.integer(parts[2]); mm <- as.integer(parts[3]); dd <- as.integer(parts[4])
      suppressWarnings({
        dt <- try(as.Date(sprintf("%04d-%02d-%02d", yyyy, mm, dd)), silent = TRUE)
      })
      if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m-%d"))
    }
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
  date_for_sample <- function(sample_name) {
    if (is.null(meta_df)) return("unknown_date")
    needed_cols <- c("sample", "ctime")
    if (!all(needed_cols %in% names(meta_df))) return("unknown_date")
    meta_sample_norm <- norm_tok(meta_df$sample)
    needle <- norm_tok(sample_name)
    rows <- which(meta_sample_norm == needle)
    if (!length(rows)) rows <- which(grepl(needle, meta_sample_norm, fixed = TRUE))
    if (!length(rows)) return("unknown_date")
    ctimes <- meta_df$ctime[rows]
    for (ct in ctimes) {
      ymd <- extract_ymd(ct)
      if (!is.na(ymd) && nzchar(ymd)) return(ymd)
    }
    "unknown_date"
  }
  
  pick_id_col <- function(df) {
    for (nm in c("Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "id", "vpid_1", "vpid")) {
      if (nm %in% names(df)) return(nm)
    }
    return(NA_character_)
  }
  
  looks_like_path_col <- function(v, src_root) {
    if (!is.character(v)) return(FALSE)
    vv <- trimws(v)
    any(grepl("\\.(txt|csv)(\\b|$)", vv, ignore.case = TRUE) |
          file.exists(file.path(src_root, vv)), na.rm = TRUE)
  }
  detect_test_cols <- function(df, src_root) {
    cand <- names(df)[vapply(df, looks_like_path_col, logical(1), src_root = src_root)]
    if (length(cand)) return(cand)
    names(df)[grepl("^(WCST|LNS|BACS).*", names(df), ignore.case = TRUE) &
                vapply(df, is.character, logical(1))]
  }
  
  if (is.null(env_objects)) env_objects <- ls(envir = .GlobalEnv)
  env_objects <- as.character(env_objects)
  
  all_logs <- list()
  staged <- list()
  staged_all <- list()
  
  # NEW: purge guards so we only delete once per project (and once per sample for ALL)
  purged_projects <- character(0)
  purged_samples_all <- character(0)
  
  ensure_experiment_dir <- function(project, middle_subdir, sample_for_regex = NULL) {
    project_block  <- sprintf("%s_backbone", project)
    experiment_dir <- if (is.null(middle_subdir) || !nzchar(middle_subdir)) {
      file.path(cogtest_out_path, project_block, "experiment_data")
    } else {
      file.path(cogtest_out_path, project_block, middle_subdir, "experiment_data")
    }
    if (!dir.exists(experiment_dir)) dir.create(experiment_dir, recursive = TRUE, showWarnings = FALSE)
    
    # purge once per project
    if (purge_old_dated && !(project %in% purged_projects)) {
      olds <- list.dirs(experiment_dir, full.names = TRUE, recursive = FALSE)
      # keep only dated _cogtest_data folders (any sample)
      olds <- olds[grepl("^[0-9A-Z]+_\\d{4}-\\d{2}-\\d{2}_.+_cogtest_data$", basename(olds), ignore.case = TRUE)]
      if (length(olds)) for (od in olds) unlink(od, recursive = TRUE, force = TRUE)
      purged_projects <<- c(purged_projects, project)
    }
    experiment_dir
  }
  
  # ---------- stage jobs ----------
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
    if (!(project %in% allowed_projects)) next
    
    src_root <- sample_root(sample)
    if (!dir.exists(src_root)) next
    
    df <- get(obj, envir = .GlobalEnv)
    if (!is.data.frame(df)) next
    
    path_cols <- if (is.null(test_cols)) detect_test_cols(df, src_root) else test_cols
    path_cols <- path_cols[path_cols %in% names(df)]
    path_cols <- path_cols[vapply(df[path_cols], is.character, logical(1))]
    if (!length(path_cols)) {
      message(sprintf("copy_psytool_files: No path columns detected for %s (project %s).", sample, project))
      next
    }
    
    date_tag <- date_for_sample(sample)
    
    # project-specific target
    exp_dir <- ensure_experiment_dir(project, middle_subdir)
    dated_folder_name <- sprintf("%s_%s_%s_cogtest_data", project, date_tag, sample)
    dest_dir <- file.path(exp_dir, dated_folder_name)
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ALL mirror (purge ONCE per sample; only that sample's folders)
    if (isTRUE(write_all_projects)) {
      all_exp_dir <- file.path(cogtest_out_path, "all_projects_backbone", "experiment_data")
      if (!dir.exists(all_exp_dir)) dir.create(all_exp_dir, recursive = TRUE, showWarnings = FALSE)
      
      if (purge_old_dated && !(sample %in% purged_samples_all)) {
        olds_all <- list.dirs(all_exp_dir, full.names = TRUE, recursive = FALSE)
        # delete ONLY folders for this sample (safe!)
        pat <- sprintf("^ALL_\\d{4}-\\d{2}-\\d{2}_%s_cogtest_data$", sample)
        olds_all <- olds_all[grepl(pat, basename(olds_all), ignore.case = TRUE)]
        if (length(olds_all)) for (od in olds_all) unlink(od, recursive = TRUE, force = TRUE)
        purged_samples_all <<- c(purged_samples_all, sample)
      }
      
      all_dated_name <- sprintf("ALL_%s_%s_cogtest_data", date_tag, sample)
      dest_all_dir <- file.path(all_exp_dir, all_dated_name)
      if (!dir.exists(dest_all_dir)) dir.create(dest_all_dir, recursive = TRUE, showWarnings = FALSE)
    } else {
      dest_all_dir <- NULL
    }
    
    id_col <- pick_id_col(df)
    pid_vec <- if (!is.na(id_col)) df[[id_col]] else rep(NA, nrow(df))
    
    for (test in path_cols) {
      rel <- as.character(df[[test]])
      keep <- nzchar(rel) & !is.na(rel)
      if (!any(keep)) next
      
      rel_ok <- rel[keep]
      src <- file.path(src_root, rel_ok)
      exists_mask <- file.exists(src)
      
      # log rows (copy outcome finalized later)
      log_chunk <- data.frame(
        env_object   = obj,
        project      = project,
        sample       = sample,
        participant_id = pid_vec[keep],
        test         = test,
        source       = src,
        destination  = file.path(dest_dir, basename(src)),
        status       = ifelse(exists_mask, "queued", "missing_source"),
        dest_dir_used= dest_dir,
        stringsAsFactors = FALSE
      )
      all_logs[[length(all_logs) + 1L]] <- log_chunk
      
      if (any(exists_mask)) {
        jobs_ok <- data.frame(
          src   = src[exists_mask],
          dest  = file.path(dest_dir, basename(src[exists_mask])),
          sample= sample,
          project = project,
          env_object = obj,
          pid   = pid_vec[keep][exists_mask],
          test  = test,
          stringsAsFactors = FALSE
        )
        staged[[length(staged) + 1L]] <- jobs_ok
        if (!is.null(dest_all_dir)) {
          staged_all[[length(staged_all) + 1L]] <-
            transform(jobs_ok, dest = file.path(dest_all_dir, basename(src)))
        }
      }
    }
  }
  
  jobs <- if (length(staged)) do.call(rbind, staged) else data.frame()
  jobs_all <- if (length(staged_all)) do.call(rbind, staged_all) else data.frame()
  
  total_to_copy <- nrow(jobs) + nrow(jobs_all)
  if (!total_to_copy) {
    message("copy_psytool_files: No files to copy (nothing queued).")
    log_df <- if (length(all_logs)) do.call(rbind, all_logs) else data.frame()
    return(invisible(log_df))
  }
  
  # ---------- progress ----------
  pb <- if (isTRUE(progress)) utils::txtProgressBar(min = 0, max = total_to_copy, style = 3) else NULL
  copied <- 0L
  advance <- function(n) {
    if (!is.null(pb)) utils::setTxtProgressBar(pb, n)
    if (is.null(pb) && progress_every > 0L && (n %% progress_every == 0L)) {
      message(sprintf("copy_psytool_files: copied %d / %d files...", n, total_to_copy))
    }
  }
  
  # ---------- batched copies ----------
  copy_chunked <- function(src_vec, dest_vec) {
    n <- length(src_vec); if (!n) return(invisible(NULL))
    idx <- seq_len(n)
    splits <- split(idx, ceiling(idx / max(1L, chunk_size)))
    for (sp in splits) {
      file.copy(src_vec[sp], dest_vec[sp], overwrite = FALSE, copy.date = TRUE)
      copied <<- copied + length(sp)
      advance(copied)
    }
  }
  copy_chunked(jobs$src, jobs$dest)
  if (nrow(jobs_all)) copy_chunked(jobs_all$src, jobs_all$dest)
  if (!is.null(pb)) close(pb)
  
  # ---------- finalize logs ----------
  log_df <- if (length(all_logs)) do.call(rbind, all_logs) else data.frame()
  if (nrow(log_df)) {
    exists_now <- file.exists(log_df$destination)
    log_df$status <- ifelse(log_df$status == "missing_source", "missing_source",
                            ifelse(exists_now, "exists_or_copied", "copy_failed"))
  }
  invisible(log_df)
}
