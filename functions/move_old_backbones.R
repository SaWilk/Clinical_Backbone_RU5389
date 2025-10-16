# -------------------------------------------------------------------------
# move_old_backbones() — move files-only everywhere *except* inside
#                        experiment_data and questionnaire*, which are
#                        moved entirely (folders + files).
# -------------------------------------------------------------------------
move_old_backbones <- function(base_dir, dry_run = TRUE, method = c("auto","robocopy","base"), mt = 16) {
  method <- match.arg(method)
  base_dir <- normalizePath(base_dir, mustWork = TRUE)
  old_data_dir <- file.path(base_dir, "old_data")
  if (!dir.exists(old_data_dir) && !dry_run) dir.create(old_data_dir, recursive = TRUE)
  
  win <- .Platform$OS.type == "windows"
  have_robo <- nzchar(Sys.which("robocopy"))
  if (method == "auto") method <- if (win && have_robo) "robocopy" else "base"
  
  # names to treat as "move entire subtree"
  special_names <- c("experiment_data",
                     "questionnaire_data","questionnaires","questionnaire","questionnaire-data","questionnaires_data")
  
  # --- utils
  escape_rx <- function(x) gsub("([\\^$.|()?*+{}\\[\\]\\\\])","\\\\\\1", x)
  
  # ---- BASE method helpers ----
  move_dir_tree_base <- function(src_subdir, dest_subdir) {
    message(if (dry_run) "[DRY RUN] " else "", "Move DIR tree:\n  ", src_subdir, "\n  -> ", dest_subdir)
    if (dry_run) return(invisible())
    dir.create(dest_subdir, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(src_subdir, dirname(dest_subdir), recursive = TRUE, copy.date = TRUE, overwrite = TRUE)
    if (!ok) warning("Copy failed for dir: ", src_subdir)
    # remove original subtree
    unlink(src_subdir, recursive = TRUE, force = TRUE)
  }
  
  move_files_only_base <- function(src_dir, dest_dir, exclude_dirs = character(0)) {
    # mirror directories at destination
    src_dirs <- c(src_dir, list.dirs(src_dir, recursive = TRUE, full.names = TRUE))
    # drop excluded trees from mirroring (they were moved already)
    if (length(exclude_dirs)) {
      keep <- !Reduce(`|`, lapply(exclude_dirs, function(ed) startsWith(src_dirs, paste0(ed, .Platform$file.sep))), init = FALSE)
      src_dirs <- src_dirs[keep]
    }
    dest_dirs <- sub(paste0("^", escape_rx(src_dir)), dest_dir, src_dirs)
    if (!dry_run) for (d in unique(dest_dirs)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    
    # collect files excluding special trees
    paths <- list.files(src_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(exclude_dirs)) {
      paths <- paths[!Reduce(`|`, lapply(exclude_dirs, function(ed) startsWith(paths, paste0(ed, .Platform$file.sep))), init = FALSE)]
    }
    files <- paths[file.info(paths)$isdir == FALSE]
    if (!length(files)) return(invisible())
    
    dest_files <- sub(paste0("^", escape_rx(src_dir)), dest_dir, files)
    message(if (dry_run) "[DRY RUN] " else "", "Move FILES (keep dirs): ", length(files), "\n  ", src_dir, "\n  -> ", dest_dir)
    if (!dry_run) {
      ok <- file.copy(from = files, to = dest_files, overwrite = TRUE, copy.date = TRUE)
      if (!all(ok)) warning("Some files failed to copy in: ", src_dir)
      unlink(files[ok], recursive = FALSE, force = TRUE)
    }
  }
  
  # ---- ROBOCOPY method helpers (Windows) ----
  move_dir_tree_robo <- function(src_subdir, dest_subdir) {
    if (!dry_run && !dir.exists(dest_subdir)) dir.create(dest_subdir, recursive = TRUE)
    cmd <- sprintf('robocopy "%s" "%s" /E /MOVE /COPY:DAT /DCOPY:DAT /R:2 /W:1 /MT:%d /NFL /NDL',
                   src_subdir, dest_subdir, as.integer(mt))
    message(if (dry_run) "[DRY RUN] " else "", cmd)
    if (!dry_run) {
      status <- suppressWarnings(system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE))
      if (!is.numeric(status)) status <- 0
      if (status >= 8) stop("ROBOCOPY failed with exit code ", status, " for: ", src_subdir)
    }
  }
  
  move_files_only_robo <- function(src_dir, dest_dir, exclude_dirs = character(0)) {
    if (!dry_run && !dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    # /E /MOV moves files (keeps dir tree). Exclude special dirs already moved with /XD
    xd <- if (length(exclude_dirs)) paste(' /XD', paste(sprintf(' "%s"', exclude_dirs), collapse = "")) else ""
    cmd <- sprintf('robocopy "%s" "%s" /E /MOV /COPY:DAT /DCOPY:DAT /R:2 /W:1 /MT:%d /NFL /NDL%s',
                   src_dir, dest_dir, as.integer(mt), xd)
    message(if (dry_run) "[DRY RUN] " else "", cmd)
    if (!dry_run) {
      status <- suppressWarnings(system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE))
      if (!is.numeric(status)) status <- 0
      if (status >= 8) stop("ROBOCOPY failed with exit code ", status, " for: ", src_dir)
    }
  }
  
  # find top-level numbered dirs
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  numbered_dirs <- dirs[grepl("^\\d", basename(dirs))]
  if (!length(numbered_dirs)) { message("No top-level numbered folders in: ", base_dir); return(invisible()) }
  
  message("Method: ", toupper(method), " | dry_run = ", dry_run)
  for (src_dir in numbered_dirs) {
    if (normalizePath(src_dir) == normalizePath(old_data_dir)) next
    dest_dir <- file.path(old_data_dir, basename(src_dir))
    if (!dry_run && !dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    
    message("\n📂 Processing: ", basename(src_dir))
    
    # Identify special subfolders (immediate children only)
    present_special <- file.path(src_dir, basename(list.dirs(src_dir, full.names = FALSE, recursive = FALSE)))
    present_special <- present_special[basename(present_special) %in% special_names]
    present_special <- present_special[dir.exists(present_special)]
    
    # 1) MOVE entire subtree for each special folder
    for (ss in present_special) {
      dest_ss <- file.path(dest_dir, basename(ss))
      if (method == "robocopy") move_dir_tree_robo(ss, dest_ss) else move_dir_tree_base(ss, dest_ss)
    }
    
    # 2) Move FILES-ONLY for the rest of the folder, excluding the special ones already moved
    if (method == "robocopy") {
      move_files_only_robo(src_dir, dest_dir, exclude_dirs = present_special)
    } else {
      move_files_only_base(src_dir, dest_dir, exclude_dirs = present_special)
    }
  }
  
  message("\n✅ Done. Inside 'experiment_data' and 'questionnaire*' we moved folders + files; elsewhere we moved files only.")
}
