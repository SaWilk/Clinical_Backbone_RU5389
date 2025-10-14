# -------------------------------------------------------------------------
#   move_old_backbones()
# Purpose:
#   Consolidate files from top-level numbered folders into an "old_data"
# area, preserving each folder’s internal directory tree while leaving the
# original directories in place (only files are moved).
# Behavior:
#   - Target selection:
#   • Scans base_dir (non-recursive) for folders whose names start with a digit.
# • Skips the "old_data" folder itself if present.
# - Destination:
#   • Creates <base_dir>/old_data/<source-folder-name> (if not dry-run).
# - Move strategy:
#   • "auto" (default): uses "robocopy" on Windows if available; otherwise falls back to "base".
# • "robocopy" (Windows): runs robocopy /E /MOV so only files are moved and the directory tree is recreated.
# • "base" (cross-platform): mirrors the source directory tree, copies files with timestamps, then deletes
# only the successfully copied files; directories remain intact.
# - Dry run:
#   • When dry_run = TRUE, prints planned actions/commands; does not create, copy, or delete anything.
# - Messaging:
#   • Prints the selected method, per-folder processing, and summary when done.
# Inputs:
#   base_dir : string. Root directory containing the top-level numbered folders.
# dry_run : logical. If TRUE, simulate without modifying the filesystem. Default TRUE.
# method : {"auto","robocopy","base"}. Move method (see above). Default "auto".
# mt : integer. Thread count for robocopy /MT:. Ignored for "base". Default 16.
# Output:
#   Returns (invisibly): NULL. Side effects are filesystem changes (unless dry-run).
# Notes:
#   - Only files are moved; all source directories remain (possibly empty).
# - Windows "robocopy" exit codes ≥ 8 are treated as errors.
# - Hidden files are included. Symlinks/junctions are treated as regular entries by the respective method.
# - Paths are normalized via normalizePath(); base_dir must exist.
# Example:
#   move_old_backbones(base_dir = "D:/projects/mydata", dry_run = FALSE)
# -------------------------------------------------------------------------

move_old_backbones <- function(base_dir, dry_run = TRUE, method = c("auto","robocopy","base"), mt = 16) {
  method <- match.arg(method)
  base_dir <- normalizePath(base_dir, mustWork = TRUE)
  old_data_dir <- file.path(base_dir, "old_data")
  if (!dir.exists(old_data_dir) && !dry_run) dir.create(old_data_dir)
  
  win <- .Platform$OS.type == "windows"
  have_robo <- nzchar(Sys.which("robocopy"))
  if (method == "auto") method <- if (win && have_robo) "robocopy" else "base"
  
  # ---- BASE method: move FILES only; keep all source dirs ----
  move_files_base <- function(src_dir, dest_dir) {
    # 1) create all destination subdirs to mirror source
    src_dirs <- c(src_dir, list.dirs(src_dir, recursive = TRUE, full.names = TRUE))
    dest_dirs <- sub(paste0("^", gsub("([\\^$.|()?*+{}])","\\\\\\1", src_dir)), dest_dir, src_dirs)
    if (!dry_run) for (d in unique(dest_dirs)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    
    # 2) copy files preserving relative paths
    files <- list.files(src_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[file.info(files)$isdir == FALSE]
    if (!length(files)) return(invisible())
    dest_files <- sub(paste0("^", gsub("([\\^$.|()?*+{}])","\\\\\\1", src_dir)), dest_dir, files)
    
    message(if (dry_run) "[DRY RUN] " else "", "Moving ", length(files), " files from\n  ", src_dir, "\n  -> ", dest_dir)
    if (!dry_run) {
      ok <- file.copy(from = files, to = dest_files, overwrite = TRUE, copy.date = TRUE, recursive = FALSE)
      if (!all(ok)) warning("Some files failed to copy in: ", src_dir)
      # Remove only the files that copied successfully; leave all directories intact
      unlink(files[ok], recursive = FALSE, force = TRUE)
    }
  }
  
  # ---- ROBOCOPY method (Windows): /MOV moves files ONLY (keeps dirs), /E creates dir tree ----
  move_files_robocopy <- function(src_dir, dest_dir) {
    if (!dry_run && !dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    cmd <- sprintf('robocopy "%s" "%s" /E /MOV /COPY:DAT /DCOPY:DAT /R:2 /W:1 /MT:%d /NFL /NDL',
                   src_dir, dest_dir, as.integer(mt))
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
    message("\n📂 Processing: ", basename(src_dir))
    if (method == "robocopy") move_files_robocopy(src_dir, dest_dir) else move_files_base(src_dir, dest_dir)
  }
  
  message("\n✅ Done. Numbered folders and their subfolders remain; only files were moved.")
}
