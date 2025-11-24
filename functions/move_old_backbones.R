# Moves the *current* contents of each backbone folder into parallel old_data trees,
# for BOTH per-project backbones (2..9) and all_projects_backbone.
# On Windows, uses robocopy (fast, keeps timestamps). Elsewhere falls back to R.
move_old_backbones <- function(out_path, dry_run = FALSE, quiet = TRUE) {
  base <- normalizePath(out_path, winslash = "/", mustWork = FALSE)
  if (!nzchar(base)) stop("Invalid out_path")
  message("Method: ", if (.Platform$OS.type == "windows") "ROBOCOPY" else "R base copy",
          " | dry_run = ", dry_run, " | quiet = ", quiet)
  
  # internal: wrap a robocopy call quietly
  .rc <- function(src, dst, extra_args) {
    if (dry_run || !dir.exists(src)) return(invisible(TRUE))
    if (!dir.exists(dst)) dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    args <- c(shQuote(src), shQuote(dst), "*.*",
              "/R:2", "/W:1", "/E", "/MT:16", "/DCOPY:DAT", "/COPY:DAT",
              extra_args)
    # super-quiet flags: no headers, no summary, no file/dir list, no %progress
    quiet_flags <- c("/NJH", "/NJS", "/NDL", "/NFL", "/NS", "/NC", "/NP")
    if (.Platform$OS.type == "windows") {
      if (quiet) {
        # suppress stdout/stderr completely
        system2("robocopy", c(args, quiet_flags), stdout = FALSE, stderr = FALSE)
      } else {
        system2("robocopy", c(args), stdout = "", stderr = "")
      }
    } else {
      # non-Windows fallback: do nothing here; handled in move_tree()
      invisible(TRUE)
    }
  }
  
  move_tree <- function(src_dir, dst_dir) {
    if (!dir.exists(src_dir)) return(invisible(FALSE))
    if (!dir.exists(dst_dir) && !dry_run) dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
    
    if (.Platform$OS.type == "windows") {
      # 1) move experiment_data (whole subtree)
      if (dir.exists(file.path(src_dir, "experiment_data"))) {
        .rc(file.path(src_dir, "experiment_data"), file.path(dst_dir, "experiment_data"), c("/MOVE"))
      }
      # 2) move questionnaires (whole subtree)
      if (dir.exists(file.path(src_dir, "questionnaires"))) {
        .rc(file.path(src_dir, "questionnaires"), file.path(dst_dir, "questionnaires"), c("/MOVE"))
      }
      # 3) move everything else under src_dir, EXCLUDING experiment_data, questionnaires, and old_data (prevent nesting)
      excl <- c(
        shQuote(file.path(src_dir, "experiment_data")),
        shQuote(file.path(src_dir, "questionnaires")),
        shQuote(file.path(src_dir, "old_data"))
      )
      .rc(src_dir, dst_dir, c("/E", "/MOV", "/XD", excl))
    } else {
      # cross-platform quiet fallback in pure R
      sub <- list.files(src_dir, all.files = FALSE, full.names = TRUE, no.. = TRUE)
      if (!length(sub)) return(invisible(TRUE))
      for (p in sub) {
        # skip excluded dirs
        if (basename(p) %in% c("experiment_data", "questionnaires")) {
          next
        }
        if (basename(p) == "old_data") next
        dst <- file.path(dst_dir, basename(p))
        if (dry_run) next
        if (file.info(p)$isdir) {
          if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
          dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
          if (!file.rename(p, dst)) {
            file.copy(p, dst, recursive = TRUE)
            unlink(p, recursive = TRUE, force = TRUE)
          }
        } else {
          file.rename(p, dst) || (file.copy(p, dst) && unlink(p))
        }
      }
      # now move experiment_data + questionnaires subtrees
      for (subdir in c("experiment_data", "questionnaires")) {
        s <- file.path(src_dir, subdir)
        d <- file.path(dst_dir, subdir)
        if (dir.exists(s)) {
          if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
          files <- list.files(s, full.names = TRUE, all.files = FALSE, no.. = TRUE)
          for (p in files) {
            dst <- file.path(d, basename(p))
            file.rename(p, dst) || (file.copy(p, dst, recursive = TRUE) && unlink(p, recursive = TRUE))
          }
        }
      }
    }
    invisible(TRUE)
  }
  
  # per-project backbones (2..9)
  for (pid in as.character(2:9)) {
    src <- file.path(base, sprintf("%s_backbone", pid))
    if (dir.exists(src)) {
      message("\n Processing: ", pid, "_backbone")
      dst <- file.path(base, "old_data", sprintf("%s_backbone", pid))
      move_tree(src, dst)
    }
  }
  
  # all_projects_backbone
  all_src <- file.path(base, "all_projects_backbone")
  if (dir.exists(all_src)) {
    message("\n Processing: all_projects_backbone")
    all_dst <- file.path(base, "old_data", "all_projects_backbone")
    move_tree(file.path(all_src, "experiment_data"), file.path(all_dst, "experiment_data"))
    move_tree(file.path(all_src, "questionnaires"), file.path(all_dst, "questionnaires"))
    # last pass: everything else at the root of all_projects_backbone, excluding those two + old_data
    if (dir.exists(all_src)) move_tree(all_src, all_dst)
  }
  
  message("\n✅ Done. (robocopy output suppressed; excluded 'old_data' to avoid self-nesting.)")
}
