# Archives the replaceable raw/preprocessed contents of every active backbone
# before a new preprocessing run starts. The derivatives folder is persistent
# analysis output and is therefore excluded from every move, comparison, and
# cleanup operation.
move_old_backbones <- function(out_path, dry_run = FALSE, quiet = TRUE) {
  base <- normalizePath(out_path, winslash = "/", mustWork = FALSE)
  if (!nzchar(base)) stop("Invalid out_path")

  unresolved_files_total <- 0L
  projects_with_archive_warnings <- character(0)

  method <- if (.Platform$OS.type == "windows") "ROBOCOPY" else "R base move"
  message("Method: ", method, " | dry_run = ", dry_run, " | quiet = ", quiet)

  files_below <- function(path) {
    if (!dir.exists(path)) return(character(0))
    list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE,
      include.dirs = FALSE
    )
  }

  files_to_archive <- function(path) {
    files <- files_below(path)
    if (!length(files)) return(character(0))

    source_root <- normalizePath(path, winslash = "/", mustWork = TRUE)
    source_root <- sub("/+$", "", source_root)
    source_prefix <- paste0(source_root, "/")
    normalized_files <- normalizePath(files, winslash = "/", mustWork = TRUE)

    inside_source <- startsWith(tolower(normalized_files), tolower(source_prefix))
    if (!all(inside_source)) {
      stop("Cannot inspect files outside the active backbone: ", path)
    }

    relative_paths <- substring(normalized_files, nchar(source_prefix) + 1L)
    files[!grepl("^derivatives(/|$)", relative_paths, ignore.case = TRUE)]
  }

  remove_empty_non_derivative_entries <- function(path) {
    if (!dir.exists(path)) return(invisible(TRUE))

    entries <- list.files(
      path,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    entries <- entries[tolower(basename(entries)) != "derivatives"]

    for (entry in entries) {
      info <- file.info(entry)
      if (isTRUE(info$isdir) && !length(files_below(entry))) {
        unlink(entry, recursive = TRUE, force = TRUE)
      }
    }

    invisible(TRUE)
  }

  run_robocopy_move <- function(src, dst) {
    if (!dir.exists(dst)) {
      dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    }

    args <- c(
      shQuote(src),
      shQuote(dst),
      "*",
      "/MOVE",
      "/E",
      "/XJ",
      "/R:2",
      "/W:1",
      "/MT:16",
      "/DCOPY:DAT",
      "/COPY:DAT",
      "/XD",
      shQuote(file.path(src, "derivatives"))
    )
    if (isTRUE(quiet)) {
      args <- c(args, "/NJH", "/NJS", "/NDL", "/NFL", "/NS", "/NC", "/NP")
    }

    output <- tryCatch(
      suppressWarnings(system2("robocopy", args, stdout = TRUE, stderr = TRUE)),
      error = function(e) {
        warning(
          "Could not start robocopy while archiving ", src, ": ",
          conditionMessage(e),
          ". The preprocessing script will continue and the files will be ",
          "left in place."
        )
        NULL
      }
    )
    if (is.null(output)) return(invisible(NA_integer_))

    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    status <- as.integer(status)

    if (!isTRUE(quiet) && length(output)) {
      cat(output, sep = "\n")
      cat("\n")
    }

    # Robocopy codes 0--7 are successful outcomes. Higher codes are warnings
    # here: archiving is best-effort and must never block preprocessing.
    if (is.na(status) || status >= 8L) {
      details <- if (length(output)) {
        paste(tail(output, 20L), collapse = "\n")
      } else {
        "No robocopy diagnostic output was returned."
      }
      warning(
        "Robocopy could not completely archive the active backbone. The ",
        "preprocessing script will continue and unresolved files will remain ",
        "in place.\nSource: ", src,
        "\nDestination: ", dst,
        "\nExit code: ", status,
        "\n", details
      )
    }

    invisible(status)
  }

  remove_byte_identical_archived_files <- function(src, dst, source_files) {
    if (!length(source_files)) return(invisible(0L))

    source_root <- normalizePath(src, winslash = "/", mustWork = TRUE)
    source_root <- sub("/+$", "", source_root)
    source_prefix <- paste0(source_root, "/")
    source_files <- normalizePath(source_files, winslash = "/", mustWork = TRUE)

    inside_source <- startsWith(tolower(source_files), tolower(source_prefix))
    if (!all(inside_source)) {
      stop("Cannot derive archive paths for files outside the source backbone.")
    }

    relative_paths <- substring(source_files, nchar(source_prefix) + 1L)
    archive_files <- file.path(dst, relative_paths)
    archive_exists <- file.exists(archive_files) & !dir.exists(archive_files)

    source_sizes <- file.info(source_files)$size
    archive_sizes <- rep(NA_real_, length(archive_files))
    archive_sizes[archive_exists] <- file.info(archive_files[archive_exists])$size
    same_size <- archive_exists &
      !is.na(source_sizes) &
      !is.na(archive_sizes) &
      source_sizes == archive_sizes

    same_content <- rep(FALSE, length(source_files))
    hash_candidates <- which(same_size)
    if (length(hash_candidates)) {
      source_hashes <- unname(tools::md5sum(source_files[hash_candidates]))
      archive_hashes <- unname(tools::md5sum(archive_files[hash_candidates]))
      same_content[hash_candidates] <-
        !is.na(source_hashes) &
        !is.na(archive_hashes) &
        source_hashes == archive_hashes
    }

    duplicates <- source_files[same_content]
    if (!length(duplicates)) return(invisible(0L))

    suppressWarnings(file.remove(duplicates))
    removed <- !file.exists(duplicates)
    if (any(removed)) {
      message(
        "Removed ", sum(removed),
        " active file(s) whose byte-identical archive copy already existed."
      )
    }

    invisible(sum(removed))
  }

  move_entry_with_r <- function(src, dst) {
    info <- file.info(src)
    if (is.na(info$isdir)) stop("Cannot inspect source while archiving: ", src)

    if (isTRUE(info$isdir)) {
      if (file.exists(dst) && !dir.exists(dst)) {
        stop("Archive destination is a file but source is a directory: ", dst)
      }

      if (!dir.exists(dst) && file.rename(src, dst)) return(invisible(TRUE))
      if (!dir.exists(dst)) {
        if (!dir.create(dst, recursive = TRUE, showWarnings = FALSE)) {
          stop("Could not create archive directory: ", dst)
        }
      }

      children <- list.files(
        src,
        full.names = TRUE,
        all.files = TRUE,
        no.. = TRUE
      )
      for (child in children) {
        move_entry_with_r(child, file.path(dst, basename(child)))
      }

      leftovers <- list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      if (length(leftovers)) {
        stop(
          "Archiving was incomplete; source entries remain in: ", src,
          "\n", paste(leftovers, collapse = "\n")
        )
      }
      if (unlink(src, recursive = TRUE, force = TRUE) != 0L || dir.exists(src)) {
        stop("Could not remove the now-empty source directory: ", src)
      }
      return(invisible(TRUE))
    }

    if (dir.exists(dst)) {
      stop("Archive destination is a directory but source is a file: ", dst)
    }
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)

    moved <- !file.exists(dst) && file.rename(src, dst)
    if (!moved) {
      copied <- file.copy(
        src,
        dst,
        overwrite = TRUE,
        copy.mode = TRUE,
        copy.date = TRUE
      )
      if (!isTRUE(copied)) stop("Could not copy file into archive: ", src)
      if (unlink(src, force = TRUE) != 0L || file.exists(src)) {
        stop("File was copied but could not be removed from active data: ", src)
      }
    }

    invisible(TRUE)
  }

  move_tree <- function(src, dst) {
    if (!dir.exists(src)) return(invisible(FALSE))
    if (isTRUE(dry_run)) {
      message("[dry run] ", src, " -> ", dst)
      return(invisible(TRUE))
    }

    if (.Platform$OS.type == "windows") {
      run_robocopy_move(src, dst)
    } else {
      if (!dir.exists(dst)) {
        dir.create(dst, recursive = TRUE, showWarnings = FALSE)
      }
      entries <- list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      entries <- entries[tolower(basename(entries)) != "derivatives"]
      for (entry in entries) {
        move_entry_with_r(entry, file.path(dst, basename(entry)))
      }
    }

    remaining_files <- files_to_archive(src)
    if (.Platform$OS.type == "windows" && length(remaining_files)) {
      # Robocopy normally skips files that already exist unchanged at the
      # destination. With /MOVE, skipped files remain in the source. Remove
      # them only after an exact size + MD5 comparison proves that the archive
      # already contains the same bytes at the corresponding relative path.
      tryCatch(
        remove_byte_identical_archived_files(src, dst, remaining_files),
        error = function(e) {
          warning(
            "Could not verify already archived duplicate files below ", src,
            ": ", conditionMessage(e),
            ". The files will be left in place and preprocessing will continue."
          )
        }
      )
      remaining_files <- files_to_archive(src)
    }

    if (length(remaining_files)) {
      preview <- head(remaining_files, 20L)
      more <- length(remaining_files) - length(preview)
      unresolved_files_total <<- unresolved_files_total + length(remaining_files)
      warning(
        "Archiving was incomplete, but preprocessing will continue. Active ",
        "files remain in:\n", src,
        "\nThese files either have no byte-identical archive copy or could ",
        "not be removed (for example because they are locked).\n",
        paste(preview, collapse = "\n"),
        if (more > 0L) paste0("\n... and ", more, " more file(s).") else ""
      )
    }

    # Remove only empty shells outside derivatives. The active backbone root
    # and its complete derivatives tree remain exactly where they are.
    remove_empty_non_derivative_entries(src)

    invisible(TRUE)
  }

  backbone_names <- c(paste0(2:9, "_backbone"), "all_projects_backbone")
  for (backbone_name in backbone_names) {
    src <- file.path(base, backbone_name)
    if (!dir.exists(src)) next

    message("\nProcessing: ", backbone_name)
    dst <- file.path(base, "old_data", backbone_name)
    tryCatch(
      move_tree(src, dst),
      error = function(e) {
        projects_with_archive_warnings <<- c(
          projects_with_archive_warnings,
          backbone_name
        )
        warning(
          "Unexpected archive problem in ", backbone_name, ": ",
          conditionMessage(e),
          ". This project was left as-is and preprocessing will continue."
        )
      }
    )
  }

  message(
    "\nDone. Backbone archiving finished in best-effort mode; derivatives ",
    "were left untouched."
  )
  if (unresolved_files_total > 0L) {
    message(
      unresolved_files_total,
      " file(s) could not be archived and remain in their active folders."
    )
  }
  if (length(projects_with_archive_warnings)) {
    message(
      "Projects with unexpected archive warnings: ",
      paste(unique(projects_with_archive_warnings), collapse = ", ")
    )
  }
  invisible(TRUE)
}
