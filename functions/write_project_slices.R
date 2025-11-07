write_project_slices <- function(
    prep,
    export_csv = FALSE,
    dry_run = FALSE
) {
  stopifnot(inherits(prep, "project_prep"))
  export_csv <- isTRUE(export_csv)
  verbose <- isTRUE(prep$verbose)
  if (!dry_run && !requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. install.packages('writexl')")
  }
  
  collected_dirs <- character(0)
  for (i in seq_along(prep$split_list)) {
    info <- prep$split_list[[i]]
    d <- info$df
    if (info$pid %in% c("0","99","unknown") || .is_empty_df(d)) next
    .ensure_dir(info$pid_dir, dry_run)
    if (!dry_run) {
      if (export_csv) utils::write.csv(d, file.path(info$pid_dir, paste0(info$file_base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(d, file.path(info$pid_dir, paste0(info$file_base, ".xlsx")))
    }
    if (verbose) message("Saved: ", file.path(info$pid_dir, paste0(info$file_base, ".xlsx")), " | env: ", info$varname)
    collected_dirs <- c(collected_dirs, info$pid_dir)
  }
  
  # Composite write (skip for pilot adults per your rule)
  if (!(isTRUE(prep$pilot_mode) && identical(prep$sample_name, "adults"))) {
    comp_df <- if (!is.null(prep$composite_var) && exists(prep$composite_var, envir = .GlobalEnv))
      get(prep$composite_var, envir = .GlobalEnv) else NULL
    if (!is.null(comp_df)) {
      all_projects_dir <- file.path(prep$base_dir, "all_projects_backbone", prep$subfolder_main)
      .ensure_dir(all_projects_dir, dry_run)
      comp_base <- paste(c("ALL", prep$date_str, if (prep$pilot_mode) "PILOT" else NULL,
                           prep$sample_name, prep$file_suffix), collapse = "_")
      if (!dry_run) {
        if (export_csv) utils::write.csv(comp_df, file.path(all_projects_dir, paste0(comp_base, ".csv")), row.names = FALSE, na = "")
        writexl::write_xlsx(comp_df, file.path(all_projects_dir, paste0(comp_base, ".xlsx")))
      }
      if (verbose) message("Saved composite sample dataset: ", file.path(all_projects_dir, paste0(comp_base, ".xlsx")))
    }
  } else if (verbose) {
    message("Skipping composite write for pilot adults (per specification).")
  }
  
  collected_dirs <- unique(collected_dirs)
  if (prep$pilot_mode) {
    names(collected_dirs) <- basename(dirname(dirname(collected_dirs)))
  } else {
    names(collected_dirs) <- basename(dirname(collected_dirs))
  }
  collected_dirs
}
