#' Separate a dataset into per-project slices and export files
#'
#' @description
#' Splits `df` by a detected **project** column and writes one file per project
#' into a backbone directory structure. Also assigns each per-project slice into
#' `.GlobalEnv` (e.g., `data_<sample>_p_<pid>_<env>`) and returns the set of
#' output directories that were touched.
#'
#' @details
#' **Purpose**
#' - Create per-project deliverables (`.xlsx` and optionally `.csv`) under
#'   `"<base>/<PID>_backbone/<subfolder>"`.
#'
#' **Behavior**
#' - **Base directory resolution**
#'   1. If `out_path` is provided, normalize it; if it ends with
#'      `"experiment_data"` or `"questionnaires"`, its parent is used as base.
#'   2. Else try to detect the script dir (command line `--file=`, RStudio
#'      active document, or `knitr::current_input()`).
#'   3. Else fall back to `getwd()` (with a warning).
#'   4. The **subfolder** is `"questionnaires"` or `"experiment_data"` depending
#'      on `data_type`.
#'
#' - **Project column**
#'   Looks for a column among:
#'   `c("project","Projekt...Project","Projekt.","Projekt","projekt","p","proj","Proj")`.
#'   If none is found, creates `"Projekt."` with value `"6"` for all rows and warns.
#'
#' - **Pre-filtering**
#'   * If column `"Versuchspersonennummer."` exists, rows with value `99999`
#'     (numeric or character) are removed (treated as test data).
#'   * Project labels are `trimws()`ed; rows with empty project labels or exactly
#'     `"Testing mode(No actual data collection)"` are dropped.
#'
#' - **Sample detection**
#'   * If `sample` is provided, it is normalized to one of:
#'     `"adults"`, `"adolescents"`, `"children_parents"`, `"parents_p6"`, `"children_p6"`.
#'   * Otherwise, attempts to infer a sample name from the call/site (argument
#'     symbols, parent frame objects). If unsuccessful, uses `"unknown"`.
#'
#' - **Metadata (optional)**
#'   * If `metadata_info` is a data.frame or a path to `.csv`/`.xlsx`, the code
#'     tries to match `sample` in column `sample` and extract the first non-`NA`
#'     `ctime`, which becomes the file date string (`YYYY-MM-DD`). If this fails,
#'     `"unknown_date"` is used (with a warning).
#'
#' - **Export suffixes**
#'   * `data_type = "questionnaires"` → file suffix `"questionnaire"`,
#'     env suffix `"questionnaire"`, subfolder `"questionnaires"`.
#'   * `data_type = "experiment_data"` → `"cogtests"`, `"cogtest"`, `"experiment_data"`.
#'
#' - **Splitting and writing**
#'   * Split `df` by project label (from the detected/created project column).
#'   * For each split, compute `pid` as the digits from the label; if no digits,
#'     use `"unknown"`.
#'   * Assign the split into `.GlobalEnv` as
#'     `data_<sample>_p_<pid>_<env_suffix>`.
#'   * Skip `pid %in% c("0","99","unknown")` and empty/all-NA splits.
#'   * Destination directory: `file.path(base_dir, sprintf("%s_backbone", pid), subfolder)`.
#'   * File base name: `"<pid>_<date>_<sample>_<suffix>"`.
#'   * If `dry_run = FALSE`, write an `.xlsx` (via **writexl**) and, if
#'     `export_csv = TRUE`, also a `.csv`.
#'   * Collect and return the set of unique project directories touched; the
#'     returned vector is **named** by `"<pid>_backbone"`.
#'
#' - **Messages**
#'   * Verbose progress (base dir, project column, sample, per-file saves).
#'   * Warnings when metadata is missing/invalid or base dir cannot be detected.
#'
#' **Special handling / caveats**
#' - Rows assigned to `"unknown"` `pid` (labels with no digits) are **skipped**.
#' - If multiple candidate project columns exist, the first match in the alias
#'   list is used.
#' - Per-project variables are created in `.GlobalEnv` as a side effect.
#'
#' @param df data.frame. Must contain a **project** column (or a recognizable
#'   alias). Optionally a `"Versuchspersonennummer."` column to remove test rows.
#' @param out_path `NULL` or string. Base path or a subfolder that ends with
#'   `"questionnaires"`/`"experiment_data"` (its parent becomes the base).
#' @param sample `NULL` or string. Sample label to embed in filenames and object
#'   names. If `NULL`, an internal heuristic tries to infer it.
#' @param export_csv logical. Also write `.csv` next to `.xlsx` (default `FALSE`).
#' @param data_type character. One of `"questionnaires"` (default) or
#'   `"experiment_data"`. Controls subfolder and filename suffixes.
#' @param metadata_info Optional. A data.frame or path to `.csv`/`.xlsx`
#'   containing columns `sample` and `ctime` (used to set the date in filenames).
#' @param dry_run logical. If `TRUE`, do not create directories or write files;
#'   still performs splitting, variable assignment, and messages.
#' @param verbose logical. Print progress messages (default `TRUE`).
#'
#' @returns
#' A **named character vector** of unique output directories that were targeted.
#' Names are `"<pid>_backbone"`, values are full paths like
#' `"<base>/<pid>_backbone/<subfolder>"`.
#'
#' @section Side effects:
#' - Creates directories on disk (unless `dry_run = TRUE`).
#' - Writes `.xlsx` (and optionally `.csv`) files.
#' - Assigns per-project data.frames into `.GlobalEnv`.
#'
#' @examples
#' \dontrun{
#' dirs <- separate_by_project(
#'   df = survey_df,
#'   out_path = "D:/exports/questionnaires",
#'   sample = "adults",
#'   export_csv = TRUE,
#'   data_type = "questionnaires",
#'   metadata_info = "metadata.csv",
#'   dry_run = FALSE,
#'   verbose = TRUE
#' )
#' }
separate_by_project <- function(
    df,
    out_path = NULL,
    sample = NULL,
    export_csv = FALSE,
    data_type = c("questionnaires", "experiment_data"),
    metadata_info = NULL, # optional
    dry_run = FALSE,
    verbose = TRUE, 
    pilot_mode = FALSE
) {
  data_type <- match.arg(data_type)
  export_csv <- isTRUE(export_csv)
  
  # ----- resolve base directory -----
  detect_script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    idx <- grep("^--file=", args)
    if (length(idx)) {
      p <- sub("^--file=", "", args[idx[length(idx)]])
      if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
    }
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
      if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
    }
    if (requireNamespace("knitr", quietly = TRUE)) {
      p <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) "")
      if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
    }
    ""
  }
  sanitize_base <- function(p) {
    if (is.null(p)) return(NULL)
    p2 <- normalizePath(p, winslash = "/", mustWork = FALSE)
    tail <- tolower(basename(p2))
    if (tail %in% c("experiment_data", "questionnaires")) dirname(p2) else p2
  }
  base_dir <- sanitize_base(out_path)
  if (is.null(base_dir) || !nzchar(base_dir)) {
    base_dir <- detect_script_dir()
    if (!nzchar(base_dir)) {
      base_dir <- getwd()
      if (verbose) warning("Could not detect script directory. Using getwd(): ", base_dir)
    }
  }
  if (!dir.exists(base_dir) && !dry_run) dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  if (verbose) message("Base dir: ", base_dir)
  
  if (!dry_run && !requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  
  # ----- locate or make project column -----
  proj_cols <- c("project", "Projekt...Project", "Projekt.", "Projekt", "projekt", "p", "proj", "Proj")
  lower_names <- tolower(names(df))
  match_idx <- which(tolower(proj_cols) %in% lower_names)[1]
  if (is.na(match_idx)) {
    warning("No project column found. Assigning all rows to default project '6'.")
    proj_col <- "Projekt."
    df[[proj_col]] <- "6"
  } else {
    proj_col <- names(df)[match(tolower(proj_cols[match_idx]), lower_names)]
  }
  if (verbose) message("Project column: '", proj_col, "'")
  
  # ----- remove test participants -----
  if ("Versuchspersonennummer." %in% names(df)) {
    n0 <- nrow(df)
    keep <- !(df$Versuchspersonennummer. == 99999 | as.character(df$Versuchspersonennummer.) == "99999")
    keep[is.na(keep)] <- TRUE
    df <- df[keep, , drop = FALSE]
    n1 <- nrow(df)
    if (n1 < n0 && verbose) message("Removed ", n0 - n1, " test rows.")
  }
  
  # ----- normalize labels -----
  df[[proj_col]] <- trimws(as.character(df[[proj_col]]))
  df <- df[!(df[[proj_col]] %in% c("", "Testing mode(No actual data collection)")), , drop = FALSE]
  
  # ----- sample detection -----
  normalize_sample <- function(x) {
    x <- tolower(x)
    if (grepl("parents_p6", x)) return("parents_p6")
    if (grepl("children_p6", x)) return("children_p6")
    if (grepl("children_parents", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(children)(?:$|[_\\.-])", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(adolescents)(?:$|[_\\.-])", x)) return("adolescents")
    if (grepl("(?:^|[_\\.-])(adults)(?:$|[_\\.-])", x)) return("adults")
    "unknown"
  }
  infer_sample_from_call <- function() {
    txt1 <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
    s1 <- normalize_sample(txt1); if (s1 != "unknown") return(s1)
    mc <- tryCatch(match.call(expand.dots = FALSE)$df, error = function(e) NULL)
    txt2 <- tryCatch(if (!is.null(mc)) paste(deparse(mc), collapse = "") else "", error = function(e) "")
    s2 <- normalize_sample(txt2); if (s2 != "unknown") return(s2)
    pf <- parent.frame()
    nms <- ls(envir = pf, all.names = TRUE)
    cand <- nms[grepl("adults|adolescents|children_parents|children|parents_p6|children_p6", tolower(nms))]
    for (nm in cand) {
      s3 <- normalize_sample(nm)
      if (s3 != "unknown") return(s3)
    }
    "unknown"
  }
  sample_name <- if (!is.null(sample)) normalize_sample(sample) else infer_sample_from_call()
  if (verbose) message("Sample: ", sample_name)
  
  # ----- metadata (optional) -----
  date_str <- "unknown_date"
  if (is.null(metadata_info)) {
    warning("No 'metadata_info' provided; using 'unknown_date' in filenames.")
  } else {
    load_metadata <- function(meta) {
      if (is.data.frame(meta)) return(meta)
      if (is.character(meta) && length(meta) == 1L && nzchar(meta)) {
        ext <- tolower(tools::file_ext(meta))
        if (ext == "csv") {
          return(utils::read.csv(meta, stringsAsFactors = FALSE))
        } else if (ext %in% c("xlsx", "xls")) {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("To read '", ext, "' metadata files, install.packages('readxl').")
          }
          return(as.data.frame(readxl::read_excel(meta)))
        } else stop("Unsupported metadata file extension: .", ext)
      }
      stop("metadata_info must be a data.frame or a path to a .csv/.xlsx file.")
    }
    ok <- TRUE
    meta_df <- tryCatch(load_metadata(metadata_info), error = function(e) { warning(conditionMessage(e)); return(NULL) })
    if (is.null(meta_df)) {
      ok <- FALSE; warning("Could not load metadata; using 'unknown_date'.")
    } else if (!all(c("sample", "ctime") %in% names(meta_df))) {
      ok <- FALSE; warning("Metadata missing 'sample'/'ctime'; using 'unknown_date'.")
    }
    if (ok) {
      ms <- tolower(as.character(meta_df$sample))
      idx <- which(ms == tolower(sample_name))
      if (length(idx) == 0) {
        warning("No metadata match for sample='", sample_name, "'; using 'unknown_date'.")
      } else {
        ctimes <- meta_df$ctime[idx]
        first_non_na <- which(!is.na(ctimes))[1]
        if (!is.na(first_non_na)) {
          parsed_date <- tryCatch(as.Date(ctimes[first_non_na]), error = function(e) NA)
          if (!is.na(parsed_date)) {
            date_str <- format(parsed_date, "%Y-%m-%d")
            if (verbose) message("Using ctime from metadata: ", date_str)
          } else {
            warning("Could not parse ctime for sample='", sample_name, "'; using 'unknown_date'.")
          }
        } else {
          warning("All ctime NA for sample='", sample_name, "'; using 'unknown_date'.")
        }
      }
    }
  }
  
  # ----- suffixes -----
  file_suffix    <- if (data_type == "experiment_data") "cogtests"        else "questionnaire"
  env_suffix     <- if (data_type == "experiment_data") "cogtest"         else "questionnaire"
  subfolder_main <- if (data_type == "experiment_data") "experiment_data" else "questionnaires"
  
  # final subpath under <pid>_backbone/...
  subpath_parts <- if (pilot_mode) c("pilot_data", subfolder_main) else c(subfolder_main)
  
  # ----- helpers -----
  parse_project <- function(lbl) {
    s <- trimws(as.character(lbl))
    digits <- gsub("\\D+", "", s)
    list(pid = ifelse(nzchar(digits), digits, "unknown"))
  }
  is_empty_df <- function(x) is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  ensure_dir <- function(path) if (!dir.exists(path) && !dry_run) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  # ===== Stage A: split, collect (no writing yet) =================================
  parts <- split(df, df[[proj_col]], drop = TRUE)
  split_list <- list()
  
  for (lbl in names(parts)) {
    d <- parts[[lbl]]
    pid_clean <- parse_project(lbl)$pid
    varname <- sprintf("data_%s_p_%s_%s", sample_name, pid_clean, env_suffix)
    assign(varname, d, envir = .GlobalEnv) # side effect per original spec
    
    # skip empty/unknown later; still store bookkeeping
    project_dir <- file.path(base_dir, sprintf("%s_backbone", pid_clean))
    pid_dir     <- do.call(file.path, as.list(c(project_dir, subpath_parts)))
    
    name_parts <- c(pid_clean, date_str, if (pilot_mode) "PILOT" else NULL, sample_name, file_suffix)
    base       <- paste(name_parts, collapse = "_")
    
    split_list[[length(split_list) + 1]] <- list(
      pid = pid_clean,
      label = lbl,
      varname = varname,
      df = d,
      project_dir = project_dir,
      pid_dir = pid_dir,
      file_base = base
    )
  }
  
  # ===== Stage B: compute rushing flag on combined data (per sample) ==============
  # Only if typical questionnaire columns exist
  can_flag <- any(vapply(split_list, function(x) all(c("vpid", "interviewtime") %in% names(x$df)), logical(1)))
  cutoff_seconds <- NA_real_
  center_seconds <- NA_real_
  log_mean <- NA_real_; log_sd <- NA_real_
  
  if (can_flag) {
    # combine only valid per-project slices (exclude unknown/empty)
    all_valid <- lapply(split_list, function(x) {
      d <- x$df
      if (x$pid %in% c("0","99","unknown") || is_empty_df(d)) return(NULL)
      # ensure numeric
      d$interviewtime <- suppressWarnings(as.numeric(d$interviewtime))
      d$.pid <- x$pid
      d
    })
    all_valid <- do.call(rbind, all_valid)
    
    if (!is.null(all_valid) && nrow(all_valid) > 0) {
      idx <- !is.na(all_valid$interviewtime) & all_valid$interviewtime > 0
      if (sum(idx) > 1L) {
        log_times <- log(all_valid$interviewtime[idx])
        log_mean  <- mean(log_times)
        log_sd    <- stats::sd(log_times)
        cutoff_log <- log_mean - 2 * log_sd
        center_seconds <- exp(log_mean)
        cutoff_seconds <- exp(cutoff_log)
      } else {
        if (verbose) message("Not enough positive interviewtime values to compute log-based cutoff for sample '", sample_name, "'.")
        can_flag <- FALSE
      }
    } else {
      can_flag <- FALSE
    }
  }
  
  if (verbose && isTRUE(can_flag)) {
    message(sprintf("Rushing rule (sample='%s'): log mean = %.4f, log SD = %.4f, cutoff(seconds) = %.2f", 
                    sample_name, log_mean, log_sd, cutoff_seconds))
  }
  
  # ===== Stage C: augment, write per-project, and collect output dirs =============
  collected_dirs <- character(0)
  
  for (i in seq_along(split_list)) {
    info <- split_list[[i]]
    d <- info$df
    
    # add rushing flag if possible
    if (isTRUE(can_flag) && all(c("vpid","interviewtime") %in% names(d))) {
      d$interviewtime <- suppressWarnings(as.numeric(d$interviewtime))
      d$rushing_2sd_log <- with(d, !is.na(interviewtime) & interviewtime > 0 & log(interviewtime) < (log_mean - 2 * log_sd))
    } else {
      if (verbose) message("Skipping rushing flag for ", info$varname, " (missing vpid/interviewtime or cutoff unavailable).")
    }
    
    # reassign augmented df into .GlobalEnv
    assign(info$varname, d, envir = .GlobalEnv)
    
    # skip writing for unknown/empty
    if (info$pid %in% c("0","99","unknown") || is_empty_df(d)) next
    
    ensure_dir(info$pid_dir)
    fp_xlsx <- file.path(info$pid_dir, paste0(info$file_base, ".xlsx"))
    if (!dry_run) {
      if (export_csv) utils::write.csv(d, file.path(info$pid_dir, paste0(info$file_base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(d, fp_xlsx)
    }
    if (verbose) message("Saved: ", fp_xlsx, " | env: ", info$varname)
    collected_dirs <- c(collected_dirs, info$pid_dir)
  }
  
  # ===== Stage D: write composite "all projects" per-sample dataset ===============
  if (length(split_list) > 0) {
    comp_list <- lapply(split_list, function(x) {
      d <- x$df
      if (!is.null(d)) {
        d$.pid <- x$pid
        d
      } else NULL
    })
    comp_df <- do.call(rbind, comp_list)
    
    all_projects_dir <- file.path(base_dir, "all_projects_backbone", subfolder_main)
    ensure_dir(all_projects_dir)
    
    comp_base <- paste(c("ALL", date_str, if (pilot_mode) "PILOT" else NULL, sample_name, file_suffix), collapse = "_")
    if (!dry_run) {
      if (export_csv) utils::write.csv(comp_df, file.path(all_projects_dir, paste0(comp_base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(comp_df, file.path(all_projects_dir, paste0(comp_base, ".xlsx")))
    }
    if (verbose) message("Saved composite sample dataset: ", file.path(all_projects_dir, paste0(comp_base, ".xlsx")))
  }
  
  # ===== Stage E: export histograms to 'out/' next to script ======================
  # One on original seconds scale (with exp(mean_log) & exp(mean_log-2*SD));
  # One on log scale (with mean_log & cutoff_log).
  out_dir <- file.path(base_dir, "out")
  ensure_dir(out_dir)
  if (isTRUE(can_flag)) {
    # gather valid seconds for plotting
    plot_vec <- all_valid$interviewtime[idx]
    
    # seconds-scale histogram
    png(file.path(out_dir, sprintf("%s_%s_hist_seconds.png", date_str, sample_name)), width = 1200, height = 800, res = 150)
    hist(plot_vec, breaks = "FD",
         main = sprintf("Interview Time (seconds) — %s", sample_name),
         xlab = "Interview time (seconds)")
    abline(v = exp(log_mean), lwd = 2)
    abline(v = cutoff_seconds, lwd = 2, lty = 2)
    legend("topright",
           legend = c(sprintf("exp(mean_log) = %.2f s", exp(log_mean)),
                      sprintf("Cutoff = %.2f s", cutoff_seconds)),
           lwd = c(2,2), lty = c(1,2), bty = "n")
    dev.off()
    
    # log-scale histogram
    png(file.path(out_dir, sprintf("%s_%s_hist_log.png", date_str, sample_name)), width = 1200, height = 800, res = 150)
    hist(log(plot_vec), breaks = "FD",
         main = sprintf("log(Interview Time) — %s", sample_name),
         xlab = "log(interview time)")
    abline(v = log_mean, lwd = 2)
    abline(v = (log_mean - 2 * log_sd), lwd = 2, lty = 2)
    legend("topright",
           legend = c(sprintf("Mean(log) = %.3f", log_mean),
                      sprintf("Cutoff(log) = %.3f", log_mean - 2*log_sd)),
           lwd = c(2,2), lty = c(1,2), bty = "n")
    dev.off()
    
    if (verbose) message("Saved histograms to: ", out_dir)
  } else if (verbose) {
    message("Histograms not created (cutoff unavailable).")
  }
  
  # ===== finalize return value ====================================================
  collected_dirs <- unique(collected_dirs)
  if (pilot_mode) {
    names(collected_dirs) <- basename(dirname(dirname(collected_dirs)))
  } else {
    names(collected_dirs) <- basename(dirname(collected_dirs))
  }
  return(collected_dirs)
}
