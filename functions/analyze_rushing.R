analyze_rushing <- function(prep, combine_samples = c("adults", "adolescents")) {
  
  # -------- helpers --------
  .collect_valid <- function(one_prep) {
    stopifnot(inherits(one_prep, "project_prep"))
    verbose <- isTRUE(one_prep$verbose)
    
    valid_rows <- lapply(one_prep$split_list, function(x) {
      if (x$pid %in% c("0","99","unknown") || .is_empty_df(x$df)) return(NULL)
      d <- x$df
      if (!"interviewtime" %in% names(d)) return(NULL)
      
      d$interviewtime <- suppressWarnings(as.numeric(d$interviewtime))
      d <- d[!is.na(d$interviewtime) & d$interviewtime > 0, , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      
      d$.pid    <- x$pid
      d$.sample <- one_prep$sample_name
      d
    })
    
    out <- dplyr::bind_rows(valid_rows)
    if (!NROW(out) && verbose) {
      message("No positive 'interviewtime' rows for sample: ", one_prep$sample_name)
    }
    out
  }
  
  .apply_flag <- function(one_prep, method, cutoff_log, center_log) {
    stopifnot(inherits(one_prep, "project_prep"))
    
    flag_row <- function(d) {
      d$interviewtime <- suppressWarnings(as.numeric(d$interviewtime))
      ok <- !is.na(d$interviewtime) & d$interviewtime > 0
      
      # minutes (requested)
      mins <- d$interviewtime / 60
      
      d$rushing_method     <- method
      d$rushing_flag       <- FALSE
      d$rushing_flag[ok]   <- log(mins[ok]) < cutoff_log
      
      # helpful extras in minutes
      d$rushing_center_min <- exp(center_log)
      d$rushing_cutoff_min <- exp(cutoff_log)
      d
    }
    
    for (i in seq_along(one_prep$split_list)) {
      info <- one_prep$split_list[[i]]
      d <- info$df
      if ("interviewtime" %in% names(d)) {
        d <- flag_row(d)
        assign(info$varname, d, envir = .GlobalEnv)
        one_prep$split_list[[i]]$df <- d
      } else if (isTRUE(one_prep$verbose)) {
        message("Skipping rushing flag for ", info$varname, " (no 'interviewtime').")
      }
    }
    
    if (!is.null(one_prep$composite_var) && exists(one_prep$composite_var, envir = .GlobalEnv)) {
      comp <- get(one_prep$composite_var, envir = .GlobalEnv)
      if (!is.null(comp) && nrow(comp) > 0 && "interviewtime" %in% names(comp)) {
        comp <- flag_row(comp)
        assign(one_prep$composite_var, comp, envir = .GlobalEnv)
      }
    }
    
    one_prep
  }
  
  .compute_breaks <- function(x) {
    h_tmp <- hist(x, breaks = "FD", plot = FALSE)
    if ((length(h_tmp$breaks) - 1) < 10) {
      seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 11)
    } else h_tmp$breaks
  }
  
  # ===== CASE A: prep is a LIST of project_prep (combined adults+adolescents) =====
  if (is.list(prep) && !inherits(prep, "project_prep") &&
      all(vapply(prep, inherits, logical(1), "project_prep"))) {
    
    # keep only the intended samples (typically adults + adolescents)
    preps <- prep
    keep <- vapply(preps, function(p) p$sample_name %in% combine_samples, logical(1))
    preps_keep <- preps[keep]
    
    if (length(preps_keep) < 2L) {
      # not enough to combine; just analyze each separately (minutes)
      return(lapply(preps, analyze_rushing, combine_samples = combine_samples))
    }
    
    verbose_any <- any(vapply(preps_keep, function(p) isTRUE(p$verbose), logical(1)))
    
    all_valid <- dplyr::bind_rows(lapply(preps_keep, .collect_valid))
    if (is.null(all_valid) || nrow(all_valid) < 2L) {
      if (verbose_any) message("Not enough pooled positive 'interviewtime' rows; skipping combined rushing analysis.")
      preps_keep <- lapply(preps_keep, function(p) { p$stats <- NULL; p })
      preps[keep] <- preps_keep
      return(preps)
    }
    
    # minutes + log transform
    mins <- all_valid$interviewtime / 60
    logx <- log(mins)
    n <- length(logx)
    
    # requested: assume normality on log scale, use mean - 2SD always
    method <- "log_2sd_combined_adults_adolescents"
    center_log <- mean(logx)
    sd_log     <- stats::sd(logx)
    cutoff_log <- center_log - 2 * sd_log
    
    center_min <- exp(center_log)
    cutoff_min <- exp(cutoff_log)
    
    stats <- list(
      method = method, n = n,
      center_log = center_log, sd_log = sd_log, cutoff_log = cutoff_log,
      center_min = center_min, cutoff_min = cutoff_min,
      unit = "minutes",
      pooled_samples = combine_samples
    )
    
    if (verbose_any) {
      message(sprintf(
        "Rushing (%s): pooled n=%d, mean_log=%.3f, sd_log=%.3f, cutoff_log=%.3f, cutoff_min=%.2f",
        method, n, center_log, sd_log, cutoff_log, cutoff_min
      ))
    }
    
    # apply flags to BOTH samples using shared cutoff
    preps_keep <- lapply(preps_keep, function(p) {
      p <- .apply_flag(p, method = method, cutoff_log = cutoff_log, center_log = center_log)
      p$stats <- stats
      p
    })
    preps[keep] <- preps_keep
    
    # one output image only (pooled)
    # choose a stable base_dir (first kept prep)
    base_dir <- preps_keep[[1]]$base_dir
    out_dir <- file.path(dirname(base_dir), "out", "survey_duration_distribution")
    .ensure_dir(out_dir, dry_run = FALSE)
    
    date_str <- preps_keep[[1]]$date_str
    file_suffixes <- unique(vapply(preps_keep, function(p) p$file_suffix, character(1)))
    file_suffix <- if (length(file_suffixes) == 1) file_suffixes else "mixed"
    
    prefix <- sprintf("%s_%s_%s", date_str, "adults_adolescents", file_suffix)
    out_file <- file.path(out_dir, sprintf("%s_duration_combined.png", prefix))
    
    png(out_file, width = 1600, height = 800, res = 150)
    op <- par(mfrow = c(1, 2))
    
    # Panel 1: minutes
    br_min <- .compute_breaks(mins)
    hist(mins, breaks = br_min,
         main = "Survey duration (pooled adults + adolescents)",
         xlab = "Interview time (minutes)")
    abline(v = center_min, lwd = 2)
    abline(v = cutoff_min, lwd = 2, lty = 2)
    legend("topright",
           legend = c(sprintf("center = exp(mean(log(min))) = %.2f min", center_min),
                      sprintf("cutoff = exp(mean(log(min)) - 2×SD) = %.2f min", cutoff_min),
                      sprintf("n = %d", n)),
           lwd = c(2,2,NA), lty = c(1,2,NA), bty = "n")
    
    # Panel 2: log(minutes)
    br_log <- .compute_breaks(logx)
    hist(logx, breaks = br_log,
         main = "log(Interview time in minutes) (pooled)",
         xlab = "log(minutes)")
    abline(v = center_log, lwd = 2)
    abline(v = cutoff_log, lwd = 2, lty = 2)
    legend("topright",
           legend = c(sprintf("mean(log) = %.3f", center_log),
                      sprintf("cutoff(log) = mean - 2×SD = %.3f", cutoff_log)),
           lwd = c(2,2), lty = c(1,2), bty = "n")
    
    par(op)
    dev.off()
    
    if (verbose_any) message("Combined histogram saved to: ", out_file)
    
    return(preps)
  }
  
  # ===== CASE B: single prep (kept as fallback; now minutes-based) =====
  stopifnot(inherits(prep, "project_prep"))
  verbose <- isTRUE(prep$verbose)
  
  all_valid <- .collect_valid(prep)
  if (is.null(all_valid) || nrow(all_valid) < 2L) {
    if (verbose) message("Not enough positive 'interviewtime' rows; skipping rushing analysis and plots.")
    prep$stats <- NULL
    return(prep)
  }
  
  mins <- all_valid$interviewtime / 60
  logx <- log(mins)
  n <- length(logx)
  
  # keep old adaptive method for non-combined samples (but minutes-based)
  method <- if (n >= 100) "log_2sd" else "log_1.5iqr"
  
  if (method == "log_2sd") {
    center_log <- mean(logx)
    sd_log     <- stats::sd(logx)
    cutoff_log <- center_log - 2 * sd_log
  } else {
    center_log <- stats::median(logx)
    iqr_log    <- stats::IQR(logx)
    cutoff_log <- center_log - 1.5 * iqr_log
  }
  
  stats <- list(
    method = method, n = n,
    center_log = center_log, cutoff_log = cutoff_log,
    center_min = exp(center_log), cutoff_min = exp(cutoff_log),
    unit = "minutes"
  )
  if (method == "log_2sd") stats$sd_log <- sd_log
  if (method == "log_1.5iqr") stats$iqr_log <- iqr_log
  
  prep <- .apply_flag(prep, method = method, cutoff_log = cutoff_log, center_log = center_log)
  
  # keep existing output behavior (two images) for single samples, but in minutes
  out_dir <- file.path(dirname(prep$base_dir), "out", "survey_duration_distribution")
  .ensure_dir(out_dir, dry_run = FALSE)
  prefix <- sprintf("%s_%s_%s", prep$date_str, prep$sample_name, prep$file_suffix)
  
  # minutes
  png(file.path(out_dir, sprintf("%s_hist_minutes.png", prefix)), width = 1200, height = 800, res = 150)
  hist(mins, breaks = .compute_breaks(mins),
       main = sprintf("Survey duration — %s (%s)", prep$sample_name, method),
       xlab = "Interview time (minutes)")
  abline(v = exp(center_log), lwd = 2)
  abline(v = exp(cutoff_log), lwd = 2, lty = 2)
  dev.off()
  
  # log(minutes)
  png(file.path(out_dir, sprintf("%s_hist_log_minutes.png", prefix)), width = 1200, height = 800, res = 150)
  hist(logx, breaks = .compute_breaks(logx),
       main = sprintf("log(Interview time in minutes) — %s (%s)", prep$sample_name, method),
       xlab = "log(minutes)")
  abline(v = center_log, lwd = 2)
  abline(v = cutoff_log, lwd = 2, lty = 2)
  dev.off()
  
  if (verbose) message("Histogram(s) saved to: ", out_dir)
  
  prep$stats <- stats
  prep
}
