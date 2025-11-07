analyze_rushing <- function(prep) {
  stopifnot(inherits(prep, "project_prep"))
  verbose <- isTRUE(prep$verbose)
  
  # collect valid rows (exclude pid 0/99/unknown; need interviewtime > 0)
  valid_rows <- lapply(prep$split_list, function(x){
    if (x$pid %in% c("0","99","unknown") || .is_empty_df(x$df)) return(NULL)
    d <- x$df
    if (!"interviewtime" %in% names(d)) return(NULL)
    d$interviewtime <- suppressWarnings(as.numeric(d$interviewtime))
    d <- d[!is.na(d$interviewtime) & d$interviewtime > 0, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    d$.pid <- x$pid
    d
  })
  all_valid <- do.call(rbind, valid_rows)
  if (is.null(all_valid) || nrow(all_valid) < 2L) {
    if (verbose) message("Not enough positive 'interviewtime' rows; skipping rushing analysis and plots.")
    prep$stats <- NULL
    return(prep)
  }
  
  sec <- all_valid$interviewtime
  logx <- log(sec)
  n <- length(logx)
  
  # ----- method choice on LOG scale -----
  method <- if (n >= 100) "log_2sd" else "log_1.5iqr"
  
  if (method == "log_2sd") {
    mean_log <- mean(logx)
    sd_log   <- stats::sd(logx)
    center_log <- mean_log
    cutoff_log <- mean_log - 2 * sd_log
  } else { # log_1.5iqr
    med_log <- stats::median(logx)
    iqr_log <- stats::IQR(logx)
    center_log <- med_log
    cutoff_log <- med_log - 1.5 * iqr_log
  }
  
  # back-transform for seconds plot/labels
  center_sec <- exp(center_log)
  cutoff_sec <- exp(cutoff_log)  # always > 0
  
  stats <- list(
    method = method, n = n,
    center_log = center_log, cutoff_log = cutoff_log,
    center_sec = center_sec, cutoff_sec = cutoff_sec
  )
  if (method == "log_2sd") {
    stats$mean_log <- mean_log; stats$sd_log <- sd_log
  } else {
    stats$median_log <- med_log; stats$iqr_log <- iqr_log
  }
  
  if (verbose) {
    if (method == "log_2sd") {
      message(sprintf("Rushing (%s): n=%d, mean_log=%.3f, sd_log=%.3f, cutoff_log=%.3f, cutoff_sec=%.2f",
                      method, n, stats$mean_log, stats$sd_log, cutoff_log, cutoff_sec))
    } else {
      message(sprintf("Rushing (%s): n=%d, median_log=%.3f, iqr_log=%.3f, cutoff_log=%.3f, cutoff_sec=%.2f",
                      method, n, stats$median_log, stats$iqr_log, cutoff_log, cutoff_sec))
    }
  }
  
  # ----- add flags (compare on LOG scale) -----
  flag_row <- function(d) {
    d$interviewtime <- suppressWarnings(as.numeric(d$interviewtime))
    ok <- !is.na(d$interviewtime) & d$interviewtime > 0
    d$rushing_method <- method
    d$rushing_flag <- FALSE
    d$rushing_flag[ok] <- log(d$interviewtime[ok]) < cutoff_log
    d
  }
  
  for (i in seq_along(prep$split_list)) {
    info <- prep$split_list[[i]]
    d <- info$df
    if ("interviewtime" %in% names(d)) {
      d <- flag_row(d)
      assign(info$varname, d, envir = .GlobalEnv)
      prep$split_list[[i]]$df <- d
    } else if (verbose) {
      message("Skipping rushing flag for ", info$varname, " (no 'interviewtime').")
    }
  }
  if (!is.null(prep$composite_var) && exists(prep$composite_var, envir = .GlobalEnv)) {
    comp <- get(prep$composite_var, envir = .GlobalEnv)
    if (!is.null(comp) && nrow(comp) > 0 && "interviewtime" %in% names(comp)) {
      comp <- flag_row(comp)
      assign(prep$composite_var, comp, envir = .GlobalEnv)
    }
  }
  
  # ----- plotting: seconds + log (always) -----
  out_dir <- file.path(dirname(prep$base_dir), "out", "survey_duration_distribution")
  .ensure_dir(out_dir, dry_run = FALSE)
  
  compute_breaks <- function(x) {
    h_tmp <- hist(x, breaks = "FD", plot = FALSE)
    if ((length(h_tmp$breaks) - 1) < 10) {
      seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 11)
    } else h_tmp$breaks
  }
  
  prefix <- sprintf("%s_%s_%s", prep$date_str, prep$sample_name, prep$file_suffix)
  
  ## Seconds-scale
  br_sec <- compute_breaks(sec)
  png(file.path(out_dir, sprintf("%s_hist.png", prefix)), width = 1200, height = 800, res = 150)
  hist(sec, breaks = br_sec,
       main = sprintf("Survey duration distribution — %s (%s)", prep$sample_name, method),
       xlab = "Interview time (seconds)")
  abline(v = center_sec, lwd = 2)
  abline(v = cutoff_sec, lwd = 2, lty = 2)
  legend_text_sec <- if (method == "log_2sd") {
    c(sprintf("center = exp(mean_log) = %.2f s", center_sec),
      sprintf("cutoff = %.2f s", cutoff_sec))
  } else {
    c(sprintf("center = exp(median_log) = %.2f s", center_sec),
      sprintf("cutoff = exp(median_log - 1.5×IQR_log) = %.2f s", cutoff_sec))
  }
  lg <- legend("topright", legend = legend_text_sec, lwd = c(2,2), lty = c(1,2), bty = "n", plot = FALSE)
  legend(lg$rect$left, lg$rect$top, legend = legend_text_sec,
         lwd = c(2,2), lty = c(1,2), bty = "n", xjust = 0, yjust = 1)
  text(x = lg$rect$left,
       y = lg$rect$top - lg$rect$h - strheight("n", cex = par("cex")) * 0.7,
       labels = sprintf("n = %d", n), adj = c(0, 1))
  dev.off()
  
  ## Log-scale
  br_log <- compute_breaks(logx)
  png(file.path(out_dir, sprintf("%s_hist_log.png", prefix)), width = 1200, height = 800, res = 150)
  hist(logx, breaks = br_log,
       main = sprintf("log(Interview time) — %s (%s)", prep$sample_name, method),
       xlab = "log(interview time)")
  abline(v = center_log, lwd = 2)
  abline(v = cutoff_log, lwd = 2, lty = 2)
  legend_text_log <- if (method == "log_2sd") {
    c(sprintf("mean(log) = %.3f", center_log),
      sprintf("cutoff(log) = mean - 2×SD = %.3f", cutoff_log))
  } else {
    c(sprintf("median(log) = %.3f", center_log),
      sprintf("cutoff(log) = median - 1.5×IQR = %.3f", cutoff_log))
  }
  lg2 <- legend("topright", legend = legend_text_log, lwd = c(2,2), lty = c(1,2), bty = "n", plot = FALSE)
  legend(lg2$rect$left, lg2$rect$top, legend = legend_text_log,
         lwd = c(2,2), lty = c(1,2), bty = "n", xjust = 0, yjust = 1)
  text(x = lg2$rect$left,
       y = lg2$rect$top - lg2$rect$h - strheight("n", cex = par("cex")) * 0.7,
       labels = sprintf("n = %d", n), adj = c(0, 1))
  dev.off()
  
  if (verbose) message("Histogram(s) saved to: ", out_dir)
  
  prep$stats <- stats
  prep
}
