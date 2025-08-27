resolve_duplicates <- function(df, vp_col, dataset_name = "dataset", lastpage_threshold = 17) {
  if (!all(c(vp_col, "submitdate", "lastpage") %in% names(df))) {
    stop(sprintf("[%s] Missing required columns. Need: %s, submitdate, lastpage",
                 dataset_name, vp_col))
  }
  if (dataset_name == "children_parents" && !"project" %in% names(df)) {
    stop("[children_parents] Missing required column: project")
  }
  if (dataset_name == "children_parents" && !"form" %in% names(df)) {
    stop("[children_parents] Missing required column: form")
  }
  
  # helper flags
  has_submit <- function(x) !is.na(x) & trimws(as.character(x)) != ""
  df$.__has_submit__. <- has_submit(df$submitdate)
  df$.__lp_num__.     <- suppressWarnings(as.numeric(df$lastpage))
  vp_chr <- trimws(as.character(df[[vp_col]]))
  
  # ignore rows with missing/empty vp_id AND incomplete
  ignore_idx <- which(
    (is.na(vp_chr) | vp_chr == "") &
      !df$.__has_submit__. &
      (is.na(df$.__lp_num__.) | df$.__lp_num__. <= lastpage_threshold)
  )
  keep_flag <- rep(TRUE, nrow(df))
  keep_flag[ignore_idx] <- FALSE
  
  # grouping key
  key <- vp_chr
  need_form_disambig <- (dataset_name == "children_parents") & !is.na(df$project) & df$project == 8
  if (any(need_form_disambig)) {
    form_chr <- if ("form" %in% names(df)) trimws(as.character(df$form)) else NA_character_
    key_cp8 <- paste(vp_chr, form_chr, sep = "||")
    key[need_form_disambig] <- key_cp8[need_form_disambig]
  }
  
  nonempty_key <- !(is.na(vp_chr) | vp_chr == "")
  dup_keys <- unique(key[duplicated(key) & nonempty_key])
  
  trash_bin <- df[0, , drop = FALSE]
  trash_bin$.__reason__. <- character(0)
  
  for (k in dup_keys) {
    idx <- which(key == k)
    
    vpid_vals <- unique(vp_chr[idx])
    vpid_lab <- if (length(vpid_vals) == 1) vpid_vals else paste(vpid_vals, collapse = ",")
    form_vals <- if ("form" %in% names(df)) unique(trimws(as.character(df$form[idx]))) else NA_character_
    form_lab <- if (!all(is.na(form_vals)) && length(form_vals) == 1) form_vals else NA
    
    n_submit <- sum(df$.__has_submit__.[idx], na.rm = TRUE)
    
    if (n_submit == 1) {
      # keep the one complete, remove the rest
      keeper <- idx[df$.__has_submit__.[idx]]
      losers <- setdiff(idx, keeper)
      if (length(losers) > 0) {
        keep_flag[losers] <- FALSE
        tb <- df[losers, , drop = FALSE]
        msg <- if (!is.na(form_lab))
          sprintf("Removed duplicate %s=%s & form=%s; another row has submitdate.", vp_col, vpid_lab, form_lab) else
            sprintf("Removed duplicate %s=%s; another row has submitdate.", vp_col, vpid_lab)
        tb$.__reason__. <- msg
        trash_bin <- rbind(trash_bin, tb)
      }
      
    } else if (n_submit > 1) {
      # NEW: remove all incomplete rows; keep all complete rows; warn for manual resolution
      incompletes <- idx[!df$.__has_submit__.[idx]]
      if (length(incompletes) > 0) {
        keep_flag[incompletes] <- FALSE
        tb <- df[incompletes, , drop = FALSE]
        msg <- if (!is.na(form_lab))
          sprintf("Removed incomplete duplicate %s=%s & form=%s; other rows have submitdate.",
                  vp_col, vpid_lab, form_lab) else
                    sprintf("Removed incomplete duplicate %s=%s; other rows have submitdate.",
                            vp_col, vpid_lab)
        tb$.__reason__. <- msg
        trash_bin <- rbind(trash_bin, tb)
      }
      # message about multiple completes remaining
      if (!is.na(form_lab)) {
        message(sprintf("[%s] Multiple complete datasets for %s=%s, form=%s — please resolve manually.",
                        dataset_name, vp_col, vpid_lab, form_lab))
      } else {
        message(sprintf("[%s] Multiple complete datasets for %s=%s — please resolve manually.",
                        dataset_name, vp_col, vpid_lab))
      }
      
    } else {
      # no submitdate — use lastpage > threshold as tie-break
      high_lp <- idx[which(df$.__lp_num__.[idx] > lastpage_threshold)]
      if (length(high_lp) == 1) {
        keeper <- high_lp
        losers <- setdiff(idx, keeper)
        if (length(losers) > 0) {
          keep_flag[losers] <- FALSE
          tb <- df[losers, , drop = FALSE]
          msg <- if (!is.na(form_lab))
            sprintf("Removed duplicate %s=%s & form=%s; another row has lastpage > %s.",
                    vp_col, vpid_lab, form_lab, lastpage_threshold) else
                      sprintf("Removed duplicate %s=%s; another row has lastpage > %s.",
                              vp_col, vpid_lab, lastpage_threshold)
          tb$.__reason__. <- msg
          trash_bin <- rbind(trash_bin, tb)
        }
      } else if (length(high_lp) > 1) {
        # still ambiguous; keep all, warn
        if (!is.na(form_lab)) {
          message(sprintf("[%s] Multiple incomplete datasets with lastpage > %s for %s=%s, form=%s — please resolve manually.",
                          dataset_name, lastpage_threshold, vp_col, vpid_lab, form_lab))
        } else {
          message(sprintf("[%s] Multiple incomplete datasets with lastpage > %s for %s=%s — please resolve manually.",
                          dataset_name, lastpage_threshold, vp_col, vpid_lab))
        }
      } else {
        # no clear winner; keep all, warn
        if (!is.na(form_lab)) {
          message(sprintf("[%s] Multiple incomplete datasets for %s=%s, form=%s — please resolve manually.",
                          dataset_name, vp_col, vpid_lab, form_lab))
        } else {
          message(sprintf("[%s] Multiple incomplete datasets for %s=%s — please resolve manually.",
                          dataset_name, vp_col, vpid_lab))
        }
      }
    }
  }
  
  # finalize: drop helpers
  cleaned <- df[keep_flag, , drop = FALSE]
  cleaned$.__has_submit__. <- NULL
  cleaned$.__lp_num__. <- NULL
  trash_bin$.__has_submit__. <- NULL
  trash_bin$.__lp_num__. <- NULL
  
  list(cleaned = cleaned, trash_bin = trash_bin)
}
