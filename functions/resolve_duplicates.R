# -------------------------------------------------------------------------
# resolve_duplicates()
#
# Purpose:
#   Detects and resolves duplicate participant datasets based on a chosen
#   VP-ID column (and, for children/parents project 8, VP-ID + form).
#
# Behavior:
#   - Identifies duplicates in the given dataset according to these rules:
#       • General case: duplicates defined by the column name passed in `vp_col`.
#       • Special case: for `children_parents` where project == 8,
#         duplicates are defined by `vp_col + form`. This allows each ID
#         to have one entry for each form (A, C, P) without being flagged.
#   - Resolution rules:
#       1. If exactly one row has a non-empty `submit_col`, keep it and remove
#          all others for that ID (or ID + form).
#       2. If multiple rows have `submit_col`:
#            - Remove all incomplete rows (no `submit_col`).
#            - Keep all complete rows but print a warning (⚠️) to resolve manually.
#       3. If no row has `submit_col` and a `lastpage` column exists:
#            - If exactly one row has `lastpage` > threshold (default 17),
#              keep it and remove the others.
#            - If multiple rows have `lastpage` > threshold, keep all and
#              print a warning (⚠️).
#            - If none have `lastpage` > threshold, keep all and print a warning (⚠️).
#       4. If no row has `submit_col` and no `lastpage` column exists:
#            - Keep all and print a warning (⚠️).
#   - Special handling:
#       • Rows with missing/empty IDs and also incomplete
#         (no `submit_col` and, if present, lastpage ≤ threshold) are ignored entirely
#         (not kept, not moved to trash_bin).
#   - All removed rows are collected in a `trash_bin` data frame with a reason
#     column for easy cross-checking.
#
# Input:
#   df                 : data frame with at least these columns:
#                          - vp_col (string you supply) : participant ID
#                          - submit_col (string you supply) : completion flag
#                        Optionally:
#                          - lastpage : numeric, last page reached
#                        For dataset_name == "children_parents":
#                          - project : must exist
#                          - form    : must exist if project == 8
#   vp_col             : string, the column name in `df` containing the ID.
#   submit_col         : string, the column name in `df` indicating submitdate.
#   dataset_name       : string, used in messages (e.g. "adults",
#                        "adolescents", "children_parents").
#   lastpage_threshold : numeric, cutoff for considering a dataset as
#                        "almost complete" (default: 17). Ignored if no lastpage column.
#
# Output:
#   - Returns a list with two elements:
#       $cleaned   : data frame with duplicates resolved/removed.
#       $trash_bin : data frame of removed rows, with an added column
#                    `.__reason__` describing why they were removed.
#   - Prints messages (⚠️) to the console when manual resolution is required.
#
# Example:
#   res <- resolve_duplicates(dat_adults,
#                             vp_col = "vp_id",
#                             submit_col = "submitdate",
#                             dataset_name = "adults")
#   dat_adults <- res$cleaned
#   trash_adults <- res$trash_bin
#
# -------------------------------------------------------------------------
resolve_duplicates <- function(df,
                               vp_col,
                               submit_col = "submitdate",
                               dataset_name = "dataset",
                               lastpage_threshold = 17,
                               project_col = "project") {
  # --- Required columns check ---
  if (!all(c(vp_col, submit_col) %in% names(df))) {
    stop(sprintf("[%s] Missing required columns. Need at least: %s, %s",
                 dataset_name, vp_col, submit_col))
  }
  
  has_lastpage <- "lastpage" %in% names(df)
  has_project <- !is.null(project_col) && (project_col %in% names(df))
  has_form <- "form" %in% names(df)
  
  # --- children_parents special rules ---
  if (dataset_name == "children_parents") {
    if (!has_form) {
      stop("[children_parents] Missing required column: form")
    }
    # ✅ no longer require project column — just warn if missing
    if (!has_project) {
      warning("[children_parents] No project column found — skipping project-based disambiguation.")
    }
  }
  
  # --- Helper flags ---
  has_submit <- function(x) !is.na(x) & trimws(as.character(x)) != ""
  df$.__has_submit__. <- has_submit(df[[submit_col]])
  df$.__lp_num__. <- if (has_lastpage) suppressWarnings(as.numeric(df$lastpage)) else NA_real_
  
  id_chr <- trimws(as.character(df[[vp_col]]))
  
  # --- Ignore empty IDs with incomplete data ---
  ignore_idx <- which(
    (is.na(id_chr) | id_chr == "") &
      !df$.__has_submit__. &
      (!has_lastpage | is.na(df$.__lp_num__.) | df$.__lp_num__. <= lastpage_threshold)
  )
  keep_flag <- rep(TRUE, nrow(df))
  keep_flag[ignore_idx] <- FALSE
  
  # --- Build grouping key ---
  key <- id_chr
  
  # children_parents: disambiguate by form if project == 8
  if (dataset_name == "children_parents" && has_project) {
    proj_num <- suppressWarnings(as.numeric(df[[project_col]]))
    need_form_disambig <- !is.na(proj_num) & proj_num == 8
    if (any(need_form_disambig)) {
      form_chr <- trimws(as.character(df$form))
      key_cp8 <- paste(id_chr, form_chr, sep = "||")
      key[need_form_disambig] <- key_cp8[need_form_disambig]
    }
  }
  
  nonempty_key <- !(is.na(id_chr) | id_chr == "")
  dup_keys <- unique(key[duplicated(key) & nonempty_key])
  
  trash_bin <- df[0, , drop = FALSE]
  trash_bin$.__reason__. <- character(0)
  
  # --- Main duplicate resolution loop ---
  for (k in dup_keys) {
    idx <- which(key == k)
    id_vals <- unique(id_chr[idx])
    id_lab <- if (length(id_vals) == 1) id_vals else paste(id_vals, collapse = ",")
    form_vals <- if (has_form) unique(trimws(as.character(df$form[idx]))) else NA_character_
    form_lab <- if (!all(is.na(form_vals)) && length(form_vals) == 1) form_vals else NA
    n_submit <- sum(df$.__has_submit__.[idx], na.rm = TRUE)
    
    if (n_submit == 1) {
      # Keep the complete row, drop others
      keeper <- idx[df$.__has_submit__.[idx]]
      losers <- setdiff(idx, keeper)
      if (length(losers) > 0) {
        keep_flag[losers] <- FALSE
        tb <- df[losers, , drop = FALSE]
        msg <- if (!is.na(form_lab))
          sprintf("Removed duplicate %s=%s & form=%s; another row has %s.",
                  vp_col, id_lab, form_lab, submit_col) else
                    sprintf("Removed duplicate %s=%s; another row has %s.",
                            vp_col, id_lab, submit_col)
        tb$.__reason__. <- msg
        trash_bin <- rbind(trash_bin, tb)
      }
      
    } else if (n_submit > 1) {
      # Keep all complete, drop all incomplete
      incompletes <- idx[!df$.__has_submit__.[idx]]
      if (length(incompletes) > 0) {
        keep_flag[incompletes] <- FALSE
        tb <- df[incompletes, , drop = FALSE]
        msg <- if (!is.na(form_lab))
          sprintf("Removed incomplete duplicate %s=%s & form=%s; other rows have %s.",
                  vp_col, id_lab, form_lab, submit_col) else
                    sprintf("Removed incomplete duplicate %s=%s; other rows have %s.",
                            vp_col, id_lab, submit_col)
        tb$.__reason__. <- msg
        trash_bin <- rbind(trash_bin, tb)
      }
      if (!is.na(form_lab)) {
        message(sprintf("⚠️ [%s] Multiple complete datasets for %s=%s, form=%s — please resolve manually.",
                        dataset_name, vp_col, id_lab, form_lab))
      } else {
        message(sprintf("⚠️ [%s] Multiple complete datasets for %s=%s — please resolve manually.",
                        dataset_name, vp_col, id_lab))
      }
      
    } else {
      # No submits: use lastpage if available
      if (has_lastpage) {
        high_lp <- idx[which(df$.__lp_num__.[idx] > lastpage_threshold)]
        if (length(high_lp) == 1) {
          keeper <- high_lp
          losers <- setdiff(idx, keeper)
          if (length(losers) > 0) {
            keep_flag[losers] <- FALSE
            tb <- df[losers, , drop = FALSE]
            msg <- if (!is.na(form_lab))
              sprintf("Removed duplicate %s=%s & form=%s; another row has lastpage > %s.",
                      vp_col, id_lab, form_lab, lastpage_threshold) else
                        sprintf("Removed duplicate %s=%s; another row has lastpage > %s.",
                                vp_col, id_lab, lastpage_threshold)
            tb$.__reason__. <- msg
            trash_bin <- rbind(trash_bin, tb)
          }
        } else {
          # Multiple or none: warn
          msg_base <- sprintf("⚠️ [%s] Multiple incomplete datasets for %s=%s",
                              dataset_name, vp_col, id_lab)
          msg_form <- if (!is.na(form_lab)) paste0(msg_base, ", form=", form_lab) else msg_base
          msg_lp <- if (length(high_lp) > 1) paste0(msg_form, " with lastpage > threshold") else msg_form
          message(msg_lp, " — please resolve manually.")
        }
      } else {
        # No lastpage column
        msg_base <- sprintf("⚠️ [%s] Multiple incomplete datasets for %s=%s (no lastpage column)",
                            dataset_name, vp_col, id_lab)
        msg_form <- if (!is.na(form_lab)) paste0(msg_base, ", form=", form_lab) else msg_base
        message(msg_form, " — please resolve manually.")
      }
    }
  }
  
  # --- Cleanup ---
  cleaned <- df[keep_flag, , drop = FALSE]
  cleaned$.__has_submit__. <- NULL
  cleaned$.__lp_num__. <- NULL
  trash_bin$.__has_submit__. <- NULL
  trash_bin$.__lp_num__. <- NULL
  
  list(cleaned = cleaned, trash_bin = trash_bin)
}
