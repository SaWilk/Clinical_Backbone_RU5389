# -------------------------------------------------------------------------
# resolve_duplicates()
#
# Purpose:
#   Detects and resolves duplicate participant datasets based on a chosen
#   VP-ID column (and, for children/parents project 8, VP-ID + form).
#
# Behavior:
#   - Identifies duplicates according to:
#       • General: duplicates by the column passed in `vp_col`.
#       • Special: for `children_parents` where project == 8, duplicates are
#         by `vp_col + form` (one of each A, C, P is allowed).
#   - Resolution rules:
#       1) Exactly one row with non-empty `submit_col` → keep it, remove others.
#       2) >1 rows with `submit_col` → remove all incomplete rows; warn (⚠️).
#       3) No `submit_col`:
#            • If `lastpage` exists: pick unique max (> threshold) if exactly one;
#              else warn (⚠️).
#            • If no `lastpage`: warn (⚠️).
#   - Special handling:
#       • Rows with empty/missing ID and also incomplete (no submit and, if
#         present, lastpage ≤ threshold) are ignored (not kept, not trashed).
#   - All removed rows go to `trash_bin` with a `.__reason__`.
#   - ⚠️ warnings are printed AND (optionally) written to `logger$write(...)`.
#
# Input:
#   df                 : data frame; must contain:
#                          - vp_col (you supply): participant ID
#                          - submit_col (you supply): completion flag
#                        Optionally:
#                          - lastpage: numeric progress
#                        For dataset_name == "children_parents":
#                          - form (required)
#                          - project_col (optional; if present used to detect 8)
#   vp_col             : string, the ID column name in `df`.
#   submit_col         : string, the submit/completion column name in `df`.
#   dataset_name       : string, used in messages (e.g. "adults", "children_parents").
#   lastpage_threshold : numeric, cutoff for "almost complete" (default: 17).
#   project_col        : string, name of project column (default "project"); if
#                        missing, project-based behavior is skipped with a warning.
#   logger             : optional logger object with method `$write(text)`.
#
# Output:
#   Returns a list with:
#     $cleaned    : data frame after resolution/removal.
#     $trash_bin  : removed rows with `.__reason__`.
#     $warnings   : character vector of all ⚠️ warning messages emitted.
#
# Example:
#   res <- resolve_duplicates(dat_adults,
#                             vp_col = "vp_id",
#                             submit_col = "submitdate",
#                             dataset_name = "adults",
#                             logger = logger)
#   dat_adults <- res$cleaned
#   trash_adults <- res$trash_bin
#   logger$write(sprintf("Completed: %s duplicate pass", "adults"))
#
# -------------------------------------------------------------------------
resolve_duplicates <- function(df,
                               vp_col,
                               submit_col = "submitdate",
                               dataset_name = "dataset",
                               data_type = "data",
                               lastpage_threshold = 17,
                               project_col = "project",
                               logger = NULL) {
  # --- Required columns check ---
  if (!all(c(vp_col, submit_col) %in% names(df))) {
    stop(sprintf("[%s | %s] Missing required columns. Need at least: %s, %s",
                 dataset_name, data_type, vp_col, submit_col))
  }
  
  has_lastpage <- "lastpage" %in% names(df)
  has_project  <- !is.null(project_col) && (project_col %in% names(df))
  has_form     <- "form" %in% names(df)
  
  if (dataset_name == "children_parents") {
    if (!has_form) stop("[children_parents] Missing required column: form")
    if (!has_project) warning("[children_parents] No project column found — skipping project-based disambiguation.")
  }
  
  # --- Helper: emit warning to console + logger ---
  out_warnings <- character(0)
  emit_warn <- function(text) {
    tag <- sprintf("[%s | %s]", dataset_name, data_type)
    msg <- paste0("⚠️ ", tag, " ", text, " — please resolve manually.")
    message(msg)
    if (!is.null(logger) && !is.null(logger$write)) {
      safe_line <- paste0("Warning: ", tag, " ", text)
      try(logger$write(safe_line), silent = TRUE)
    }
    out_warnings <<- c(out_warnings, msg)
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
    id_lab  <- if (length(id_vals) == 1) id_vals else paste(id_vals, collapse = ",")
    form_vals <- if (has_form) unique(trimws(as.character(df$form[idx]))) else NA_character_
    form_lab  <- if (!all(is.na(form_vals)) && length(form_vals) == 1) form_vals else NA
    n_submit  <- sum(df$.__has_submit__.[idx], na.rm = TRUE)
    
    # 1. exactly one submit
    if (n_submit == 1) {
      keeper <- idx[df$.__has_submit__.[idx]]
      losers <- setdiff(idx, keeper)
      if (length(losers) > 0) {
        keep_flag[losers] <- FALSE
        tb <- df[losers, , drop = FALSE]
        tb$.__reason__. <- if (!is.na(form_lab))
          sprintf("Removed duplicate %s=%s & form=%s; another row has %s.", vp_col, id_lab, form_lab, submit_col) else
            sprintf("Removed duplicate %s=%s; another row has %s.", vp_col, id_lab, submit_col)
        trash_bin <- rbind(trash_bin, tb)
      }
      
      # 2. multiple submits
    } else if (n_submit > 1) {
      incompletes <- idx[!df$.__has_submit__.[idx]]
      if (length(incompletes) > 0) {
        keep_flag[incompletes] <- FALSE
        tb <- df[incompletes, , drop = FALSE]
        tb$.__reason__. <- if (!is.na(form_lab))
          sprintf("Removed incomplete duplicate %s=%s & form=%s; other rows have %s.",
                  vp_col, id_lab, form_lab, submit_col) else
                    sprintf("Removed incomplete duplicate %s=%s; other rows have %s.",
                            vp_col, id_lab, submit_col)
        trash_bin <- rbind(trash_bin, tb)
      }
      emit_warn(if (!is.na(form_lab))
        sprintf("Multiple complete datasets for %s=%s, form=%s", vp_col, id_lab, form_lab)
        else
          sprintf("Multiple complete datasets for %s=%s", vp_col, id_lab)
      )
      
      # 3. no submit: check lastpage
    } else {
      if (has_lastpage) {
        high_lp <- idx[df$.__lp_num__.[idx] > lastpage_threshold]
        if (length(high_lp) == 1) {
          keeper <- high_lp
          losers <- setdiff(idx, keeper)
          if (length(losers) > 0) {
            keep_flag[losers] <- FALSE
            tb <- df[losers, , drop = FALSE]
            tb$.__reason__. <- if (!is.na(form_lab))
              sprintf("Removed duplicate %s=%s & form=%s; another row has lastpage > %s.",
                      vp_col, id_lab, form_lab, lastpage_threshold) else
                        sprintf("Removed duplicate %s=%s; another row has lastpage > %s.",
                                vp_col, id_lab, lastpage_threshold)
            trash_bin <- rbind(trash_bin, tb)
          }
        } else {
          emit_warn(if (!is.na(form_lab))
            sprintf("Multiple incomplete datasets for %s=%s, form=%s", vp_col, id_lab, form_lab)
            else
              sprintf("Multiple incomplete datasets for %s=%s", vp_col, id_lab)
          )
        }
      } else {
        emit_warn(if (!is.na(form_lab))
          sprintf("Multiple incomplete datasets for %s=%s, form=%s (no lastpage column)", vp_col, id_lab, form_lab)
          else
            sprintf("Multiple incomplete datasets for %s=%s (no lastpage column)", vp_col, id_lab)
        )
      }
    }
  }
  
  # --- Cleanup ---
  cleaned <- df[keep_flag, , drop = FALSE]
  cleaned$.__has_submit__. <- NULL
  cleaned$.__lp_num__. <- NULL
  trash_bin$.__has_submit__. <- NULL
  trash_bin$.__lp_num__. <- NULL
  
  list(cleaned = cleaned, trash_bin = trash_bin, warnings = out_warnings)
}