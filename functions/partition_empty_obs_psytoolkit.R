# -------------------------------------------------------------------------
# partition_empty_obs_psytoolkit()
#
# Purpose:
#   Partition a dataset into:
#     (a) rows to keep,
#     (b) rows with some score present but a missing/blank ID, and
#     (c) rows with all target scores missing.
#
# Behavior:
#   - ID handling:
#       • Uses `id_col` (default "id"). If it’s absent, treats all rows as missing ID
#         and emits a warning.
#       • Considers IDs missing if NA or blank/whitespace (works for factor/character).
#   - Score columns:
#       • Checks only `score_cols` that are present in `df`; warns about missing ones.
#       • If none of the `score_cols` exist, treats all rows as "empty" (no measures).
#       • Defines “empty” as: all listed (present) score columns are NA/blank.
#       • Defines “some score present” as: NOT all scores missing.
#   - Buckets (returned as a list):
#       • $kept   : rows that have at least one score present AND a non-missing ID.
#       • $no_id  : rows with some score present BUT missing/blank ID.
#       • $empty  : rows where all target scores are missing (ID irrelevant).
#
# Input:
#   df         : data.frame with observations.
#   id_col     : string; column name for participant ID (default "id").
#   score_cols : character vector of score/measure column names to check.
#
# Output:
#   Returns:
#     list(kept = data.frame, no_id = data.frame, empty = data.frame)
#
# Notes:
#   - Missingness rules treat character blanks ("", whitespace) as missing.
#   - Non-character ID columns are assessed via NA only.
#   - Warnings are issued for absent `id_col` and absent `score_cols`.
#   - The function does not drop rows in-place; it returns partitioned subsets.
#
# Example:
#   parts <- partition_empty_obs_psytoolkit(
#              df = dat_psy,
#              id_col = "participant_id",
#              score_cols = c("WCST_1","LNS_1","BACS_1")
#            )
#   dat_clean <- parts$kept
# -------------------------------------------------------------------------


# partition_empty_obs_psytoolkit.R
# Robustly split PsyToolkit info exports into: kept / empty / no_id
# - Broader task signal detection (tokens, metric suffixes, legacy names)
# - STRICT: TIME_* alone is not "signal"
# - No deduping here (that stays in resolve_duplicates)

partition_empty_obs_psytoolkit <- function(
    df,
    id_col         = "id",
    time_start_col = "TIME_start",
    time_end_col   = "TIME_end",
    # extend this if new task names appear
    task_tokens    = c("wcst","lns","bacs","ant","cpt","flanker","gng","stroop","nback"),
    admin_like     = c("id","vpid","p","project","proj","comp",
                       "timestart","timeend","timetotal","startdate","enddate",
                       "starttime","endtime"),
    verbose        = TRUE
) {
  stopifnot(is.data.frame(df))
  if (!nrow(df)) return(list(kept = df, empty = df, no_id = df))
  
  canon    <- function(x) gsub("[^a-z0-9]", "", tolower(x), perl = TRUE)
  has_col  <- function(nm) isTRUE(nm %in% names(df))
  safe_get <- function(nm) if (has_col(nm)) df[[nm]] else rep(NA, nrow(df))
  
  cols       <- names(df)
  cols_canon <- canon(cols)
  
  id_vec <- safe_get(id_col)
  
  # --- detect potential measure columns (broader) ---
  is_admin  <- cols_canon %in% unique(canon(admin_like))
  
  # 1) column names containing any task token
  token_hit <- Reduce(`|`, lapply(task_tokens, function(tok) grepl(tok, cols_canon, fixed = TRUE)))
  
  # 2) typical metric/file suffixes
  suffix_hit <- grepl("(_acc|_rt|_score|_n?trials|_hits?|_errors?|_omissions?|_file|_path|_txt)$",
                      cols_canon)
  
  # 3) legacy item-style names: wcst1, lns_2, ...
  legacy_rx <- paste0("^(?:", paste(task_tokens, collapse="|"), ")[0-9_]+$")
  legacy_hit <- grepl(legacy_rx, cols_canon)
  
  measure_idx <- which((token_hit | suffix_hit | legacy_hit) & !is_admin)
  
  # fallback: any numeric, non-admin, non time/id
  if (!length(measure_idx)) {
    numericish <- vapply(df, function(x) is.numeric(x) || is.integer(x) || is.logical(x), logical(1))
    not_admin  <- !is_admin & !(cols_canon %in% c(canon(id_col), canon(time_start_col), canon(time_end_col)))
    measure_idx <- which(numericish & not_admin)
    if (verbose) {
      warning("partition_empty_obs_psytoolkit(): No token-named task columns found; ",
              "falling back to ", length(measure_idx), " numeric non-admin column(s).")
    }
  }
  
  measure_cols <- cols[measure_idx]
  if (verbose) {
    if (length(measure_cols)) {
      message("partition_empty_obs_psytoolkit(): Using measure columns: ",
              paste(measure_cols, collapse = ", "))
    } else {
      warning("partition_empty_obs_psytoolkit(): Still no measure columns recognized. ",
              "Rows must have an ID and at least one non-missing non-admin numeric value to be kept.")
    }
  }
  
  has_id <- !(is.na(id_vec) | (is.character(id_vec) & trimws(id_vec) == ""))
  
  has_signal <- rep(FALSE, nrow(df))
  if (length(measure_cols)) {
    sub <- df[measure_cols]
    # zero is a valid value; only NA/blank count as missing
    has_signal <- rowSums(!vapply(sub, function(x) {
      if (is.character(x) || is.factor(x)) {
        z <- if (is.factor(x)) as.character(x) else x
        is.na(z) | trimws(z) == ""
      } else {
        is.na(x)
      }
    }, logical(nrow(df)))) > 0
  }
  
  # STRICT: TIME_* alone does NOT count as signal
  keep_mask <- has_id & has_signal
  
  kept  <- df[keep_mask, , drop = FALSE]
  no_id <- df[!has_id & has_signal, , drop = FALSE]
  empty <- df[has_id & !has_signal, , drop = FALSE]
  
  if (verbose) {
    message(sprintf(
      "partition_empty_obs_psytoolkit(): kept=%d, empty=%d, no_id=%d (of %d)",
      nrow(kept), nrow(empty), nrow(no_id), nrow(df)
    ))
  }
  
  list(kept = kept, empty = empty, no_id = no_id)
}
