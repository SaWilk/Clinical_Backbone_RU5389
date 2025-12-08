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
# - Handles legacy item-level names (e.g., WCST_1) and new aggregate names (WCST)
# - Case-insensitive, tolerant to underscores/digits/extra chars
# - Falls back to other numeric, non-admin columns if tasks renamed again
# - Never silently discards everything; emits clear warnings

partition_empty_obs_psytoolkit <- function(
    df,
    id_col         = "id",
    time_start_col = "TIME_start",
    time_end_col   = "TIME_end",
    # Add your third-battery tokens here (examples included)
    task_tokens    = c("wcst","lns","bacs","ant","cpt","flanker","gng","stroop","nback"),
    admin_like     = c("id","vpid","p","project","proj","comp",
                       "timestart","timeend","timetotal","startdate","enddate",
                       "starttime","endtime"),
    verbose        = TRUE
) {
  stopifnot(is.data.frame(df))
  if (!nrow(df)) return(list(kept = df, empty = df, no_id = df))
  
  canon <- function(x) gsub("[^a-z0-9]", "", tolower(x), perl = TRUE)
  has_col  <- function(nm) isTRUE(nm %in% names(df))
  safe_get <- function(nm) if (has_col(nm)) df[[nm]] else rep(NA, nrow(df))
  
  cols       <- names(df)
  cols_canon <- canon(cols)
  
  id_vec       <- safe_get(id_col)
  time_end_vec <- safe_get(time_end_col)
  
  is_admin  <- cols_canon %in% unique(canon(admin_like))
  token_hit <- Reduce(`|`, lapply(task_tokens, function(tok) grepl(tok, cols_canon, fixed = TRUE)))
  measure_1 <- which(token_hit & !is_admin)
  
  legacy_rx <- paste0("^(?:", paste(task_tokens, collapse="|"), ")[0-9_]+$")
  measure_2 <- which(grepl(legacy_rx, cols_canon))
  
  measure_idx  <- unique(c(measure_1, measure_2))
  
  # Last fallback: numeric, non-admin, non time/id
  if (!length(measure_idx)) {
    numericish <- vapply(df, function(x) is.numeric(x) || is.integer(x) || is.logical(x), logical(1))
    not_admin  <- !is_admin & !(cols_canon %in% c(canon(id_col), canon(time_start_col), canon(time_end_col)))
    measure_idx <- which(numericish & not_admin)
    if (verbose) {
      warning("partition_empty_obs_psytoolkit(): No task-named columns found. ",
              "Falling back to ", length(measure_idx), " numeric non-admin column(s) as measures.")
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
    # zero is a valid value; only NA/blank are treated as missing
    has_signal <- rowSums(!vapply(sub, function(x) {
      if (is.character(x) || is.factor(x)) {
        z <- if (is.factor(x)) as.character(x) else x
        is.na(z) | trimws(z) == ""
      } else {
        is.na(x)
      }
    }, logical(nrow(df)))) > 0
  }
  
  # STRICT rule: must have ID AND any test signal. TIME_end alone is NOT enough.
  keep_mask <- has_id & has_signal
  
  kept   <- df[keep_mask, , drop = FALSE]
  no_id  <- df[!has_id & has_signal, , drop = FALSE]
  empty  <- df[has_id & !has_signal, , drop = FALSE]
  
  if (verbose) {
    message(sprintf(
      "partition_empty_obs_psytoolkit(): kept=%d, empty=%d, no_id=%d (of %d)",
      nrow(kept), nrow(empty), nrow(no_id), nrow(df)
    ))
  }
  
  list(kept = kept, empty = empty, no_id = no_id)
}
