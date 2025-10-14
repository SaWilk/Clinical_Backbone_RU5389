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


partition_empty_obs_psytoolkit <- function(df,
                                           id_col = "id",
                                           score_cols = c("WCST_1", "LNS_1", "BACS_1")) {
  # helper: TRUE if NA or blank/whitespace (handles factors too)
  is_missing_vec <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      is.na(x) | trimws(x) == ""
    } else {
      is.na(x)
    }
  }
  
  # figure out which measure columns are present
  present_score_cols <- intersect(score_cols, names(df))
  missing_score_cols <- setdiff(score_cols, names(df))
  if (length(missing_score_cols) > 0) {
    warning("These measure columns are missing and will be ignored: ",
            paste(missing_score_cols, collapse = ", "))
  }
  
  # id missing handling (if id column itself is missing, treat all as missing id)
  if (!(id_col %in% names(df))) {
    warning("ID column '", id_col, "' is missing; treating all rows as missing id.")
    id_missing <- rep(TRUE, nrow(df))
  } else {
    id_missing <- is_missing_vec(df[[id_col]])
  }
  
  # compute score-missing logic using only present measure columns
  if (length(present_score_cols) == 0) {
    # no measures available -> define all rows as "empty" (no measure info)
    all_scores_miss   <- rep(TRUE, nrow(df))
    any_score_present <- rep(FALSE, nrow(df))
  } else {
    # ensure it's a matrix even if length=1
    miss_mat <- sapply(present_score_cols, function(cn) is_missing_vec(df[[cn]]), simplify = "matrix")
    all_scores_miss   <- apply(miss_mat, 1, all)
    any_score_present <- !all_scores_miss
  }
  
  # buckets per your rule
  empty       <- df[ all_scores_miss, , drop = FALSE]                  # irrespective of id
  no_id       <- df[ id_missing & any_score_present, , drop = FALSE]   # id missing but some measure present
  remove_cond <- all_scores_miss | (id_missing & any_score_present)
  kept        <- df[ !remove_cond, , drop = FALSE]
  
  list(
    kept = kept,
    no_id = no_id,
    empty = empty
  )
}
