remove_empty_obs_psytoolkit <- function(df,
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
    miss_mat <- sapply(present_score_cols, function(cn) is_missing_vec(df[[cn]]))
    # ensure it's a matrix even if length=1
    miss_mat <- as.data.frame(miss_mat)
    all_scores_miss   <- Reduce(`&`, miss_mat)
    any_score_present <- !all_scores_miss
  }
  
  # buckets per your rule
  empty    <- df[ all_scores_miss, , drop = FALSE]                  # irrespective of id
  no_id    <- df[ id_missing & any_score_present, , drop = FALSE]   # id missing but some measure present
  remove_cond <- all_scores_miss | (id_missing & any_score_present)
  kept     <- df[ !remove_cond, , drop = FALSE]
  
  list(
    kept = kept,
    no_id = no_id,
    empty = empty
  )
}
