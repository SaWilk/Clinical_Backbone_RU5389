library(dplyr)

# -------------------------------------------------------------------------
# check_vpid_forms()
#
# Purpose:
#   Checks whether each vpid in dat_children_parents (restricted to project == 8)
#   contains exactly one entry for each required form: "A", "C", and "P".
#
# Behavior:
#   - If a required form is missing, it reports which ones are missing.
#   - If a required form occurs more than once, it reports which ones are duplicated.
#   - Results are printed in a readable format with warning symbols (⚠️).
#
# Input:
#   dat_children_parents : a data frame with at least the columns:
#       - vpid    : identifier
#       - form    : type of form (expects "A", "C", "P")
#       - project : project code (only vpid with project == 8 are checked)
#
# Output:
#   - Prints a warning message for each problematic vpid.
#   - Returns nothing (invisible), but could be adapted to return a summary table.
#
# Example:
#   check_vpid_forms(dat_children_parents)
#
# -------------------------------------------------------------------------
check_vpid_forms <- function(dat_children_parents,
                             logger = NULL,
                             dataset_name = "dat_children_parents",
                             data_type = "forms",
                             submit_col = "submitdate",
                             lastpage_col = "lastpage",
                             lastpage_threshold = 17) {
  
  required_forms <- c("A", "C", "P")
  
  has_submit <- submit_col %in% names(dat_children_parents)
  has_lastpage <- lastpage_col %in% names(dat_children_parents)
  
  if (!"vpid" %in% names(dat_children_parents)) {
    stop("check_vpid_forms(): missing required column 'vpid'")
  }
  if (!"form" %in% names(dat_children_parents)) {
    stop("check_vpid_forms(): missing required column 'form'")
  }
  if (!"project" %in% names(dat_children_parents)) {
    stop("check_vpid_forms(): missing required column 'project'")
  }
  
  dat_p8 <- dat_children_parents %>%
    dplyr::filter(project == 8) %>%
    dplyr::mutate(
      .form = trimws(as.character(form)),
      .has_submit = if (has_submit) (!is.na(.data[[submit_col]]) & trimws(as.character(.data[[submit_col]])) != "") else FALSE,
      .lastpage_num = if (has_lastpage) suppressWarnings(as.numeric(.data[[lastpage_col]])) else NA_real_
    )
  
  # classify duplicate forms within each vpid as complete vs incomplete
  dup_detail <- dat_p8 %>%
    dplyr::filter(!is.na(.form), .form != "") %>%
    dplyr::group_by(vpid, .form) %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_submit = sum(.has_submit, na.rm = TRUE),
      max_lastpage = suppressWarnings(max(.lastpage_num, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_rows > 1) %>%
    dplyr::mutate(
      dup_status = dplyr::case_when(
        n_submit > 1 ~ "complete",
        n_submit == 0 ~ "incomplete",
        TRUE ~ "mixed"
      )
    )
  
  problems <- dat_p8 %>%
    dplyr::group_by(vpid) %>%
    dplyr::summarise(
      forms_present = list(sort(unique(.form[!is.na(.form) & .form != ""]))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      missing = purrr::map(forms_present, ~ setdiff(required_forms, .x))
    ) %>%
    dplyr::left_join(
      dup_detail %>%
        dplyr::group_by(vpid, dup_status) %>%
        dplyr::summarise(forms = list(sort(unique(.form))), .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from = dup_status,
          values_from = forms,
          names_prefix = "dup_"
        ),
      by = "vpid"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      missing_txt = if (length(unlist(missing)) > 0) {
        paste0("Missing forms: ", paste(unlist(missing), collapse = ", "))
      } else NA_character_,
      
      dup_complete_txt = if ("dup_complete" %in% names(cur_data()) && !all(is.na(dup_complete))) {
        vals <- unlist(dup_complete)
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0) paste0("Duplicate complete forms: ", paste(vals, collapse = ", ")) else NA_character_
      } else NA_character_,
      
      dup_incomplete_txt = if ("dup_incomplete" %in% names(cur_data()) && !all(is.na(dup_incomplete))) {
        vals <- unlist(dup_incomplete)
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0) paste0("Duplicate incomplete forms: ", paste(vals, collapse = ", ")) else NA_character_
      } else NA_character_,
      
      dup_mixed_txt = if ("dup_mixed" %in% names(cur_data()) && !all(is.na(dup_mixed))) {
        vals <- unlist(dup_mixed)
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0) paste0("Duplicate mixed forms: ", paste(vals, collapse = ", ")) else NA_character_
      } else NA_character_
    ) %>%
    dplyr::mutate(
      details = paste(
        c(
          missing_txt[!is.na(missing_txt)],
          dup_complete_txt[!is.na(dup_complete_txt)],
          dup_incomplete_txt[!is.na(dup_incomplete_txt)],
          dup_mixed_txt[!is.na(dup_mixed_txt)]
        ),
        collapse = "; "
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(details != "") %>%
    dplyr::mutate(
      vpid_str = ifelse(is.na(vpid) | vpid == "", "unknown", as.character(vpid)),
      console_msg = paste0(
        "⚠️ vpid ", vpid_str, ":\n",
        paste0("   - ", gsub("; ", "\n   - ", details))
      ),
      log_line = paste0(
        "⚠️ [", dataset_name, " | ", data_type, "] vpid ", vpid_str,
        " — ", details, " — please resolve manually."
      )
    )
  
  if (nrow(problems)) {
    cat(paste0(problems$console_msg, collapse = "\n"), "\n")
    if (!is.null(logger) && !is.null(logger$write)) {
      for (ln in problems$log_line) try(logger$write(ln), silent = TRUE)
    }
  }
  
  invisible(problems)
}