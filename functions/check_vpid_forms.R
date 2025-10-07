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
                             data_type = "forms") {
  required_forms <- c("A", "C", "P")
  
  problems <- dat_children_parents %>%
    dplyr::filter(project == 8) %>%
    dplyr::group_by(vpid) %>%
    dplyr::summarise(forms = list(form), .groups = "drop") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      counts    = list(table(unlist(forms))),
      missing   = list(setdiff(required_forms, names(counts))),
      duplicate = list(names(counts[counts > 1]))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(lengths(missing) > 0 | lengths(duplicate) > 0) %>%
    dplyr::mutate(
      vpid_str = ifelse(is.na(vpid) | vpid == "", "unknown", as.character(vpid)),
      details = paste(
        c(
          if (lengths(missing) > 0) paste0("Missing forms: ", paste(unlist(missing), collapse = ", ")),
          if (lengths(duplicate) > 0) paste0("Duplicate forms: ", paste(unlist(duplicate), collapse = ", "))
        ),
        collapse = "; "
      ),
      console_msg = paste0(
        "⚠️ vpid ", vpid_str, ":\n",
        paste0("   - ", gsub("; ", "\n   - ", details))
      ),
      log_line = paste0(
        "⚠️ [", dataset_name, " | ", data_type, "] vpid ", vpid_str,
        " — ", details, " — please resolve manually."
      )
    )
  
  # --- Print to console ---
  if (nrow(problems)) {
    cat(paste0(problems$console_msg, collapse = "\n"))
    
    # --- Write to logger ---
    if (!is.null(logger) && !is.null(logger$write)) {
      for (ln in problems$log_line) {
        try(logger$write(ln), silent = TRUE)
      }
    }
  }
  
  invisible(NULL)
}
