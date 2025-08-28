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
check_vpid_forms <- function(dat_children_parents) {
  required_forms <- c("A", "C", "P")
  
  dat_children_parents %>%
    filter(project == 8) %>%
    group_by(vpid) %>%
    summarise(forms = list(form), .groups = "drop") %>%
    rowwise() %>%
    mutate(
      counts = list(table(forms)),
      missing = list(setdiff(required_forms, names(counts))),
      duplicate = list(names(counts[counts > 1]))
    ) %>%
    filter(length(missing) > 0 | length(duplicate) > 0) %>%
    mutate(
      report = paste0(
        "⚠️ vpid ", vpid, ":\n",
        if (length(missing) > 0) paste0("   - Missing forms: ", paste(missing, collapse = ", "), "\n") else "",
        if (length(duplicate) > 0) paste0("   - Duplicate forms: ", paste(duplicate, collapse = ", "), "\n") else ""
      )
    ) %>%
    pull(report) %>%
    cat(sep = "")
}
