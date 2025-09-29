#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: Test Script - Comparing VPIDs for Sources Project 6
# Author: Saskia Wilken (saskia.wilken@uni-hamburg.de)
# 2025-09-26 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script 
# (1) reads questionnaire data exported from LimeSurvey and PsyToolkit

# (2) Fixes Issues with VP ID assignment

# (3) Creates project-specific data files in the environment as well as on disk


# Do Children and Parent Questionnaires Match in VP ID?

res3 <- compare_vpcodes_three(
  dat_children_p6,
  dat_parents_p6,
  dat_children_parents,     # will be filtered to project == 6
  id_col_12   = "VPCode",
  wcst_id_col   = "vpid",
  wcst_project_col = "project",
  project_value = 6
)


# Build a wide table: one row per unique ID, columns = categories
all_ids <- sort(unique(unlist(res3$ids)))
cats <- names(res3$ids)

wide_ids <- data.frame(vpid = all_ids, stringsAsFactors = FALSE)
for (nm in cats) {
  present <- all_ids %in% res3$ids[[nm]]
  wide_ids[[nm]] <- ifelse(present, all_ids, NA)
}

# Write Excel: counts + wide IDs
wb <- createWorkbook()
addWorksheet(wb, "Counts")
writeData(wb, "Counts", res3$counts)

addWorksheet(wb, "IDs_wide")
writeData(wb, "IDs_wide", wide_ids)

saveWorkbook(wb, "VPCode_comparison_wide.xlsx", overwrite = TRUE)