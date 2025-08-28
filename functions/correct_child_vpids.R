# -------------------------------------------------------------------------
# correct_child_vpids()
#
# Purpose:
#   Corrects wrongly entered VP-IDs in `dat_children_parents` for project 8
#   using a mapping provided in an Excel file. Only rows with project == 8
#   are updated; all others remain unchanged.
#
# Behavior:
#   - Reads the mapping Excel file with two columns:
#       1. "Original VP-ID"  (incorrect ID)
#       2. "Neue Child VP-ID" (correct ID)
#   - Replaces VP-IDs in `dat_children_parents$vpid` (restricted to project == 8)
#     according to the mapping.
#   - If any project 8 VP-IDs have no mapping, they are left unchanged and
#     printed as a message.
#
# Input:
#   dat            : a data frame with at least the columns:
#       - vpid    : identifier (numeric or character)
#       - project : project code (only project == 8 is corrected)
#   mapping_file   : path to the Excel mapping file (default:
#                   "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx")
#   sheet          : sheet index or name in the Excel file (default: 1)
#
# Output:
#   - Returns the updated data frame with corrected VP-IDs.
#   - Prints a message listing any project 8 VP-IDs that were not altered
#     because they were not found in the mapping.
#
# Example:
#   dat_children_parents <- correct_child_vpids(dat_children_parents)
#
# -------------------------------------------------------------------------

library(readxl)

correct_child_vpids <- function(
    dat,
    mapping_file = "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx",
    sheet = 1
) {
  # read mapping
  map_df <- read_excel(mapping_file, sheet = sheet, col_names = TRUE)[,1:2]
  names(map_df) <- c("original", "new")
  
  # make sure both are character (avoids Excel number formatting issues)
  map_df$original <- as.character(map_df$original)
  map_df$new      <- as.character(map_df$new)
  
  # create lookup
  map_vec <- setNames(map_df$new, map_df$original)
  
  # subset project 8
  is_proj8 <- dat$project == 8
  vpid_chr <- as.character(dat$vpid)
  
  # which project 8 vpids have a mapping?
  has_map <- is_proj8 & vpid_chr %in% names(map_vec)
  
  # apply replacements
  vpid_chr[has_map] <- unname(map_vec[vpid_chr[has_map]])
  
  # cast back to numeric if original was numeric
  if (is.numeric(dat$vpid)) {
    dat$vpid <- as.numeric(vpid_chr)
  } else {
    dat$vpid <- vpid_chr
  }
  
  # report untouched vpids in project 8
  untouched <- unique(vpid_chr[is_proj8 & !has_map])
  if (length(untouched) > 0) {
    message("These project 8 VP-IDs were not altered (no mapping found): ",
            paste(untouched, collapse = ", "))
  } else {
    message("All project 8 VP-IDs were updated according to the mapping.")
  }
  
  dat
}
