# -------------------------------------------------------------------------
# correct_child_vpids()
#
# Purpose:
#   Corrects wrongly entered VP-IDs in `dat_children_parents` for project 8
#   using a mapping provided in an Excel file. Only rows with project == 8
#   are updated; all others remain unchanged.
#
# Behavior:
#   - Reads the mapping Excel file and selects the two columns by NAME:
#       1. "Original VP-ID"      (incorrect ID)
#       2. "Neue Child VP-ID"    (correct ID)
#     If these columns are not found, the function stops with a clear error.
#   - Removes mapping rows where either column is NA/empty and warns with a count.
#   - If there are duplicate "Original VP-ID" values in the mapping, a warning
#     is printed listing the duplicated VP-IDs; the LAST occurrence is used.
#   - Replaces VP-IDs in `dat_children_parents$vpid` (restricted to project == 8)
#     according to the mapping.
#   - If there are no rows with project == 8, nothing is changed and a message
#     is printed: "No rows with project == 8 found; nothing to update."
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
#   - Prints messages about dropped mapping rows, duplicate mapping keys,
#     unmapped VP-IDs in project 8, or if there were no project 8 rows.
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
  # read mapping (expect named columns)
  map_raw <- read_excel(mapping_file, sheet = sheet, col_names = TRUE)
  
  required_cols <- c("Original VP-ID", "Neue Child VP-ID")
  missing_cols  <- setdiff(required_cols, names(map_raw))
  if (length(missing_cols) > 0) {
    stop("Mapping file is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Expected columns: 'Original VP-ID' and 'Neue Child VP-ID'.")
  }
  
  # select and normalize
  map_df <- map_raw[, required_cols]
  names(map_df) <- c("original", "new")
  
  # make sure both are character (avoids Excel number formatting issues)
  map_df$original <- trimws(as.character(map_df$original))
  map_df$new      <- trimws(as.character(map_df$new))
  
  # drop NA/empty rows with a warning  (Fix 6)
  na_rows <- is.na(map_df$original) | map_df$original == "" |
    is.na(map_df$new)      | map_df$new == ""
  if (any(na_rows)) {
    warning("Dropped ", sum(na_rows), " mapping row(s) with empty/NA original/new values.")
    map_df <- map_df[!na_rows, , drop = FALSE]
  }
  
  # warn on duplicate 'original' keys; keep LAST occurrence (Fix 5)
  if (any(duplicated(map_df$original))) {
    dup_keys <- unique(map_df$original[duplicated(map_df$original)])
    warning("Duplicate 'Original VP-ID' entries in mapping for: ",
            paste(dup_keys, collapse = ", "),
            ". Using the last occurrence for each duplicate.")
    # keep last occurrence by de-duplicating from the bottom
    map_df <- map_df[rev(!duplicated(rev(map_df$original))), , drop = FALSE]
  }
  
  # create lookup
  map_vec <- setNames(map_df$new, map_df$original)
  
  # subset project 8
  is_proj8 <- dat$project == 8
  if (!any(is_proj8, na.rm = TRUE)) {
    message("No rows with project == 8 found; nothing to update.")  # (Fix 8)
    return(dat)
  }
  
  vpid_chr <- as.character(dat$vpid)
  
  # which project 8 vpids have a mapping?
  has_map <- is_proj8 & vpid_chr %in% names(map_vec)
  
  # apply replacements
  vpid_chr[has_map] <- unname(map_vec[vpid_chr[has_map]])
  
  # cast back to numeric if original was numeric (note: leading zeros may be lost)
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
