# -------------------------------------------------------------------------
# find_pilot_ids()
#
# Purpose:
#   Identify participant IDs (vpid) that belong to "pilot" data collections
#   in df1 and also appear in df2.
#
# Behavior:
#   - Column usage (configurable via args):
#       • df1: vpid from `vpid_col_df1`, data collection label from
#         `datacol_col_df1`.
#       • df2: vpid from `vpid_col_df2`.
#   - Pilot detection:
#       • Selects rows in df1 where `datacol_col_df1` matches "pilot"
#         (case-insensitive).
#   - Matching:
#       • Returns vpids that are present both in the pilot subset of df1 and in df2.
#
# Input:
#   df1             : data frame containing vpids and a data-collection column.
#   df2             : data frame containing vpids to check against.
#   vpid_col_df1    : string, vpid column name in df1 (default: "vpid").
#   datacol_col_df1 : string, data-collection column name in df1 (default: "datacollection").
#   vpid_col_df2    : string, vpid column name in df2 (default: "vpid").
#
# Output:
#   Character vector of unique vpids that are marked as "pilot" in df1 and
#   present in df2 (order not guaranteed).
#
# Notes:
#   - The "pilot" test is a simple grepl on the data-collection column.
#   - For safety, both vpid columns are coerced to character and NAs dropped.
#   - If required columns are missing, the function stops with an error.
#
# Example:
#   pilot_vpids <- find_pilot_ids(df1 = meta, df2 = main,
#                                 vpid_col_df1 = "vpid",
#                                 datacol_col_df1 = "datacollection",
#                                 vpid_col_df2 = "participant_id")
#
# -------------------------------------------------------------------------



# Function definition
find_pilot_ids <- function(df1, df2, 
                           vpid_col_df1 = "vpid", 
                           datacol_col_df1 = "datacollection", 
                           vpid_col_df2 = "vpid") {
  # Check that required columns exist
  if (!all(c(vpid_col_df1, datacol_col_df1) %in% names(df1))) {
    stop("df1 must contain columns: ", vpid_col_df1, " and ", datacol_col_df1)
  }
  if (!(vpid_col_df2 %in% names(df2))) {
    stop("df2 must contain column: ", vpid_col_df2)
  }
  
  # Step 1: Identify vpids in df1 where datacollection contains "pilot"
  pilot_ids <- df1[[vpid_col_df1]][
    grepl("pilot", df1[[datacol_col_df1]], ignore.case = TRUE)
  ]
  
  # Step 2: Find which of those vpids exist in df2
  matching_ids <- intersect(pilot_ids, df2[[vpid_col_df2]])
  
  # Step 3: Return as character vector
  return(as.character(matching_ids))
}