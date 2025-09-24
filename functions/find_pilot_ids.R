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

# --- Example usage ---

# # Example data frames
# df1 <- data.frame(
#   alt_id = c(1, 2, 3, 4),
#   datacollection = c("pilot study", "main study", "pilot test", "validation")
# )
# 
# df2 <- data.frame(
#   my_id = c(1, 3, 5, 6),
#   other_info = c("A", "B", "C", "D")
# )
# 
# # Call the function with custom column names
# result <- find_pilot_ids(df1, df2, 
#                          vpid_col_df1 = "alt_id", 
#                          datacol_col_df1 = "datacollection", 
#                          vpid_col_df2 = "my_id")
# 
# print(result)
