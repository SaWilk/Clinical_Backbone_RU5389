# Function definition
find_pilot_ids <- function(df1, df2, vpid_col = "vpid", datacol_col = "datacollection") {
  # Check that required columns exist
  if (!all(c(vpid_col, datacol_col) %in% names(df1))) {
    stop("df1 must contain columns: ", vpid_col, " and ", datacol_col)
  }
  if (!(vpid_col %in% names(df2))) {
    stop("df2 must contain column: ", vpid_col)
  }
  
  # Step 1: Identify vpids in df1 where datacollection contains "pilot"
  pilot_ids <- df1[[vpid_col]][grepl("pilot", df1[[datacol_col]], ignore.case = TRUE)]
  
  # Step 2: Find which of those vpids exist in df2
  matching_ids <- intersect(pilot_ids, df2[[vpid_col]])
  
  # Step 3: Return as character vector
  return(as.character(matching_ids))
}

# # --- Example usage ---
# 
# # Example data frames
# df1 <- data.frame(
#   vpid = c(1, 2, 3, 4),
#   datacollection = c("pilot study", "main study", "pilot test", "validation")
# )
# 
# df2 <- data.frame(
#   vpid = c(1, 3, 5, 6),
#   other_info = c("A", "B", "C", "D")
# )
# 
# # Call the function
# result <- find_pilot_ids(df1, df2)
# print(result)
