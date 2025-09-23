compare_vpcodes_three <- function(
    children_df,
    parents_df,
    wcst_df,
    id_col_12 = "VPCode",     # column name in children_df and parents_df
    wcst_id_col = "vpid",     # column name in wcst_df
    wcst_project_col = "project",
    project_value = 6,
    drop_na = TRUE,
    trim_ws = TRUE
) {
  # helper to clean IDs
  clean_ids <- function(x) {
    if (trim_ws && is.character(x)) x <- trimws(x)
    if (drop_na) x <- x[!is.na(x)]
    unique(x)
  }
  
  # extract and clean IDs
  child_ids  <- clean_ids(children_df[[id_col_12]])
  parent_ids <- clean_ids(parents_df[[id_col_12]])
  
  # filter wcst dataset to the requested project and extract IDs
  wcst_subset <- wcst_df[wcst_df[[wcst_project_col]] == project_value, , drop = FALSE]
  wcst_ids    <- clean_ids(wcst_subset[[wcst_id_col]])
  
  # unions / intersections
  overlap_all         <- Reduce(intersect, list(child_ids, parent_ids, wcst_ids))
  overlap_child_parent <- intersect(child_ids, parent_ids)
  overlap_child_wcst   <- intersect(child_ids, wcst_ids)
  overlap_parent_wcst  <- intersect(parent_ids, wcst_ids)
  
  only_children <- setdiff(child_ids,  union(parent_ids, wcst_ids))
  only_parents  <- setdiff(parent_ids, union(child_ids,  wcst_ids))
  only_wcst     <- setdiff(wcst_ids,   union(child_ids,  parent_ids))
  
  # in exactly two, but not the third
  child_parent_not_wcst <- setdiff(overlap_child_parent, wcst_ids)
  child_wcst_not_parent <- setdiff(overlap_child_wcst,   parent_ids)
  parent_wcst_not_child <- setdiff(overlap_parent_wcst,  child_ids)
  
  # counts summary
  counts <- data.frame(
    metric = c(
      "n_children_unique", "n_parents_unique", "n_wcst_unique_filtered",
      "n_overlap_all_three",
      "n_overlap_child_parent", "n_overlap_child_wcst", "n_overlap_parent_wcst",
      "n_only_children", "n_only_parents", "n_only_wcst",
      "n_child_parent_not_wcst", "n_child_wcst_not_parent", "n_parent_wcst_not_child"
    ),
    value = c(
      length(child_ids), length(parent_ids), length(wcst_ids),
      length(overlap_all),
      length(overlap_child_parent), length(overlap_child_wcst), length(overlap_parent_wcst),
      length(only_children), length(only_parents), length(only_wcst),
      length(child_parent_not_wcst), length(child_wcst_not_parent), length(parent_wcst_not_child)
    ),
    row.names = NULL
  )
  
  list(
    counts = counts,
    ids = list(
      overlap_all = overlap_all,
      overlap_child_parent = overlap_child_parent,
      overlap_child_wcst = overlap_child_wcst,
      overlap_parent_wcst = overlap_parent_wcst,
      only_children = only_children,
      only_parents = only_parents,
      only_wcst = only_wcst,
      child_parent_not_wcst = child_parent_not_wcst,
      child_wcst_not_parent = child_wcst_not_parent,
      parent_wcst_not_child = parent_wcst_not_child
    ),
    settings = list(
      id_col_12 = id_col_12,
      wcst_id_col = wcst_id_col,
      wcst_project_col = wcst_project_col,
      project_value = project_value,
      drop_na = drop_na,
      trim_ws = trim_ws
    )
  )
}
