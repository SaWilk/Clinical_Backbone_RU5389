check_ranges <- function(dat_adults,
                         item_info_adults,
                         scoring_info,
                         id_col = NULL) {
  
  #--- Build a row id and (optionally) an id_map we can join later
  dat_with_rowid <- dat_adults %>%
    mutate(.row_id = dplyr::row_number())
  
  if (!is.null(id_col)) {
    stopifnot(id_col %in% names(dat_adults))
    id_map <- tibble(
      .row_id = seq_len(nrow(dat_adults)),
      !!id_col := dat_adults[[id_col]]
    )
  } else {
    id_map <- NULL
  }
  
  #--- Normalize lookups
  scoring_norm <- .normalize_scoring_info(scoring_info)
  
  link_parts   <- .build_item_link(dat_adults, item_info_adults)
  item_link    <- link_parts$link      # item, scale, item_key, data_col (may be NA)
  present_link <- link_parts$present   # rows where we found a data_col
  missing_link <- link_parts$missing   # rows where we did not
  
  #--- Warnings / notes
  missing_in_data <- missing_link$item
  if (length(missing_in_data) > 0) {
    warning(sprintf(
      "Items in item_info_adults but not in dat_adults (after name normalization): %s",
      paste(missing_in_data, collapse = ", ")
    ))
  }
  
  all_item_cols  <- present_link$data_col
  missing_in_map <- setdiff(names(dat_adults), c(all_item_cols, if (!is.null(id_col)) id_col))
  
  #--- Long data for items we can evaluate
  long <- dat_with_rowid %>%
    # ensure pivot won't choke on mixed types
    mutate(across(all_of(all_item_cols), as.character)) %>%
    tidyr::pivot_longer(
      cols = all_of(all_item_cols),
      names_to = "data_col",
      values_to = "value"
    ) %>%
    # attach Item & Scale via data_col
    left_join(present_link %>% select(item, scale, data_col), by = "data_col") %>%
    # attach scoring ranges
    left_join(scoring_norm, by = "scale") %>%
    # treat empty strings as NA, keep numeric copy
    mutate(
      value     = na_if(value, ""),
      value_num = suppressWarnings(as.numeric(value))
    )
  
  #--- Missingness (NA after coercion)
  missing_df <- long %>%
    mutate(is_missing = is.na(value_num)) %>%
    filter(is_missing) %>%
    transmute(
      .row_id,
      scale, item,
      issue = "missing",
      value = as.character(value)
    )
  
  #--- Range violations (only where ranges exist and value is numeric)
  violations_df <- long %>%
    filter(!is.na(value_num), !is.na(min), !is.na(max)) %>%
    mutate(
      below_min = value_num < min,
      above_max = value_num > max,
      ok        = !(below_min | above_max),
      direction = dplyr::case_when(
        below_min ~ "below_min",
        above_max ~ "above_max",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!ok) %>%
    transmute(
      .row_id,
      scale, item,
      value = value_num,
      min, max,
      direction
    )
  
  #--- Non-numeric (present but couldn't be coerced)
  non_numeric_df <- long %>%
    filter(!is.na(value), is.na(value_num)) %>%
    transmute(
      .row_id,
      scale, item,
      issue = "non_numeric_value",
      value = as.character(value)
    )
  
  #--- Attach the optional ID to outputs (via .row_id)
  if (!is.null(id_map)) {
    violations_df <- violations_df %>% left_join(id_map, by = ".row_id")
    missing_df    <- missing_df    %>% left_join(id_map, by = ".row_id")
    non_numeric_df<- non_numeric_df%>% left_join(id_map, by = ".row_id")
  }
  
  #--- Summary per scale
  summary_df <- long %>%
    mutate(is_missing = is.na(value_num)) %>%
    group_by(scale) %>%
    summarise(
      n_items      = n_distinct(item),
      n_obs        = n_distinct(.row_id),
      n_missing    = sum(is_missing, na.rm = TRUE),
      n_violations = nrow(violations_df %>% dplyr::filter(scale %in% cur_group()$scale)),
      .groups = "drop"
    )
  
  list(
    violations = violations_df,
    missing    = dplyr::bind_rows(missing_df, non_numeric_df),
    summary    = summary_df,
    notes      = list(
      items_missing_in_dat         = missing_in_data,
      columns_ignored_not_in_map   = missing_in_map
    )
  )
}
