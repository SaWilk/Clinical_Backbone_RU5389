# Returns a *named list* of data frames: one data frame per scale,
# columns renamed to the canonical Item names (nice for downstream work).
extract_scales <- function(dat_adults, item_info_adults) {
  link_parts <- .build_item_link(dat_adults, item_info_adults)
  present_link <- link_parts$present
  
  items_by_scale <- present_link %>%
    group_by(scale) %>%
    summarise(
      data_cols = list(data_col),
      item_names = list(item),
      .groups = "drop"
    )
  
  out <- map(
    seq_len(nrow(items_by_scale)),
    function(i) {
      cols <- items_by_scale$data_cols[[i]]
      nm   <- items_by_scale$item_names[[i]]
      df   <- dat_adults %>% select(all_of(cols))
      # rename to Item names
      names(df) <- nm
      df
    }
  )
  set_names(out, items_by_scale$scale)
}