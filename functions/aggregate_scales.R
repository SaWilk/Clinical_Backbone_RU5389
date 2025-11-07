# Aggregate to scale scores using any function (mean by default)
aggregate_scales <- function(dat_adults,
                             item_info_adults,
                             id_col   = NULL,
                             fun      = mean,
                             na.rm    = TRUE,
                             min_items = 1) {
  
  if (!is.null(id_col)) {
    stopifnot(id_col %in% names(dat_adults))
    id_sym <- rlang::ensym(id_col)
  }
  
  link_parts   <- .build_item_link(dat_adults, item_info_adults)
  present_link <- link_parts$present
  
  all_item_cols <- present_link$data_col
  
  long <- dat_adults %>%
    mutate(.row_id = row_number()) %>%
    select(any_of(if (!is.null(id_col)) rlang::as_name(id_sym) else character()),
           all_of(all_item_cols),
           .row_id) %>%
    mutate(across(all_of(all_item_cols), as.character)) %>%
    pivot_longer(cols = all_of(all_item_cols),
                 names_to = "data_col", values_to = "value") %>%
    left_join(present_link %>% select(item, scale, data_col), by = "data_col") %>%
    mutate(value = suppressWarnings(as.numeric(na_if(value, ""))))
  
  counts <- long %>%
    group_by(.row_id, scale) %>%
    summarise(n_non_missing = sum(!is.na(value)), .groups = "drop")
  
  scores <- long %>%
    group_by(.row_id, scale) %>%
    summarise(
      score = ifelse(sum(!is.na(value)) >= min_items,
                     suppressWarnings(fun(value, na.rm = na.rm)),
                     NA_real_),
      .groups = "drop"
    ) %>%
    left_join(counts, by = c(".row_id","scale")) %>%
    mutate(valid = n_non_missing >= min_items) %>%
    select(-n_non_missing) %>%
    pivot_wider(names_from = scale, values_from = score)
  
  if (!is.null(id_col)) {
    out <- dat_adults %>%
      transmute(!!id_sym) %>%
      mutate(.row_id = row_number()) %>%
      left_join(scores, by = ".row_id") %>%
      select(-.row_id)
  } else {
    out <- scores
  }
  out
}