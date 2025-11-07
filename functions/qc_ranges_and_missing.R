qc_ranges_and_missing <- function(dat,
                                  item_info_adults,
                                  scoring_info,
                                  id_col = "vpid",
                                  all_or_nothing_scales = c("FHSfamilytree"),
                                  exclude_scales_from_qc = c("id")) {
  
  # --- ID handling (robust) ---
  have_id <- !is.null(id_col) && id_col %in% names(dat)
  dat_with_rowid <- dat %>% dplyr::mutate(.row_id = dplyr::row_number())
  
  id_map <- tibble::tibble(.row_id = seq_len(nrow(dat)))
  if (have_id) id_map[[id_col]] <- dat[[id_col]]
  
  # --- Mapping & ranges ---
  scoring_norm <- .normalize_scoring_info(scoring_info)
  link_parts   <- .build_item_link(dat, item_info_adults)
  
  # keep present items but drop excluded scales (e.g., "id")
  present_link <- link_parts$present %>%
    dplyr::filter(!(scale %in% exclude_scales_from_qc))
  
  missing_link <- link_parts$missing %>%
    dplyr::filter(!(scale %in% exclude_scales_from_qc))
  
  all_item_cols <- present_link$data_col
  
  if (length(all_item_cols) == 0) {
    return(list(
      violations              = dplyr::tibble(),
      per_participant         = dplyr::tibble(),
      per_scale_summary       = dplyr::tibble(),
      full_missing_ids        = dplyr::tibble(),
      partial_missing_ids     = dplyr::tibble(),
      items_missing_in_dat    = missing_link$item
    ))
  }
  
  # --- Long form; coerce to character before pivot (prevents type-mix issues) ---
  long <- dat_with_rowid %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_item_cols), as.character)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(all_item_cols),
                        names_to = "data_col", values_to = "value") %>%
    dplyr::left_join(present_link %>% dplyr::select(item, scale, data_col), by = "data_col") %>%
    dplyr::left_join(scoring_norm, by = "scale") %>%
    dplyr::mutate(
      value     = dplyr::na_if(value, ""),
      value_num = suppressWarnings(as.numeric(value))
    )
  
  # --- Range violations ---
  violations <- long %>%
    dplyr::filter(!is.na(value_num), !is.na(min), !is.na(max)) %>%
    dplyr::mutate(
      below_min = value_num < min,
      above_max = value_num > max,
      ok        = !(below_min | above_max),
      direction = dplyr::case_when(
        below_min ~ "below_min",
        above_max ~ "above_max",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!ok) %>%
    dplyr::transmute(.row_id, scale, item, value = value_num, min, max, direction) %>%
    dplyr::left_join(id_map, by = ".row_id")
  
  # --- Missingness per participant × scale ---
  per_ps <- long %>%
    dplyr::group_by(.row_id, scale) %>%
    dplyr::summarise(
      n_items    = dplyr::n_distinct(item),
      n_answered = sum(!is.na(value_num)),
      n_missing  = n_items - n_answered,
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(scale)) %>%
    dplyr::mutate(
      full_missing    = n_answered == 0,
      partial_missing = n_answered > 0 & n_missing > 0,
      missing_rate    = dplyr::if_else(n_items > 0, n_missing / n_items, NA_real_)
    ) %>%
    # all-or-nothing rule (e.g., FHSfamilytree)
    dplyr::mutate(
      partial_missing = dplyr::if_else(scale %in% all_or_nothing_scales, FALSE, partial_missing)
    ) %>%
    dplyr::left_join(id_map, by = ".row_id")
  
  # --- Participant lists (with vpid if available) ---
  if (have_id) {
    full_missing_ids <- per_ps %>%
      dplyr::filter(full_missing) %>%
      dplyr::select(scale, .row_id, !!rlang::sym(id_col)) %>%
      dplyr::arrange(scale, .row_id)
    
    partial_missing_ids <- per_ps %>%
      dplyr::filter(partial_missing) %>%
      dplyr::transmute(
        scale, .row_id, !!rlang::sym(id_col),
        n_items, n_missing, missing_rate
      ) %>%
      dplyr::arrange(scale, dplyr::desc(missing_rate), .row_id)
  } else {
    full_missing_ids <- per_ps %>%
      dplyr::filter(full_missing) %>%
      dplyr::select(scale, .row_id) %>%
      dplyr::arrange(scale, .row_id)
    
    partial_missing_ids <- per_ps %>%
      dplyr::filter(partial_missing) %>%
      dplyr::transmute(
        scale, .row_id,
        n_items, n_missing, missing_rate
      ) %>%
      dplyr::arrange(scale, dplyr::desc(missing_rate), .row_id)
  }
  
  # --- Summary per scale (averages only over partial-missing participants) ---
  per_scale_summary <- per_ps %>%
    dplyr::group_by(scale) %>%
    dplyr::summarise(
      n_items             = max(n_items, na.rm = TRUE),
      n_obs               = dplyr::n_distinct(.row_id),
      n_full_missing      = sum(full_missing, na.rm = TRUE),
      n_partial_missing   = sum(partial_missing, na.rm = TRUE),
      pct_full_missing    = n_full_missing / n_obs,
      pct_partial_missing = n_partial_missing / n_obs,
      avg_items_missing_partial =
        {x <- ifelse(partial_missing, n_missing, NA_real_); if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)},
      median_items_missing_partial =
        {x <- ifelse(partial_missing, n_missing, NA_real_); if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)},
      avg_missing_rate_partial =
        {x <- ifelse(partial_missing, missing_rate, NA_real_); if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)},
      .groups = "drop"
    ) %>%
    # For all-or-nothing scales, partial metrics are meaningless → set to NA
    dplyr::mutate(
      dplyr::across(
        c(avg_items_missing_partial, median_items_missing_partial, avg_missing_rate_partial),
        ~ dplyr::if_else(scale %in% all_or_nothing_scales, NA_real_, .x)
      )
    ) %>%
    dplyr::arrange(scale)
  
  list(
    violations              = violations,           # with vpid if present
    per_participant         = per_ps,               # has .row_id and vpid (if present)
    per_scale_summary       = per_scale_summary,
    full_missing_ids        = full_missing_ids,     # vpids who skipped entire scale
    partial_missing_ids     = partial_missing_ids,  # vpids with partial missing (+ how much)
    items_missing_in_dat    = missing_link$item
  )
}
