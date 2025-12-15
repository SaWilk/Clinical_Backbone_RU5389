# --- plot_density_by_project.R -----------------------------------------------
plot_density_by_project <- function(df, var, proj_col, out_png, title, xlab,
                                    palette, group_filter = NULL,
                                    adjust = NULL, mode_text = NA, xlim = NULL,
                                    vlines = NULL) {
  
  # ---- helper: adaptive density smoothing (self-contained) ----
  .adjust_from_nvals_local <- function(n_vals) {
    # <=10 distinct values -> 0.50; >=50 -> 0.75; linear between
    if (!is.finite(n_vals) || n_vals <= 0) return(0.75)
    if (n_vals <= 10) return(0.50)
    if (n_vals >= 50) return(0.75)
    0.50 + (n_vals - 10) * (0.25 / 40)
  }
  
  # optional group filter
  if (!is.null(group_filter)) {
    if (!("group" %in% names(df)) || all(is.na(df$group))) {
      warning(sprintf("plot_density_by_project: 'group' missing/empty; skipping group='%s'.", group_filter), call. = FALSE)
      return(invisible(NULL))
    }
    df <- df[df$group %in% group_filter, , drop = FALSE]
    if (!nrow(df)) {
      warning(sprintf("plot_density_by_project: no rows after filtering group='%s'.", group_filter), call. = FALSE)
      return(invisible(NULL))
    }
  }
  
  if (!(proj_col %in% names(df))) { warning("plot_density_by_project: project column not found; skipping.", call. = FALSE); return(invisible(NULL)) }
  if (!(var %in% names(df)))      { warning(sprintf("plot_density_by_project: var '%s' not present; skipping.", var), call. = FALSE); return(invisible(NULL)) }
  
  xnum <- suppressWarnings(as.numeric(df[[var]]))
  d <- df[is.finite(xnum) & !is.na(df[[proj_col]]), , drop = FALSE]
  if (!nrow(d)) { warning("plot_density_by_project: no complete rows.", call. = FALSE); return(invisible(NULL)) }
  d[[var]] <- xnum[is.finite(xnum) & !is.na(df[[proj_col]])]
  
  # palette-key normalization
  to_palette_key <- function(x) {
    sx <- trimws(as.character(x)); sx <- gsub("\\s+", " ", sx); lx <- tolower(sx)
    base <- ifelse(grepl("^p", lx), lx, paste0("p", lx))
    base <- sub("^p8$", "p8 adults", base)
    base
  }
  d$.__pal_key <- to_palette_key(d[[proj_col]])
  
  # drop degenerate groups
  ok_tbl <- d |>
    dplyr::group_by(.__pal_key) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_unique = dplyr::n_distinct(.data[[var]]),
      var_x = stats::var(.data[[var]], na.rm = TRUE),
      var_ok = is.finite(var_x) && !is.na(var_x) && var_x > 0,
      .groups = "drop"
    ) |>
    dplyr::filter(n >= 2, n_unique >= 2, var_ok)
  
  d <- d[d$.__pal_key %in% ok_tbl$.__pal_key, , drop = FALSE]
  if (!nrow(d)) { warning("plot_density_by_project: all projects degenerate; skipping.", call. = FALSE); return(invisible(NULL)) }
  
  # legend with Ns
  legend_labels <- setNames(
    paste0(ok_tbl$.__pal_key, " (N=", ok_tbl$n, ")"),
    ok_tbl$.__pal_key
  )
  
  # colors for every present level
  levels_present <- unique(d$.__pal_key)
  values <- setNames(rep("#999999", length(levels_present)), levels_present)
  known <- intersect(levels_present, names(palette))
  if (length(known)) values[known] <- palette[known]
  
  # subtitle + caption
  subtitle <- if (!is.na(mode_text)) paste0("Scoring: ", mode_text, " — area = 100% per project") else "Area = 100% per project"
  
  x_all <- d[[var]]
  rng_emp <- range(x_all, na.rm = TRUE)
  m_emp   <- mean(x_all, na.rm = TRUE)
  n_emp   <- sum(is.finite(x_all))
  cap <- paste0(
    "Possible min/max: ",
    if (!is.null(xlim) && all(is.finite(xlim))) paste0("[", signif(xlim[1],4), ", ", signif(xlim[2],4), "]") else "unknown",
    " | Empirical range: [", signif(rng_emp[1],4), ", ", signif(rng_emp[2],4), "]",
    " | Mean: ", signif(m_emp,4),
    " | N total: ", n_emp
  )
  
  # adaptive smoothing if adjust is NULL (no external dependency anymore)
  if (is.null(adjust)) {
    n_vals <- length(unique(x_all[is.finite(x_all)]))
    adjust <- .adjust_from_nvals_local(n_vals)
  }
  
  p <- ggplot2::ggplot(
    d, ggplot2::aes(
      x = .data[[var]],
      color = .data$.__pal_key,
      fill  = .data$.__pal_key
    )
  ) +
    ggplot2::geom_density(
      ggplot2::aes(y = after_stat(density * 100), group = .data$.__pal_key),
      alpha = 0.20, adjust = adjust, na.rm = TRUE
    ) +
    { if (!is.null(vlines)) ggplot2::geom_vline(xintercept = vlines, linetype = "dotted") } +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab,
                  y = "Scaled % density (area = 100)", caption = cap) +
    { if (!is.null(xlim) && all(is.finite(xlim))) ggplot2::coord_cartesian(xlim = xlim) } +
    ggplot2::scale_color_manual(values = values,
                                limits = levels_present,
                                labels = legend_labels[levels_present],
                                drop = FALSE, na.translate = FALSE) +
    ggplot2::scale_fill_manual(values  = values,
                               limits = levels_present,
                               labels = legend_labels[levels_present],
                               drop = FALSE, na.translate = FALSE) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())
  
  ggplot2::ggsave(out_png, p, width = 9, height = 5.8, dpi = 150, limitsize = FALSE)
  invisible(TRUE)
}
