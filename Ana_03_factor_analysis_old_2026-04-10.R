#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Flexible EFA driver (items OR subscales) + split-half stability via Tucker congruence
# - Single k or grid k=2..6
# - Exports: scree PNG + loadings heatmap + factorcor heatmap + observed-cor heatmap
#            + Tucker congruence violin (square, grey fill) + XLSX per k
# Output: ROOT/out/internal_data_analysis/factor_analysis/<DATE>/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# ---- Packages ---------------------------------------------------------------
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "dplyr","tibble","stringr","fs","psych","lavaan","writexl","tidyr",
  "readxl","ggplot2","rstudioapi"
))

# ---- Paths ------------------------------------------------------------------
script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

ROOT <- script_dir()
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
DIR_OUT  <- fs::path(ROOT, "out", "internal_data_analysis", "factor_analysis", DATE_TAG)
fs::dir_create(DIR_OUT)

# ---- CONFIG -----------------------------------------------------------------
CFG <- list(
  sample = "complete",
  
  # Keep as descriptive metadata for output names / titles / bookkeeping.
  # It is no longer used to resolve the actual input file automatically.
  input_set = "hitop",  # descriptive only
  analysis_input_dir = fs::path(ROOT, "03_analysis_input"),
  
  # NEW: explicit input file + sheet
  input_file  = fs::path(ROOT, "03_analysis_input", "adults_adolescents_HiTOP_items_lt030.xlsx"),
  input_sheet = "combined",
  
  level = "items",  # "items" or "subscales"
  corr  = "pearson",    # "pearson" | "polychoric" | "tetrachoric"
  
  # Factor choice (used only if efa_mode == "single")
  n_factors_mode   = "scree_prompt",  # "manual" | "parallel" | "scree_prompt"
  n_factors_manual = 6,
  
  estimator = "WLSMV",    # "ML" | "MLR" | "WLSMV"
  rotation  = "geomin",
  pa_iter   = 200,
  min_levels_keep = 3,
  
  # Output naming tag
  tag = "",
  
  # NEW: suffix indicating whether item filtering by internal consistency threshold was used
  # Examples:
  #   ""       -> no extra suffix
  #   "full"   -> output names include "_full"
  #   "lt030"  -> output names include "_lt030"
  ic_filter_suffix = "lt030",
  
  # --- Full-pipeline factor solution mode ---
  # "single" = choose one k via n_factors_mode
  # "grid"   = run k in efa_k_min..efa_k_max (each k gets full pipeline + its own XLSX)
  efa_mode = "grid",
  efa_k_min = 2,
  efa_k_max = 6,
  
  # --- Repeated stratified split-half stability ---
  do_split_half     = TRUE,
  split_reps        = 10, # takes between 10 and 40 sec per rep, depending on number of factors
  split_seed_base   = 1000,
  split_frac_train  = 0.50,
  age_bins          = 6,
  
  # Stratification metadata file
  strat_file = fs::path(ROOT, "03_analysis_input", "adults_adolescents_stratification_info.xlsx"),
  id_col_in_strat = "vpid",
  
  # KMO / Bartlett
  do_kmo      = TRUE,
  do_bartlett = FALSE,
  
  # Optional factor scores
  compute_factor_scores = FALSE
)

CFG$split_reps <- as.integer(CFG$split_reps)
if (is.na(CFG$split_reps) || CFG$split_reps < 1) stop("CFG$split_reps must be a positive integer.")

# ---- Helpers ----------------------------------------------------------------
detect_id_col <- function(nms, prefer = NULL) {
  nms0 <- tolower(nms)
  if (!is.null(prefer) && tolower(prefer) %in% nms0) return(nms[match(tolower(prefer), nms0)])
  candidates <- c("vpid","vp_id","vp","participant_id","participantid","id")
  hit <- candidates[candidates %in% nms0]
  if (!length(hit)) return(NA_character_)
  nms[match(hit[1], nms0)]
}

get_input_correlation <- function(X_ord, X_num, cfg) {
  corr <- tolower(cfg$corr)
  
  if (corr == "pearson") {
    return(stats::cor(X_num, use = "pairwise.complete.obs", method = "pearson"))
  }
  
  if (corr == "polychoric") {
    pc <- psych::polychoric(
      X_num,
      global   = FALSE,
      correct  = 0,
      smooth   = TRUE,
      na.rm    = TRUE,
      delete   = TRUE,
      progress = FALSE
    )
    return(pc$rho)
  }
  
  if (corr == "tetrachoric") {
    tc <- psych::tetrachoric(
      X_num,
      correct = 0.1,
      smooth  = TRUE,
      na.rm   = TRUE,
      delete  = TRUE
    )
    return(tc$rho)
  }
  
  if (corr == "spearman") {
    return(stats::cor(X_num, use = "pairwise.complete.obs", method = "spearman"))
  }
  
  stop("Unknown corr: ", cfg$corr, " (use pearson|polychoric|tetrachoric)")
}

read_input_xlsx <- function(path_xlsx, sheet = NULL) {
  stopifnot(fs::file_exists(path_xlsx))
  
  if (is.null(sheet) || !nzchar(sheet)) {
    df <- readxl::read_xlsx(path_xlsx)
  } else {
    sheets_avail <- readxl::excel_sheets(path_xlsx)
    if (!sheet %in% sheets_avail) {
      stop(
        "Requested sheet not found in input XLSX: ", sheet,
        "\nAvailable sheets: ", paste(sheets_avail, collapse = ", "),
        call. = FALSE
      )
    }
    df <- readxl::read_xlsx(path_xlsx, sheet = sheet)
  }
  
  as.data.frame(df, check.names = FALSE)
}

resolve_input_file <- function(cfg) {
  path_xlsx <- cfg$input_file
  if (is.null(path_xlsx) || !nzchar(path_xlsx)) {
    stop("CFG$input_file must be a non-empty path to an .xlsx file.", call. = FALSE)
  }
  if (!fs::file_exists(path_xlsx)) {
    stop("Input XLSX not found: ", path_xlsx, call. = FALSE)
  }
  list(
    data_xlsx  = path_xlsx,
    data_sheet = cfg$input_sheet
  )
}

clean_suffix <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return("")
  x <- trimws(as.character(x))
  if (!startsWith(x, "_")) x <- paste0("_", x)
  x
}

build_name_tag <- function(cfg) {
  paste0(
    clean_suffix(cfg$tag),
    clean_suffix(cfg$ic_filter_suffix)
  )
}

as_ordered_factor <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  vals <- sort(unique(x[!is.na(x)]))
  factor(x, levels = vals, ordered = TRUE)
}
as_numeric <- function(x) suppressWarnings(as.numeric(x))

drop_low_variance_levels <- function(df, min_levels = 3) {
  n_levels <- sapply(df, function(v) length(unique(v[!is.na(v)])))
  keep <- names(n_levels)[n_levels >= min_levels]
  drop <- names(n_levels)[n_levels <  min_levels]
  list(keep = keep, drop = drop, n_levels = n_levels)
}

psych_cor_arg <- function(corr) {
  corr <- tolower(corr)
  if (corr == "pearson") return("cor")
  if (corr == "polychoric") return("poly")
  if (corr == "tetrachoric") return("tet")
  if (corr == "spearman") return("spearman")
  stop("Unknown corr: ", corr, " (use pearson|polychoric|tetrachoric)")
}

assert_binary_for_tetra <- function(df) {
  levs <- sapply(df, function(v) length(unique(v[!is.na(v)])))
  if (any(levs > 2)) {
    bad <- names(levs)[levs > 2]
    stop(
      "Tetrachoric selected, but some indicators have >2 categories. Examples: ",
      paste(head(bad, 10), collapse = ", "),
      if (length(bad) > 10) " ..." else "",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

run_parallel_analysis <- function(df, cfg) {
  set.seed(1)
  cor_arg <- psych_cor_arg(cfg$corr)
  pa <- psych::fa.parallel(df, fa = "fa", cor = cor_arg, n.iter = cfg$pa_iter, plot = FALSE)
  k <- seq_along(pa$fa.values)
  tibble::tibble(k = k, eigen_observed = pa$fa.values, eigen_sim_mean = pa$fa.sim)
}

write_and_show_scree <- function(eigen_df, out_png_base, title, n_suggest = NULL) {
  eig <- eigen_df$eigen_observed
  gn_df <- NULL; gn_suggest <- NULL; gn_scaled <- NULL; scale_factor <- NULL
  
  if (length(eig) >= 2) {
    drops <- eig[-length(eig)] - eig[-1]
    gn_index <- drops / eig[1]
    gn_df <- data.frame(k = seq_along(gn_index), gn = gn_index)
    gn_suggest <- gn_df$k[which.max(gn_df$gn)]
    scale_factor <- max(eigen_df$eigen_observed, na.rm = TRUE)
    gn_scaled <- gn_df$gn * scale_factor
  }
  
  png(paste0(out_png_base, "_withPA.png"), width = 1100, height = 750)
  plot(eigen_df$k, eigen_df$eigen_observed, type="b", pch=16, lwd=2,
       xlab="Factor number", ylab="Eigenvalue", main=title, col="black")
  lines(eigen_df$k, eigen_df$eigen_sim_mean, type="b", pch=1, lwd=2, lty=2, col="grey60")
  abline(h=1, lty=3, lwd=2)
  if (!is.null(n_suggest)) abline(v=n_suggest, lty=4, lwd=2)
  
  if (!is.null(gn_df)) {
    lines(gn_df$k, gn_scaled, type="b", pch=17, lwd=2, col="blue")
    abline(v=gn_suggest, lty=5, lwd=2, col="blue")
    axis(4, at = pretty(gn_scaled), labels = round(pretty(gn_scaled) / scale_factor, 3))
    mtext("Gorsuch–Nelson index", side=4, line=3)
  }
  
  legend("topright",
         legend=c("Observed eigenvalues",
                  "Parallel analysis (sim mean)",
                  "Kaiser (1)",
                  if (!is.null(n_suggest)) paste0("Parallel suggestion (k=", n_suggest, ")"),
                  if (!is.null(gn_df)) "Gorsuch–Nelson index",
                  if (!is.null(gn_df)) paste0("GN suggestion (k=", gn_suggest, ")")),
         col=c("black","grey60","black","black","blue","blue"),
         lty=c(1,2,3,4,1,5),
         lwd=2,
         pch=c(16,1,NA,NA,17,NA),
         bty="n", cex=1)
  dev.off()
}

choose_n_factors <- function(cfg, pa_tbl) {
  mode <- tolower(cfg$n_factors_mode)
  if (mode == "manual") return(as.integer(cfg$n_factors_manual))
  
  n_suggest <- sum(pa_tbl$eigen_observed > pa_tbl$eigen_sim_mean, na.rm = TRUE)
  n_suggest <- max(1L, as.integer(n_suggest))
  
  if (mode == "parallel") return(n_suggest)
  
  if (mode == "scree_prompt") {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      ans_txt <- rstudioapi::showPrompt(
        title = "EFA: number of factors",
        message = paste0("Enter number of factors (default ", n_suggest, "):"),
        default = as.character(n_suggest)
      )
      ans_txt <- trimws(ans_txt)
    } else {
      ans_txt <- trimws(readline(paste0("Enter number of factors [default ", n_suggest, "]: ")))
      if (!nzchar(ans_txt)) ans_txt <- as.character(n_suggest)
    }
    ans <- suppressWarnings(as.integer(ans_txt))
    if (is.na(ans) || ans < 1) return(n_suggest)
    return(ans)
  }
  
  stop("Unknown n_factors_mode: ", cfg$n_factors_mode, " (manual|parallel|scree_prompt)")
}

fit_efa <- function(df, n_factors, cfg) {
  vars <- colnames(df)
  model <- paste(
    sapply(seq_len(n_factors), function(i) {
      paste0('efa("efa")*F', i, ' =~ ', paste(vars, collapse = " + "))
    }),
    collapse = "\n"
  )
  
  est <- toupper(cfg$estimator)
  
  if (est == "WLSMV") {
    lavaan::cfa(model, data=df, ordered=vars, estimator="WLSMV",
                rotation=cfg$rotation, std.lv=TRUE, missing="pairwise",
                parameterization="theta")
  } else if (est %in% c("ML", "MLR")) {
    lavaan::cfa(model, data=df, estimator=est,
                rotation=cfg$rotation, std.lv=TRUE, missing="fiml")
  } else {
    stop("Unknown estimator: ", cfg$estimator, " (ML|MLR|WLSMV)")
  }
}

is_bad_lavaan_fit <- function(fit) {
  if (inherits(fit, "error")) return(TRUE)
  if (!isTRUE(lavaan::inspect(fit, "converged"))) return(TRUE)
  pe <- suppressWarnings(lavaan::parameterEstimates(fit, standardized = FALSE))
  bad_ov <- pe |>
    dplyr::filter(op == "~~", lhs == rhs, !grepl("^F\\d+$", lhs)) |>
    dplyr::summarise(any_neg = any(est < 0, na.rm = TRUE)) |>
    dplyr::pull(any_neg)
  isTRUE(bad_ov)
}

extract_solution_tables <- function(fit) {
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  
  std_col <- dplyr::coalesce(
    if ("est.std" %in% names(pe)) pe$est.std else NA_real_,
    if ("std.all" %in% names(pe)) pe$std.all else NA_real_,
    if ("std.lv"  %in% names(pe)) pe$std.lv  else NA_real_,
    if ("std.nox" %in% names(pe)) pe$std.nox else NA_real_
  )
  pe2 <- dplyr::mutate(pe, .std = std_col)
  
  loadings <- pe2 %>%
    dplyr::filter(op == "=~") %>%
    dplyr::transmute(factor = lhs, indicator = rhs, loading = .std) %>%
    tidyr::pivot_wider(names_from = factor, values_from = loading) %>%
    dplyr::arrange(indicator)
  
  factor_cor <- pe2 %>%
    dplyr::filter(op == "~~", lhs != rhs, grepl("^F\\d+$", lhs), grepl("^F\\d+$", rhs)) %>%
    dplyr::transmute(factor1 = lhs, factor2 = rhs, r = .std)
  
  uniq <- pe2 %>%
    dplyr::filter(op == "~~", lhs == rhs, !grepl("^F\\d+$", lhs)) %>%
    dplyr::transmute(indicator = lhs, uniqueness = .std)
  
  measures <- c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr",
                "chisq","pvalue","cfi","tli","rmsea")
  fm <- suppressWarnings(lavaan::fitMeasures(fit, measures))
  fit_tbl <- tibble::tibble(measure = names(fm), value = as.numeric(fm))
  
  list(loadings = loadings, factor_cor = factor_cor, uniqueness = uniq, fit = fit_tbl)
}

make_loading_long <- function(loadings_wide) {
  stopifnot("indicator" %in% names(loadings_wide))
  loadings_wide |>
    tidyr::pivot_longer(cols = -indicator, names_to = "factor", values_to = "loading")
}

plot_loadings_heatmap <- function(loadings_wide, out_png, title = "EFA loadings (standardized)") {
  df_long <- make_loading_long(loadings_wide)
  
  ord <- df_long |>
    dplyr::group_by(indicator) |>
    dplyr::summarise(mx = max(abs(loading), na.rm = TRUE), .groups="drop") |>
    dplyr::arrange(dplyr::desc(mx)) |>
    dplyr::pull(indicator)
  
  df_long$indicator <- factor(df_long$indicator, levels = rev(ord))
  
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x=factor, y=indicator, fill=loading)) +
    ggplot2::geom_tile(color="white", linewidth=0.2) +
    ggplot2::scale_fill_gradient2(midpoint=0) +
    ggplot2::labs(title=title, x=NULL, y=NULL, fill="Loading") +
    ggplot2::theme_minimal(base_size=12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill="white", color=NA),
                   legend.key = ggplot2::element_rect(fill="white", color=NA))
  ggplot2::ggsave(out_png, plot=p, width=9, height=max(4, 0.25*length(ord)), dpi=150, bg="white")
}

plot_matrix_heatmap <- function(M, out_png, title="Heatmap", value_name="value") {
  M <- as.matrix(M)
  rn <- rownames(M); cn <- colnames(M)
  if (is.null(rn)) rn <- seq_len(nrow(M))
  if (is.null(cn)) cn <- seq_len(ncol(M))
  
  df <- as.data.frame(as.table(M), stringsAsFactors = FALSE)
  names(df) <- c("row","col", value_name)
  df$row <- factor(df$row, levels = rev(rn))
  df$col <- factor(df$col, levels = cn)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x=col, y=row, fill=.data[[value_name]])) +
    ggplot2::geom_tile(color="white", linewidth=0.1) +
    ggplot2::scale_fill_gradient2(midpoint=0) +
    ggplot2::labs(title=title, x=NULL, y=NULL, fill=value_name) +
    ggplot2::theme_minimal(base_size=11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1, vjust=1),
                   panel.grid = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill="white", color=NA),
                   legend.key = ggplot2::element_rect(fill="white", color=NA))
  ggplot2::ggsave(out_png, plot=p, width=9, height=8, dpi=150, bg="white")
}

run_kmo <- function(R) {
  k <- psych::KMO(R)
  list(
    kmo_overall = tibble::tibble(metric = "KMO_overall", value = unname(k$MSA)),
    kmo_per_var = tibble::tibble(variable = names(k$MSAi), kmo = unname(k$MSAi))
  )
}

run_bartlett <- function(R, n) {
  out <- psych::cortest.bartlett(R, n = n)
  tibble::tibble(
    chisq = unname(out$chisq),
    df    = unname(out$df),
    p     = unname(out$p.value)
  )
}

read_strat_info <- function(path_xlsx, id_col = NULL) {
  stopifnot(fs::file_exists(path_xlsx))
  df <- readxl::read_xlsx(path_xlsx)
  id_col2 <- detect_id_col(names(df), prefer = id_col)
  if (is.na(id_col2)) {
    stop("Could not detect ID column in stratification file. Columns are: ",
         paste(names(df), collapse=", "), call. = FALSE)
  }
  message("Using ID column in stratification XLSX: ", id_col2)
  
  df %>%
    dplyr::mutate(
      project = as.character(project),
      gender  = as.character(gender),
      group   = dplyr::if_else(is.na(group) | trimws(as.character(group))=="", "HC", as.character(group)),
      age     = suppressWarnings(as.numeric(age)),
      !!id_col2 := as.character(.data[[id_col2]])
    )
}

make_age_bins <- function(age, n_bins = 6) {
  qs <- unique(as.numeric(stats::quantile(age, probs = seq(0,1,length.out=n_bins+1), na.rm=TRUE)))
  if (length(qs) < 3) return(factor(rep("all", length(age))))
  cut(age, breaks=qs, include.lowest=TRUE, ordered_result=TRUE)
}

stratified_split_ids <- function(meta, frac_train = 0.5, seed = 1, age_bins = 6) {
  set.seed(seed)
  meta2 <- meta %>%
    dplyr::mutate(
      age_bin = make_age_bins(age, n_bins = age_bins),
      strata = interaction(age_bin, gender, project, group, drop=TRUE, sep=" | ")
    )
  train_ids <- meta2 %>%
    dplyr::group_by(strata) %>%
    dplyr::slice_sample(prop = frac_train) %>%
    dplyr::ungroup() %>%
    dplyr::pull(participant_id)
  
  list(train_ids = unique(train_ids),
       test_ids  = setdiff(meta2$participant_id, unique(train_ids)),
       meta = meta2)
}

get_std_loading_matrix <- function(fit, vars) {
  ss <- lavaan::standardizedSolution(fit)
  std_name <- dplyr::coalesce(
    if ("est.std" %in% names(ss)) "est.std" else NA_character_,
    if ("std.all" %in% names(ss)) "std.all" else NA_character_,
    if ("std.lv"  %in% names(ss)) "std.lv"  else NA_character_,
    if ("std.nox" %in% names(ss)) "std.nox" else NA_character_
  )
  if (is.na(std_name)) stop("No standardized column found in standardizedSolution().")
  
  L <- ss %>%
    dplyr::filter(op == "=~") %>%
    dplyr::transmute(factor = lhs, indicator = rhs, loading = .data[[std_name]]) %>%
    tidyr::pivot_wider(names_from = factor, values_from = loading)
  
  Lm <- as.matrix(L[, -1, drop=FALSE])
  rownames(Lm) <- L$indicator
  Lm <- Lm[vars, , drop=FALSE]
  
  if (any(is.na(Lm))) {
    bad_rows <- rownames(Lm)[apply(Lm, 1, function(r) any(is.na(r)))]
    stop("NA loadings in standardized solution for indicators (first 10): ",
         paste(head(bad_rows, 10), collapse=", "), call. = FALSE)
  }
  Lm
}

tucker_congruence_procrustes <- function(L_train, L_test) {
  pr <- psych::Procrustes(L_train, L_test)
  L_test_rot <- pr$loadings
  cong <- psych::factor.congruence(L_train, L_test_rot)
  fac_cong <- diag(cong)
  tibble::tibble(factor = paste0("F", seq_along(fac_cong)),
                 congruence = as.numeric(fac_cong))
}

summarize_congruence <- function(cong_long) {
  cong_long %>%
    dplyr::group_by(factor) %>%
    dplyr::summarise(
      n = sum(!is.na(congruence)),
      median = stats::median(congruence, na.rm = TRUE),
      p05 = stats::quantile(congruence, 0.05, na.rm = TRUE),
      p25 = stats::quantile(congruence, 0.25, na.rm = TRUE),
      p75 = stats::quantile(congruence, 0.75, na.rm = TRUE),
      prop_gt_085 = mean(congruence > 0.85, na.rm = TRUE),
      prop_gt_095 = mean(congruence > 0.95, na.rm = TRUE),
      .groups="drop"
    )
}

plot_congruence_violin_png <- function(cong_long, out_png,
                                       title_base = "Tucker congruence across repeated stratified splits",
                                       fill_grey = "grey80") {
  stopifnot(all(c("rep","factor","congruence") %in% names(cong_long)))
  
  fac_levels <- unique(cong_long$factor)
  if (all(grepl("^F\\d+$", fac_levels))) {
    fac_levels <- fac_levels[order(as.integer(sub("^F","",fac_levels)))]
  } else fac_levels <- sort(fac_levels)
  
  df <- cong_long %>% dplyr::mutate(factor = factor(factor, levels = fac_levels))
  
  reps_total <- length(unique(df$rep))
  reps_ok <- df %>%
    dplyr::group_by(rep) %>%
    dplyr::summarise(ok = any(is.finite(congruence)), .groups="drop") %>%
    dplyr::summarise(n_ok = sum(ok), .groups="drop") %>%
    dplyr::pull(n_ok)
  
  reps_failed <- reps_total - reps_ok
  title <- paste0(title_base, " (splits used: ", reps_ok, "/", reps_total,
                  if (reps_failed > 0) paste0(", failed: ", reps_failed) else "", ")")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x=factor, y=congruence)) +
    ggplot2::geom_violin(trim=FALSE, fill=fill_grey, color="grey20", linewidth=0.3) +
    ggplot2::geom_boxplot(width=0.18, outlier.shape=NA, fill="white", color="grey20", linewidth=0.3) +
    ggplot2::geom_hline(yintercept=0.85, linetype=2, linewidth=0.4) +
    ggplot2::geom_hline(yintercept=0.95, linetype=2, linewidth=0.4) +
    ggplot2::coord_cartesian(ylim=c(0,1)) +
    ggplot2::labs(title=title, x=NULL, y="Tucker congruence") +
    ggplot2::theme_minimal(base_size=12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size=11),
      plot.background = ggplot2::element_rect(fill="white", color=NA),
      panel.background = ggplot2::element_rect(fill="white", color=NA)
    )
  
  ggplot2::ggsave(out_png, plot=p, width=7, height=7, dpi=200, bg="white")
}

run_one_solution <- function(k, X_fit, R_obs, CFG, DIR_OUT) {
  message("\n--- Fitting EFA with k = ", k, " ---")
  fit <- fit_efa(X_fit, k, CFG)
  
  if (is_bad_lavaan_fit(fit)) {
    warning("k=", k, " produced non-converged/Heywood. Skipping exports for this k.")
    return(list(k = k, ok = FALSE, fit = fit, sol = NULL))
  }
  
  sol <- extract_solution_tables(fit)
  name_tag <- build_name_tag(CFG)
  
  plot_tag <- paste0(
    CFG$sample,
    name_tag,
    "_set-", CFG$input_set,
    "_lvl-", CFG$level,
    "_cor-", CFG$corr,
    "_est-", tolower(CFG$estimator),
    "_nf", k,
    "_rot-", CFG$rotation
  )
  
  png_load <- fs::path(DIR_OUT, paste0(plot_tag, "_loadings_heatmap.png"))
  plot_loadings_heatmap(
    sol$loadings, png_load,
    title = paste0("Loadings heatmap (", CFG$input_set, " / ", CFG$level, ", nf=", k, ")")
  )
  message("Saved: ", png_load)
  
  if (!is.null(sol$factor_cor) && nrow(sol$factor_cor) > 0) {
    facs <- sort(unique(c(sol$factor_cor$factor1, sol$factor_cor$factor2)))
    Phi <- matrix(0, nrow = length(facs), ncol = length(facs), dimnames = list(facs, facs))
    diag(Phi) <- 1
    for (i in seq_len(nrow(sol$factor_cor))) {
      f1 <- sol$factor_cor$factor1[i]
      f2 <- sol$factor_cor$factor2[i]
      r  <- sol$factor_cor$r[i]
      Phi[f1, f2] <- r
      Phi[f2, f1] <- r
    }
    png_phi <- fs::path(DIR_OUT, paste0(plot_tag, "_factorcor_heatmap.png"))
    plot_matrix_heatmap(Phi, png_phi, title = "Factor correlations (standardized)", value_name = "r")
    message("Saved: ", png_phi)
  }
  
  png_r <- fs::path(DIR_OUT, paste0(plot_tag, "_observed_cor_heatmap.png"))
  plot_matrix_heatmap(R_obs, png_r, title = "Observed-variable correlations", value_name = "r")
  message("Saved: ", png_r)
  
  list(k = k, ok = TRUE, fit = fit, sol = sol, plot_tag = plot_tag)
}

# ---- Main -------------------------------------------------------------------

# Input
paths <- resolve_input_file(CFG)
message("Using input XLSX: ", paths$data_xlsx)
if (!is.null(paths$data_sheet) && nzchar(paths$data_sheet)) {
  message("Using input sheet: ", paths$data_sheet)
}

X_raw <- read_input_xlsx(paths$data_xlsx, sheet = paths$data_sheet)
id_col <- detect_id_col(names(X_raw), prefer = CFG$id_col_in_strat)
if (is.na(id_col)) stop("Could not detect an ID column in input XLSX.", call. = FALSE)
message("Using ID column in input XLSX: ", id_col)

ids <- as.character(X_raw[[id_col]])
X_dat <- X_raw[, setdiff(names(X_raw), id_col), drop=FALSE]

# Convert columns depending on corr/estimator
if (tolower(CFG$corr) %in% c("polychoric","tetrachoric") || toupper(CFG$estimator) == "WLSMV") {
  X0 <- as.data.frame(lapply(X_dat, as_ordered_factor), check.names = FALSE)
} else {
  X0 <- as.data.frame(lapply(X_dat, as_numeric), check.names = FALSE)
}

# Drop too-few-level indicators
var_check <- drop_low_variance_levels(X0, min_levels = CFG$min_levels_keep)
X_ord <- X0[, var_check$keep, drop = FALSE]
if (ncol(X_ord) < 2) stop("After dropping low-level variables, <2 indicators remain.", call. = FALSE)
if (tolower(CFG$corr) == "tetrachoric") assert_binary_for_tetra(X_ord)

# Numeric-coded copy for helper functions that need raw numeric input
X_num <- as.data.frame(lapply(X_ord, as.numeric), check.names = FALSE)

# Central correlation matrix used everywhere correlation-based
R_input <- get_input_correlation(X_ord, X_num, CFG)

# Parallel analysis + scree (once; same input for all k)
pa_tbl <- run_parallel_analysis(X_num, CFG)
n_suggest_pa <- sum(pa_tbl$eigen_observed > pa_tbl$eigen_sim_mean, na.rm = TRUE)
n_suggest_pa <- max(1L, as.integer(n_suggest_pa))

name_tag <- build_name_tag(CFG)

scree_png_base <- fs::path(
  DIR_OUT,
  paste0(CFG$sample,
         name_tag,
         "_set-", CFG$input_set,
         "_lvl-", CFG$level,
         "_", CFG$corr,
         "_scree")
)
write_and_show_scree(
  pa_tbl,
  scree_png_base,
  title = paste0("Scree plot (", CFG$input_set, " / ", CFG$level, " / ", CFG$corr, ")"),
  n_suggest = n_suggest_pa
)
message("Saved scree plot: ", scree_png_base, "_withPA.png")

# Scale once (same for all k)
if (toupper(CFG$estimator) == "WLSMV") {
  X_fit <- X_ord
} else {
  X_fit <- as.data.frame(scale(X_num), check.names = FALSE)
}

# Read/align stratification meta once (if needed)
meta2 <- NULL
id_meta_col <- NA_character_
if (isTRUE(CFG$do_split_half)) {
  meta <- read_strat_info(CFG$strat_file, id_col = CFG$id_col_in_strat)
  id_meta_col <- detect_id_col(names(meta), prefer = CFG$id_col_in_strat)
  
  meta2 <- meta %>% dplyr::filter(.data[[id_meta_col]] %in% ids)
  idx <- match(ids, meta2[[id_meta_col]])
  if (any(is.na(idx))) stop("Split-half: could not match some IDs.", call. = FALSE)
  meta2 <- meta2[idx, , drop=FALSE]
  
  if (nrow(meta2) != nrow(X_ord)) stop("Split-half: nrow(meta) != nrow(X_ord) after aligning.", call. = FALSE)
  }

# Decide k list
efa_mode <- tolower(CFG$efa_mode)
if (efa_mode == "grid") {
  k_min <- as.integer(CFG$efa_k_min); k_max <- as.integer(CFG$efa_k_max)
  if (is.na(k_min) || is.na(k_max) || k_min < 1 || k_max < k_min) stop("Invalid efa_k_min/max.", call. = FALSE)
  k_list <- k_min:k_max
} else if (efa_mode == "single") {
  k_list <- choose_n_factors(CFG, pa_tbl)
} else {
  stop("CFG$efa_mode must be 'grid' or 'single'.", call. = FALSE)
}
message("\nRunning FULL PIPELINE for k = ", paste(k_list, collapse=", "))

# ---- Run FULL pipeline per k -------------------------------------------------
for (k in k_list) {
  
  message("\n==============================")
  message("Starting full pipeline for k = ", k)
  message("==============================\n")
  
  # Fit + heatmaps
  res <- run_one_solution(k, X_fit, R_input, CFG, DIR_OUT)
  if (!isTRUE(res$ok)) next
  fit <- res$fit
  sol <- res$sol
  
  # Extra summaries per k
  extra_summaries <- list()
  
  # KMO per k (same input, but you might still want it in each XLSX)
  if (isTRUE(CFG$do_kmo)) {
    kmo <- run_kmo(R_input)
    extra_summaries$kmo_overall <- kmo$kmo_overall
    extra_summaries$kmo_per_var <- kmo$kmo_per_var
  }
  if (isTRUE(CFG$do_bartlett)) {
    extra_summaries$bartlett <- run_bartlett(R_input, n = nrow(X_ord))
  }
  
  # ---- Split-half stability (per k) ----
  if (isTRUE(CFG$do_split_half)) {
    
    factor_levels <- paste0("F", seq_len(k))
    
    cong_long <- tibble::tibble(
      rep = integer(), factor = character(), congruence = numeric(),
      split_seed = integer(), n_train = integer(), n_test = integer()
    )
    pb <- utils::txtProgressBar(min = 0, max = CFG$split_reps, style = 3)
    
    for (r in seq_len(CFG$split_reps)) {
      utils::setTxtProgressBar(pb, r)
      this_seed <- CFG$split_seed_base + r
      
      sp <- stratified_split_ids(
        meta = meta2 %>% dplyr::rename(participant_id = !!id_meta_col),
        frac_train = CFG$split_frac_train,
        seed = this_seed,
        age_bins = CFG$age_bins
      )
      
      is_train <- meta2[[id_meta_col]] %in% sp$train_ids
      
      if (toupper(CFG$estimator) == "WLSMV") {
        X_train <- X_ord[is_train, , drop = FALSE]
        X_test  <- X_ord[!is_train, , drop = FALSE]
      } else {
        X_train <- as.data.frame(scale(X_num[is_train, , drop = FALSE]), check.names = FALSE)
        X_test  <- as.data.frame(scale(X_num[!is_train, , drop = FALSE]), check.names = FALSE)
      }
      
      fit_train <- tryCatch(fit_efa(X_train, k, CFG), error = function(e) e)
      fit_test  <- tryCatch(fit_efa(X_test,  k, CFG), error = function(e) e)
      
      if (is_bad_lavaan_fit(fit_train) || is_bad_lavaan_fit(fit_test)) {
        cong_long <- dplyr::bind_rows(
          cong_long,
          tibble::tibble(
            rep = r, factor = factor_levels, congruence = NA_real_,
            split_seed = this_seed, n_train = sum(is_train), n_test = sum(!is_train)
          )
        )
        next
      }
      
      vars <- intersect(colnames(X_train), colnames(X_test))
      L_train <- tryCatch(get_std_loading_matrix(fit_train, vars), error = function(e) e)
      L_test  <- tryCatch(get_std_loading_matrix(fit_test,  vars), error = function(e) e)
      
      if (inherits(L_train, "error") || inherits(L_test, "error")) {
        cong_long <- dplyr::bind_rows(
          cong_long,
          tibble::tibble(
            rep = r, factor = factor_levels, congruence = NA_real_,
            split_seed = this_seed, n_train = sum(is_train), n_test = sum(!is_train)
          )
        )
        next
      }
      
      cong_tbl <- tucker_congruence_procrustes(L_train, L_test) %>%
        dplyr::mutate(
          rep = r,
          split_seed = this_seed,
          n_train = sum(is_train),
          n_test = sum(!is_train)
        ) %>%
        dplyr::select(rep, factor, congruence, split_seed, n_train, n_test)
      
      cong_long <- dplyr::bind_rows(cong_long, cong_tbl)
    }
    
    try(utils::close(pb), silent = TRUE)
    
    extra_summaries$split_counts <- tibble::tibble(
      reps = CFG$split_reps,
      frac_train = CFG$split_frac_train,
      seed_base = CFG$split_seed_base,
      age_bins = CFG$age_bins
    )
    extra_summaries$tucker_congruence_long <- cong_long
    extra_summaries$tucker_congruence_summary <- summarize_congruence(cong_long)
    
    cong_violin_png <- fs::path(
      DIR_OUT,
      paste0(CFG$sample,
             name_tag,
             "_nf", k,
             "_set-", CFG$input_set,
             "_reps", CFG$split_reps,
             "_tucker_violin.png")
    )
    plot_congruence_violin_png(cong_long, cong_violin_png)
    message("\nSaved: ", cong_violin_png)
  }
  
  # Optional factor scores (per k)
  scores_tbl <- NULL
  if (isTRUE(CFG$compute_factor_scores)) {
    fscores <- as.data.frame(lavaan::lavPredict(fit))
    scores_tbl <- tibble::as_tibble(fscores)
  }
  
  # ---- Metadata tables (per k) ----
  settings_tbl <- tibble::tibble(
    setting = names(CFG),
    value   = vapply(CFG, function(x) paste(x, collapse = ", "), character(1))
  ) %>%
    dplyr::bind_rows(tibble::tibble(setting = "n_factors_used", value = as.character(k))) %>%
    dplyr::bind_rows(tibble::tibble(setting = "n_indicators_used", value = as.character(ncol(X_ord))))
  
  coverage_tbl <- tibble::tibble(
    variable = names(X_ord),
    pct_missing = sapply(X_ord, function(v) 100 * mean(is.na(v)))
  ) %>% dplyr::arrange(dplyr::desc(pct_missing))
  
  dropped_tbl <- tibble::tibble(
    dropped_variables = var_check$drop,
    reason = paste0("n_levels < ", CFG$min_levels_keep)
  )
  
  # ---- Write XLSX (per k) ----
  ts <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  out_xlsx <- fs::path(
    DIR_OUT,
    paste0("efa_", CFG$sample,
           name_tag,
           "_set-", CFG$input_set,
           "_lvl-", CFG$level,
           "_cor-", CFG$corr,
           "_est-", tolower(CFG$estimator),
           "_nf", k,
           "_rot-", CFG$rotation,
           "_", ts, ".xlsx")
  )
  
  sheets <- list(
    "settings"           = settings_tbl,
    "coverage"           = coverage_tbl,
    "dropped_low_levels" = dropped_tbl,
    "parallel_analysis"  = pa_tbl,
    "loadings"           = sol$loadings,
    "factor_cor"         = sol$factor_cor,
    "uniqueness"         = sol$uniqueness,
    "fit"                = sol$fit
  )
  
  if (length(extra_summaries)) sheets <- c(sheets, extra_summaries)
  if (!is.null(scores_tbl)) sheets[["factor_scores"]] <- scores_tbl
  
  writexl::write_xlsx(sheets, out_xlsx)
  message("Saved XLSX: ", out_xlsx)
}

message("\nDone.\nDIR_OUT:\n", DIR_OUT, "\n", sep="")
print(list.files(DIR_OUT, all.files = TRUE))