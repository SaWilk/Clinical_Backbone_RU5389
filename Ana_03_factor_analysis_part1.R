#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Flexible EFA driver (items OR subscales) with configurable correlations + estimator
# + Input switch: HiTOP vs COMPLETE (reads pre-exported analysis-input XLSX files)
#
# Exports: scree PNG + human-readable XLSX
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
  "jsonlite","dplyr","tibble","stringr","fs","psych","lavaan","writexl","tidyr",
  "readxl","ggplot2","GPArotation","rstudioapi"
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
  sample = "adults",
  
  # Choose which exported input set to use:
  # "hitop" or "complete"
  input_set = "hitop",
  
  # Where analysis-input files live (created by your export script)
  analysis_input_dir = fs::path(ROOT, "03_analysis_input"),
  
  # keys.json still needed for metadata tables (and optional fallbacks)
  keys_json = fs::path(ROOT, "02_cleaned", "keys", "adults_keys.json"),
  
  # Analysis level: "items" or "subscales"
  level = "subscales",
  
  # Correlation matrix: "pearson" | "polychoric" | "tetrachoric"
  corr = "pearson",
  
  # Factor choice: "manual" | "parallel" | "scree_prompt"
  n_factors_mode   = "scree_prompt",
  n_factors_manual = 6,
  
  # Estimator: "ML" | "WLSMV"
  estimator = "MLR",
  
  # Rotation (lavaan EFA rotation)
  rotation  = "geomin",
  
  # Parallel analysis stability
  pa_iter   = 200,
  
  # Drop indicators with too few observed categories (relevant for poly/tetra; also useful generally)
  min_levels_keep = 3,
  
  # Optional factor scores
  compute_factor_scores = FALSE,
  
  # Tag for output naming
  tag = ""
)

CFG <- within(CFG, {
  # --- Split-half stability (train/test) ---
  do_split_half     <- TRUE
  split_frac_train  <- 0.50
  split_seed        <- 1
  age_bins          <- 6
  
  # Stratification metadata file
  strat_file <- fs::path(ROOT, "03_analysis_input", "stratification_info.xlsx")
  
  # Which column in stratification_info is the participant id
  id_col_in_strat = "vpid"
  
  # KMO / Bartlett
  do_kmo            <- TRUE
  do_bartlett       <- FALSE
})

# ---- Helpers ----------------------------------------------------------------

# ---- Output visualizations ---------------------------------------------------

make_loading_long <- function(loadings_wide) {
  stopifnot("indicator" %in% names(loadings_wide))
  loadings_wide |>
    tidyr::pivot_longer(
      cols = -indicator,
      names_to = "factor",
      values_to = "loading"
    )
}

plot_loadings_heatmap <- function(loadings_wide, out_png, title = "EFA loadings (standardized)") {
  df_long <- make_loading_long(loadings_wide)
  
  # Order indicators by max absolute loading
  ord <- df_long |>
    dplyr::group_by(indicator) |>
    dplyr::summarise(mx = max(abs(loading), na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(mx)) |>
    dplyr::pull(indicator)
  
  df_long$indicator <- factor(df_long$indicator, levels = rev(ord))
  
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = factor, y = indicator, fill = loading)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient2(midpoint = 0) +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = "Loading") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  ggplot2::ggsave(out_png, plot = p, width = 9, height = max(4, 0.25 * length(ord)), dpi = 150, bg = "white")
}

plot_matrix_heatmap <- function(M, out_png, title = "Heatmap", value_name = "value") {
  stopifnot(is.matrix(M) || is.data.frame(M))
  M <- as.matrix(M)
  rn <- rownames(M); cn <- colnames(M)
  if (is.null(rn)) rn <- seq_len(nrow(M))
  if (is.null(cn)) cn <- seq_len(ncol(M))
  
  df <- as.data.frame(as.table(M), stringsAsFactors = FALSE)
  names(df) <- c("row", "col", value_name)
  
  # Keep original order (top-left = [1,1])
  df$row <- factor(df$row, levels = rev(rn))
  df$col <- factor(df$col, levels = cn)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = col, y = row, fill = .data[[value_name]])) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient2(midpoint = 0) +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = value_name) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  ggplot2::ggsave(out_png, plot = p, width = 9, height = 8, dpi = 150, bg = "white")
}


read_keys <- function(path_json) {
  stopifnot(fs::file_exists(path_json))
  jsonlite::fromJSON(path_json)
}

# Read the exported XLSX (single sheet)
read_input_xlsx <- function(path_xlsx) {
  stopifnot(fs::file_exists(path_xlsx))
  df <- readxl::read_xlsx(path_xlsx)
  as.data.frame(df, check.names = FALSE)
}

# Resolve input file paths based on CFG$input_set + CFG$level
resolve_input_paths <- function(cfg) {
  set <- tolower(cfg$input_set)
  lvl <- tolower(cfg$level)
  
  if (!set %in% c("hitop","complete")) stop("CFG$input_set must be 'hitop' or 'complete'.")
  if (!lvl %in% c("items","subscales")) stop("CFG$level must be 'items' or 'subscales'.")
  
  base <- paste0(cfg$sample, "_", if (set == "hitop") "HiTOP" else "complete")
  
  file <- if (lvl == "items") {
    fs::path(cfg$analysis_input_dir, paste0(base, "_items.xlsx"))
  } else {
    fs::path(cfg$analysis_input_dir, paste0(base, "_subscales.xlsx"))
  }
  
  if (!fs::file_exists(file)) {
    stop("Input XLSX not found: ", file, call. = FALSE)
  }
  
  list(data_xlsx = file)
}

# Convert numeric-ish vectors to ordered factors (for poly/tetra + WLSMV)
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
  
  pa <- psych::fa.parallel(
    df,
    fa     = "fa",
    cor    = cor_arg,
    n.iter = cfg$pa_iter,
    plot   = FALSE
  )
  
  k <- seq_along(pa$fa.values)
  tibble::tibble(
    k = k,
    eigen_observed = pa$fa.values,
    eigen_sim_mean = pa$fa.sim
  )
}

write_and_show_scree <- function(eigen_df, out_png_base, title, n_suggest = NULL) {
  
  # ---- Compute Gorsuch–Nelson index ----
  eig <- eigen_df$eigen_observed
  if (length(eig) >= 2) {
    drops <- eig[-length(eig)] - eig[-1]
    gn_index <- drops / eig[1]
    gn_df <- data.frame(
      k = seq_along(gn_index),
      gn = gn_index
    )
    gn_suggest <- gn_df$k[which.max(gn_df$gn)]
  } else {
    gn_df <- NULL
    gn_suggest <- NULL
  }
  
  # Scale GN to eigenvalue range for overlay
  if (!is.null(gn_df)) {
    scale_factor <- max(eigen_df$eigen_observed, na.rm = TRUE)
    gn_scaled <- gn_df$gn * scale_factor
  }
  
  # ----------------------------
  # VERSION 1: WITH parallel
  # ----------------------------
  
  png(paste0(out_png_base, "_withPA.png"), width = 1100, height = 750)
  
  plot(
    eigen_df$k, eigen_df$eigen_observed,
    type = "b",
    pch = 16,
    lwd = 2,
    xlab = "Factor number",
    ylab = "Eigenvalue",
    main = title,
    col = "black"
  )
  
  # Parallel eigenvalues
  lines(
    eigen_df$k, eigen_df$eigen_sim_mean,
    type = "b",
    pch = 1,
    lwd = 2,
    lty = 2,
    col = "grey60"
  )
  
  # Kaiser rule
  abline(h = 1, lty = 3, lwd = 2)
  
  # Parallel suggestion
  if (!is.null(n_suggest)) {
    abline(v = n_suggest, lty = 4, lwd = 2)
  }
  
  # ---- Overlay GN index ----
  if (!is.null(gn_df)) {
    lines(
      gn_df$k,
      gn_scaled,
      type = "b",
      pch = 17,
      lwd = 2,
      col = "blue"
    )
    
    # GN suggestion line
    abline(v = gn_suggest, lty = 5, lwd = 2, col = "blue")
    
    # Add secondary axis
    axis(4,
         at = pretty(gn_scaled),
         labels = round(pretty(gn_scaled) / scale_factor, 3))
    mtext("Gorsuch–Nelson index", side = 4, line = 3)
  }
  
  legend(
    "topright",
    legend = c(
      "Observed eigenvalues",
      "Parallel analysis (simulated mean)",
      "Kaiser criterion (Eigenvalue = 1)",
      if (!is.null(n_suggest)) paste0("Parallel suggestion (k = ", n_suggest, ")"),
      if (!is.null(gn_df)) "Gorsuch–Nelson index",
      if (!is.null(gn_df)) paste0("GN suggestion (k = ", gn_suggest, ")")
    ),
    col = c("black", "grey60", "black", "black", "blue", "blue"),
    lty = c(1, 2, 3, 4, 1, 5),
    lwd = 2,
    pch = c(16, 1, NA, NA, 17, NA),
    bty = "n",
    cex = 1
  )
  
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
    lavaan::cfa(
      model,
      data = df,
      ordered = vars,
      estimator = "WLSMV",
      rotation = cfg$rotation,
      std.lv = TRUE,
      missing = "pairwise",
      parameterization = "theta"
    )
  } else if (est %in% c("ML", "MLR")) {
    lavaan::cfa(
      model,
      data = df,
      estimator = est,          # "ML" or "MLR"
      rotation = cfg$rotation,
      std.lv = TRUE,
      missing = "fiml"
    )
  } else {
    stop("Unknown estimator: ", cfg$estimator, " (ML|MLR|WLSMV)")
  }
}

extract_solution_tables <- function(fit) {
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  
  std_col <- dplyr::coalesce(
    if ("est.std"  %in% names(pe)) pe$est.std else NA_real_,
    if ("std.all"  %in% names(pe)) pe$std.all else NA_real_,
    if ("std.lv"   %in% names(pe)) pe$std.lv  else NA_real_,
    if ("std.nox"  %in% names(pe)) pe$std.nox else NA_real_
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
  
  measures <- c(
    "chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr",
    "chisq","pvalue","cfi","tli","rmsea"
  )
  fm <- suppressWarnings(lavaan::fitMeasures(fit, measures))
  fit_tbl <- tibble::tibble(measure = names(fm), value = as.numeric(fm))
  
  list(loadings = loadings, factor_cor = factor_cor, uniqueness = uniq, fit = fit_tbl)
}

run_kmo <- function(X) {
  R <- stats::cor(X, use = "pairwise.complete.obs")
  k <- psych::KMO(R)
  list(
    kmo_overall = tibble::tibble(metric = "KMO_overall", value = unname(k$MSA)),
    kmo_per_var = tibble::tibble(variable = names(k$MSAi), kmo = unname(k$MSAi))
  )
}

read_strat_info <- function(path_xlsx, id_col = "vpid") {
  stopifnot(fs::file_exists(path_xlsx))
  df <- readxl::read_xlsx(path_xlsx)
  
  if (!id_col %in% names(df)) {
    stop("Stratification file is missing ID column '", id_col, "'. Columns are: ",
         paste(names(df), collapse = ", "), call. = FALSE)
  }
  
  df %>%
    dplyr::mutate(
      project = as.character(project),
      gender  = as.character(gender),
      group   = dplyr::if_else(is.na(group) | trimws(as.character(group)) == "", "HC", as.character(group)),
      age     = suppressWarnings(as.numeric(age)),
      !!id_col := as.character(.data[[id_col]])
    )
}


make_age_bins <- function(age, n_bins = 6) {
  qs <- unique(as.numeric(stats::quantile(age, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)))
  if (length(qs) < 3) return(factor(rep("all", length(age))))
  cut(age, breaks = qs, include.lowest = TRUE, ordered_result = TRUE)
}

stratified_split_ids <- function(meta, frac_train = 0.5, seed = 1, age_bins = 6) {
  set.seed(seed)
  
  meta2 <- meta %>%
    dplyr::mutate(
      age_bin = make_age_bins(age, n_bins = age_bins),
      strata = interaction(age_bin, gender, project, group, drop = TRUE, sep = " | ")
    )
  
  train_ids <- meta2 %>%
    dplyr::group_by(strata) %>%
    dplyr::slice_sample(prop = frac_train) %>%
    dplyr::ungroup() %>%
    dplyr::pull(participant_id)
  
  list(
    train_ids = unique(train_ids),
    test_ids  = setdiff(meta2$participant_id, unique(train_ids)),
    meta = meta2
  )
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
  
  Lm <- as.matrix(L[, -1, drop = FALSE])
  rownames(Lm) <- L$indicator
  
  # ensure same indicator order
  Lm <- Lm[vars, , drop = FALSE]
  
  # hard fail if NA (prevents the cryptic NULL downstream)
  if (any(is.na(Lm))) {
    bad_rows <- rownames(Lm)[apply(Lm, 1, function(r) any(is.na(r)))]
    stop("NA loadings in standardized solution for indicators (first 10): ",
         paste(head(bad_rows, 10), collapse=", "),
         call. = FALSE)
  }
  Lm
}

tucker_congruence_procrustes <- function(L_train, L_test) {
  # sanity checks
  if (is.null(L_train) || is.null(L_test)) stop("Loading matrices are NULL.", call. = FALSE)
  if (!all(dim(L_train) == dim(L_test))) {
    stop("Train/test loading matrices have different dimensions: ",
         paste(dim(L_train), collapse="x"), " vs ",
         paste(dim(L_test), collapse="x"), call. = FALSE)
  }
  if (any(!is.finite(L_train)) || any(!is.finite(L_test))) {
    stop("Non-finite values (NA/Inf) in loading matrices. Check lavaan convergence/standardizedSolution.", call. = FALSE)
  }
  
  # Procrustes alignment (psych has this; GPArotation exports can be inconsistent)
  pr <- psych::Procrustes(L_train, L_test)
  L_test_rot <- pr$loadings
  
  if (is.null(L_test_rot) || any(!is.finite(L_test_rot))) {
    stop("Procrustes rotation produced NULL or non-finite loadings.", call. = FALSE)
  }
  
  cong <- psych::factor.congruence(L_train, L_test_rot)
  if (is.null(cong) || !is.matrix(cong)) {
    stop("factor.congruence() returned NULL/non-matrix. This usually means unstable/degenerate solutions.", call. = FALSE)
  }
  
  fac_cong <- diag(cong)
  tibble::tibble(
    factor = paste0("F", seq_along(fac_cong)),
    congruence = as.numeric(fac_cong)
  )
}

plot_congruence_png <- function(cong_tbl, out_png, title = "Tucker's congruence (train vs test)") {
  p <- ggplot2::ggplot(cong_tbl, ggplot2::aes(x = factor, y = congruence)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0.85, linetype = 2) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = 2) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = title, x = NULL, y = "Congruence") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(out_png, plot = p, width = 7, height = 4.5, dpi = 150)
}

# ---- Main -------------------------------------------------------------------

# Resolve input files
paths <- resolve_input_paths(CFG)
message("Using input XLSX: ", paths$data_xlsx)

# Read data matrix from XLSX (includes ID as first column)
X_raw <- read_input_xlsx(paths$data_xlsx)

# Identify / assert ID column
id_col <- CFG$id_col_in_strat
if (!id_col %in% names(X_raw)) {
  stop("Input XLSX is missing ID column '", id_col, "'. Columns are: ",
       paste(names(X_raw), collapse = ", "), call. = FALSE)
}

# Extract IDs + drop from analysis matrix
ids <- as.character(X_raw[[id_col]])
X_dat <- X_raw[, setdiff(names(X_raw), id_col), drop = FALSE]

# Convert columns depending on corr/estimator
if (tolower(CFG$corr) %in% c("polychoric","tetrachoric") || toupper(CFG$estimator) == "WLSMV") {
  X0 <- as.data.frame(lapply(X_dat, as_ordered_factor), check.names = FALSE)
} else {
  X0 <- as.data.frame(lapply(X_dat, as_numeric), check.names = FALSE)
}

# Drop too-few-level indicators (COLUMNS only)
var_check <- drop_low_variance_levels(X0, min_levels = CFG$min_levels_keep)
X2 <- X0[, var_check$keep, drop = FALSE]

if (ncol(X2) < 2) stop("After dropping low-level variables, <2 indicators remain.", call. = FALSE)

# Tetrachoric validity check
if (tolower(CFG$corr) == "tetrachoric") assert_binary_for_tetra(X2)

# Parallel analysis + scree
pa_tbl <- run_parallel_analysis(X2, CFG)

# ---- Gorsuch–Nelson index ----------------------------------------------------

compute_gn_index <- function(eigenvalues) {
  eigenvalues <- as.numeric(eigenvalues)
  if (length(eigenvalues) < 2) return(NULL)
  
  drops <- eigenvalues[-length(eigenvalues)] - eigenvalues[-1]
  gn <- drops / eigenvalues[1]
  
  tibble::tibble(
    k = seq_along(gn),
    lambda_k = eigenvalues[seq_along(gn)],
    lambda_kplus1 = eigenvalues[seq_along(gn) + 1],
    drop = drops,
    gn_index = gn
  )
}

gn_tbl <- compute_gn_index(pa_tbl$eigen_observed)

# Suggested factor number = position of largest GN index
gn_suggest <- gn_tbl$k[which.max(gn_tbl$gn_index)]

message("Gorsuch–Nelson suggested nf = ", gn_suggest)


n_suggest_pa <- sum(pa_tbl$eigen_observed > pa_tbl$eigen_sim_mean, na.rm = TRUE)
n_suggest_pa <- max(1L, as.integer(n_suggest_pa))

scree_png_base <- fs::path(
  DIR_OUT,
  paste0(CFG$sample, "_", CFG$tag,
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


# Decide number of factors
n_fac <- choose_n_factors(CFG, pa_tbl)

# Fit EFA
X_fit <- as.data.frame(scale(X2))
fit <- fit_efa(X_fit, n_fac, CFG)
pe <- lavaan::parameterEstimates(fit, standardized = FALSE)
heywood <- pe |>
  dplyr::filter(op == "~~", lhs == rhs, !grepl("^F\\d+$", lhs)) |>
  dplyr::transmute(indicator = lhs, resid_var = est) |>
  dplyr::filter(resid_var < 0) |>
  dplyr::arrange(resid_var)

heywood

pe_std <- lavaan::parameterEstimates(fit, standardized = TRUE)
heywood_std <- pe_std |>
  dplyr::filter(op == "~~", lhs == rhs, !grepl("^F\\d+$", lhs)) |>
  dplyr::transmute(indicator = lhs, resid_var_std = std.all) |>
  dplyr::filter(resid_var_std < 0) |>
  dplyr::arrange(resid_var_std)

heywood_std

R <- cor(X2, use="pairwise.complete.obs")
which(abs(R) > .95 & upper.tri(R), arr.ind = TRUE)

sol <- extract_solution_tables(fit)

# ---- Visualization exports ---------------------------------------------------

plot_tag <- paste0(
  CFG$sample, "_", CFG$tag,
  "_set-", CFG$input_set,
  "_lvl-", CFG$level,
  "_cor-", CFG$corr,
  "_est-", tolower(CFG$estimator),
  "_nf", n_fac,
  "_rot-", CFG$rotation
)

# 1) Loadings heatmap
png_load <- fs::path(DIR_OUT, paste0(plot_tag, "_loadings_heatmap.png"))
plot_loadings_heatmap(
  sol$loadings,
  png_load,
  title = paste0("Loadings heatmap (", CFG$input_set, " / ", CFG$level, ", nf=", n_fac, ")")
)
message("Saved: ", png_load)

# 2) Factor correlation heatmap (only if present)
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
} else {
  message("No factor correlations found (factor_cor table empty) -> skipping factor correlation heatmap.")
}

# 3) Observed-variable correlation heatmap (uses the data you actually fit)
R_obs <- stats::cor(X_fit, use = "pairwise.complete.obs")
png_r <- fs::path(DIR_OUT, paste0(plot_tag, "_observed_cor_heatmap.png"))
plot_matrix_heatmap(R_obs, png_r, title = "Observed-variable correlations", value_name = "r")
message("Saved: ", png_r)


# ---- KMO / Bartlett ---------------------------------------------------------
extra_summaries <- list()
if (isTRUE(CFG$do_kmo)) {
  kmo <- run_kmo(X2)
  extra_summaries$kmo_overall <- kmo$kmo_overall
  extra_summaries$kmo_per_var <- kmo$kmo_per_var
}

# ---- Split-half stability (stratified) --------------------------------------
if (isTRUE(CFG$do_split_half)) {
  
  meta <- read_strat_info(CFG$strat_file, id_col = CFG$id_col_in_strat)
  
  # Keep only meta rows that exist in the analysis input, and reorder to match X2
  meta2 <- meta %>% dplyr::filter(.data[[CFG$id_col_in_strat]] %in% ids)
  ids_u <- unique(ids)
  meta_ids_u <- unique(meta[[CFG$id_col_in_strat]])
  
  missing_meta <- setdiff(ids_u, meta_ids_u)
  if (length(missing_meta)) {
    warning("Some IDs are missing in stratification_info.xlsx (first 10): ",
            paste(head(missing_meta, 10), collapse = ", "))
  }
  
  # Reorder meta to match the XLSX row order
  idx <- match(ids, meta2[[CFG$id_col_in_strat]])
  if (any(is.na(idx))) {
    stop("Split-half: could not match some IDs between input XLSX and stratification_info.xlsx.", call. = FALSE)
  }
  meta2 <- meta2[idx, , drop = FALSE]
  
  # Now meta2 rows align to X2 rows by construction
  if (nrow(meta2) != nrow(X2)) {
    stop("Split-half: after aligning by ID, nrow(meta) != nrow(X2).", call. = FALSE)
  }
  
  sp <- stratified_split_ids(
    meta = meta2 %>% dplyr::rename(participant_id = !!CFG$id_col_in_strat),
    frac_train = CFG$split_frac_train,
    seed = CFG$split_seed,
    age_bins = CFG$age_bins
  )
  
  is_train <- meta2[[CFG$id_col_in_strat]] %in% sp$train_ids
  X_train <- X2[is_train, , drop = FALSE]
  X_test  <- X2[!is_train, , drop = FALSE]
  
  fit_train <- fit_efa(X_train, n_fac, CFG)
  fit_test  <- fit_efa(X_test,  n_fac, CFG)
  
  vars <- intersect(colnames(X_train), colnames(X_test))
  X_train <- X_train[, vars, drop = FALSE]
  X_test  <- X_test[, vars, drop = FALSE]
  
  L_train <- get_std_loading_matrix(fit_train, vars)
  L_test  <- get_std_loading_matrix(fit_test,  vars)
  
  cong_tbl <- tucker_congruence_procrustes(L_train, L_test)
  cong_png <- fs::path(DIR_OUT, paste0(CFG$sample, "_", CFG$tag, "_nf", n_fac, "_set-", CFG$input_set, "_split_congruence.png"))
  plot_congruence_png(cong_tbl, cong_png)
  
  split_tbl <- tibble::tibble(
    set = c("train","test"),
    n = c(sum(is_train), sum(!is_train)),
    frac = c(mean(is_train), mean(!is_train))
  )
  
  extra_summaries$split_counts <- split_tbl
  extra_summaries$tucker_congruence <- cong_tbl
}

# ---- Optional factor scores --------------------------------------------------
scores_tbl <- NULL
if (isTRUE(CFG$compute_factor_scores)) {
  fscores <- as.data.frame(lavaan::lavPredict(fit))
  scores_tbl <- tibble::as_tibble(fscores)
}

# ---- Metadata tables ---------------------------------------------------------
settings_tbl <- tibble::tibble(
  setting = names(CFG),
  value   = vapply(CFG, function(x) paste(x, collapse = ", "), character(1))
) %>%
  dplyr::bind_rows(tibble::tibble(setting = "n_factors_used", value = as.character(n_fac))) %>%
  dplyr::bind_rows(tibble::tibble(setting = "n_indicators_used", value = as.character(ncol(X2))))

coverage_tbl <- tibble::tibble(
  variable = names(X2),
  pct_missing = sapply(X2, function(v) 100 * mean(is.na(v)))
) %>% dplyr::arrange(dplyr::desc(pct_missing))

dropped_tbl <- tibble::tibble(
  dropped_variables = var_check$drop,
  reason = paste0("n_levels < ", CFG$min_levels_keep)
)

# ---- Write XLSX --------------------------------------------------------------
ts <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
out_xlsx <- fs::path(
  DIR_OUT,
  paste0("efa_", CFG$sample, "_", CFG$tag,
         "_set-", CFG$input_set,
         "_lvl-", CFG$level,
         "_cor-", CFG$corr,
         "_est-", tolower(CFG$estimator),
         "_nf", n_fac,
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
message("Saved scree plot: ", scree_png_base)
message("Done.")


cat("\nROOT:\n", ROOT, "\n", sep="")
cat("\nDIR_OUT:\n", DIR_OUT, "\n", sep="")

cat("\nDIR_OUT exists? ", dir.exists(DIR_OUT), "\n", sep="")
cat("Can I write there? ", file.access(DIR_OUT, 2) == 0, "\n", sep="")

# write test
testfile <- file.path(DIR_OUT, "___write_test.txt")
ok <- tryCatch({ writeLines("hello", testfile); TRUE }, error = function(e) e)
cat("Write test result: ", if (isTRUE(ok)) "OK" else paste("FAILED:", ok$message), "\n", sep="")

cat("\nFiles currently in DIR_OUT:\n")
print(list.files(DIR_OUT, all.files = TRUE))
