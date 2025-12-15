#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subscale-level factor analysis (EFA) with ordinal data
# - subscale scores built from item-level data using keys.json
# - parallel analysis + scree on polychoric correlations
# - EFA via lavaan using WLSMV (ordered indicators)
# - exports a human-readable XLSX
# Output: ROOT/out/internal_data_analysis/factor_analysis/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# ---- Packages ---------------------------------------------------------------

ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c("jsonlite", "dplyr", "tibble", "stringr", "fs", "psych", "lavaan", "writexl"))

# ---- Paths ------------------------------------------------------------------

script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

ROOT <- script_dir()

DIR_OUT <- fs::path(ROOT, "out", "internal_data_analysis", "factor_analysis")
fs::dir_create(DIR_OUT)

# ---- CONFIG (edit this block to test decisions) ------------------------------

CFG <- list(
  sample = "adults",
  
  # Input files (these follow your cleaning pipeline conventions)
  data_csv  = fs::path(ROOT, "02_cleaned", "adults", "adults_clean_master.csv"),
  keys_json = fs::path(ROOT, "02_cleaned", "keys", "adults_keys.json"),
  
  # Subscale scoring
  score_method   = "sum",   # "sum" | "mean" | "prorated_sum"
  min_prop_items = 0.80,    # require >=80% items present to score a subscale
  
  # Which scales to include (set NULL to include all scales in keys)
  include_scales = c("IDAS", "CAPE", "IUS", "APS", "BISBAS", "TICS", "AQ", "MAP-SR", "SUQ", "CTQ"),
  exclude_scales = c("FHSfamilytree"),  # typical meta-family-history block
  
  # EFA settings
  n_factors = 6,            # choose manually; you will also get parallel analysis suggestion
  rotation  = "geomin",     # "geomin" is common for EFA; alternatives: "oblimin", "promax"
  
  # Parallel analysis settings
  pa_iter   = 200,          # increase for stability (e.g., 500–1000)
  
  # Optional: compute factor scores (can be large sheets)
  compute_factor_scores = FALSE,
  
  # Output naming tag (helps when you iterate)
  tag = "v1"
)

# ---- Helpers ----------------------------------------------------------------

read_keys <- function(path_json) {
  stopifnot(fs::file_exists(path_json))
  jsonlite::fromJSON(path_json)
}

read_clean_master <- function(path_csv) {
  stopifnot(fs::file_exists(path_csv))
  # write.csv2 -> read.csv2 is the natural pair (sep=";", dec=",")
  read.csv2(path_csv, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

make_subscale_table <- function(keys) {
  # Expect keys$items_by_subscale: list of scale/subscale/items
  x <- keys$items_by_subscale
  df <- tibble::tibble(
    scale    = as.character(x$scale),
    subscale = if ("subscale" %in% names(x)) as.character(x$subscale) else NA_character_,
    items    = x$items
  )
  
  df <- df %>%
    mutate(
      subscale = ifelse(is.na(subscale) | subscale == "", NA_character_, subscale),
      subscale_id = ifelse(is.na(subscale), scale, paste0(scale, "__", subscale)),
      n_items = lengths(items)
    ) %>%
    distinct(subscale_id, .keep_all = TRUE)
  
  df
}

score_one_subscale <- function(df_items, items, method = "sum", min_prop = 0.80) {
  X <- df_items[, items, drop = FALSE]
  X <- as.data.frame(lapply(X, function(v) suppressWarnings(as.numeric(v))))
  
  n_total <- ncol(X)
  n_obs   <- apply(X, 1, function(r) sum(!is.na(r)))
  ok      <- n_obs >= ceiling(min_prop * n_total)
  
  if (method == "sum") {
    out <- apply(X, 1, function(r) if (all(is.na(r))) NA_real_ else sum(r, na.rm = TRUE))
    out[!ok] <- NA_real_
    return(out)
  }
  
  if (method == "mean") {
    out <- apply(X, 1, function(r) if (all(is.na(r))) NA_real_ else mean(r, na.rm = TRUE))
    out[!ok] <- NA_real_
    return(out)
  }
  
  if (method == "prorated_sum") {
    mn  <- apply(X, 1, function(r) if (all(is.na(r))) NA_real_ else mean(r, na.rm = TRUE))
    out <- mn * n_total
    out[!ok] <- NA_real_
    return(out)
  }
  
  stop("Unknown score_method: ", method)
}

as_ordered_factor <- function(x) {
  # keep numeric ordering; drop NA
  vals <- sort(unique(x[!is.na(x)]))
  factor(x, levels = vals, ordered = TRUE)
}

build_subscale_scores <- function(df_raw, sub_tbl, cfg) {
  # Only keep items that actually exist in df_raw
  has_items <- function(items) all(items %in% names(df_raw))
  sub_tbl2 <- sub_tbl %>%
    mutate(all_items_present = vapply(items, has_items, logical(1))) %>%
    filter(all_items_present)
  
  scores <- lapply(seq_len(nrow(sub_tbl2)), function(i) {
    id    <- sub_tbl2$subscale_id[i]
    items <- sub_tbl2$items[[i]]
    score_one_subscale(df_raw, items, cfg$score_method, cfg$min_prop_items) |>
      setNames(id)
  })
  
  df_scores <- as.data.frame(scores)
  df_scores <- as.data.frame(lapply(df_scores, as_ordered_factor))
  
  list(scores = df_scores, subscale_table = sub_tbl2)
}

drop_low_variance <- function(df_ord, min_levels = 3) {
  n_levels <- sapply(df_ord, function(v) length(unique(v[!is.na(v)])))
  keep <- names(n_levels)[n_levels >= min_levels]
  drop <- names(n_levels)[n_levels <  min_levels]
  list(keep = keep, drop = drop, n_levels = n_levels)
}

run_parallel_analysis <- function(df_ord, cfg) {
  set.seed(1)
  pa <- psych::fa.parallel(
    df_ord,
    fa   = "fa",
    cor  = "poly",
    n.iter = cfg$pa_iter,
    plot = FALSE
  )
  
  # Build a tidy table (observed vs simulated)
  # fa.values: observed eigenvalues; fa.sim: mean simulated eigenvalues (psych)
  k <- seq_along(pa$fa.values)
  tibble::tibble(
    k = k,
    eigen_observed = pa$fa.values,
    eigen_sim_mean = pa$fa.sim
  )
}

fit_efa_wlsmv <- function(df_ord, n_factors, rotation) {
  vars <- colnames(df_ord)
  
  model <- paste(
    sapply(seq_len(n_factors), function(i) {
      paste0('efa("efa")*F', i, ' =~ ', paste(vars, collapse = " + "))
    }),
    collapse = "\n"
  )
  
  lavaan::cfa(
    model,
    data = df_ord,
    ordered = vars,
    estimator = "WLSMV",
    rotation = rotation,
    std.lv = TRUE,
    missing = "pairwise",
    parameterization = "theta"
  )
}

extract_solution_tables <- function(fit) {
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  
  loadings <- pe %>%
    filter(op == "=~") %>%
    transmute(
      factor = lhs,
      indicator = rhs,
      loading = est.std
    ) %>%
    tidyr::pivot_wider(names_from = factor, values_from = loading) %>%
    arrange(indicator)
  
  factor_cor <- pe %>%
    filter(op == "~~", lhs != rhs, grepl("^F\\d+$", lhs), grepl("^F\\d+$", rhs)) %>%
    transmute(factor1 = lhs, factor2 = rhs, r = est.std)
  
  uniq <- pe %>%
    filter(op == "~~", lhs == rhs, !grepl("^F\\d+$", lhs)) %>%
    transmute(indicator = lhs, uniqueness = est.std)
  
  fit_tbl <- tibble::tibble(
    measure = c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"),
    value   = suppressWarnings(lavaan::fitMeasures(
      fit,
      c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr")
    ))
  )
  
  list(loadings = loadings, factor_cor = factor_cor, uniqueness = uniq, fit = fit_tbl)
}

write_scree_png <- function(eigen_df, out_png) {
  png(out_png, width = 900, height = 600)
  plot(
    eigen_df$k, eigen_df$eigen_observed, type = "b",
    xlab = "Component / factor number", ylab = "Eigenvalue",
    main = "Scree plot (observed polychoric eigenvalues)"
  )
  abline(h = 1, lty = 2)
  dev.off()
}

# ---- Main -------------------------------------------------------------------

keys <- read_keys(CFG$keys_json)
sub_tbl0 <- make_subscale_table(keys)

# scale include/exclude filters
sub_tbl <- sub_tbl0
if (!is.null(CFG$include_scales)) sub_tbl <- sub_tbl %>% filter(scale %in% CFG$include_scales)
if (!is.null(CFG$exclude_scales)) sub_tbl <- sub_tbl %>% filter(!(scale %in% CFG$exclude_scales))

df_raw <- read_clean_master(CFG$data_csv)

scored <- build_subscale_scores(df_raw, sub_tbl, CFG)
df_sub <- scored$scores
sub_tbl_used <- scored$subscale_table

# Drop subscales with too few categories to support polychoric / WLSMV well
var_check <- drop_low_variance(df_sub, min_levels = 3)
df_sub2   <- df_sub[, var_check$keep, drop = FALSE]

# Parallel analysis + scree
pa_tbl <- run_parallel_analysis(df_sub2, CFG)
scree_png <- fs::path(DIR_OUT, paste0(CFG$sample, "_", CFG$tag, "_scree.png"))
write_scree_png(pa_tbl, scree_png)

# Fit EFA
fit <- fit_efa_wlsmv(df_sub2, CFG$n_factors, CFG$rotation)
sol <- extract_solution_tables(fit)

# Optional factor scores
scores_tbl <- NULL
if (isTRUE(CFG$compute_factor_scores)) {
  fscores <- as.data.frame(lavaan::lavPredict(fit))
  scores_tbl <- tibble::as_tibble(fscores)
}

# Build nice metadata tables
settings_tbl <- tibble::tibble(
  setting = names(CFG),
  value   = vapply(CFG, function(x) paste(x, collapse = ", "), character(1))
)

subscales_tbl <- sub_tbl_used %>%
  mutate(items = vapply(items, function(x) paste(x, collapse = ", "), character(1))) %>%
  select(subscale_id, scale, subscale, n_items, items)

coverage_tbl <- tibble::tibble(
  subscale_id = names(df_sub2),
  pct_missing = sapply(df_sub2, function(v) 100 * mean(is.na(v)))
) %>% arrange(desc(pct_missing))

dropped_tbl <- tibble::tibble(
  dropped_subscales = var_check$drop
)

# Write XLSX
ts <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
out_xlsx <- fs::path(
  DIR_OUT,
  paste0("efa_", CFG$sample, "_", CFG$tag,
         "_nf", CFG$n_factors,
         "_rot-", CFG$rotation,
         "_", ts, ".xlsx")
)

sheets <- list(
  "settings"        = settings_tbl,
  "subscales_used"  = subscales_tbl,
  "coverage"        = coverage_tbl,
  "dropped_low_var" = dropped_tbl,
  "parallel_analysis" = pa_tbl,
  "loadings"        = sol$loadings,
  "factor_cor"      = sol$factor_cor,
  "uniqueness"      = sol$uniqueness,
  "fit"             = sol$fit
)

if (!is.null(scores_tbl)) sheets[["factor_scores"]] <- scores_tbl

writexl::write_xlsx(sheets, out_xlsx)

message("Saved XLSX: ", out_xlsx)
message("Saved scree plot: ", scree_png)
message("Done.")
