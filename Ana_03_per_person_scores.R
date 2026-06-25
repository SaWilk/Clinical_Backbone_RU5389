# ============================================================
# Score persons from EFA scoring bundle
#
# Input:
#   RDS file exported by the EFA pipeline as e.g.
#   k03__psych_ml_oblimin_subraw_gauto__scoring_bundle.rds
#
# Output:
#   - per-person factor score table (.xlsx + .csv)
#   - score summary table
#   - score correlations
#   - histogram plots per factor and combined overview
# ============================================================

# ---------------------------
# 0) CONFIG: only edit this block
# ---------------------------
config <- list(

  # Folder produced by the EFA script, i.e. the folder that contains /summary, /tables, /plots.
  # Example:
  # efa_run_dir = "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/out/factor_analysis/efa/2026-06-17__subscales__adults_adolescents_HiTOP_subscales__kset_3-4-5"
  efa_run_dir = "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/out/factor_analysis/efa/2026-06-22__subscales__complete__kset_3-4-5__b30s20260622_pg",

      k_target = 4,
  
  scoring_bundle_file = file.path(
    "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/out/factor_analysis/efa/2026-06-22__subscales__complete__kset_3-4-5__b30s20260622_pg",
    "summary",
    "k05__minres_oblimin_nrot10_subraw_gauto__scoring_bundle.rds"
  ),
  # Optional: score a different file than the original bundle$input_file.
  # Leave NULL to score the original input file saved inside the bundle.
  score_data_file = NULL,
  score_data_sheet = 1,

  # Recommended: "tenBerge" for oblique EFA; "Thurstone" is regression scoring.
  # Allowed: "tenBerge", "Thurstone", "regression", "Bartlett", "Anderson", "Harman".
  factor_score_method = "tenBerge",

  # Missing data handling inside psych::factor.scores.
  # Usually keep this TRUE. The script still reports missingness per person.
  factor_scores_missing = TRUE,
  factor_scores_impute = "median",

  # Because imputation can otherwise produce a score for people with many missing values.
  # Set to 0 if you never want scores set to NA based on missingness.
  min_valid_prop_all_scoring_vars = 0.50,

  # Optional manual sign flip after inspecting factor interpretation.
  # Use raw factor names from the EFA bundle, e.g. c("MR2") or c("ML2").
  reverse_factors = character(0),

  # Metadata columns to keep at the beginning of the per-person table if present.
  id_cols_keep = c(
    "vp_id", "vpid", "VPCode", "vpcode", "id", "limesurvey_id",
    "sample", "project", "group", "age", "age_years", "gender", "sex"
  ),

  # Optional metadata from stratification_info.xlsx.
  # "auto" uses bundle$stratification_path when it exists.
  # Set FALSE if you do not want this.
  use_stratification_metadata = "auto",
  stratification_file = NULL,
  stratification_sheet = 1,
  stratification_merge_method = "auto", # c("auto", "by_key", "by_row_order")
  stratification_key = NULL,

  # Histograms: set to NULL for normal histograms.
  # Set e.g. "group", "sample", or "project" to facet histograms by this column.
  hist_by = NULL,

  # Output folder. NULL means: efa_run_dir/person_scores/kXX__METHOD
  out_dir = NULL
)


# ---------------------------
# 1) Packages + utilities
# ---------------------------
required_pkgs <- c("readxl", "readr", "writexl", "dplyr", "tidyr", "stringr", "ggplot2", "psych", "tibble")

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall with: install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(psych)
  library(tibble)
})

safe_mkdir <- function(path) {
  if (is.null(path) || is.na(path) || trimws(path) == "") return(invisible(FALSE))
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = TRUE)
  if (!dir.exists(path)) {
    Sys.sleep(0.2)
    dir.create(path, recursive = TRUE, showWarnings = TRUE)
  }
  if (!dir.exists(path)) stop("Could not create directory: ", path, call. = FALSE)
  invisible(TRUE)
}

sanitize_tag <- function(x) {
  x <- as.character(x)
  x <- gsub("\\.[A-Za-z0-9]+$", "", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "untagged", x)
}

format_k_single <- function(k) {
  paste0("k", sprintf("%02d", as.integer(k)))
}

trim_colnames <- function(x) {
  names(x) <- trimws(names(x))
  x
}

read_any_table <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    out <- readxl::read_excel(path, sheet = sheet)
    return(trim_colnames(as.data.frame(out)))
  }

  if (ext %in% c("csv")) {
    out <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    return(trim_colnames(as.data.frame(out)))
  }

  if (ext %in% c("tsv", "txt")) {
    # Try TSV first, then CSV-style fallback.
    out <- tryCatch(
      suppressMessages(readr::read_tsv(path, show_col_types = FALSE)),
      error = function(e) suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    )
    return(trim_colnames(as.data.frame(out)))
  }

  stop("Unsupported input file type: ", path, call. = FALSE)
}

as_numeric_robust <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))

  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "\\*", "")
  x_chr <- stringr::str_replace_all(x_chr, "\\s+", "")
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null", ".")] <- NA_character_

  dot <- suppressWarnings(readr::parse_number(x_chr, locale = readr::locale(decimal_mark = ".")))
  comma <- suppressWarnings(readr::parse_number(x_chr, locale = readr::locale(decimal_mark = ",")))

  if (sum(!is.na(comma)) > sum(!is.na(dot))) comma else dot
}

match_columns <- function(required_names, available_names) {
  required_names <- as.character(required_names)
  available_names <- as.character(available_names)

  direct <- required_names %in% available_names
  matched <- rep(NA_character_, length(required_names))
  matched[direct] <- required_names[direct]

  still <- which(!direct)
  if (length(still) > 0) {
    lookup <- setNames(available_names, tolower(trimws(available_names)))
    matched[still] <- unname(lookup[tolower(trimws(required_names[still]))])
  }

  data.frame(
    required = required_names,
    matched = matched,
    found = !is.na(matched),
    row.names = NULL
  )
}

find_scoring_bundle <- function(efa_run_dir, k_target) {
  summary_dir <- file.path(efa_run_dir, "summary")
  search_dir <- if (dir.exists(summary_dir)) summary_dir else efa_run_dir

  files <- list.files(search_dir, pattern = "scoring_bundle.*\\.rds$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

  if (length(files) == 0) {
    stop("No scoring_bundle RDS file found under: ", search_dir, call. = FALSE)
  }

  k_tag <- format_k_single(k_target)
  files_k <- files[stringr::str_detect(basename(files), paste0("^", k_tag, "__"))]

  if (length(files_k) == 0) {
    stop(
      "Found scoring bundles, but none matched ", k_tag, ".\nAvailable bundles:\n",
      paste(files, collapse = "\n"),
      call. = FALSE
    )
  }

  if (length(files_k) > 1) {
    files_k <- files_k[order(file.info(files_k)$mtime, decreasing = TRUE)]
    message("Multiple matching bundles found. Using newest one:\n", files_k[1])
  }

  files_k[1]
}

make_score_colnames <- function(factor_names, k, method) {
  paste0(format_k_single(k), "__", sanitize_tag(method), "__", sanitize_tag(factor_names), "__score")
}

safe_stat <- function(x, fun, ...) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(fun(x, ...))
}

summarise_scores <- function(score_tbl, score_cols) {
  dplyr::bind_rows(lapply(score_cols, function(sc) {
    x <- score_tbl[[sc]]
    data.frame(
      score = sc,
      n_valid = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      mean = safe_stat(x, mean),
      sd = safe_stat(x, stats::sd),
      min = safe_stat(x, min),
      p25 = safe_stat(x, stats::quantile, probs = 0.25, names = FALSE),
      median = safe_stat(x, stats::median),
      p75 = safe_stat(x, stats::quantile, probs = 0.75, names = FALSE),
      max = safe_stat(x, max),
      row.names = NULL
    )
  }))
}

merge_stratification_metadata <- function(df, bundle) {
  use_meta <- config$use_stratification_metadata

  if (identical(use_meta, FALSE)) return(df)

  strat_path <- config$stratification_file

  if (is.null(strat_path) && identical(use_meta, "auto")) {
    strat_path <- bundle$stratification_path
  }

  if (is.null(strat_path) || is.na(strat_path) || !file.exists(strat_path)) {
    message("No stratification metadata merged.")
    return(df)
  }

  strat <- read_any_table(strat_path, sheet = config$stratification_sheet)
  method <- config$stratification_merge_method

  message("Loaded stratification metadata: ", strat_path)

  if (identical(method, "by_key")) {
    key <- config$stratification_key
    if (is.null(key) || !(key %in% names(df)) || !(key %in% names(strat))) {
      stop("stratification_merge_method='by_key', but key is missing in df or stratification table.", call. = FALSE)
    }
    return(dplyr::left_join(df, strat, by = key))
  }

  if (identical(method, "by_row_order")) {
    if (nrow(df) != nrow(strat)) {
      stop("by_row_order merge requested, but nrow(df) != nrow(strat).", call. = FALSE)
    }
    dup <- intersect(names(df), names(strat))
    strat <- strat[, setdiff(names(strat), dup), drop = FALSE]
    return(dplyr::bind_cols(df, strat))
  }

  # auto
  shared <- intersect(names(df), names(strat))
  id_like <- shared[grepl("(^id$|vp_id|vpid|vpcode|participant|subject|case)", shared, ignore.case = TRUE)]

  if (length(id_like) >= 1) {
    key <- id_like[1]
    message("Auto-merging stratification metadata by key: ", key)
    return(dplyr::left_join(df, strat, by = key))
  }

  if (nrow(df) == nrow(strat)) {
    message("Auto-merging stratification metadata by row order.")
    dup <- intersect(names(df), names(strat))
    strat <- strat[, setdiff(names(strat), dup), drop = FALSE]
    return(dplyr::bind_cols(df, strat))
  }

  warning("Could not auto-merge stratification metadata. Continuing without it.", call. = FALSE)
  df
}

make_factor_validity <- function(X_score, bundle) {
  assignment <- bundle$assignment
  factor_names <- colnames(bundle$loadings)

  if (is.null(assignment) || !is.data.frame(assignment) || !("variable" %in% names(assignment))) {
    return(as.data.frame(matrix(nrow = nrow(X_score), ncol = 0)))
  }

  if (!("primary_factor_raw" %in% names(assignment))) {
    if ("primary_factor" %in% names(assignment)) {
      assignment$primary_factor_raw <- assignment$primary_factor
    } else {
      return(as.data.frame(matrix(nrow = nrow(X_score), ncol = 0)))
    }
  }

  keep_col <- NULL
  if ("keep_interpretive" %in% names(assignment)) keep_col <- "keep_interpretive"
  if ("keep_strict" %in% names(assignment)) keep_col <- "keep_strict"

  validity <- as.data.frame(matrix(nrow = nrow(X_score), ncol = 0))

  for (fac in factor_names) {
    a <- assignment[assignment$primary_factor_raw == fac, , drop = FALSE]
    if (!is.null(keep_col)) a <- a[a[[keep_col]] %in% TRUE, , drop = FALSE]

    vars <- intersect(a$variable, names(X_score))
    n_possible <- length(vars)

    n_col <- paste0("valid_n__", sanitize_tag(fac))
    prop_col <- paste0("valid_prop__", sanitize_tag(fac))

    if (n_possible == 0) {
      validity[[n_col]] <- NA_integer_
      validity[[prop_col]] <- NA_real_
      next
    }

    n_valid <- rowSums(!is.na(X_score[, vars, drop = FALSE]))
    validity[[n_col]] <- n_valid
    validity[[prop_col]] <- n_valid / n_possible
  }

  validity
}

score_with_factor_scores <- function(X_score, bundle, factor_score_method) {
  method <- factor_score_method
  if (tolower(method) == "regression") method <- "Thurstone"

  res <- psych::factor.scores(
    x = X_score,
    f = bundle$efa_object,
    method = method,
    missing = config$factor_scores_missing,
    impute = config$factor_scores_impute
  )

  as.data.frame(res$scores)
}

# Backup option, not used by default. This is not a model-based factor score.
score_with_weighted_composite <- function(X_score, bundle) {
  L <- as.matrix(bundle$loadings)
  L <- L[rownames(L) %in% names(X_score), , drop = FALSE]
  X <- X_score[, rownames(L), drop = FALSE]

  Xz <- as.data.frame(lapply(X, function(v) {
    m <- mean(v, na.rm = TRUE)
    s <- stats::sd(v, na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(rep(NA_real_, length(v)))
    (v - m) / s
  }))

  W <- L
  denom <- colSums(abs(W), na.rm = TRUE)
  denom[denom == 0] <- NA_real_

  S <- as.matrix(Xz) %*% W
  S <- sweep(S, 2, denom, `/`)
  as.data.frame(S)
}

# ---------------------------
# 2) Load bundle and score data
# ---------------------------
if (is.null(config$scoring_bundle_file)) {
  scoring_bundle_file <- find_scoring_bundle(config$efa_run_dir, config$k_target)
} else {
  scoring_bundle_file <- config$scoring_bundle_file
}

if (!file.exists(scoring_bundle_file)) {
  stop("Scoring bundle not found: ", scoring_bundle_file, call. = FALSE)
}

bundle <- readRDS(scoring_bundle_file)
message("Loaded scoring bundle: ", scoring_bundle_file)

required_bundle_fields <- c("k", "training_variables", "efa_object", "loadings", "input_file")
missing_fields <- required_bundle_fields[!required_bundle_fields %in% names(bundle)]
if (length(missing_fields) > 0) {
  stop("Bundle is missing required fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
}

if (!is.null(bundle$efa_engine) && !identical(as.character(bundle$efa_engine), "psych")) {
  stop("This scoring script expects a psych::fa scoring bundle. Bundle efa_engine = ", bundle$efa_engine, call. = FALSE)
}

if (as.integer(bundle$k) != as.integer(config$k_target)) {
  warning("config$k_target = ", config$k_target, " but bundle$k = ", bundle$k, ". Using bundle$k for names.", call. = FALSE)
}

score_data_file <- config$score_data_file
if (is.null(score_data_file)) score_data_file <- bundle$input_file

if (!file.exists(score_data_file)) {
  stop("Score data file not found: ", score_data_file, call. = FALSE)
}

message("Reading score data: ", score_data_file)
df_raw <- read_any_table(score_data_file, sheet = config$score_data_sheet)
df_full <- merge_stratification_metadata(df_raw, bundle)

training_variables <- as.character(bundle$training_variables)
col_match <- match_columns(training_variables, names(df_full))

if (any(!col_match$found)) {
  missing_df <- col_match[!col_match$found, , drop = FALSE]
  stop(
    "The score data file is missing variables required by the EFA scoring bundle:\n",
    paste(missing_df$required, collapse = ", "),
    "\nThis usually means the data file is not the same processing level as the EFA input.",
    call. = FALSE
  )
}

X_score <- df_full[, col_match$matched, drop = FALSE]
names(X_score) <- col_match$required
X_score <- as.data.frame(lapply(X_score, as_numeric_robust))

factor_names <- colnames(as.matrix(bundle$loadings))
if (is.null(factor_names) || any(is.na(factor_names)) || any(factor_names == "")) {
  factor_names <- paste0("Factor", seq_len(as.integer(bundle$k)))
}

# ---------------------------
# 3) Compute scores
# ---------------------------
score_raw <- score_with_factor_scores(
  X_score = X_score,
  bundle = bundle,
  factor_score_method = config$factor_score_method
)

if (ncol(score_raw) != length(factor_names)) {
  warning(
    "Number of score columns does not match number of loading columns. Keeping psych::factor.scores names.",
    call. = FALSE
  )
  factor_names_for_output <- names(score_raw)
} else {
  factor_names_for_output <- factor_names
  names(score_raw) <- factor_names_for_output
}

# Optional sign flips
for (fac in config$reverse_factors) {
  if (fac %in% names(score_raw)) {
    score_raw[[fac]] <- -score_raw[[fac]]
  } else {
    warning("reverse_factors contains unknown factor name: ", fac, call. = FALSE)
  }
}

score_cols <- make_score_colnames(names(score_raw), bundle$k, config$factor_score_method)
names(score_raw) <- score_cols

n_valid_all <- rowSums(!is.na(X_score))
prop_valid_all <- n_valid_all / ncol(X_score)

if (is.finite(config$min_valid_prop_all_scoring_vars) && config$min_valid_prop_all_scoring_vars > 0) {
  too_missing <- prop_valid_all < config$min_valid_prop_all_scoring_vars
  if (any(too_missing)) {
    message(
      "Setting factor scores to NA for ", sum(too_missing),
      " row(s) with prop_valid_all_scoring_vars < ", config$min_valid_prop_all_scoring_vars
    )
    score_raw[too_missing, score_cols] <- NA_real_
  }
}

factor_validity <- make_factor_validity(X_score, bundle)

meta_cols_present <- intersect(config$id_cols_keep, names(df_full))
meta <- df_full[, meta_cols_present, drop = FALSE]

score_tbl <- dplyr::bind_cols(
  meta,
  data.frame(
    scoring_row = seq_len(nrow(df_full)),
    n_valid_all_scoring_vars = n_valid_all,
    prop_valid_all_scoring_vars = prop_valid_all,
    stringsAsFactors = FALSE
  ),
  factor_validity,
  score_raw
)

score_long <- score_tbl |>
  dplyr::select(dplyr::any_of(meta_cols_present), scoring_row, dplyr::all_of(score_cols)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(score_cols),
    names_to = "score_name",
    values_to = "score"
  ) |>
  dplyr::mutate(
    factor_raw = stringr::str_replace(score_name, paste0("^", format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method), "__"), ""),
    factor_raw = stringr::str_replace(factor_raw, "__score$", "")
  )

score_summary <- summarise_scores(score_tbl, score_cols)

score_correlations <- as.data.frame(stats::cor(score_tbl[, score_cols, drop = FALSE], use = "pairwise.complete.obs"))
score_correlations <- tibble::rownames_to_column(score_correlations, "score")

bundle_metadata <- data.frame(
  field = c(
    "scoring_bundle_file", "score_data_file", "k", "created_at_bundle",
    "analysis_level", "subscale_score_version", "correlation_type",
    "efa_engine", "efa_fm", "efa_rotate", "n_training_bundle", "p_training_bundle",
    "factor_score_method", "min_valid_prop_all_scoring_vars", "reverse_factors"
  ),
  value = c(
    scoring_bundle_file, score_data_file, as.character(bundle$k), as.character(bundle$created_at),
    as.character(bundle$analysis_level), as.character(bundle$subscale_score_version), as.character(bundle$correlation_type),
    as.character(bundle$efa_engine), as.character(bundle$efa_fm), as.character(bundle$efa_rotate),
    as.character(bundle$n_training), as.character(bundle$p_training),
    as.character(config$factor_score_method), as.character(config$min_valid_prop_all_scoring_vars),
    paste(config$reverse_factors, collapse = ", ")
  ),
  row.names = NULL
)

assignment_out <- if (!is.null(bundle$assignment)) as.data.frame(bundle$assignment) else data.frame()
factor_map_out <- if (!is.null(bundle$factor_map)) as.data.frame(bundle$factor_map) else data.frame(factor_raw = factor_names)

# ---------------------------
# 4) Write tables
# ---------------------------
if (is.null(config$out_dir)) {
  out_dir <- file.path(
    config$efa_run_dir,
    "person_scores",
    paste0(format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method))
  )
} else {
  out_dir <- config$out_dir
}

plot_dir <- file.path(out_dir, "histograms")
safe_mkdir(out_dir)
safe_mkdir(plot_dir)

out_stem <- paste0(format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method), "__person_factor_scores")

csv_file <- file.path(out_dir, paste0(out_stem, ".csv"))
long_csv_file <- file.path(out_dir, paste0(out_stem, "__long.csv"))
xlsx_file <- file.path(out_dir, paste0(out_stem, ".xlsx"))

readr::write_csv(score_tbl, csv_file)
readr::write_csv(score_long, long_csv_file)

xlsx_sheets <- list(
  person_factor_scores = score_tbl,
  person_factor_scores_long = score_long,
  score_summary = score_summary,
  score_correlations = score_correlations,
  factor_map = factor_map_out,
  assignment_from_bundle = assignment_out,
  bundle_metadata = bundle_metadata,
  variable_matching = col_match
)

writexl::write_xlsx(xlsx_sheets, xlsx_file)

message("Saved score table: ", xlsx_file)

# ---------------------------
# 5) Histograms
# ---------------------------
plot_df <- score_tbl |>
  dplyr::select(dplyr::any_of(c(meta_cols_present, config$hist_by)), dplyr::all_of(score_cols)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(score_cols),
    names_to = "score_name",
    values_to = "score"
  ) |>
  dplyr::mutate(
    factor = stringr::str_replace(score_name, paste0("^", format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method), "__"), ""),
    factor = stringr::str_replace(factor, "__score$", "")
  )

for (fac in unique(plot_df$factor)) {
  pdat <- plot_df |> dplyr::filter(factor == fac)

  p <- ggplot(pdat, aes(x = score)) +
    geom_histogram(bins = 30, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Distribution of person factor scores: ", fac),
      subtitle = paste0("k = ", bundle$k, "; method = ", config$factor_score_method),
      x = "Factor score",
      y = "Count"
    )

  if (!is.null(config$hist_by) && config$hist_by %in% names(pdat)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", config$hist_by)))
  }

  ggsave(
    filename = file.path(plot_dir, paste0("hist__", format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method), "__", sanitize_tag(fac), ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

p_all <- ggplot(plot_df, aes(x = score)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~factor, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of person factor scores",
    subtitle = paste0("k = ", bundle$k, "; method = ", config$factor_score_method),
    x = "Factor score",
    y = "Count"
  )

ggsave(
  filename = file.path(plot_dir, paste0("hist__", format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method), "__all_factors.png")),
  plot = p_all,
  width = 10,
  height = 7,
  dpi = 300
)

# Optional score-correlation heatmap
cor_long <- score_correlations |>
  tidyr::pivot_longer(cols = -score, names_to = "score2", values_to = "r")

p_cor <- ggplot(cor_long, aes(x = score2, y = score, fill = r)) +
  geom_tile(color = "grey85") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank()) +
  labs(
    title = "Correlations among person factor scores",
    x = NULL,
    y = NULL,
    fill = "r"
  )

ggsave(
  filename = file.path(plot_dir, paste0("score_correlations__", format_k_single(bundle$k), "__", sanitize_tag(config$factor_score_method), ".png")),
  plot = p_cor,
  width = 8,
  height = 6,
  dpi = 300
)

message("Saved histograms in: ", plot_dir)
message("Done.")
