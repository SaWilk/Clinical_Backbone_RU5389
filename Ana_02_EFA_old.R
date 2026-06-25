# ============================================================
# Exploratory Factor Analysis (EFA) Pipeline (config-driven)
# Multi-k version
#
# What this version does:
# - Reads HiTOP item/subscale data.
# - Optionally merges stratification info.
# - Splits into training/test.
# - Computes one training correlation matrix.
# - Runs EFA for multiple factor numbers k in sequence.
# - Writes k-specific outputs with date, dataset tag, and k in filenames.
# - Produces improved correlation heatmap, factor-correlation heatmaps, and Tucker congruence plots.
# - Merges item information/codebook metadata into loading/assignment outputs.
# - Handles zero-variance / bad-correlation variables explicitly and logs dropped items.
# - Keeps interpretation outputs separate from debug/raw outputs.
# - Uses raw psych::fa factor names (MR1, MR2, ...) throughout; no extra F-label layer.
# - v9: keeps repeated Tucker dots with x-jitter; sets Tucker good=.85 and excellent=.95; fixes duplicate factor levels.
# ============================================================


# ---------------------------
# 0) CONFIG
# ---------------------------
config <- list(

  # Project root:
  # If NULL, the script tries to auto-detect the R project root.
  root = NULL,

  # Analysis level:
  # "items": ordinal questionnaire items, usually polychoric correlations.
  # "subscales": continuous/approximately continuous scores, usually Pearson correlations.
  # "diagnoses": theoretical binary branch, currently not implemented.
  analysis_level = "subscales",  # c("items", "subscales", "diagnoses")

  # Input files, relative to ROOT/03_analysis_input.
  input = list(
    dir_rel = "03_analysis_input",
    file_items = "adults_adolescents_HiTOP_items.xlsx",
    file_subscales = "adults_adolescents_HiTOP_subscales_lt030.xlsx"
  ),

  # Item information / codebook.
  # Expected default location: ROOT/information/2025-10-28_Item_Information_Adults.xlsx
  # The script joins this to loadings, item assignments, marker tables, and readable plots.
  codebook = list(
    use = TRUE,
    path_rel = file.path("information", "2025-10-28_Item_Information_Adults.xlsx"),
    sheet = "Items",
    variable_col = "Item",
    label_columns = c("Scale", "Higher-order subscale", "Subscale"),
    domain_columns = c("HiTOP-Superspectrum", "HiTOP-Spectrum", "HiTOP-Subfactor", "HiTOP-Subscale"),
    reverse_col = "ReverseCoded"
  ),

  # Optional stratification info, e.g. project variable.
  stratification = list(
    use = TRUE,
    path_rel = file.path("03_analysis_input", "analysis_ready", "stratification_info.xlsx"),
    merge_method = "auto",  # c("auto", "by_key", "by_row_order")
    key = NULL,
    strata_var = c("group", "age", "gender")
  ),

  # Training/test split.
  split = list(
    do_split = TRUE,
    seed = 20251216,
    prop = 0.50,   # 50/50 gives a real train/test split; use repeated splits later for stronger stability claims.
    strata_var = "project"
  ),

  # Correlation matrix choice.
  correlation = list(
    type = "pearson",       # c("auto", "polychoric", "pearson", "tetrachoric")
    use_pairwise = TRUE,
    smooth = TRUE,
    mc_cores = 2
  ),

  # Adequacy checks.
  adequacy = list(
    kmo_min = 0.60,
    bartlett_alpha = 0.05,
    halt_on_fail = TRUE
  ),

  # Sample size / stability heuristics.
  stability = list(
    min_n = 200,
    min_n_per_var = 5,
    warn_only = TRUE
  ),

  # Variable selection.
  variables = list(
    include_names = NULL,
    include_regex = NULL,
    exclude_names = NULL,
    exclude_regex = NULL
  ),

  # Subscale score selection.
  # If analysis_level = "subscales", the input file may contain both raw subscale
  # scores and z-scored duplicates named like z_score_*. The EFA must use either
  # raw OR z-scored columns, never both, otherwise every subscale is duplicated and
  # the correlation matrix becomes distorted/singular.
  # Recommended for Pearson-correlation EFA: "raw". Pearson correlations are scale-invariant.
  subscales = list(
    score_version = "raw",      # c("raw", "z")
    z_prefix = "z_score_",

    # For subscale analyses, use only score columns and avoid mixing questionnaire
    # totals with their own lower-order subscales.
    # include_global_scores:
    #   "none" = only columns with double-underscore subscales, e.g. score_idas__panic
    #   "all" = include all global questionnaire scores too, e.g. score_idas
    #   "only_without_subscales" = keep global scores only for scales that have no
    #      lower-order score_*__* columns in the file, e.g. score_asrs or score_map_sr.
    include_only_score_columns = TRUE,
    include_global_scores = "only_without_subscales",  # c("none", "all", "only_without_subscales")

    # Optional manual override. Usually leave NULL.
    always_include_names = NULL,
    always_exclude_names = NULL
  ),

  # Factor retention / k handling.
  # Main switch for multi-k:
  # - Set k_values = c(2, 3, 4, 5, 6) to run several solutions.
  # - Set k_values = NULL and k_fixed = 3 to run one fixed solution.
  # - Set both NULL and prompt_for_k = TRUE for interactive selection.
  retention = list(
    method = "scree_cng",       # c("scree_cng", "parallel")
    prompt_for_k = FALSE,
    k_fixed = NULL,
    k_values = c(4),
    parallel = list(
      n_iter = 50,
      fa = "fa"
    )
  ),

  # EFA estimator and rotation. Factor scores are intentionally NOT computed here;
  # use the exported scoring bundles in a separate scoring script after choosing a solution.
  efa = list(
    engine = "psych",           # c("psych", "lavaan")
    fm = "ml",              # "minres" is safer for smoothed/polychoric matrices than "ml"
    rotate = "oblimin",
    lavaan = list(
      estimator = "minres",
      rotation = "promax"
    )
  ),

  # Hierarchy options.
  hierarchy = list(
    method = "none",            # c("none", "bass_ackwards", "latent_model")
    bass_ackwards = list(
      max_factors = 6
    )
  ),

  # Test-sample validation.
  validation = list(
    refit_on_test = TRUE,
    congruence = TRUE,
    score_correlations = FALSE,
    repeated_splits = list(
      enabled = TRUE,
      n_splits = 30,
      prop = 0.50,
      seed = 20260604,
      strata_var = "project",
      # Runtime optimisation: within each split, compute train/test correlations once,
      # then reuse them for all k values. This saves most time for polychoric item EFAs.
      cache_correlations_across_k = TRUE,
      suppress_split_messages = TRUE,
      progress_every = 1,
      write_raw_split_rows = FALSE
    ),
    tucker_cutoffs = list(
      # Practical exploratory stability thresholds.
      # good = minimum "good enough" Tucker congruence; excellent = very strong/near-identical replication.
      good = 0.85,
      excellent = 0.95
    )
  ),

  # Reliability report.
  reliability = list(
    loading_cutoff = 0.30,
    crossloading_cutoff = 0.20,
    min_items_per_factor = 3,
    compute_alpha = TRUE,
    compute_omega = TRUE
  ),

  # Item-to-factor assignment rules.
  # strict: very clean simple structure, used for reliability by default.
  # interpretive: more permissive; useful for reading factors and plotting marker items.
  assignment = list(
    reliability_rule = "strict",   # c("strict", "interpretive")
    strict = list(
      primary_abs_min = 0.30,
      second_abs_max = 0.20
    ),
    interpretive = list(
      primary_abs_min = 0.30,
      gap_min = 0.20
    )
  ),

  # Plot settings.
  plots = list(
    correlation_heatmap = list(
      max_p = 250,
      show_cell_values_max_p = 50,
      width = 20,
      height = 18,
      dpi = 400,
      axis_text_size = 2.8
    ),
    factor_diagram = list(
      make_debug_diagram = FALSE,
      width = 4200,
      height = 3000,
      res = 350,
      cex = 0.38,
      cut = 0.30,
      simple = TRUE
    ),
    loading_marker_plot = list(
      enabled = FALSE,
      top_n_per_factor = 30,
      width = 13,
      min_height = 6,
      height_per_item = 0.24,
      dpi = 350
    ),
    loading_heatmap = list(
      use_rule = "interpretive",  # c("strict", "interpretive", "all")
      width = 13,
      min_height = 8,
      height_per_item = 0.12,
      dpi = 350,
      axis_text_size = 4.0
    ),
    congruence_heatmap = list(
      width = 8,
      height = 6,
      dpi = 350,
      text_size = 4
    ),
    factor_correlation_heatmap = list(
      width = 7,
      height = 6,
      dpi = 350,
      text_size = 4
    )
  ),

  # Output locations.
  output = list(
    dir_rel = file.path("out", "factor_analysis", "efa"),
    summary_subdir = "summary",
    tables_subdir = "tables",
    plots_subdir = "plots",
    write_debug = FALSE,
    debug_subdir = "debug_archive",
    raw_subdir = file.path("debug_archive", "raw_objects")
  )
)


# ---------------------------
# 1) Packages + utilities
# ---------------------------
required_pkgs <- c(
  "readxl", "writexl", "dplyr", "tibble", "rsample",
  "psych", "ggplot2", "scales"
)

optional_pkgs <- c(
  "here", "rprojroot", "nFactors", "corrplot", "stringr"
)

check_packages <- function(pkgs, optional = FALSE) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    msg <- paste0(
      if (optional) "Optional" else "Required",
      " packages missing: ", paste(missing, collapse = ", "),
      "\nInstall with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))"
    )
    if (optional) {
      message(msg)
    } else {
      stop(msg, call. = FALSE)
    }
  }
}

check_packages(required_pkgs, optional = FALSE)
check_packages(optional_pkgs, optional = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tibble)
  library(rsample)
  library(psych)
  library(ggplot2)
})

safe_mkdir <- function(path) {
  if (is.null(path) || is.na(path) || trimws(path) == "") return(invisible(FALSE))

  # Network drives can occasionally fail silently when showWarnings = FALSE.
  # So create, then verify, and try once more before stopping with a useful error.
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = TRUE)
  }

  if (!dir.exists(path)) {
    Sys.sleep(0.2)
    dir.create(path, recursive = TRUE, showWarnings = TRUE)
  }

  if (!dir.exists(path)) {
    stop("Could not create output directory: ", path, call. = FALSE)
  }

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

truncate_tag <- function(x, max_chars = 50) {
  x <- sanitize_tag(x)
  if (nchar(x) <= max_chars) return(x)
  substr(x, 1, max_chars)
}

format_kset <- function(k_values) {
  k_values <- sort(unique(as.integer(k_values)))
  k_values <- k_values[is.finite(k_values) & !is.na(k_values) & k_values >= 1]
  if (length(k_values) == 0) return("kauto")
  paste0("kset_", paste(k_values, collapse = "-"))
}

format_k_single <- function(k) {
  paste0("k", sprintf("%02d", as.integer(k)))
}

format_decimal_for_tag <- function(x) {
  x <- as.character(x)
  x <- gsub("\\.", "p", x)
  x <- gsub("[^A-Za-z0-9p]+", "", x)
  x
}

make_threshold_tag <- function() {
  paste0(
    "strictL", format_decimal_for_tag(config$assignment$strict$primary_abs_min),
    "S", format_decimal_for_tag(config$assignment$strict$second_abs_max),
    "__interpL", format_decimal_for_tag(config$assignment$interpretive$primary_abs_min),
    "G", format_decimal_for_tag(config$assignment$interpretive$gap_min)
  )
}

make_analysis_option_tag <- function() {
  base <- paste0(
    sanitize_tag(config$efa$engine),
    "_", sanitize_tag(config$efa$fm),
    "_", sanitize_tag(config$efa$rotate)
  )

  # For subscale analyses, also tag whether raw or z-scored subscales were used
  # and how global questionnaire totals were handled.
  if (identical(config$analysis_level, "subscales")) {
    score_version <- if (!is.null(config$subscales$score_version)) config$subscales$score_version else "raw"
    global_mode <- if (!is.null(config$subscales$include_global_scores)) config$subscales$include_global_scores else "only_without_subscales"
    global_tag <- switch(
      as.character(global_mode),
      none = "g0",
      all = "gall",
      only_without_subscales = "gauto",
      "gauto"
    )
    base <- paste0(base, "_", sanitize_tag(paste0("sub", score_version, "_", global_tag)))
  }

  base
}

normalize_variable_key <- function(x) {
  # Robust matching between data columns and codebook names.
  # Examples: IDAS[001] -> idas001; CAPEfreq[031] -> capefreq031; ASRS[001] -> asrs001.
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\[[[:space:]]*([0-9]+)[[:space:]]*\\]", "\\1", x)
  x <- gsub("[^a-z0-9]+", "", x)
  x
}

factor_number <- function(x) {
  x <- as.character(x)
  suppressWarnings(as.integer(sub("^.*?([0-9]+)$", "\\1", x)))
}

sort_factor_names <- function(x) {
  x <- as.character(x)
  nums <- factor_number(x)
  if (all(is.na(nums))) return(sort(x))
  x[order(ifelse(is.na(nums), Inf, nums), x)]
}

factor_raw_to_factor <- function(x) {
  # Keep the raw psych::fa factor names as the reporting IDs.
  # With fm = "minres", psych labels factors MR1, MR2, ... .
  # Crossloading rules assign ITEMS to these MR factors; they do not create new factors.
  as.character(x)
}

factor_raw_to_label <- function(x) {
  # No separate F1/F2 display layer: use MR1, MR2, ... everywhere.
  as.character(x)
}

order_tucker_matrix <- function(cong) {
  cong <- as.matrix(cong)
  row_ord <- sort_factor_names(rownames(cong))
  col_ord <- sort_factor_names(colnames(cong))
  cong[row_ord, col_ord, drop = FALSE]
}

get_project_root <- function() {
  if (requireNamespace("here", quietly = TRUE)) {
    root <- tryCatch(here::here(), error = function(e) NULL)
    if (!is.null(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
  }
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_rstudio_project), error = function(e) NULL)
    if (!is.null(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
    root <- tryCatch(rprojroot::find_root(rprojroot::is_git_root), error = function(e) NULL)
    if (!is.null(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

pause_or_stop <- function(msg, halt = TRUE) {
  warning(msg, call. = FALSE)
  if (!halt) return(invisible(TRUE))

  if (interactive()) {
    ans <- readline("Adequacy gate failed. Type 'YES' to continue anyway, anything else to stop: ")
    if (toupper(trimws(ans)) == "YES") return(invisible(TRUE))
    stop("Stopped due to adequacy gate failure.", call. = FALSE)
  } else {
    stop("Stopped due to adequacy gate failure (non-interactive run).", call. = FALSE)
  }
}

save_plot_png <- function(p, filename, width = 9, height = 6, dpi = 200) {
  safe_mkdir(dirname(filename))
  ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi)
}


# ---------------------------
# 2) Resolve paths + read data
# ---------------------------
if (is.null(config$root)) {
  config$root <- get_project_root()
} else {
  config$root <- normalizePath(config$root, winslash = "/", mustWork = FALSE)
}

message("Project ROOT = ", config$root)

input_dir <- file.path(config$root, config$input$dir_rel)

get_input_path <- function() {
  if (config$analysis_level == "items") {
    return(file.path(input_dir, config$input$file_items))
  } else if (config$analysis_level == "subscales") {
    return(file.path(input_dir, config$input$file_subscales))
  } else if (config$analysis_level == "diagnoses") {
    stop("Diagnosis-level branch is not implemented because no diagnoses input file is defined yet.", call. = FALSE)
  } else {
    stop("Unknown analysis_level: ", config$analysis_level, call. = FALSE)
  }
}

data_path <- get_input_path()
if (!file.exists(data_path)) stop("Input file not found: ", data_path, call. = FALSE)

run_date <- format(Sys.Date(), "%Y-%m-%d")

# Keep full tags for metadata, but use shortened tags in paths.
# This avoids Windows/K-drive MAX_PATH problems.
data_tag_full <- sanitize_tag(basename(data_path))
data_tag <- truncate_tag(data_tag_full, max_chars = 42)

strat_path_full <- file.path(config$root, config$stratification$path_rel)
strat_tag_full <- if (isTRUE(config$stratification$use) && file.exists(strat_path_full)) {
  paste0("strat_", sanitize_tag(basename(strat_path_full)))
} else {
  NULL
}
strat_tag <- if (!is.null(strat_tag_full)) truncate_tag(strat_tag_full, max_chars = 20) else NULL

configured_k_values <- if (!is.null(config$retention$k_values)) {
  config$retention$k_values
} else if (!is.null(config$retention$k_fixed)) {
  config$retention$k_fixed
} else {
  integer(0)
}

dataset_tag_full <- paste(c(data_tag_full, strat_tag_full), collapse = "__")
dataset_tag <- paste(c(data_tag, strat_tag), collapse = "__")
kset_tag <- format_kset(configured_k_values)
threshold_tag <- make_threshold_tag()
analysis_option_tag <- make_analysis_option_tag()

# Short run folder and shorter file prefix.
run_tag <- paste(
  run_date,
  config$analysis_level,
  dataset_tag,
  kset_tag,
  sep = "__"
)

file_prefix <- paste(
  run_date,
  config$analysis_level,
  dataset_tag,
  sep = "__"
)

base_out_dir <- file.path(config$root, config$output$dir_rel)
out_dir <- file.path(base_out_dir, run_tag)
summary_dir <- file.path(out_dir, config$output$summary_subdir)
tables_dir <- file.path(out_dir, config$output$tables_subdir)
plots_dir <- file.path(out_dir, config$output$plots_subdir)
debug_dir <- file.path(out_dir, config$output$debug_subdir)
raw_dir <- file.path(out_dir, config$output$raw_subdir)

write_debug_outputs <- isTRUE(config$output$write_debug)

for (.d in c(out_dir, summary_dir, tables_dir, plots_dir)) {
  safe_mkdir(.d)
}

if (write_debug_outputs) {
  for (.d in c(debug_dir, raw_dir)) {
    safe_mkdir(.d)
  }
}

shorten_out_stem <- function(stem, max_chars = 45) {
  stem0 <- sanitize_tag(stem)

  # Keep filenames short enough for Windows/K-drive and libxlsxwriter.
  # The full thresholds and settings are stored in run_metadata/config_snapshot;
  # filenames keep only k + engine/fm/rotation + a compact output name.
  stem_map <- c(
    correlation_matrix_training_with_codebook_labels = "corr_train_labels",
    correlation_matrix_training_label_map = "corr_train_labelmap",
    correlation_matrix_training = "corr_train",
    correlation_matrix_test = "corr_test",
    correlation_heatmap_training = "corr_heatmap_train",
    scree_plot_cng_training = "scree_cng",
    parallel_analysis_training = "parallel",
    run_metadata = "metadata",
    multi_k_run_summary = "multi_k_summary",
    efa_training_solution_readable = "efa_solution",
    efa_solution_bundle_for_scoring = "scoring_bundle",
    efa_solution_bundle_for_scoring_README = "scoring_bundle_readme",
    item_assignment_strict_and_interpretive_with_codebook = "item_assignment",
    reliability_summary_train_and_test = "reliability",
    reliability_and_assignment = "reliability_assignment",
    decision_summary = "decision_summary",
    loading_heatmap_readable_interpretive = "loadings_interpretive",
    loading_heatmap_readable_strict = "loadings_strict",
    loading_heatmap_readable_all = "loadings_all",
    factor_correlation_matrix = "factor_corr",
    factor_correlation_matrix_heatmap = "factor_corr_heatmap",
    tucker_congruence_train_vs_test_raw = "tucker_raw_matrix",
    tucker_congruence_summary_best_abs_matches = "tucker_best_matches",
    tucker_congruence_one_to_one_matching = "tucker_one_to_one",
    tucker_congruence_heatmap_raw_consistent_order = "tucker_raw_heatmap",
    tucker_congruence_heatmap_one_to_one_matched = "tucker_matched_heatmap",
    repeated_split_tucker_summary = "rep_tucker_summary",
    repeated_split_tucker_factor_summary = "rep_tucker_by_factor",
    repeated_split_tucker_overall_summary = "rep_tucker_overall",
    repeated_split_tucker_raw_matches = "rep_tucker_raw",
    repeated_split_tucker_failures = "rep_tucker_failures",
    manifest_variables_selected_training = "selected_vars_train",
    manifest_variables_selected_test = "selected_vars_test"
  )

  if (stem0 %in% names(stem_map)) {
    return(unname(stem_map[[stem0]]))
  }

  if (nchar(stem0) <= max_chars) return(stem0)
  substr(stem0, 1, max_chars)
}

make_outfile <- function(stem, ext, k_val = NULL, dir = tables_dir) {
  k_part <- if (is.null(k_val)) {
    kset_tag
  } else {
    format_k_single(k_val)
  }

  # IMPORTANT: keep filenames short. On Windows/K-drive, libxlsxwriter often fails
  # once the full path exceeds ~260 characters and reports it misleadingly as
  # "No such file or directory" / "permissions error".
  stem_tag <- shorten_out_stem(stem)
  filename <- paste0(k_part, "__", analysis_option_tag, "__", stem_tag, ".", ext)

  if (!write_debug_outputs && normalizePath(dir, winslash = "/", mustWork = FALSE) %in%
      normalizePath(c(debug_dir, raw_dir), winslash = "/", mustWork = FALSE)) {
    # Keep legacy calls harmless without creating debug_archive.
    outfile <- file.path(tempdir(), filename)
  } else {
    outfile <- file.path(dir, filename)
    safe_mkdir(dirname(outfile))
  }
  outfile
}

safe_write_csv <- function(x, file, row.names = FALSE, ...) {
  # Drop debug/raw writes by default to keep the output folder readable.
  if (!write_debug_outputs) {
    f_norm <- normalizePath(file, winslash = "/", mustWork = FALSE)
    dbg_norm <- normalizePath(debug_dir, winslash = "/", mustWork = FALSE)
    raw_norm <- normalizePath(raw_dir, winslash = "/", mustWork = FALSE)
    if (startsWith(f_norm, dbg_norm) || startsWith(f_norm, raw_norm) || startsWith(f_norm, normalizePath(tempdir(), winslash = "/", mustWork = FALSE))) {
      return(invisible(FALSE))
    }
  }
  safe_mkdir(dirname(file))
  utils::write.csv(x, file = file, row.names = row.names, ...)
}

append_log <- function(..., logfile = "efa_pipeline_warnings_log.txt") {
  cat(
    paste0(..., collapse = ""),
    "\n",
    file = file.path(out_dir, logfile),
    append = TRUE
  )
}

should_write_debug <- function(out_dir_arg = NULL) {
  !is.null(out_dir_arg) && isTRUE(write_debug_outputs)
}

clean_colname <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

first_existing_col <- function(nms, candidates) {
  candidates <- clean_colname(candidates)
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

collapse_nonempty <- function(x, sep = " | ") {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  paste(unique(x), collapse = sep)
}

read_item_information <- function() {
  if (!isTRUE(config$codebook$use)) return(NULL)

  path <- file.path(config$root, config$codebook$path_rel)
  if (!file.exists(path)) {
    message("Item information/codebook file not found. Continuing without codebook: ", path)
    append_log(Sys.time(), " | Codebook missing: ", path)
    return(NULL)
  }

  info <- readxl::read_excel(path, sheet = config$codebook$sheet) |> as.data.frame()
  names(info) <- clean_colname(names(info))

  variable_col <- clean_colname(config$codebook$variable_col)
  if (!(variable_col %in% names(info))) {
    variable_col <- first_existing_col(names(info), c("Item", "variable", "item_name", "name"))
  }
  if (is.na(variable_col) || !(variable_col %in% names(info))) {
    message("Could not identify item/variable column in codebook. Continuing without codebook merge.")
    append_log(Sys.time(), " | Could not identify codebook item column.")
    return(NULL)
  }

  info$variable <- as.character(info[[variable_col]])
  info$variable_key <- normalize_variable_key(info$variable)
  info <- info[!is.na(info$variable_key) & info$variable_key != "", , drop = FALSE]

  # Standardize common fields if present.
  alias_map <- list(
    codebook_scale = c("scale"),
    codebook_higher_order_subscale = c("higher_order_subscale", "higher_order_scale"),
    codebook_subscale = c("subscale"),
    hitop_superspectrum = c("hitop_superspectrum"),
    hitop_spectrum = c("hitop_spectrum"),
    hitop_subfactor = c("hitop_subfactor"),
    hitop_subscale = c("hitop_subscale"),
    rdoc_system = c("rdoc_system"),
    rdoc_construct = c("rdoc_construct"),
    reverse_coded = c("reversecoded", "reverse_coded")
  )

  for (std in names(alias_map)) {
    col <- first_existing_col(names(info), alias_map[[std]])
    if (!is.na(col) && col %in% names(info)) {
      info[[std]] <- info[[col]]
    } else {
      info[[std]] <- NA
    }
  }

  keep_cols <- c(
    "variable_key", "variable",
    "codebook_scale", "codebook_higher_order_subscale", "codebook_subscale",
    "hitop_superspectrum", "hitop_spectrum", "hitop_subfactor", "hitop_subscale",
    "rdoc_system", "rdoc_construct", "reverse_coded"
  )
  info <- info[, keep_cols, drop = FALSE]

  # If duplicate rows exist for the same variable, collapse metadata to keep the join one-to-one.
  info <- info |>
    dplyr::group_by(variable_key) |>
    dplyr::summarise(
      codebook_variable = collapse_nonempty(variable),
      codebook_scale = collapse_nonempty(codebook_scale),
      codebook_higher_order_subscale = collapse_nonempty(codebook_higher_order_subscale),
      codebook_subscale = collapse_nonempty(codebook_subscale),
      hitop_superspectrum = collapse_nonempty(hitop_superspectrum),
      hitop_spectrum = collapse_nonempty(hitop_spectrum),
      hitop_subfactor = collapse_nonempty(hitop_subfactor),
      hitop_subscale = collapse_nonempty(hitop_subscale),
      rdoc_system = collapse_nonempty(rdoc_system),
      rdoc_construct = collapse_nonempty(rdoc_construct),
      reverse_coded = collapse_nonempty(reverse_coded),
      .groups = "drop"
    )

  message("Loaded item information/codebook: ", basename(path), " | mapped variables = ", nrow(info))
  info
}

enrich_with_item_info <- function(df_in, item_info = NULL) {
  df_in <- as.data.frame(df_in)
  if (!("variable" %in% names(df_in))) return(df_in)
  if (is.null(item_info)) {
    df_in$codebook_variable <- NA_character_
    df_in$codebook_scale <- NA_character_
    df_in$codebook_higher_order_subscale <- NA_character_
    df_in$codebook_subscale <- NA_character_
    df_in$hitop_superspectrum <- NA_character_
    df_in$hitop_spectrum <- NA_character_
    df_in$hitop_subfactor <- NA_character_
    df_in$hitop_subscale <- NA_character_
    df_in$rdoc_system <- NA_character_
    df_in$rdoc_construct <- NA_character_
    df_in$reverse_coded <- NA_character_
    return(df_in)
  }

  df_in$variable_key <- normalize_variable_key(df_in$variable)
  out <- dplyr::left_join(df_in, item_info, by = "variable_key")
  out$variable_key <- NULL
  out
}

make_item_plot_label <- function(df_in, max_chars = 120) {
  v <- as.character(df_in$variable)
  scale <- if ("codebook_scale" %in% names(df_in)) as.character(df_in$codebook_scale) else NA_character_
  hos <- if ("codebook_higher_order_subscale" %in% names(df_in)) as.character(df_in$codebook_higher_order_subscale) else NA_character_
  sub <- if ("codebook_subscale" %in% names(df_in)) as.character(df_in$codebook_subscale) else NA_character_
  hitop <- if ("hitop_subscale" %in% names(df_in)) as.character(df_in$hitop_subscale) else NA_character_

  # The Subscale column is the most useful human-readable grouping for these plots.
  # Therefore it is shown early and for all scales, not only for SUQ.
  meta <- mapply(
    function(a, b, c, d) collapse_nonempty(c(a, c, b, d), sep = " / "),
    scale, hos, sub, hitop,
    USE.NAMES = FALSE
  )

  lab <- ifelse(!is.na(meta) & meta != "", paste0(v, " [", meta, "]"), v)
  lab <- ifelse(nchar(lab) > max_chars, paste0(substr(lab, 1, max_chars - 3), "..."), lab)
  lab
}

make_variable_label_map <- function(vars, item_info = NULL, max_chars = 220) {
  df_lab <- data.frame(variable = vars, row.names = NULL)
  df_lab <- enrich_with_item_info(df_lab, item_info)
  df_lab$label <- make_item_plot_label(df_lab, max_chars = max_chars)
  df_lab
}

write_correlation_matrix_with_labels <- function(R, item_info = NULL, label = "training") {
  lab_map <- make_variable_label_map(colnames(R), item_info, max_chars = 220)
  labels <- make.unique(lab_map$label, sep = "__")

  R_labeled <- as.data.frame(R)
  names(R_labeled) <- labels
  R_labeled <- tibble::rownames_to_column(R_labeled, "variable")
  R_labeled$variable_label <- labels[match(R_labeled$variable, lab_map$variable)]
  R_labeled <- R_labeled[, c("variable", "variable_label", labels), drop = FALSE]

  writexl::write_xlsx(
    list(
      correlation_matrix_labeled = R_labeled,
      variable_label_map = lab_map
    ),
    make_outfile(paste0("correlation_matrix_", label, "_with_codebook_labels"), "xlsx", dir = tables_dir)
  )

  safe_write_csv(
    lab_map,
    make_outfile(paste0("correlation_matrix_", label, "_label_map"), "csv", dir = tables_dir),
    row.names = FALSE
  )
}

df <- readxl::read_excel(data_path) |> as.data.frame()
item_info <- read_item_information()

message("Loaded data: ", basename(data_path), " | N = ", nrow(df), " | p = ", ncol(df))


# ---------------------------
# 2b) Merge in stratification_info.xlsx
# ---------------------------
merge_stratification <- function(df) {
  if (!isTRUE(config$stratification$use)) return(df)

  strat_path <- file.path(config$root, config$stratification$path_rel)
  if (!file.exists(strat_path)) {
    message("Stratification file not found. Continuing without: ", strat_path)
    return(df)
  }

  strat <- readxl::read_excel(strat_path) |> as.data.frame()
  message("Loaded stratification info: ", basename(strat_path), " | N = ", nrow(strat), " | p = ", ncol(strat))

  method <- config$stratification$merge_method

  if (method == "by_key") {
    key <- config$stratification$key
    if (is.null(key)) stop("merge_method='by_key' but config$stratification$key is NULL", call. = FALSE)
    if (!(key %in% names(df)) || !(key %in% names(strat))) stop("Key column not found in both df and strat: ", key, call. = FALSE)
    return(dplyr::left_join(df, strat, by = key))
  }

  if (method == "by_row_order") {
    if (nrow(df) != nrow(strat)) stop("Row-order merge requested but nrow(df) != nrow(strat).", call. = FALSE)
    dup <- intersect(names(df), names(strat))
    if (length(dup) > 0) strat <- strat |> dplyr::select(-dplyr::all_of(dup))
    return(bind_cols(df, strat))
  }

  key_candidates <- intersect(names(df), names(strat))
  if (!is.null(config$stratification$key) && config$stratification$key %in% key_candidates) {
    return(dplyr::left_join(df, strat, by = config$stratification$key))
  }

  id_like <- key_candidates[grepl("(^id$|participant|subject|case)", key_candidates, ignore.case = TRUE)]
  if (length(id_like) >= 1) {
    key <- id_like[1]
    message("Auto-merge: joining by key = ", key)
    return(dplyr::left_join(df, strat, by = key))
  }

  if (nrow(df) == nrow(strat)) {
    message("Auto-merge: no shared key found; binding stratification info by row order.")
    dup <- intersect(names(df), names(strat))
    if (length(dup) > 0) strat <- strat |> dplyr::select(-dplyr::all_of(dup))
    return(bind_cols(df, strat))
  }

  message("Auto-merge failed. Continuing without stratification info.")
  df
}

df <- merge_stratification(df)


# ---------------------------
# 3) Split into training/test
# ---------------------------
if (isTRUE(config$split$do_split)) {
  set.seed(config$split$seed)

  strata_var <- config$split$strata_var
  if (!is.null(strata_var) && !(strata_var %in% names(df))) {
    message("Requested stratification variable '", strata_var, "' not found. Disabling stratification.")
    strata_var <- NULL
  }

  if (is.null(strata_var)) {
    sp <- rsample::initial_split(df, prop = config$split$prop)
  } else {
    sp <- rsample::initial_split(df, prop = config$split$prop, strata = all_of(strata_var))
  }

  df_training <- rsample::training(sp)
  df_test <- rsample::testing(sp)

} else {
  df_training <- df
  df_test <- NULL
}

message("Training N = ", nrow(df_training), if (!is.null(df_test)) paste0(" | Test N = ", nrow(df_test)) else "")


# ---------------------------
# 4) Prepare analysis variables
# ---------------------------
non_manifest_patterns <- c("^id$", "(^|_)id$", "^vp_id$", "^vpid$", "timestamp", "date", "time")

is_manifest <- function(x, nm) {
  if (is.character(x) || is.logical(x)) return(FALSE)
  if (any(grepl(paste(non_manifest_patterns, collapse = "|"), nm, ignore.case = TRUE))) return(FALSE)
  TRUE
}

filter_subscale_score_columns <- function(X) {
  if (!identical(config$analysis_level, "subscales")) return(X)

  X_original <- X
  original_names <- names(X_original)

  score_version <- tolower(trimws(as.character(config$subscales$score_version %||% "raw")))
  z_prefix <- as.character(config$subscales$z_prefix %||% "z_score_")
  include_only_score_columns <- isTRUE(config$subscales$include_only_score_columns %||% TRUE)
  global_mode <- tolower(trimws(as.character(config$subscales$include_global_scores %||% "only_without_subscales")))

  if (!(score_version %in% c("raw", "z"))) {
    stop(
      "config$subscales$score_version must be either 'raw' or 'z'. Current value: ",
      score_version,
      call. = FALSE
    )
  }

  if (!(global_mode %in% c("none", "all", "only_without_subscales"))) {
    stop(
      "config$subscales$include_global_scores must be 'none', 'all', or 'only_without_subscales'. Current value: ",
      global_mode,
      call. = FALSE
    )
  }

  z_cols <- startsWith(names(X), z_prefix)

  if (score_version == "raw") {
    if (any(z_cols)) {
      message(
        "Subscale mode: using RAW score columns and excluding ", sum(z_cols),
        " z-scored duplicate columns with prefix '", z_prefix, "'."
      )
    }
    X <- X[, !z_cols, drop = FALSE]
  }

  if (score_version == "z") {
    if (!any(z_cols)) {
      stop(
        "Subscale mode requested score_version = 'z', but no columns with prefix '",
        z_prefix, "' were found.",
        call. = FALSE
      )
    }
    message(
      "Subscale mode: using only ", sum(z_cols),
      " z-scored score columns with prefix '", z_prefix, "'. Raw duplicate columns are excluded."
    )
    X <- X[, z_cols, drop = FALSE]
  }

  # Work with a raw-equivalent name for classification.
  # z_score_idas__panic is treated as score_idas__panic for deciding whether it is a
  # questionnaire total or a lower-order subscale.
  base_names <- names(X)
  if (score_version == "z") {
    base_names <- paste0("score_", sub(paste0("^", z_prefix), "", base_names))
  }

  score_like <- startsWith(base_names, "score_")

  if (include_only_score_columns) {
    non_score <- !score_like
    if (any(non_score)) {
      message(
        "Subscale mode: excluding ", sum(non_score),
        " numeric non-score column(s): ", paste(names(X)[non_score], collapse = ", ")
      )
    }
    X <- X[, score_like, drop = FALSE]
    base_names <- base_names[score_like]
  }

  lower_order <- grepl("__", base_names, fixed = TRUE)
  scale_key <- sub("__.*$", "", base_names)
  global_score <- !lower_order
  scales_with_lower_order <- unique(scale_key[lower_order])

  keep <- rep(TRUE, length(base_names))

  if (global_mode == "none") {
    keep <- lower_order
  }

  if (global_mode == "all") {
    keep <- rep(TRUE, length(base_names))
  }

  if (global_mode == "only_without_subscales") {
    keep <- lower_order | (global_score & !(scale_key %in% scales_with_lower_order))
  }

  always_include <- config$subscales$always_include_names
  always_exclude <- config$subscales$always_exclude_names

  if (!is.null(always_include)) {
    keep[names(X) %in% always_include] <- TRUE
  }
  if (!is.null(always_exclude)) {
    keep[names(X) %in% always_exclude] <- FALSE
  }

  excluded_by_global_rule <- names(X)[!keep]
  if (length(excluded_by_global_rule) > 0) {
    message(
      "Subscale mode: excluding ", length(excluded_by_global_rule),
      " questionnaire total/duplicate score column(s) by include_global_scores = '",
      global_mode, "': ", paste(excluded_by_global_rule, collapse = ", ")
    )
  }

  X <- X[, keep, drop = FALSE]

  message(
    "Subscale mode: selected ", ncol(X), " manifest score column(s): ",
    paste(names(X), collapse = ", ")
  )

  if (ncol(X) < 2) {
    stop(
      "Subscale column selection left fewer than 2 variables. Check config$subscales and config$variables.",
      call. = FALSE
    )
  }

  X
}

select_manifest <- function(df_in) {
  keep <- mapply(function(col, nm) is_manifest(col, nm), df_in, names(df_in))
  X <- df_in[, keep, drop = FALSE]

  auto_exclude <- unique(na.omit(c(
    config$split$strata_var,
    config$stratification$strata_var,
    config$stratification$key
  )))

  if (length(auto_exclude) > 0) {
    X <- X[, setdiff(names(X), auto_exclude), drop = FALSE]
  }

  X <- filter_subscale_score_columns(X)

  if (!is.null(config$variables$include_names)) {
    X <- X[, intersect(names(X), config$variables$include_names), drop = FALSE]
  }
  if (!is.null(config$variables$include_regex)) {
    X <- X[, grepl(config$variables$include_regex, names(X)), drop = FALSE]
  }
  if (!is.null(config$variables$exclude_names)) {
    X <- X[, setdiff(names(X), config$variables$exclude_names), drop = FALSE]
  }
  if (!is.null(config$variables$exclude_regex)) {
    X <- X[, !grepl(config$variables$exclude_regex, names(X)), drop = FALSE]
  }

  X
}

clean_manifest_matrix <- function(X, label = "training", out_dir = NULL) {
  X <- as.data.frame(X)

  n_nonmiss <- vapply(X, function(v) sum(!is.na(v)), integer(1))
  n_unique <- vapply(X, function(v) length(unique(v[!is.na(v)])), integer(1))
  sd_val <- vapply(X, function(v) {
    suppressWarnings(stats::sd(as.numeric(v), na.rm = TRUE))
  }, numeric(1))

  drop <- n_nonmiss < 2 | n_unique < 2 | !is.finite(sd_val) | sd_val == 0

  report <- data.frame(
    variable = names(X),
    sample = label,
    n_nonmiss = n_nonmiss,
    n_unique = n_unique,
    sd = sd_val,
    drop = drop,
    reason = ifelse(
      n_nonmiss < 2, "fewer than 2 non-missing values",
      ifelse(
        n_unique < 2, "fewer than 2 observed categories/values",
        ifelse(!is.finite(sd_val), "non-finite SD", ifelse(sd_val == 0, "zero SD", "kept"))
      )
    ),
    row.names = NULL
  )

  dropped_report <- report[report$drop, , drop = FALSE]

  if (should_write_debug(out_dir)) {
    safe_mkdir(debug_dir)

    safe_write_csv(
      report,
      file.path(debug_dir, paste0("manifest_variable_screening_", label, ".csv")),
      row.names = FALSE
    )

    safe_write_csv(
      dropped_report,
      file.path(debug_dir, paste0("dropped_manifest_variables_", label, ".csv")),
      row.names = FALSE
    )

    log_path <- file.path(debug_dir, "dropped_manifest_variables_log.txt")

    cat(
      "\n============================================================\n",
      "Manifest variable screening: ", label, "\n",
      "Timestamp: ", as.character(Sys.time()), "\n",
      "Input variables: ", ncol(X), "\n",
      "Dropped variables: ", sum(drop), "\n",
      "Kept variables: ", sum(!drop), "\n",
      "============================================================\n",
      file = log_path,
      append = TRUE,
      sep = ""
    )

    if (nrow(dropped_report) > 0) {
      capture.output(print(dropped_report), file = log_path, append = TRUE)
      cat("\n", file = log_path, append = TRUE)
    } else {
      cat("No manifest variables dropped.\n\n", file = log_path, append = TRUE)
    }
  }

  if (any(drop)) {
    message(
      "Dropping ", sum(drop), " manifest variables from ", label,
      " before EFA because they have zero variance / too few observed values."
    )
    message("Dropped variables: ", paste(names(X)[drop], collapse = ", "))
  }

  X <- X[, !drop, drop = FALSE]

  if (ncol(X) < 2) {
    stop("After manifest cleaning, fewer than 2 variables remain in ", label, ".", call. = FALSE)
  }

  X
}

X_train <- select_manifest(df_training)
X_test <- if (!is.null(df_test)) select_manifest(df_test) else NULL

X_train <- clean_manifest_matrix(X_train, label = "training", out_dir = out_dir)

if (!is.null(X_test)) {
  missing_in_test <- setdiff(names(X_train), names(X_test))
  if (length(missing_in_test) > 0) {
    stop("These training variables are missing in test data: ", paste(missing_in_test, collapse = ", "), call. = FALSE)
  }
  X_test <- X_test[, names(X_train), drop = FALSE]
}

safe_write_csv(
  data.frame(
    variable = names(X_train),
    analysis_level = config$analysis_level,
    subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
    include_global_scores = if (identical(config$analysis_level, "subscales")) config$subscales$include_global_scores else NA_character_,
    sample = "training",
    row.names = NULL
  ),
  make_outfile("manifest_variables_selected_training", "csv", dir = summary_dir),
  row.names = FALSE
)

if (!is.null(X_test)) {
  safe_write_csv(
    data.frame(
      variable = names(X_test),
      analysis_level = config$analysis_level,
      subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
      include_global_scores = if (identical(config$analysis_level, "subscales")) config$subscales$include_global_scores else NA_character_,
      sample = "test",
      row.names = NULL
    ),
    make_outfile("manifest_variables_selected_test", "csv", dir = summary_dir),
    row.names = FALSE
  )
}

N <- nrow(X_train)
p <- ncol(X_train)
ratio <- N / p

if (N < config$stability$min_n || ratio < config$stability$min_n_per_var) {
  warning(
    sprintf(
      "Potential instability warning: N=%d, p=%d, N/p=%.2f. Low N relative to p can yield unstable EFA solutions.",
      N, p, ratio
    ),
    call. = FALSE
  )
}


# ---------------------------
# 5) Correlation matrix
# ---------------------------
options(mc.cores = config$correlation$mc_cores)

infer_corr_type <- function() {
  if (config$correlation$type != "auto") return(config$correlation$type)
  if (config$analysis_level == "items") return("polychoric")
  if (config$analysis_level == "subscales") return("pearson")
  if (config$analysis_level == "diagnoses") return("tetrachoric")
  stop("Cannot infer correlation type.", call. = FALSE)
}

corr_type <- infer_corr_type()
message("Correlation type = ", corr_type)

prep_for_ordinal_corr <- function(X) {
  X2 <- as.data.frame(X)
  for (nm in names(X2)) {
    v <- X2[[nm]]
    if (is.factor(v)) {
      X2[[nm]] <- as.ordered(v)
      next
    }
    if (is.numeric(v)) {
      u <- unique(v[!is.na(v)])
      if (length(u) <= 10 && all(abs(u - round(u)) < 1e-8)) {
        X2[[nm]] <- as.ordered(factor(v, levels = sort(unique(v), na.last = NA)))
      } else {
        X2[[nm]] <- v
      }
    }
  }
  X2
}

compute_R <- function(X) {
  X <- as.data.frame(X)

  if (corr_type == "pearson") {
    use <- if (isTRUE(config$correlation$use_pairwise)) "pairwise.complete.obs" else "complete.obs"
    return(suppressWarnings(stats::cor(X, use = use)))
  }

  if (corr_type == "polychoric") {
    Xo <- prep_for_ordinal_corr(X)
    pc <- suppressWarnings(psych::polychoric(Xo, correct = 0, smooth = config$correlation$smooth))
    return(pc$rho)
  }

  if (corr_type == "tetrachoric") {
    Xo <- prep_for_ordinal_corr(X)
    tc <- suppressWarnings(psych::tetrachoric(Xo, correct = 0, smooth = config$correlation$smooth))
    return(tc$rho)
  }

  stop("Unknown correlation type: ", corr_type, call. = FALSE)
}

compute_R_named <- function(X, label = "sample") {
  X <- as.data.frame(X)

  if (is.null(names(X)) || any(is.na(names(X))) || any(names(X) == "")) {
    stop("X for ", label, " has missing/empty variable names.", call. = FALSE)
  }

  R <- compute_R(X)
  R <- as.matrix(R)

  if (nrow(R) != ncol(X) || ncol(R) != ncol(X)) {
    stop(
      "Correlation matrix dimension mismatch in ", label, ": ",
      "dim(R) = ", paste(dim(R), collapse = " x "),
      ", ncol(X) = ", ncol(X), ". ",
      "This means psych::polychoric deleted variables internally. ",
      "Prescreen this sample for zero-variance or single-category items before compute_R().",
      call. = FALSE
    )
  }

  rownames(R) <- names(X)
  colnames(R) <- names(X)

  R
}

sanitize_correlation_matrix <- function(R, X = NULL, label = "training", out_dir = NULL) {
  if (should_write_debug(out_dir)) {
    safe_mkdir(debug_dir)
  }

  R <- as.matrix(R)
  storage.mode(R) <- "double"

  if (is.null(rownames(R)) || is.null(colnames(R))) {
    if (!is.null(X) && nrow(R) == ncol(X) && ncol(R) == ncol(X)) {
      rownames(R) <- names(as.data.frame(X))
      colnames(R) <- names(as.data.frame(X))
    } else {
      stop("Correlation matrix for ", label, " has no names and cannot be repaired.", call. = FALSE)
    }
  }

  dropped <- character()

  repeat {
    bad <- which(!is.finite(R), arr.ind = TRUE)
    if (nrow(bad) == 0) break

    bad_vars <- c(rownames(R)[bad[, 1]], colnames(R)[bad[, 2]])
    bad_counts <- sort(table(bad_vars), decreasing = TRUE)
    drop_var <- names(bad_counts)[1]

    message("Dropping variable from ", label, " because correlation matrix contains NA/Inf involving it: ", drop_var)

    dropped <- c(dropped, drop_var)
    keep <- setdiff(colnames(R), drop_var)

    R <- R[keep, keep, drop = FALSE]
    if (!is.null(X)) X <- X[, keep, drop = FALSE]

    if (ncol(R) < 2) {
      stop("After removing variables with NA/Inf correlations, fewer than 2 variables remain in ", label, ".", call. = FALSE)
    }
  }

  keep_names <- colnames(R)

  R <- (R + t(R)) / 2
  rownames(R) <- keep_names
  colnames(R) <- keep_names
  diag(R) <- 1

  if (any(!is.finite(R))) {
    stop("Correlation matrix for ", label, " still contains NA/Inf after sanitizing.", call. = FALSE)
  }

  eig_check <- eigen(R, symmetric = TRUE, only.values = TRUE)$values

  if (any(eig_check <= 1e-8)) {
    message("Correlation matrix for ", label, " is not positive definite; applying psych::cor.smooth().")
    R <- psych::cor.smooth(R)
    R <- as.matrix(R)
    rownames(R) <- keep_names
    colnames(R) <- keep_names

    R <- (R + t(R)) / 2
    rownames(R) <- keep_names
    colnames(R) <- keep_names
    diag(R) <- 1
  }

  if (should_write_debug(out_dir)) {
    dropped_df <- if (length(dropped) > 0) {
      data.frame(variable = dropped, sample = label, reason = "NA_or_Inf_in_correlation_matrix", row.names = NULL)
    } else {
      data.frame(variable = character(), sample = character(), reason = character())
    }

    safe_write_csv(
      dropped_df,
      file.path(debug_dir, paste0("variables_dropped_due_to_bad_correlations_", label, ".csv")),
      row.names = FALSE
    )
  }

  list(R = R, X = X, dropped = dropped)
}

R_train_raw <- compute_R_named(X_train, label = "training")

san_train <- sanitize_correlation_matrix(
  R_train_raw,
  X = X_train,
  label = "training",
  out_dir = out_dir
)

R_train <- san_train$R
X_train <- san_train$X

if (!is.null(X_test)) {
  X_test <- X_test[, names(X_train), drop = FALSE]
}

p <- ncol(X_train)

if (write_debug_outputs) {
  write.csv(R_train, make_outfile("correlation_matrix_training", "csv", dir = debug_dir), row.names = TRUE)
}
write_correlation_matrix_with_labels(R_train, item_info, label = "training")


# ---------------------------
# 5b) Improved correlation heatmap
# ---------------------------
plot_correlation_heatmap <- function(R, label = "training") {
  p <- ncol(R)

  if (p > config$plots$correlation_heatmap$max_p) {
    message("Skipping correlation heatmap because p = ", p, " > ", config$plots$correlation_heatmap$max_p)
    return(invisible(NULL))
  }

  Rm <- as.data.frame(as.table(R))
  names(Rm) <- c("Var1", "Var2", "r")

  show_cell_labels <- p <= config$plots$correlation_heatmap$show_cell_values_max_p

  heat <- ggplot(Rm, aes(x = Var1, y = Var2, fill = r)) +
    geom_tile() +
    {
      if (show_cell_labels) {
        geom_text(aes(label = sprintf("%.2f", r)), size = 1.8)
      }
    } +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      name = "r"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = config$plots$correlation_heatmap$axis_text_size
      ),
      axis.text.y = element_text(size = config$plots$correlation_heatmap$axis_text_size),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("Correlation matrix (", label, ")"),
      subtitle = paste0("Scale fixed from -1 to 1; white midpoint = 0")
    )

  save_plot_png(
    heat,
    make_outfile(paste0("correlation_heatmap_", label), "png", dir = plots_dir),
    width = config$plots$correlation_heatmap$width,
    height = config$plots$correlation_heatmap$height,
    dpi = config$plots$correlation_heatmap$dpi
  )
}

plot_correlation_heatmap(R_train, label = "training")


# ---------------------------
# 6) Adequacy checks + scree/CNG
# ---------------------------
N_eff <- nrow(X_train)

kmo <- tryCatch(psych::KMO(R_train), error = function(e) {
  append_log(Sys.time(), " | KMO failed: ", conditionMessage(e))
  NULL
})

bart <- tryCatch(psych::cortest.bartlett(R_train, n = N_eff), error = function(e) {
  append_log(Sys.time(), " | Bartlett failed: ", conditionMessage(e))
  NULL
})

message("\n--- Adequacy checks (training) ---")
if (!is.null(kmo)) {
  message("KMO (overall MSA): ", round(kmo$MSA, 3))
} else {
  message("KMO failed; see log.")
}

if (!is.null(bart)) {
  message("Bartlett test: chi2 = ", round(bart$chisq, 2), ", df = ", bart$df, ", p = ", signif(bart$p.value, 3))
} else {
  message("Bartlett failed; see log.")
}

fails <- c()
if (!is.null(kmo) && is.finite(kmo$MSA) && kmo$MSA < config$adequacy$kmo_min) {
  fails <- c(fails, sprintf("KMO %.3f < %.2f", kmo$MSA, config$adequacy$kmo_min))
}
if (!is.null(bart) && is.finite(bart$p.value) && bart$p.value > config$adequacy$bartlett_alpha) {
  fails <- c(fails, sprintf("Bartlett p=%.3g > %.2f", bart$p.value, config$adequacy$bartlett_alpha))
}

if (length(fails) > 0) {
  pause_or_stop(
    paste("Adequacy checks out of range:", paste(fails, collapse = "; ")),
    halt = isTRUE(config$adequacy$halt_on_fail)
  )
}

eigs <- eigen(R_train, symmetric = TRUE, only.values = TRUE)$values
scree_df <- data.frame(
  k = seq_along(eigs),
  eigenvalue = eigs
)

k_suggest_cng <- NA_integer_
if (requireNamespace("nFactors", quietly = TRUE)) {
  cng <- tryCatch(nFactors::nCng(eigs, details = TRUE), error = function(e) NULL)
  if (!is.null(cng)) k_suggest_cng <- suppressWarnings(as.integer(cng$nFactors))
} else {
  message("Package nFactors not installed: CNG suggestion will be skipped.")
}

scree_plot <- ggplot(scree_df, aes(x = k, y = eigenvalue)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 11) +
  labs(
    title = "Scree plot (training)",
    x = "Component / factor number",
    y = "Eigenvalue"
  )

if (is.finite(k_suggest_cng)) {
  scree_plot <- scree_plot +
    geom_vline(xintercept = k_suggest_cng, linetype = "dashed") +
    annotate(
      "text",
      x = k_suggest_cng,
      y = max(eigs, na.rm = TRUE),
      label = paste0("CNG suggests k = ", k_suggest_cng),
      vjust = -0.5,
      hjust = 0.0
    )
}

save_plot_png(
  scree_plot,
  make_outfile("scree_plot_cng_training", "png", dir = plots_dir),
  width = 10,
  height = 7,
  dpi = 300
)

k_suggest_parallel <- NA_integer_
if (config$retention$method == "parallel") {
  message("\nRunning parallel analysis (training)...")
  cor_arg <- if (corr_type %in% c("polychoric", "tetrachoric")) "poly" else "cor"

  pa <- psych::fa.parallel(
    X_train,
    fm = config$efa$fm,
    fa = config$retention$parallel$fa,
    n.iter = config$retention$parallel$n_iter,
    cor = cor_arg,
    plot = TRUE
  )

  k_suggest_parallel <- tryCatch({
    if (!is.null(pa$nfact)) as.integer(pa$nfact) else as.integer(pa$nfactors)
  }, error = function(e) NA_integer_)

  parallel_plot_file <- make_outfile("parallel_analysis_training", "png", dir = plots_dir)
  safe_mkdir(dirname(parallel_plot_file))
  png(parallel_plot_file, width = 1800, height = 1200, res = 220)
  psych::fa.parallel(
    X_train,
    fm = config$efa$fm,
    fa = config$retention$parallel$fa,
    n.iter = config$retention$parallel$n_iter,
    cor = cor_arg,
    plot = TRUE
  )
  dev.off()
}


# ---------------------------
# 7) Choose k values
# ---------------------------
choose_k_values <- function() {
  if (!is.null(config$retention$k_values)) {
    return(sort(unique(as.integer(config$retention$k_values))))
  }

  if (!is.null(config$retention$k_fixed)) {
    return(as.integer(config$retention$k_fixed))
  }

  k_kaiser <- max(1L, sum(eigs > 1, na.rm = TRUE))
  k_kaiser <- min(k_kaiser, max(1L, p - 1L))

  if (!isTRUE(config$retention$prompt_for_k)) {
    if (config$retention$method == "parallel" && is.finite(k_suggest_parallel)) return(as.integer(k_suggest_parallel))
    if (is.finite(k_suggest_cng)) return(as.integer(k_suggest_cng))
    return(k_kaiser)
  }

  default_k <- if (config$retention$method == "parallel" && is.finite(k_suggest_parallel)) {
    k_suggest_parallel
  } else if (is.finite(k_suggest_cng)) {
    k_suggest_cng
  } else {
    k_kaiser
  }

  message("\nScree plot saved to: ", make_outfile("scree_plot_cng_training", "png", dir = plots_dir))
  ans <- readline(paste0("Which k values do you want to run? Use comma-separated values. [default ", default_k, "]: "))

  if (nchar(trimws(ans)) == 0) return(as.integer(default_k))

  as.integer(trimws(unlist(strsplit(ans, ","))))
}

k_values <- choose_k_values()
k_values <- sort(unique(as.integer(k_values)))
k_values <- k_values[is.finite(k_values) & !is.na(k_values) & k_values >= 1]

if (length(k_values) == 0) {
  stop("No valid factor numbers supplied.", call. = FALSE)
}

too_large <- k_values >= ncol(R_train)
if (any(too_large)) {
  warning(
    "Dropping invalid k values because k must be < number of manifest variables: ",
    paste(k_values[too_large], collapse = ", "),
    call. = FALSE
  )
  k_values <- k_values[!too_large]
}

if (length(k_values) == 0) {
  stop("No valid k values remain after checking against p.", call. = FALSE)
}

message("Factor solutions to run: ", paste(k_values, collapse = ", "))

writeLines(capture.output(str(config)), file.path(summary_dir, "config_snapshot.txt"))

write.csv(
  data.frame(
    run_date = run_date,
    analysis_level = config$analysis_level,
    data_path = data_path,
    stratification_path = if (file.exists(strat_path_full)) strat_path_full else NA_character_,
    dataset_tag = dataset_tag,
    dataset_tag_full = dataset_tag_full,
    k_values = paste(k_values, collapse = ", "),
    threshold_tag = threshold_tag,
    analysis_option_tag = analysis_option_tag,
    strict_primary_abs_min = config$assignment$strict$primary_abs_min,
    strict_second_abs_max = config$assignment$strict$second_abs_max,
    interpretive_primary_abs_min = config$assignment$interpretive$primary_abs_min,
    interpretive_gap_min = config$assignment$interpretive$gap_min,
    repeated_splits_enabled = config$validation$repeated_splits$enabled,
    repeated_splits_n = config$validation$repeated_splits$n_splits,
    subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
    subscale_z_prefix = if (identical(config$analysis_level, "subscales")) config$subscales$z_prefix else NA_character_,
    n_training = nrow(X_train),
    n_test = if (!is.null(X_test)) nrow(X_test) else NA_integer_,
    p_training_after_cleaning = ncol(X_train),
    correlation_type = corr_type,
    efa_engine = config$efa$engine,
    efa_fm = config$efa$fm,
    efa_rotate = config$efa$rotate,
    factor_scores_computed = FALSE
  ),
  make_outfile("run_metadata", "csv", dir = summary_dir),
  row.names = FALSE
)


# ---------------------------
# 8) EFA helpers
# ---------------------------
fit_efa_psych <- function(R, n_obs, k) {
  psych::fa(
    r = R,
    nfactors = k,
    fm = config$efa$fm,
    rotate = config$efa$rotate,
    scores = "none",
    n.obs = n_obs
  )
}

fit_efa_lavaan <- function(X, k) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan not installed; install.packages('lavaan') or set config$efa$engine='psych'", call. = FALSE)
  }

  vars <- names(X)
  rhs <- paste(vars, collapse = " + ")
  lhs <- paste(paste0('efa("block1")*F', seq_len(k)), collapse = " + ")
  model <- paste(lhs, "=~", rhs)

  ordered_vars <- NULL
  if (corr_type %in% c("polychoric", "tetrachoric")) ordered_vars <- vars

  lavaan::cfa(
    model = model,
    data = X,
    estimator = config$efa$lavaan$estimator,
    ordered = ordered_vars,
    rotation = config$efa$lavaan$rotation,
    std.lv = TRUE
  )
}

extract_loadings_psych <- function(fa_obj) {
  L <- as.data.frame(unclass(fa_obj$loadings))
  L <- tibble::rownames_to_column(L, "variable")
  L
}

extract_fit_psych <- function(fa_obj, k) {
  tibble::tibble(
    k = k,
    factors = fa_obj$factors,
    fm = fa_obj$fm,
    rotate = fa_obj$rotate,
    RMSR = unname(fa_obj$rms),
    RMSEA = unname(fa_obj$RMSEA[1]),
    RMSEA_lower = unname(fa_obj$RMSEA[2]),
    RMSEA_upper = unname(fa_obj$RMSEA[3]),
    TLI = unname(fa_obj$TLI),
    BIC = unname(fa_obj$BIC)
  )
}

as_loading_matrix <- function(fa_obj, fallback_names = NULL) {
  L <- as.matrix(unclass(fa_obj$loadings))
  if (is.null(rownames(L)) || any(is.na(rownames(L))) || any(rownames(L) == "")) {
    if (!is.null(fallback_names) && length(fallback_names) == nrow(L)) rownames(L) <- fallback_names
  }
  L
}

make_factor_map <- function(loadings_mat, primary_factor_raw = NULL) {
  factor_raw <- colnames(loadings_mat)
  factor_raw_ordered <- sort_factor_names(factor_raw)
  ss_loading <- colSums(loadings_mat^2, na.rm = TRUE)

  if (is.null(primary_factor_raw)) {
    absL <- abs(loadings_mat)
    primary_factor_raw <- factor_raw[apply(absL, 1, which.max)]
  }

  n_primary_all <- vapply(factor_raw_ordered, function(f) sum(primary_factor_raw == f, na.rm = TRUE), numeric(1))
  nums <- factor_number(factor_raw_ordered)
  factor_order <- ifelse(is.na(nums), seq_along(factor_raw_ordered), nums)

  data.frame(
    factor_raw = factor_raw_ordered,
    factor_order = as.integer(factor_order),
    factor = factor_raw_to_factor(factor_raw_ordered),
    factor_label = factor_raw_to_label(factor_raw_ordered),
    n_primary_all = as.integer(n_primary_all),
    ss_loading = as.numeric(ss_loading[factor_raw_ordered]),
    row.names = NULL
  ) |> dplyr::arrange(factor_order)
}

assign_items_to_factors <- function(loadings_mat) {
  strict_primary_min <- config$assignment$strict$primary_abs_min
  strict_second_max <- config$assignment$strict$second_abs_max
  interp_primary_min <- config$assignment$interpretive$primary_abs_min
  interp_gap_min <- config$assignment$interpretive$gap_min

  absL <- abs(loadings_mat)
  primary_idx <- apply(absL, 1, which.max)
  primary_factor_raw <- colnames(loadings_mat)[primary_idx]
  primary_loading_abs <- absL[cbind(seq_len(nrow(absL)), primary_idx)]
  primary_loading_signed <- loadings_mat[cbind(seq_len(nrow(loadings_mat)), primary_idx)]

  second_idx <- apply(absL, 1, function(x) {
    if (length(x) < 2) return(NA_integer_)
    order(x, decreasing = TRUE)[2]
  })

  second_factor_raw <- ifelse(is.na(second_idx), NA_character_, colnames(loadings_mat)[second_idx])
  second_loading_abs <- ifelse(is.na(second_idx), NA_real_, absL[cbind(seq_len(nrow(absL)), second_idx)])
  loading_gap <- primary_loading_abs - second_loading_abs
  loading_gap[is.na(loading_gap)] <- primary_loading_abs[is.na(loading_gap)]

  keep_strict <- (primary_loading_abs >= strict_primary_min) & (is.na(second_loading_abs) | second_loading_abs < strict_second_max)
  keep_interpretive <- (primary_loading_abs >= interp_primary_min) & (is.na(loading_gap) | loading_gap >= interp_gap_min)

  drop_reason_strict <- ifelse(
    primary_loading_abs < strict_primary_min,
    paste0("primary_abs_loading_below_", strict_primary_min),
    ifelse(!is.na(second_loading_abs) & second_loading_abs >= strict_second_max,
           paste0("crossloading_second_abs_ge_", strict_second_max),
           "kept")
  )

  drop_reason_interpretive <- ifelse(
    primary_loading_abs < interp_primary_min,
    paste0("primary_abs_loading_below_", interp_primary_min),
    ifelse(!is.na(loading_gap) & loading_gap < interp_gap_min,
           paste0("crossloading_gap_below_", interp_gap_min),
           "kept")
  )

  factor_map <- make_factor_map(loadings_mat, primary_factor_raw = primary_factor_raw)
  map_factor <- setNames(factor_map$factor, factor_map$factor_raw)
  map_order <- setNames(factor_map$factor_order, factor_map$factor_raw)
  map_label <- setNames(factor_map$factor_label, factor_map$factor_raw)

  out <- data.frame(
    variable = rownames(loadings_mat),
    primary_factor = unname(map_factor[primary_factor_raw]),
    primary_factor_order = as.integer(unname(map_order[primary_factor_raw])),
    primary_factor_raw = primary_factor_raw,
    primary_factor_label = unname(map_label[primary_factor_raw]),
    primary_loading_signed = as.numeric(primary_loading_signed),
    primary_loading = as.numeric(primary_loading_abs),
    second_factor_raw = second_factor_raw,
    second_loading = as.numeric(second_loading_abs),
    loading_gap = as.numeric(loading_gap),
    keep_strict = keep_strict,
    keep_interpretive = keep_interpretive,
    keep = keep_strict,
    drop_reason_strict = drop_reason_strict,
    drop_reason_interpretive = drop_reason_interpretive,
    row.names = NULL
  )

  list(assignment = out, factor_map = factor_map)
}

make_loadings_long <- function(loadings_mat, factor_map, item_info = NULL) {
  factor_raw <- colnames(loadings_mat)
  long <- data.frame(
    variable = rep(rownames(loadings_mat), times = length(factor_raw)),
    factor_raw = rep(factor_raw, each = nrow(loadings_mat)),
    loading = as.numeric(as.vector(loadings_mat)),
    row.names = NULL
  )
  long <- dplyr::left_join(long, factor_map, by = "factor_raw")
  long <- enrich_with_item_info(long, item_info)
  long$item_label <- make_item_plot_label(long)
  long
}

make_loadings_wide_with_info <- function(loadings_mat, item_info = NULL) {
  wide <- as.data.frame(loadings_mat)
  wide <- tibble::rownames_to_column(wide, "variable")
  enrich_with_item_info(wide, item_info)
}

make_marker_table <- function(assignment_df, item_info = NULL, rule = "interpretive", top_n = 30) {
  keep_col <- if (rule == "strict") "keep_strict" else "keep_interpretive"
  dfm <- assignment_df
  if (!(keep_col %in% names(dfm))) keep_col <- "keep"
  dfm <- dfm[dfm[[keep_col]], , drop = FALSE]
  dfm <- enrich_with_item_info(dfm, item_info)
  dfm$item_label <- make_item_plot_label(dfm)

  if (nrow(dfm) == 0) return(dfm)

  dfm <- dfm |> dplyr::arrange(primary_factor_order, dplyr::desc(primary_loading))
  dfm <- dfm |> dplyr::group_by(primary_factor, primary_factor_raw, primary_factor_label) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::ungroup()
  dfm
}

plot_factor_diagram <- function(efa_train, k) {
  if (!isTRUE(config$plots$factor_diagram$make_debug_diagram)) return(invisible(NULL))

  outfile <- make_outfile("factor_diagram_training_debug", "png", k_val = k, dir = debug_dir)
  safe_mkdir(dirname(outfile))

  png(
    filename = outfile,
    width = config$plots$factor_diagram$width,
    height = config$plots$factor_diagram$height,
    res = config$plots$factor_diagram$res
  )

  try(
    psych::fa.diagram(
      efa_train,
      simple = config$plots$factor_diagram$simple,
      errors = FALSE,
      cut = config$plots$factor_diagram$cut,
      digits = 2,
      sort = TRUE,
      cex = config$plots$factor_diagram$cex,
      main = paste0("EFA factor diagram (training, k = ", k, ")")
    ),
    silent = TRUE
  )

  dev.off()
}

plot_loading_marker_items <- function(marker_df, k) {
  if (!isTRUE(config$plots$loading_marker_plot$enabled)) return(invisible(NULL))
  if (is.null(marker_df) || nrow(marker_df) == 0) return(invisible(NULL))

  factors <- unique(marker_df$primary_factor)
  for (f in factors) {
    dfp <- marker_df[marker_df$primary_factor == f, , drop = FALSE]
    if (nrow(dfp) == 0) next

    dfp <- dfp[order(dfp$primary_loading_signed), , drop = FALSE]
    dfp$item_label <- factor(dfp$item_label, levels = unique(dfp$item_label))

    raw <- unique(dfp$primary_factor_raw)
    f_label <- unique(dfp$primary_factor_label)
    h <- max(config$plots$loading_marker_plot$min_height, nrow(dfp) * config$plots$loading_marker_plot$height_per_item)

    p <- ggplot(dfp, aes(x = item_label, y = primary_loading_signed)) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 10) +
      theme(axis.text.y = element_text(size = 6), panel.grid.major.y = element_blank()) +
      labs(
        title = paste0("Top marker loadings (training, k = ", k, ", ", f_label, ")"),
        subtitle = "Label = item [Scale / Subscale / Higher-order subscale / HiTOP subscale]. Factors use raw psych::fa names: MR1, MR2, ... .",
        x = NULL,
        y = "Signed primary loading"
      )

    save_plot_png(
      p,
      make_outfile(paste0("loading_marker_plot_", raw), "png", k_val = k, dir = plots_dir),
      width = config$plots$loading_marker_plot$width,
      height = h,
      dpi = config$plots$loading_marker_plot$dpi
    )
  }
}

plot_loading_heatmap_readable <- function(loadings_mat, assignment_df, factor_map, k, item_info = NULL) {
  rule <- config$plots$loading_heatmap$use_rule
  keep_vars <- assignment_df$variable
  if (rule == "strict") keep_vars <- assignment_df$variable[assignment_df$keep_strict]
  if (rule == "interpretive") keep_vars <- assignment_df$variable[assignment_df$keep_interpretive]

  if (length(keep_vars) == 0) return(invisible(NULL))

  assignment_keep <- assignment_df[assignment_df$variable %in% keep_vars, , drop = FALSE]
  assignment_keep <- enrich_with_item_info(assignment_keep, item_info)
  assignment_keep$item_label <- make_item_plot_label(assignment_keep)
  assignment_keep <- assignment_keep |> dplyr::arrange(primary_factor_order, dplyr::desc(primary_loading))

  long <- make_loadings_long(loadings_mat, factor_map, item_info)
  long <- long[long$variable %in% assignment_keep$variable, , drop = FALSE]

  label_map <- setNames(assignment_keep$item_label, assignment_keep$variable)
  long$item_label <- unname(label_map[long$variable])
  long$item_label <- factor(long$item_label, levels = rev(unique(assignment_keep$item_label)))
  long$factor_label <- factor(long$factor_label, levels = factor_map$factor_label)

  h <- max(config$plots$loading_heatmap$min_height, length(unique(long$item_label)) * config$plots$loading_heatmap$height_per_item)

  p <- ggplot(long, aes(x = factor_label, y = item_label, fill = loading)) +
    geom_tile(color = "grey90") +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      name = "Loading"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_text(size = config$plots$loading_heatmap$axis_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("Readable loading heatmap (training, k = ", k, ")"),
      subtitle = paste0("Items shown by rule: ", rule, "; item labels include codebook scale/subscale where available."),
      x = NULL,
      y = NULL
    )

  save_plot_png(
    p,
    make_outfile(paste0("loading_heatmap_readable_", rule), "png", k_val = k, dir = plots_dir),
    width = config$plots$loading_heatmap$width,
    height = h,
    dpi = config$plots$loading_heatmap$dpi
  )
}

permute_vec <- function(x) {
  if (length(x) == 1) return(matrix(x, nrow = 1))
  out <- do.call(rbind, lapply(seq_along(x), function(i) {
    cbind(x[i], permute_vec(x[-i]))
  }))
  out
}

plot_factor_correlation_heatmap <- function(Phi, k) {
  if (is.null(Phi)) return(invisible(NULL))
  Phi <- as.matrix(Phi)
  if (nrow(Phi) < 2 || ncol(Phi) < 2) return(invisible(NULL))

  Phi <- Phi[sort_factor_names(rownames(Phi)), sort_factor_names(colnames(Phi)), drop = FALSE]
  phi_df <- as.data.frame(as.table(Phi))
  names(phi_df) <- c("Factor1", "Factor2", "r")
  phi_df$Factor1 <- factor(phi_df$Factor1, levels = rev(sort_factor_names(unique(as.character(phi_df$Factor1)))))
  phi_df$Factor2 <- factor(phi_df$Factor2, levels = sort_factor_names(unique(as.character(phi_df$Factor2))))

  p_phi <- ggplot(phi_df, aes(x = Factor2, y = Factor1, fill = r)) +
    geom_tile(color = "grey85") +
    geom_text(aes(label = sprintf("%.2f", r)), size = config$plots$factor_correlation_heatmap$text_size) +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      name = "Factor\nr"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste0("Factor correlation matrix (training, k = ", k, ")"),
      subtitle = "Oblique rotation factor correlations from psych::fa Phi matrix.",
      x = NULL,
      y = NULL
    )

  save_plot_png(
    p_phi,
    make_outfile("factor_correlation_matrix_heatmap", "png", k_val = k, dir = plots_dir),
    width = config$plots$factor_correlation_heatmap$width,
    height = config$plots$factor_correlation_heatmap$height,
    dpi = config$plots$factor_correlation_heatmap$dpi
  )
}

one_to_one_factor_match <- function(cong) {
  cong <- order_tucker_matrix(cong)
  nr <- nrow(cong)
  nc <- ncol(cong)
  if (nr == 0 || nc == 0) return(data.frame())

  k <- min(nr, nc)
  perms <- permute_vec(seq_len(nc))
  if (ncol(perms) > k) perms <- perms[, seq_len(k), drop = FALSE]

  scores <- apply(perms, 1, function(p) sum(abs(cong[cbind(seq_len(k), p)]), na.rm = TRUE))
  best <- perms[which.max(scores), ]
  train_raw <- rownames(cong)[seq_len(k)]
  test_raw <- colnames(cong)[best]
  vals <- as.numeric(cong[cbind(seq_len(k), best)])

  data.frame(
    train_factor = factor_raw_to_factor(train_raw),
    train_factor_raw = train_raw,
    train_factor_label = factor_raw_to_label(train_raw),
    matched_test_factor = factor_raw_to_factor(test_raw),
    matched_test_factor_raw = test_raw,
    matched_test_factor_label = factor_raw_to_label(test_raw),
    congruence = vals,
    abs_congruence = abs(vals),
    sign_flip = vals < 0,
    one_to_one_total_abs = max(scores, na.rm = TRUE),
    row.names = NULL
  )
}

plot_tucker_congruence <- function(cong, k, match_df = NULL) {
  cong <- order_tucker_matrix(cong)
  cong_df <- as.data.frame(as.table(cong))
  names(cong_df) <- c("TrainFactorRaw", "TestFactorRaw", "Congruence")
  train_levels <- sort_factor_names(unique(cong_df$TrainFactorRaw))
  test_levels <- sort_factor_names(unique(cong_df$TestFactorRaw))
  cong_df$TrainFactor <- factor(factor_raw_to_label(cong_df$TrainFactorRaw), levels = factor_raw_to_label(train_levels))
  cong_df$TestFactor <- factor(factor_raw_to_label(cong_df$TestFactorRaw), levels = factor_raw_to_label(test_levels))

  p_cong <- ggplot(cong_df, aes(x = TestFactor, y = TrainFactor, fill = Congruence)) +
    geom_tile(color = "grey85") +
    geom_text(aes(label = sprintf("%.2f", Congruence)), size = config$plots$congruence_heatmap$text_size) +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      name = "Tucker\ncongruence"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste0("Tucker congruence: training vs test (k = ", k, ")"),
      subtitle = "Axes use raw psych::fa factor names ordered consistently as MR1, MR2, ...; signs are arbitrary, so decision summaries use absolute congruence.",
      x = "Test factors",
      y = "Training factors"
    )

  save_plot_png(
    p_cong,
    make_outfile("tucker_congruence_heatmap_raw_consistent_order", "png", k_val = k, dir = plots_dir),
    width = config$plots$congruence_heatmap$width,
    height = config$plots$congruence_heatmap$height,
    dpi = config$plots$congruence_heatmap$dpi
  )

  if (!is.null(match_df) && nrow(match_df) > 0) {
    row_order <- match_df$train_factor_raw
    col_order <- match_df$matched_test_factor_raw
    cong_ord <- cong[row_order, col_order, drop = FALSE]

    cong_ord_df <- as.data.frame(as.table(cong_ord))
    names(cong_ord_df) <- c("TrainFactorRaw", "MatchedTestFactorRaw", "Congruence")

    row_levels <- row_order
    col_levels <- col_order
    cong_ord_df$TrainFactor <- factor(factor_raw_to_label(cong_ord_df$TrainFactorRaw), levels = factor_raw_to_label(row_levels))
    cong_ord_df$MatchedTestFactor <- factor(factor_raw_to_label(cong_ord_df$MatchedTestFactorRaw), levels = factor_raw_to_label(col_levels))

    p_match <- ggplot(cong_ord_df, aes(x = MatchedTestFactor, y = TrainFactor, fill = Congruence)) +
      geom_tile(color = "grey85") +
      geom_text(aes(label = sprintf("%.2f", Congruence)), size = config$plots$congruence_heatmap$text_size) +
      scale_fill_gradient2(
        low = "#B2182B",
        mid = "white",
        high = "#2166AC",
        midpoint = 0,
        limits = c(-1, 1),
        oob = scales::squish,
        name = "Tucker\ncongruence"
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste0("Tucker congruence: one-to-one matched order (k = ", k, ")"),
        subtitle = "Axes are globally matched by maximum absolute Tucker congruence; negative values indicate sign flips.",
        x = "Matched test factors",
        y = "Training factors"
      )

    save_plot_png(
      p_match,
      make_outfile("tucker_congruence_heatmap_one_to_one_matched", "png", k_val = k, dir = plots_dir),
      width = config$plots$congruence_heatmap$width,
      height = config$plots$congruence_heatmap$height,
      dpi = config$plots$congruence_heatmap$dpi
    )
  }
}

plot_repeated_tucker_summary <- function(factor_summary, k, raw = NULL) {
  if (is.null(factor_summary) || nrow(factor_summary) == 0) return(invisible(NULL))

  factor_levels <- factor_raw_to_label(sort_factor_names(unique(factor_summary$train_factor_raw)))

  factor_summary$train_factor_label <- factor(
    factor_summary$train_factor_label,
    levels = factor_levels
  )

  raw_plot <- NULL
  if (!is.null(raw) && nrow(raw) > 0) {
    raw_plot <- raw
    raw_factor_levels <- factor_raw_to_label(sort_factor_names(unique(raw_plot$train_factor_raw)))
    # raw contains one row per factor per split, so train_factor_raw is repeated many times.
    # factor() levels must be unique; otherwise R throws:
    # "factor level [x] is duplicated".
    raw_plot$train_factor_label <- factor(
      raw_plot$train_factor_label,
      levels = raw_factor_levels
    )
  }

  good <- config$validation$tucker_cutoffs$good
  excellent <- config$validation$tucker_cutoffs$excellent

  p <- ggplot(factor_summary, aes(x = train_factor_label, y = mean_abs_congruence)) +
    {
      if (!is.null(raw_plot)) {
        geom_jitter(
          data = raw_plot,
          aes(x = train_factor_label, y = abs_congruence),
          width = 0.12,
          height = 0,
          alpha = 0.35,
          size = 1.6,
          inherit.aes = FALSE
        )
      }
    } +
    geom_errorbar(aes(ymin = q10_abs_congruence, ymax = q90_abs_congruence), width = 0.12) +
    geom_point(size = 2.4) +
    geom_hline(yintercept = good, linetype = "dashed") +
    geom_hline(yintercept = excellent, linetype = "dotdash") +
    annotate("text", x = Inf, y = good, label = paste0("good ", good), hjust = 1.05, vjust = -0.4, size = 3) +
    annotate("text", x = Inf, y = excellent, label = paste0("excellent ", excellent), hjust = 1.05, vjust = -0.4, size = 3) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(5.5, 30, 5.5, 5.5)) +
    labs(
      title = paste0("Repeated split Tucker stability (k = ", k, ")"),
      subtitle = paste0(
        "Dots = single matched factor congruences; point/error bar = mean and 10th-90th percentile over ",
        config$validation$repeated_splits$n_splits,
        " repeated 50/50 splits. Matching is global one-to-one by maximum absolute Tucker congruence."
      ),
      x = "Training factor",
      y = "Absolute one-to-one Tucker congruence"
    )

  save_plot_png(
    p,
    make_outfile("repeated_split_tucker_summary", "png", k_val = k, dir = plots_dir),
    width = 9.5,
    height = 6,
    dpi = config$plots$congruence_heatmap$dpi
  )
}

# Factor scores are intentionally not computed in this exploration script.
# The per-k scoring bundle exported below is meant for a separate scoring script.

compute_reliability_for_factor <- function(X, vars) {
  vars <- intersect(vars, names(X))
  out <- list(n_items = length(vars))

  if (length(vars) < config$reliability$min_items_per_factor) return(out)

  Xi <- X[, vars, drop = FALSE]

  if (isTRUE(config$reliability$compute_alpha)) {
    a <- tryCatch(psych::alpha(Xi, check.keys = TRUE, warnings = FALSE), error = function(e) NULL)
    if (!is.null(a)) {
      out$alpha_raw <- unname(a$total$raw_alpha)
      out$alpha_std <- unname(a$total$std.alpha)
    }
  }

  if (isTRUE(config$reliability$compute_omega)) {
    o <- tryCatch(psych::omega(Xi, nfactors = 1, plot = FALSE, warnings = FALSE), error = function(e) NULL)
    if (!is.null(o)) {
      out$omega_total <- unname(o$omega.tot)
      out$omega_h <- unname(o$omega.h)
    }
  }

  out
}

reliability_report <- function(X, assignment_df, label, k, rule = config$assignment$reliability_rule) {
  reps <- list()

  keep_col <- if (rule == "interpretive") "keep_interpretive" else "keep_strict"
  if (!(keep_col %in% names(assignment_df))) keep_col <- "keep"

  factors <- assignment_df |> dplyr::arrange(primary_factor_order) |> dplyr::pull(primary_factor) |> unique()
  if (length(factors) == 0) {
    return(tibble::tibble())
  }

  for (f in factors) {
    vars <- assignment_df |> dplyr::filter(primary_factor == f, .data[[keep_col]]) |> dplyr::pull(variable)
    res <- compute_reliability_for_factor(X, vars)
    raw <- unique(assignment_df$primary_factor_raw[assignment_df$primary_factor == f])
    reps[[f]] <- c(
      list(factor = f, factor_raw = raw[1], sample = label, k = k, assignment_rule = rule, n_items = res$n_items),
      res[setdiff(names(res), "n_items")]
    )
  }

  all_names <- unique(unlist(lapply(reps, names)))

  df_out <- do.call(rbind, lapply(reps, function(x) {
    x[setdiff(all_names, names(x))] <- NA
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  tibble::as_tibble(df_out)
}

make_assignment_summary <- function(assignment_df, k) {
  data.frame(
    k = k,
    n_items_total = nrow(assignment_df),
    n_keep_strict = sum(assignment_df$keep_strict, na.rm = TRUE),
    n_keep_interpretive = sum(assignment_df$keep_interpretive, na.rm = TRUE),
    n_drop_strict_low_loading = sum(assignment_df$drop_reason_strict != "kept" & grepl("primary_abs_loading", assignment_df$drop_reason_strict), na.rm = TRUE),
    n_drop_strict_crossloading = sum(assignment_df$drop_reason_strict != "kept" & grepl("crossloading", assignment_df$drop_reason_strict), na.rm = TRUE),
    n_drop_interpretive_low_loading = sum(assignment_df$drop_reason_interpretive != "kept" & grepl("primary_abs_loading", assignment_df$drop_reason_interpretive), na.rm = TRUE),
    n_drop_interpretive_crossloading = sum(assignment_df$drop_reason_interpretive != "kept" & grepl("crossloading", assignment_df$drop_reason_interpretive), na.rm = TRUE),
    row.names = NULL
  )
}


# ---------------------------
# 8b) Repeated split Tucker stability
# ---------------------------
fit_efa_loadings_from_X <- function(X, k, label = "split") {
  # Legacy helper retained for one-off checks. The optimized repeated-split block below
  # avoids this function because it would recompute the expensive correlation matrix for every k.
  X <- as.data.frame(X)
  X_clean <- tryCatch(clean_manifest_matrix(X, label = paste0(label, "_manifest"), out_dir = NULL), error = function(e) NULL)
  if (is.null(X_clean) || ncol(X_clean) <= k) return(NULL)

  R_raw <- tryCatch(compute_R_named(X_clean, label = paste0(label, "_cor")), error = function(e) NULL)
  if (is.null(R_raw)) return(NULL)

  san <- tryCatch(sanitize_correlation_matrix(R_raw, X = X_clean, label = label, out_dir = NULL), error = function(e) NULL)
  if (is.null(san) || ncol(san$R) <= k) return(NULL)

  efa <- tryCatch(fit_efa_psych(san$R, n_obs = nrow(san$X), k = k), error = function(e) NULL)
  if (is.null(efa)) return(NULL)

  L <- as_loading_matrix(efa, fallback_names = colnames(san$R))
  list(loadings = L, n = nrow(san$X), p = ncol(san$R))
}

quiet_try <- function(expr, quiet = TRUE) {
  # Suppresses repeated split noise from psych::polychoric/fa/cor.smooth while preserving failures as NULL.
  tryCatch({
    if (isTRUE(quiet)) {
      out <- NULL
      utils::capture.output({
        out <- suppressWarnings(suppressMessages(expr))
      })
      out
    } else {
      expr
    }
  }, error = function(e) NULL)
}

fit_efa_loadings_from_R <- function(R, n_obs, k, label = "split", quiet = TRUE) {
  R <- as.matrix(R)
  if (is.null(R) || ncol(R) <= k) return(NULL)

  efa <- quiet_try(
    fit_efa_psych(R, n_obs = n_obs, k = k),
    quiet = quiet
  )
  if (is.null(efa)) return(NULL)

  L <- as_loading_matrix(efa, fallback_names = colnames(R))
  list(loadings = L, n = n_obs, p = ncol(R))
}

get_repeated_split_k_values <- function() {
  rs_cfg <- config$validation$repeated_splits

  if (!is.null(rs_cfg$k_values)) {
    ks <- rs_cfg$k_values
  } else if (exists("k_values", inherits = TRUE)) {
    ks <- get("k_values", inherits = TRUE)
  } else if (!is.null(config$retention$k_values)) {
    ks <- config$retention$k_values
  } else if (!is.null(config$retention$k_fixed)) {
    ks <- config$retention$k_fixed
  } else {
    ks <- integer(0)
  }

  ks <- sort(unique(as.integer(ks)))
  ks[is.finite(ks) & !is.na(ks) & ks >= 1]
}

.repeated_split_tucker_cache <- new.env(parent = emptyenv())

run_repeated_split_tucker_all_k_once <- function() {
  # Optimized repeated-split stability:
  # old approach: for each k, for each split, recompute polychoric R_train/R_test.
  # new approach: for each split, compute R_train/R_test once, then fit k = 3/4/5 on the same R.
  # This keeps the method the same but removes the main repeated runtime bottleneck.
  rs_cfg <- config$validation$repeated_splits
  if (is.null(rs_cfg) || !isTRUE(rs_cfg$enabled) || config$efa$engine != "psych") return(list())

  if (!is.null(.repeated_split_tucker_cache$results)) {
    return(.repeated_split_tucker_cache$results)
  }

  repeated_k_values <- get_repeated_split_k_values()
  if (length(repeated_k_values) == 0) return(list())

  n_splits <- as.integer(rs_cfg$n_splits)
  if (!is.finite(n_splits) || n_splits < 1) return(list())

  quiet <- if (is.null(rs_cfg$suppress_split_messages)) TRUE else isTRUE(rs_cfg$suppress_split_messages)
  progress_every <- if (is.null(rs_cfg$progress_every)) 1L else as.integer(rs_cfg$progress_every)
  if (!is.finite(progress_every) || progress_every < 1) progress_every <- 1L

  start_time <- Sys.time()
  message(
    "\n--- Repeated split Tucker stability | optimized shared correlations ---\n",
    "k values = ", paste(repeated_k_values, collapse = ", "),
    " | splits = ", n_splits,
    " | prop = ", rs_cfg$prop,
    " | correlation type = ", corr_type,
    "\nEach split computes train/test correlations once and reuses them across k."
  )

  set.seed(as.integer(rs_cfg$seed))

  split_rows <- list()
  failure_rows <- list()

  add_failure_for_all_k <- function(split, stage, detail = NA_character_) {
    for (kk in repeated_k_values) {
      failure_rows[[length(failure_rows) + 1]] <<- data.frame(
        k = kk,
        split = split,
        stage = stage,
        detail = detail,
        row.names = NULL
      )
    }
  }

  for (i in seq_len(n_splits)) {
    if (i %% progress_every == 0 || i == 1L || i == n_splits) {
      message("Repeated split ", i, "/", n_splits, ": computing shared train/test correlations...")
    }

    strata_var <- rs_cfg$strata_var
    if (!is.null(strata_var) && !(strata_var %in% names(df))) strata_var <- NULL

    sp <- quiet_try({
      if (is.null(strata_var)) {
        rsample::initial_split(df, prop = rs_cfg$prop)
      } else {
        rsample::initial_split(df, prop = rs_cfg$prop, strata = all_of(strata_var))
      }
    }, quiet = quiet)

    if (is.null(sp)) {
      add_failure_for_all_k(i, "initial_split_failed")
      next
    }

    train_df_i <- rsample::training(sp)
    test_df_i <- rsample::testing(sp)

    Xtr0 <- select_manifest(train_df_i)
    Xte0 <- select_manifest(test_df_i)

    common0 <- intersect(names(Xtr0), names(Xte0))
    Xtr0 <- Xtr0[, common0, drop = FALSE]
    Xte0 <- Xte0[, common0, drop = FALSE]

    Xtr <- quiet_try(clean_manifest_matrix(Xtr0, label = paste0("repeat", i, "_training_manifest"), out_dir = NULL), quiet = quiet)
    Xte <- quiet_try(clean_manifest_matrix(Xte0, label = paste0("repeat", i, "_test_manifest"), out_dir = NULL), quiet = quiet)

    if (is.null(Xtr) || is.null(Xte)) {
      add_failure_for_all_k(i, "manifest_cleaning_failed")
      next
    }

    common1 <- intersect(names(Xtr), names(Xte))
    if (length(common1) < max(repeated_k_values) + 1L) {
      add_failure_for_all_k(i, "too_few_common_manifest_variables_after_cleaning", paste(length(common1), "common variables"))
      next
    }

    Xtr <- Xtr[, common1, drop = FALSE]
    Xte <- Xte[, common1, drop = FALSE]

    Rtr_raw <- quiet_try(compute_R_named(Xtr, label = paste0("repeat", i, "_training_cor")), quiet = quiet)
    Rte_raw <- quiet_try(compute_R_named(Xte, label = paste0("repeat", i, "_test_cor")), quiet = quiet)

    if (is.null(Rtr_raw) || is.null(Rte_raw)) {
      add_failure_for_all_k(i, "correlation_computation_failed")
      next
    }

    san_tr <- quiet_try(sanitize_correlation_matrix(Rtr_raw, X = Xtr, label = paste0("repeat", i, "_training"), out_dir = NULL), quiet = quiet)
    san_te <- quiet_try(sanitize_correlation_matrix(Rte_raw, X = Xte, label = paste0("repeat", i, "_test"), out_dir = NULL), quiet = quiet)

    if (is.null(san_tr) || is.null(san_te)) {
      add_failure_for_all_k(i, "correlation_sanitizing_failed")
      next
    }

    # Use the same variable set for train and test within this split.
    # This makes Tucker comparison cleaner and avoids k-specific repeated correlation work.
    common2 <- intersect(colnames(san_tr$R), colnames(san_te$R))
    if (length(common2) < max(repeated_k_values) + 1L) {
      add_failure_for_all_k(i, "too_few_common_variables_after_correlation_cleaning", paste(length(common2), "common variables"))
      next
    }

    Rtr <- san_tr$R[common2, common2, drop = FALSE]
    Rte <- san_te$R[common2, common2, drop = FALSE]
    ntr <- nrow(san_tr$X)
    nte <- nrow(san_te$X)

    split_status <- c()

    for (k in repeated_k_values) {
      if (ncol(Rtr) <= k || ncol(Rte) <= k) {
        failure_rows[[length(failure_rows) + 1]] <- data.frame(k = k, split = i, stage = "too_few_variables_for_k", detail = NA_character_)
        next
      }

      tr <- fit_efa_loadings_from_R(Rtr, n_obs = ntr, k = k, label = paste0("repeat", i, "_training_k", k), quiet = quiet)
      te <- fit_efa_loadings_from_R(Rte, n_obs = nte, k = k, label = paste0("repeat", i, "_test_k", k), quiet = quiet)

      if (is.null(tr) || is.null(te)) {
        failure_rows[[length(failure_rows) + 1]] <- data.frame(k = k, split = i, stage = "efa_failed", detail = NA_character_)
        next
      }

      common_loading_vars <- intersect(rownames(tr$loadings), rownames(te$loadings))
      if (length(common_loading_vars) < max(2L, k + 1L)) {
        failure_rows[[length(failure_rows) + 1]] <- data.frame(k = k, split = i, stage = "too_few_common_loading_rows", detail = paste(length(common_loading_vars), "rows"))
        next
      }

      Ltr <- tr$loadings[common_loading_vars, , drop = FALSE]
      Lte <- te$loadings[common_loading_vars, , drop = FALSE]

      cong <- quiet_try(psych::factor.congruence(Ltr, Lte), quiet = quiet)
      if (is.null(cong)) {
        failure_rows[[length(failure_rows) + 1]] <- data.frame(k = k, split = i, stage = "tucker_failed", detail = NA_character_)
        next
      }

      cong <- order_tucker_matrix(cong)
      match_i <- one_to_one_factor_match(cong)
      if (nrow(match_i) == 0) {
        failure_rows[[length(failure_rows) + 1]] <- data.frame(k = k, split = i, stage = "matching_failed", detail = NA_character_)
        next
      }

      match_i$k <- k
      match_i$split <- i
      match_i$n_train <- tr$n
      match_i$n_test <- te$n
      match_i$p_train <- tr$p
      match_i$p_test <- te$p
      match_i$n_common_loading_vars <- length(common_loading_vars)

      split_rows[[length(split_rows) + 1]] <- match_i
      split_status <- c(split_status, paste0("k", k, " min=", sprintf("%.2f", min(match_i$abs_congruence, na.rm = TRUE))))
    }

    if ((i %% progress_every == 0 || i == 1L || i == n_splits) && length(split_status) > 0) {
      message("Repeated split ", i, "/", n_splits, " done: ", paste(split_status, collapse = " | "))
    }
  }

  raw_all <- if (length(split_rows) > 0) dplyr::bind_rows(split_rows) else data.frame()
  failures_all <- if (length(failure_rows) > 0) {
    dplyr::bind_rows(failure_rows)
  } else {
    data.frame(k = integer(), split = integer(), stage = character(), detail = character())
  }

  result_list <- list()

  for (k in repeated_k_values) {
    raw <- if (nrow(raw_all) > 0) raw_all[raw_all$k == k, , drop = FALSE] else data.frame()
    failures <- if (nrow(failures_all) > 0) failures_all[failures_all$k == k, , drop = FALSE] else data.frame(k = integer(), split = integer(), stage = character(), detail = character())

    if (nrow(raw) == 0) {
      warning("Repeated split Tucker produced no usable splits for k = ", k, call. = FALSE)
      safe_write_csv(failures, make_outfile("repeated_split_tucker_failures", "csv", k_val = k, dir = debug_dir), row.names = FALSE)
      result_list[[as.character(k)]] <- NULL
      next
    }

    factor_summary <- raw |>
      dplyr::group_by(k, train_factor, train_factor_raw, train_factor_label) |>
      dplyr::summarise(
        n_successful_matches = dplyr::n(),
        mean_abs_congruence = mean(abs_congruence, na.rm = TRUE),
        median_abs_congruence = stats::median(abs_congruence, na.rm = TRUE),
        sd_abs_congruence = stats::sd(abs_congruence, na.rm = TRUE),
        min_abs_congruence = min(abs_congruence, na.rm = TRUE),
        q10_abs_congruence = as.numeric(stats::quantile(abs_congruence, probs = 0.10, na.rm = TRUE)),
        q90_abs_congruence = as.numeric(stats::quantile(abs_congruence, probs = 0.90, na.rm = TRUE)),
        prop_ge_good = mean(abs_congruence >= config$validation$tucker_cutoffs$good, na.rm = TRUE),
        prop_ge_excellent = mean(abs_congruence >= config$validation$tucker_cutoffs$excellent, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(factor_number(train_factor_raw))

    overall_summary <- data.frame(
      k = k,
      requested_splits = n_splits,
      successful_splits = length(unique(raw$split)),
      failed_splits = n_splits - length(unique(raw$split)),
      mean_abs_congruence = mean(raw$abs_congruence, na.rm = TRUE),
      median_abs_congruence = stats::median(raw$abs_congruence, na.rm = TRUE),
      min_abs_congruence = min(raw$abs_congruence, na.rm = TRUE),
      mean_factor_mean_abs_congruence = mean(factor_summary$mean_abs_congruence, na.rm = TRUE),
      min_factor_mean_abs_congruence = min(factor_summary$mean_abs_congruence, na.rm = TRUE),
      mean_prop_ge_good = mean(factor_summary$prop_ge_good, na.rm = TRUE),
      mean_prop_ge_excellent = mean(factor_summary$prop_ge_excellent, na.rm = TRUE),
      runtime_optimization = "shared_train_test_correlations_across_k_within_each_split",
      row.names = NULL
    )

    if (isTRUE(rs_cfg$write_raw_split_rows)) {
      safe_write_csv(raw, make_outfile("repeated_split_tucker_raw_matches", "csv", k_val = k, dir = tables_dir), row.names = FALSE)
    }
    safe_write_csv(factor_summary, make_outfile("repeated_split_tucker_factor_summary", "csv", k_val = k, dir = tables_dir), row.names = FALSE)
    safe_write_csv(overall_summary, make_outfile("repeated_split_tucker_overall_summary", "csv", k_val = k, dir = tables_dir), row.names = FALSE)
    safe_write_csv(failures, make_outfile("repeated_split_tucker_failures", "csv", k_val = k, dir = debug_dir), row.names = FALSE)

    xlsx_sheets <- list(
      overall_summary = overall_summary,
      factor_summary = factor_summary
    )
    if (nrow(failures) > 0) xlsx_sheets$failures <- failures
    if (isTRUE(rs_cfg$write_raw_split_rows)) xlsx_sheets$raw_matches <- raw

    writexl::write_xlsx(
      xlsx_sheets,
      make_outfile("repeated_split_tucker_summary", "xlsx", k_val = k, dir = summary_dir)
    )

    plot_repeated_tucker_summary(factor_summary, k = k, raw = raw)

    result_list[[as.character(k)]] <- list(
      overall_summary = overall_summary,
      factor_summary = factor_summary,
      raw = raw,
      failures = failures
    )
  }

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
  message("Repeated split Tucker stability finished for all k in ", elapsed, " min.")

  .repeated_split_tucker_cache$results <- result_list
  result_list
}

run_repeated_split_tucker <- function(k) {
  rs_cfg <- config$validation$repeated_splits
  if (is.null(rs_cfg) || !isTRUE(rs_cfg$enabled) || config$efa$engine != "psych") return(NULL)

  results <- run_repeated_split_tucker_all_k_once()
  out <- results[[as.character(k)]]
  if (is.null(out)) return(NULL)
  out
}

export_solution_for_scoring <- function(efa_obj, k, X_train, R_train, assignment = NULL, factor_map = NULL, item_info = NULL) {
  # This is the only score-related export in this script.
  # It does NOT compute per-person scores. It saves the fitted EFA solution and
  # enough metadata so a separate scoring script can calculate scores later
  # after you have chosen k/fm/rotation.
  if (config$efa$engine != "psych") return(invisible(NULL))

  L <- as_loading_matrix(efa_obj, fallback_names = colnames(R_train))

  bundle <- list(
    purpose = "EFA solution bundle for a separate per-person scoring script; no scores computed here",
    k = k,
    created_at = as.character(Sys.time()),
    analysis_level = config$analysis_level,
    subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
    subscale_z_prefix = if (identical(config$analysis_level, "subscales")) config$subscales$z_prefix else NA_character_,
    input_file = data_path,
    stratification_path = if (file.exists(strat_path_full)) strat_path_full else NA_character_,
    correlation_type = corr_type,
    efa_engine = config$efa$engine,
    efa_fm = config$efa$fm,
    efa_rotate = config$efa$rotate,
    split_seed = config$split$seed,
    split_prop = config$split$prop,
    training_variables = colnames(X_train),
    n_training = nrow(X_train),
    p_training = ncol(X_train),
    efa_object = efa_obj,
    loadings = L,
    factor_correlations = if (!is.null(efa_obj$Phi)) efa_obj$Phi else NULL,
    assignment = assignment,
    factor_map = factor_map,
    item_info = item_info,
    scoring_note = paste(
      "In the scoring script, read this RDS, align the dataset to training_variables,",
      "then call psych::factor.scores(x = X_aligned, f = bundle$efa_object, method = your_chosen_method).",
      "Recommended methods after choosing a final oblique solution: tenBerge or regression."
    )
  )

  saveRDS(
    bundle,
    make_outfile("efa_solution_bundle_for_scoring", "rds", k_val = k, dir = summary_dir)
  )

  scoring_readme <- data.frame(
    field = c(
      "purpose", "k", "analysis_level", "correlation_type", "efa_engine",
      "efa_fm", "efa_rotate", "n_training", "p_training",
      "important_note"
    ),
    value = c(
      bundle$purpose, as.character(k), config$analysis_level, corr_type, config$efa$engine,
      config$efa$fm, config$efa$rotate, as.character(nrow(X_train)), as.character(ncol(X_train)),
      "This exploration script exports the fitted solution only; per-person scores should be computed in a separate scoring script."
    ),
    row.names = NULL
  )

  safe_write_csv(
    scoring_readme,
    make_outfile("efa_solution_bundle_for_scoring_README", "csv", k_val = k, dir = summary_dir),
    row.names = FALSE
  )

  invisible(bundle)
}

# ---------------------------
# 9) Run one k
# ---------------------------
# ---------------------------
run_single_k <- function(k, R_train, X_train, X_test = NULL) {
  message("\n============================================================")
  message("Running EFA for k = ", k)
  message("============================================================")

  k_summary <- list(
    k = k,
    status = "started",
    validation_status = NA_character_,
    congruence_status = NA_character_
  )

  fit_train <- NULL
  loadings_train <- NULL
  factor_map <- NULL
  assignment <- NULL
  assignment_enriched <- NULL
  rel_all <- NULL
  cong_summary <- NULL
  cong_one_to_one <- NULL
  Phi_df <- NULL
  repeated_tucker <- NULL

  if (k >= ncol(R_train)) {
    msg <- paste0("Skipping k = ", k, " because k must be smaller than p = ", ncol(R_train), ".")
    message(msg)
    append_log(Sys.time(), " | ", msg)
    k_summary$status <- "skipped_k_too_large"
    return(k_summary)
  }

  efa_train <- tryCatch(
    {
      if (config$efa$engine == "psych") {
        fit_efa_psych(R_train, n_obs = nrow(X_train), k = k)
      } else {
        fit_efa_lavaan(X_train, k = k)
      }
    },
    error = function(e) {
      msg <- paste0("Training EFA failed for k = ", k, ": ", conditionMessage(e))
      message(msg)
      append_log(Sys.time(), " | ", msg)
      NULL
    }
  )

  if (is.null(efa_train)) {
    k_summary$status <- "training_efa_failed"
    return(k_summary)
  }

  k_summary$status <- "training_efa_ok"

  if (write_debug_outputs) {
    saveRDS(efa_train, make_outfile("efa_training_fit", "rds", k_val = k, dir = raw_dir))
  }

  if (config$efa$engine == "psych") {
    L_train <- as_loading_matrix(efa_train, fallback_names = colnames(R_train))
    loadings_train <- extract_loadings_psych(efa_train)
    loadings_train <- enrich_with_item_info(loadings_train, item_info)
    fit_train <- extract_fit_psych(efa_train, k = k)

    assign_obj <- assign_items_to_factors(L_train)
    assignment <- assign_obj$assignment
    factor_map <- assign_obj$factor_map
    assignment_enriched <- enrich_with_item_info(assignment, item_info)
    assignment_enriched$item_label <- make_item_plot_label(assignment_enriched)

    loadings_wide_info <- make_loadings_wide_with_info(L_train, item_info)
    loadings_long_info <- make_loadings_long(L_train, factor_map, item_info)
    marker_table <- make_marker_table(assignment, item_info, rule = "interpretive", top_n = config$plots$loading_marker_plot$top_n_per_factor)

    comm <- tibble::tibble(
      variable = names(efa_train$communality),
      communality = unname(efa_train$communality)
    ) |> enrich_with_item_info(item_info)

    uniq <- tibble::tibble(
      variable = names(efa_train$uniquenesses),
      uniqueness = unname(efa_train$uniquenesses)
    ) |> enrich_with_item_info(item_info)

    Phi_df <- if (!is.null(efa_train$Phi)) as.data.frame(efa_train$Phi) else NULL
    if (!is.null(Phi_df)) {
      plot_factor_correlation_heatmap(Phi_df, k = k)
      safe_write_csv(
        tibble::rownames_to_column(Phi_df, "factor_raw"),
        make_outfile("factor_correlation_matrix", "csv", k_val = k, dir = tables_dir),
        row.names = FALSE
      )
    }

    sheets <- list(
      fit = fit_train,
      factor_map_ordered = factor_map,
      item_assignment = assignment_enriched,
      top_marker_items = marker_table,
      loadings_wide_with_codebook = loadings_wide_info,
      loadings_long_with_codebook = loadings_long_info,
      communalities = comm,
      uniquenesses = uniq
    )

    if (!is.null(Phi_df)) sheets$factor_correlations <- tibble::rownames_to_column(Phi_df, "factor_raw")

    writexl::write_xlsx(sheets, make_outfile("efa_training_solution_readable", "xlsx", k_val = k, dir = tables_dir))

    plot_factor_diagram(efa_train, k = k)
    plot_loading_marker_items(marker_table, k = k)
    plot_loading_heatmap_readable(L_train, assignment, factor_map, k = k, item_info = item_info)

    export_solution_for_scoring(
      efa_obj = efa_train,
      k = k,
      X_train = X_train,
      R_train = R_train,
      assignment = assignment_enriched,
      factor_map = factor_map,
      item_info = item_info
    )

  } else {
    sink(make_outfile("efa_training_solution_lavaan_summary", "txt", k_val = k, dir = tables_dir))
    print(lavaan::summary(efa_train, fit.measures = TRUE, standardized = TRUE))
    sink()
  }

  # Factor scores are intentionally skipped in this exploration script.

  # Bass-Ackwards
  if (config$hierarchy$method == "bass_ackwards") {
    if (config$efa$engine != "psych") {
      message("Bass-Ackwards is implemented here only for psych::fa objects; skipping.")
    } else {
      max_f <- config$hierarchy$bass_ackwards$max_factors
      message("Running bassAckward decomposition up to ", max_f, " factors for k = ", k, "...")

      ba <- tryCatch(
        psych::bassAckward(
          r = R_train,
          nfactors = max_f,
          fm = config$efa$fm,
          rotate = config$efa$rotate,
          n.obs = nrow(X_train)
        ),
        error = function(e) NULL
      )

      if (!is.null(ba)) {
        if (write_debug_outputs) saveRDS(ba, make_outfile("bassAckward_training", "rds", k_val = k, dir = raw_dir))
        sink(make_outfile("bassAckward_training_summary", "txt", k_val = k, dir = debug_dir))
        print(ba)
        sink()
      } else {
        message("bassAckward failed for k = ", k, ".")
      }
    }
  }

  # Validation
  if (!is.null(X_test) && isTRUE(config$validation$refit_on_test) && config$efa$engine == "psych") {
    message("\n--- Validation: refit EFA on test sample, k = ", k, " ---")

    min_vars_needed <- max(2L, k + 1L)

    X_test_val <- tryCatch(
      clean_manifest_matrix(
        X_test,
        label = paste0("test_validation_prescreen_k", k),
        out_dir = out_dir
      ),
      error = function(e) {
        msg <- paste0("Validation prescreen failed for k = ", k, ": ", conditionMessage(e))
        message(msg)
        append_log(Sys.time(), " | ", msg)
        NULL
      }
    )

    if (is.null(X_test_val)) {
      k_summary$validation_status <- "validation_prescreen_failed"
    } else {
      common_x_vars <- intersect(names(X_train), names(X_test_val))

      if (length(common_x_vars) < min_vars_needed) {
        msg <- paste0(
          "Skipping validation for k = ", k, ": only ", length(common_x_vars),
          " common usable test variables remain after prescreening; need at least ",
          min_vars_needed, "."
        )
        message(msg)
        append_log(Sys.time(), " | ", msg)
        k_summary$validation_status <- "skipped_too_few_common_variables_after_prescreen"

        write.csv(
          data.frame(
            k = k,
            n_common_vars = length(common_x_vars),
            min_vars_needed = min_vars_needed,
            common_vars = paste(common_x_vars, collapse = ", "),
            reason = "too_few_common_variables_after_test_prescreen"
          ),
          make_outfile("validation_skipped_too_few_common_variables", "csv", k_val = k, dir = debug_dir),
          row.names = FALSE
        )

      } else {
        X_train_val <- X_train[, common_x_vars, drop = FALSE]
        X_test_val <- X_test_val[, common_x_vars, drop = FALSE]

        R_test_raw <- tryCatch(
          compute_R_named(X_test_val, label = paste0("test_validation_k", k)),
          error = function(e) {
            msg <- paste0("Test correlation computation failed for k = ", k, ": ", conditionMessage(e))
            message(msg)
            append_log(Sys.time(), " | ", msg)
            NULL
          }
        )

        if (is.null(R_test_raw)) {
          k_summary$validation_status <- "test_correlation_failed"
        } else {
          san_test <- tryCatch(
            sanitize_correlation_matrix(
              R_test_raw,
              X = X_test_val,
              label = paste0("test_k", k),
              out_dir = out_dir
            ),
            error = function(e) {
              msg <- paste0("Test correlation sanitizing failed for k = ", k, ": ", conditionMessage(e))
              message(msg)
              append_log(Sys.time(), " | ", msg)
              NULL
            }
          )

          if (is.null(san_test)) {
            k_summary$validation_status <- "test_correlation_sanitizing_failed"
          } else {
            R_test_val <- san_test$R
            X_test_val <- san_test$X

            common_vars2 <- intersect(names(X_train_val), colnames(R_test_val))

            if (length(common_vars2) < min_vars_needed) {
              msg <- paste0(
                "Skipping validation for k = ", k, " after correlation cleaning: only ",
                length(common_vars2), " common variables remain; need at least ",
                min_vars_needed, "."
              )
              message(msg)
              append_log(Sys.time(), " | ", msg)
              k_summary$validation_status <- "skipped_too_few_common_variables_after_correlation_cleaning"

              write.csv(
                data.frame(
                  k = k,
                  n_common_vars = length(common_vars2),
                  min_vars_needed = min_vars_needed,
                  common_vars = paste(common_vars2, collapse = ", "),
                  reason = "too_few_common_variables_after_correlation_cleaning"
                ),
                make_outfile("validation_skipped_after_correlation_cleaning", "csv", k_val = k, dir = debug_dir),
                row.names = FALSE
              )

            } else {
              X_train_val <- X_train_val[, common_vars2, drop = FALSE]
              X_test_val <- X_test_val[, common_vars2, drop = FALSE]
              R_test_val <- R_test_val[common_vars2, common_vars2, drop = FALSE]

              write.csv(R_test_val, make_outfile("correlation_matrix_test", "csv", k_val = k, dir = debug_dir), row.names = TRUE)

              efa_test <- tryCatch(
                psych::fa(
                  r = R_test_val,
                  nfactors = k,
                  fm = config$efa$fm,
                  rotate = config$efa$rotate,
                  scores = "none",
                  n.obs = nrow(X_test_val)
                ),
                error = function(e) {
                  msg <- paste0("Test refit failed for k = ", k, ": ", conditionMessage(e))
                  message(msg)
                  append_log(Sys.time(), " | ", msg)
                  NULL
                }
              )

              if (is.null(efa_test)) {
                k_summary$validation_status <- "test_refit_failed"
              } else {
                k_summary$validation_status <- "test_refit_ok"

                if (write_debug_outputs) {
                  saveRDS(efa_test, make_outfile("efa_test_fit_refit", "rds", k_val = k, dir = raw_dir))
                }

                loadings_test <- extract_loadings_psych(efa_test)
                writexl::write_xlsx(
                  list(loadings = loadings_test),
                  make_outfile("efa_test_solution_refit", "xlsx", k_val = k, dir = debug_dir)
                )

                if (isTRUE(config$validation$congruence)) {
                  L_train <- as.matrix(unclass(efa_train$loadings))
                  L_test <- as.matrix(unclass(efa_test$loadings))

                  if (is.null(rownames(L_train)) || any(is.na(rownames(L_train))) || any(rownames(L_train) == "")) {
                    rownames(L_train) <- colnames(R_train)
                  }

                  if (is.null(rownames(L_test)) || any(is.na(rownames(L_test))) || any(rownames(L_test) == "")) {
                    rownames(L_test) <- colnames(R_test_val)
                  }

                  common_loading_vars <- intersect(rownames(L_train), rownames(L_test))

                  if (length(common_loading_vars) < min_vars_needed) {
                    msg <- paste0(
                      "Skipping Tucker congruence for k = ", k, ": only ",
                      length(common_loading_vars),
                      " common loading rows remain; need at least ",
                      min_vars_needed, "."
                    )
                    message(msg)
                    append_log(Sys.time(), " | ", msg)
                    k_summary$congruence_status <- "skipped_too_few_common_loading_rows"

                    write.csv(
                      data.frame(
                        k = k,
                        n_common_loading_vars = length(common_loading_vars),
                        min_vars_needed = min_vars_needed,
                        common_loading_vars = paste(common_loading_vars, collapse = ", "),
                        reason = "too_few_common_loading_rows_for_congruence"
                      ),
                      make_outfile("tucker_congruence_skipped_too_few_common_items", "csv", k_val = k, dir = debug_dir),
                      row.names = FALSE
                    )

                  } else {
                    L_train_sub <- L_train[common_loading_vars, , drop = FALSE]
                    L_test_sub <- L_test[common_loading_vars, , drop = FALSE]

                    if (!identical(rownames(L_train_sub), rownames(L_test_sub))) {
                      stop("Internal error: train/test loading rows are not aligned before congruence.", call. = FALSE)
                    }

                    message(
                      "Computing Tucker congruence for k = ", k,
                      " using ", length(common_loading_vars), " common loading rows."
                    )

                    cong <- psych::factor.congruence(L_train_sub, L_test_sub)
                    cong <- order_tucker_matrix(cong)

                    write.csv(
                      cong,
                      make_outfile("tucker_congruence_train_vs_test_raw", "csv", k_val = k, dir = tables_dir),
                      row.names = TRUE
                    )

                    cong_one_to_one <- one_to_one_factor_match(cong)
                    plot_tucker_congruence(cong, k = k, match_df = cong_one_to_one)

                    best_idx <- apply(abs(cong), 1, function(r) which.max(r))
                    best_signed <- cong[cbind(seq_len(nrow(cong)), best_idx)]

                    train_raw_best <- rownames(cong)
                    test_raw_best <- colnames(cong)[best_idx]
                    cong_summary <- data.frame(
                      k = k,
                      train_factor = factor_raw_to_factor(train_raw_best),
                      train_factor_raw = train_raw_best,
                      train_factor_label = factor_raw_to_label(train_raw_best),
                      best_test_factor = factor_raw_to_factor(test_raw_best),
                      best_test_factor_raw = test_raw_best,
                      best_test_factor_label = factor_raw_to_label(test_raw_best),
                      congruence_signed = as.numeric(best_signed),
                      congruence_abs = abs(as.numeric(best_signed)),
                      sign_flip = as.numeric(best_signed) < 0,
                      row.names = NULL
                    )

                    write.csv(
                      cong_summary,
                      make_outfile("tucker_congruence_summary_best_abs_matches", "csv", k_val = k, dir = tables_dir),
                      row.names = FALSE
                    )

                    write.csv(
                      cong_one_to_one,
                      make_outfile("tucker_congruence_one_to_one_matching", "csv", k_val = k, dir = tables_dir),
                      row.names = FALSE
                    )

                    message("Saved Tucker congruence outputs for k = ", k)
                    print(cong_summary)
                    print(cong_one_to_one)

                    k_summary$congruence_status <- "congruence_ok"
                    k_summary$mean_best_abs_congruence <- mean(cong_summary$congruence_abs, na.rm = TRUE)
                    k_summary$min_best_abs_congruence <- min(cong_summary$congruence_abs, na.rm = TRUE)
                    k_summary$mean_one_to_one_abs_congruence <- mean(cong_one_to_one$abs_congruence, na.rm = TRUE)
                    k_summary$min_one_to_one_abs_congruence <- min(cong_one_to_one$abs_congruence, na.rm = TRUE)
                  }
                } else {
                  k_summary$congruence_status <- "congruence_disabled"
                }
              }
            }
          }
        }
      }
    }
  } else {
    k_summary$validation_status <- "validation_disabled_or_unavailable"
    k_summary$congruence_status <- "validation_disabled_or_unavailable"
  }

  # Repeated split stability
  repeated_tucker <- run_repeated_split_tucker(k)
  if (!is.null(repeated_tucker)) {
    k_summary$repeated_mean_abs_congruence <- repeated_tucker$overall_summary$mean_abs_congruence[1]
    k_summary$repeated_median_abs_congruence <- repeated_tucker$overall_summary$median_abs_congruence[1]
    k_summary$repeated_min_abs_congruence <- repeated_tucker$overall_summary$min_abs_congruence[1]
    k_summary$repeated_successful_splits <- repeated_tucker$overall_summary$successful_splits[1]
    k_summary$repeated_min_factor_mean_abs_congruence <- repeated_tucker$overall_summary$min_factor_mean_abs_congruence[1]
  }

  # Reliability + compact decision outputs
  if (config$efa$engine == "psych") {
    if (is.null(assignment)) {
      L_train <- as_loading_matrix(efa_train, fallback_names = colnames(R_train))
      assign_obj <- assign_items_to_factors(L_train)
      assignment <- assign_obj$assignment
      factor_map <- assign_obj$factor_map
      assignment_enriched <- enrich_with_item_info(assignment, item_info)
      assignment_enriched$item_label <- make_item_plot_label(assignment_enriched)
    }

    rel_train <- reliability_report(X_train, assignment, label = "training", k = k, rule = config$assignment$reliability_rule)
    rel_test <- if (!is.null(X_test)) reliability_report(X_test, assignment, label = "test", k = k, rule = config$assignment$reliability_rule) else NULL

    rel_all <- bind_rows(rel_train, rel_test)

    message("\n--- Reliability summary, k = ", k, " ---")
    print(rel_all)

    write.csv(
      assignment_enriched,
      make_outfile("item_assignment_strict_and_interpretive_with_codebook", "csv", k_val = k, dir = tables_dir),
      row.names = FALSE
    )

    write.csv(
      rel_all,
      make_outfile("reliability_summary_train_and_test", "csv", k_val = k, dir = tables_dir),
      row.names = FALSE
    )

    assignment_summary <- make_assignment_summary(assignment, k = k)

    factor_size_summary <- assignment |>
      dplyr::group_by(primary_factor, primary_factor_raw, primary_factor_label, primary_factor_order) |>
      dplyr::summarise(
        n_primary_all = dplyr::n(),
        n_keep_strict = sum(keep_strict, na.rm = TRUE),
        n_keep_interpretive = sum(keep_interpretive, na.rm = TRUE),
        max_abs_loading = max(primary_loading, na.rm = TRUE),
        median_abs_loading = stats::median(primary_loading, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(primary_factor_order)

    decision_summary <- data.frame(
      k = k,
      status = k_summary$status,
      validation_status = k_summary$validation_status,
      congruence_status = k_summary$congruence_status,
      n_training = nrow(X_train),
      n_test = if (!is.null(X_test)) nrow(X_test) else NA_integer_,
      n_items_training = ncol(X_train),
      n_keep_strict = assignment_summary$n_keep_strict,
      n_keep_interpretive = assignment_summary$n_keep_interpretive,
      n_drop_strict_low_loading = assignment_summary$n_drop_strict_low_loading,
      n_drop_strict_crossloading = assignment_summary$n_drop_strict_crossloading,
      n_drop_interpretive_low_loading = assignment_summary$n_drop_interpretive_low_loading,
      n_drop_interpretive_crossloading = assignment_summary$n_drop_interpretive_crossloading,
      mean_best_abs_congruence = ifelse(is.null(k_summary$mean_best_abs_congruence), NA_real_, k_summary$mean_best_abs_congruence),
      min_best_abs_congruence = ifelse(is.null(k_summary$min_best_abs_congruence), NA_real_, k_summary$min_best_abs_congruence),
      mean_one_to_one_abs_congruence = ifelse(is.null(k_summary$mean_one_to_one_abs_congruence), NA_real_, k_summary$mean_one_to_one_abs_congruence),
      min_one_to_one_abs_congruence = ifelse(is.null(k_summary$min_one_to_one_abs_congruence), NA_real_, k_summary$min_one_to_one_abs_congruence),
      repeated_successful_splits = ifelse(is.null(k_summary$repeated_successful_splits), NA_integer_, k_summary$repeated_successful_splits),
      repeated_mean_abs_congruence = ifelse(is.null(k_summary$repeated_mean_abs_congruence), NA_real_, k_summary$repeated_mean_abs_congruence),
      repeated_median_abs_congruence = ifelse(is.null(k_summary$repeated_median_abs_congruence), NA_real_, k_summary$repeated_median_abs_congruence),
      repeated_min_abs_congruence = ifelse(is.null(k_summary$repeated_min_abs_congruence), NA_real_, k_summary$repeated_min_abs_congruence),
      repeated_min_factor_mean_abs_congruence = ifelse(is.null(k_summary$repeated_min_factor_mean_abs_congruence), NA_real_, k_summary$repeated_min_factor_mean_abs_congruence),
      RMSR = if (!is.null(fit_train)) fit_train$RMSR[1] else NA_real_,
      RMSEA = if (!is.null(fit_train)) fit_train$RMSEA[1] else NA_real_,
      TLI = if (!is.null(fit_train)) fit_train$TLI[1] else NA_real_,
      BIC = if (!is.null(fit_train)) fit_train$BIC[1] else NA_real_,
      row.names = NULL
    )

    summary_sheets <- list(
      decision_summary = decision_summary,
      factor_map_ordered = factor_map,
      factor_size_summary = factor_size_summary,
      assignment_summary = assignment_summary,
      reliability = rel_all
    )

    if (!is.null(cong_summary)) summary_sheets$tucker_best_abs_matches <- cong_summary
    if (!is.null(cong_one_to_one)) summary_sheets$tucker_one_to_one_matching <- cong_one_to_one
    if (!is.null(repeated_tucker)) {
      summary_sheets$repeated_tucker_overall <- repeated_tucker$overall_summary
      summary_sheets$repeated_tucker_by_factor <- repeated_tucker$factor_summary
    }
    if (!is.null(Phi_df)) summary_sheets$factor_correlations_raw <- tibble::rownames_to_column(Phi_df, "factor_raw")

    writexl::write_xlsx(
      summary_sheets,
      make_outfile("decision_summary", "xlsx", k_val = k, dir = summary_dir)
    )

    writexl::write_xlsx(
      list(
        reliability = rel_all,
        assignment = assignment_enriched,
        factor_map = factor_map
      ),
      make_outfile("reliability_and_assignment", "xlsx", k_val = k, dir = tables_dir)
    )

    sink(make_outfile("reliability_summary_train_and_test", "txt", k_val = k, dir = tables_dir))
    cat("Reliability summary based on training-derived item assignment rules\n")
    cat("k = ", k, "\n", sep = "")
    cat("Reliability assignment rule: ", config$assignment$reliability_rule, "\n", sep = "")
    cat("Strict rule: |primary loading| >= ", config$assignment$strict$primary_abs_min,
        " and 2nd-highest |loading| < ", config$assignment$strict$second_abs_max, "\n", sep = "")
    cat("Interpretive rule: |primary loading| >= ", config$assignment$interpretive$primary_abs_min,
        " and primary-vs-second loading gap >= ", config$assignment$interpretive$gap_min, "\n\n", sep = "")
    print(rel_all)
    sink()
  }

  k_summary
}


# ---------------------------
# 10) Run all k values
# ---------------------------
all_results <- lapply(k_values, function(k) {
  run_single_k(
    k = k,
    R_train = R_train,
    X_train = X_train,
    X_test = X_test
  )
})

run_summary <- dplyr::bind_rows(lapply(all_results, function(x) {
  as.data.frame(x, stringsAsFactors = FALSE)
}))

write.csv(
  run_summary,
  make_outfile("multi_k_run_summary", "csv", dir = summary_dir),
  row.names = FALSE
)

writexl::write_xlsx(
  list(run_summary = run_summary),
  make_outfile("multi_k_run_summary", "xlsx", dir = summary_dir)
)

message("\nDONE. Outputs in: ", out_dir)
message("Key summaries in: ", summary_dir)
message("Readable tables in: ", tables_dir)
message("Plots in: ", plots_dir)
if (write_debug_outputs) message("Debug/raw outputs in: ", debug_dir)
message("Run summary:")
print(run_summary)
