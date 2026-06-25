# ============================================================
# Exploratory Factor Analysis (EFA) Pipeline — bootstrap stability (automation-safe v9)
#
# Core design:
# - Fits the reference EFA on the complete original sample.
# - Uses psych::fa() only.
# - Uses a stratified nonparametric bootstrap to estimate how stable the
#   selected k-factor solution is under resampling of persons.
# - Preserves the joint project x group composition in every bootstrap sample.
# - Aligns bootstrap factors to the reference solution by one-to-one column
#   permutation and sign reflection; no Procrustes target rotation is used.
# - Caches bootstrap correlation matrices so they can be reused for other
#   factor numbers, extraction methods, and rotations.
# - Exports factor-level Tucker congruence, item-loading uncertainty,
#   assignment/crossloading stability, plots, and the scoring bundle.
# ============================================================


# ---------------------------
# 0) CONFIG
# ---------------------------
config <- list(

  # Project root:
  # If NULL, the script tries to auto-detect the R project root.
  root = NULL,

  # =========================================================
  # RUN MODE
  # =========================================================
  # TRUE: run the complete unattended analysis grid in separate R processes.
  # FALSE: run only the single manually configured variant below.
  automation = list(
    enabled = TRUE,

    # Automated grid. Each dataset/level combination is run with all extraction
    # methods and rotations below. Every job estimates k = 3, 4, and 5.
    analysis_levels = c("items", "subscales"),
    dataset_scopes = c("hitop", "complete"),
    fm_values = c("minres", "ml", "pa"),
    rotations = c("oblimin", "promax"),
    k_values = c(3, 4, 5),

    # Jobs run sequentially so they can safely reuse the same correlation cache.
    # A failed method-specific job is logged and the remaining jobs continue.
    continue_on_error = TRUE,

    # Before the 24-job grid starts, run one fast child-process preflight using
    # subscales + HiTOP + minres + oblimin. It stops before the bootstrap step.
    # This catches path, package, merge, and child-process problems immediately.
    preflight_child = TRUE,
    print_failed_log_tail = TRUE,
    failed_log_tail_n = 80,

    log_dir_rel = file.path("out", "factor_analysis", "efa", "_automation_logs")
  ),

  # =========================================================
  # MANUAL SINGLE-RUN SETTINGS
  # Used only when automation$enabled = FALSE.
  # =========================================================
  # "items" uses polychoric correlations; "subscales" uses Pearson correlations
  # because correlation$type is set to "auto" below.
  analysis_level = "subscales",  # c("items", "subscales")

  # "hitop" uses the reduced HiTOP input; "complete" uses all available scales/items.
  dataset_scope = "hitop",       # c("hitop", "complete")

  # Input files, relative to ROOT/03_analysis_input.
  input = list(
    dir_rel = "03_analysis_input",
    file_hitop_items = "adults_adolescents_HiTOP_items.xlsx",
    file_hitop_subscales = "adults_adolescents_HiTOP_subscales_lt030.xlsx",
    file_complete_items = "adults_adolescents_complete_items.xlsx",
    file_complete_subscales = "adults_adolescents_complete_subscales_lt030.xlsx"
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
    path_rel = file.path("03_analysis_input", "adults_adolescents_stratification_info.xlsx"),
    merge_method = "auto",  # c("auto", "by_key", "by_row_order")
    key = NULL,

    # In auto mode, prefer the person ID itself. Compound keys such as
    # vp_id + sample are only fallbacks because differently coded sample labels
    # can otherwise create large numbers of unmatched rows.
    preferred_auto_key = "vp_id",

    # The separate stratification file is authoritative for project/group.
    # Existing values in the analysis file are retained only where the
    # stratification file has no value.
    prefer_stratification_values = TRUE,

    # Handling for rows that still lack project/group after the merge.
    # Dataset coding rule: an empty group value means healthy control and is
    # therefore recoded to "HC". Missing project values remain explicitly marked
    # rather than guessed. Set unmatched_strata_action = "stop" for a final run
    # if unresolved project values should halt the pipeline.
    unmatched_strata_action = "explicit_level",  # c("stop", "drop", "explicit_level")
    missing_strata_labels = c(
      project = "<UNCLASSIFIED_PROJECT>",
      group = "HC"
    ),
    max_unmatched_drop_n = 10,
    max_unmatched_drop_prop = 0.02,

    # Temporary handling for duplicate person keys that contain conflicting
    # non-missing values. "keep_most_complete" keeps exactly one row per key:
    # the row with the largest number of non-missing fields; ties keep the first
    # row in the source file. All conflicts and selections are written to the
    # summary folder. Set this back to "stop" for the final confirmatory run.
    unresolved_duplicate_action = "keep_most_complete",  # c("stop", "keep_most_complete", "keep_first")

    strata_var = c("group", "age", "gender")
  ),

  # Bootstrap stability of the full-sample reference solution.
  # Strata are the JOINT combinations of project and group. Each bootstrap
  # sample contains exactly the original number of rows from every joint stratum.
  bootstrap = list(
    enabled = TRUE,
    # Use 50-100 for a quick smoke test; use 1000 for the final analysis.
    n_boot = 30,
    seed = 20260622,
    strata_vars = c("project", "group"),
    min_stratum_n_warn = 10,
    allow_missing_strata = FALSE,
    # 1.00 requires the exact reference item set in every retained bootstrap EFA.
    # Lowering this can increase the number of usable replicates but may make
    # congruence optimistic by omitting unstable variables.
    min_common_variable_prop = 1.00,
    # Parallel workers for the expensive correlation calculation and for the
    # subsequent bootstrap EFA fits. Reduce these values if RAM is limited.
    workers = 4,
    fit_workers = 4,
    progress_every = 25,

    # Expensive correlation matrices are cached independently of k/fm/rotation.
    # This is especially important for item-level polychoric analyses.
    cache_correlations = TRUE,
    reuse_cache = TRUE,
    cache_dir_rel = file.path("out", "factor_analysis", "efa", "_bootstrap_cache"),

    # Save large raw objects only when needed.
    save_raw_congruence_rows = TRUE,
    save_aligned_loading_array = FALSE,

    tucker_cutoffs = list(
      good = 0.85,
      excellent = 0.95
    )
  ),

  # Correlation matrix choice.
  correlation = list(
    type = "auto",          # items -> polychoric; subscales -> Pearson
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
    k_values = c(3, 4, 5),
    parallel = list(
      n_iter = 50,
      fa = "fa"
    )
  ),

  # EFA extraction and rotation. This pipeline uses psych::fa() only.
  efa = list(
    fm = "ml",
    rotate = "oblimin",
    n_rotations = 10
  ),

  # Hierarchy options.
  hierarchy = list(
    method = "none",            # c("none", "bass_ackwards", "latent_model")
    bass_ackwards = list(
      max_factors = 6
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
    bootstrap_congruence = list(
      width = 9.5,
      height = 6.5,
      dpi = 350
    ),
    bootstrap_assignment_heatmap = list(
      width = 11,
      min_height = 8,
      height_per_item = 0.10,
      dpi = 350,
      axis_text_size = 3.5
    ),
    bootstrap_item_stability = list(
      top_n = 40,
      width = 11,
      min_height = 7,
      height_per_item = 0.22,
      dpi = 350
    ),
    bootstrap_factor_correlations = list(
      width = 11,
      height = 6.5,
      dpi = 350
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
# 0b) Automation child-process overrides
# ---------------------------
# The unattended controller starts a frozen copy of this script once per
# analysis variant. Explicit overrides keep child processes independent of the
# current working directory and of unsaved/manual config differences.
.automation_child <- identical(Sys.getenv("EFA_AUTOMATION_CHILD", unset = "0"), "1")
.automation_preflight_only <- identical(Sys.getenv("EFA_PREFLIGHT_ONLY", unset = "0"), "1")

if (.automation_child) {
  config$automation$enabled <- FALSE

  env_root <- Sys.getenv("EFA_ROOT", unset = "")
  env_level <- Sys.getenv("EFA_ANALYSIS_LEVEL", unset = "")
  env_scope <- Sys.getenv("EFA_DATASET_SCOPE", unset = "")
  env_fm <- Sys.getenv("EFA_FM", unset = "")
  env_rotate <- Sys.getenv("EFA_ROTATE", unset = "")
  env_k <- Sys.getenv("EFA_K_VALUES", unset = "")
  env_strat_path <- Sys.getenv("EFA_STRAT_PATH_REL", unset = "")
  env_lib_paths <- Sys.getenv("EFA_LIB_PATHS", unset = "")

  if (nzchar(env_root)) {
    config$root <- normalizePath(env_root, winslash = "/", mustWork = FALSE)
  }
  if (nzchar(env_level)) config$analysis_level <- env_level
  if (nzchar(env_scope)) config$dataset_scope <- env_scope
  if (nzchar(env_fm)) config$efa$fm <- env_fm
  if (nzchar(env_rotate)) config$efa$rotate <- env_rotate
  if (nzchar(env_strat_path)) config$stratification$path_rel <- env_strat_path

  # Preserve the package library paths of the parent R session. This matters
  # when RStudio/renv and Rscript would otherwise see different libraries.
  if (nzchar(env_lib_paths)) {
    inherited_libs <- strsplit(env_lib_paths, .Platform$path.sep, fixed = TRUE)[[1]]
    inherited_libs <- inherited_libs[nzchar(inherited_libs) & dir.exists(inherited_libs)]
    if (length(inherited_libs) > 0) {
      .libPaths(unique(c(inherited_libs, .libPaths())))
    }
  }

  if (nzchar(env_k)) {
    parsed_k <- suppressWarnings(as.integer(strsplit(env_k, ",", fixed = TRUE)[[1]]))
    parsed_k <- parsed_k[is.finite(parsed_k) & !is.na(parsed_k) & parsed_k >= 1]
    if (length(parsed_k) == 0) stop("Automation supplied no valid k values.", call. = FALSE)
    config$retention$k_values <- sort(unique(parsed_k))
    config$retention$k_fixed <- NULL
    config$retention$prompt_for_k <- FALSE
  }
}


# ---------------------------
# 1) Packages + utilities
# ---------------------------
required_pkgs <- c(
  "readxl", "writexl", "dplyr", "tibble",
  "psych", "ggplot2", "scales", "tidyr"
)

optional_pkgs <- c(
  "here", "rprojroot", "nFactors", "stringr"
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
  library(psych)
  library(ggplot2)
  library(tidyr)
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
    sanitize_tag(config$efa$fm),
    "_", sanitize_tag(config$efa$rotate),
    "_nrot", as.integer(config$efa$n_rotations)
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

detect_current_script_path <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) > 0) {
    candidate <- sub("^--file=", "", file_arg[1])
    if (file.exists(candidate)) return(normalizePath(candidate, winslash = "/"))
  }

  # Works when the saved file is sourced from RStudio.
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    candidate <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(candidate) && file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/"))
    }
  }

  frame_files <- vapply(
    sys.frames(),
    function(fr) {
      x <- fr$ofile
      if (is.null(x) || length(x) == 0) "" else as.character(x[1])
    },
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files) & file.exists(frame_files)]
  if (length(frame_files) > 0) {
    return(normalizePath(frame_files[length(frame_files)], winslash = "/"))
  }

  stop(
    "Could not determine the path of this script. Save the script and run it with Rscript or RStudio Source.",
    call. = FALSE
  )
}

resolve_configured_input_filename <- function(level, scope) {
  key <- paste(scope, level, sep = "_")
  switch(
    key,
    hitop_items = config$input$file_hitop_items,
    hitop_subscales = config$input$file_hitop_subscales,
    complete_items = config$input$file_complete_items,
    complete_subscales = config$input$file_complete_subscales,
    stop("Unknown analysis level / dataset scope combination: ", key, call. = FALSE)
  )
}

run_automation_grid <- function() {
  script_path <- detect_current_script_path()
  root <- if (is.null(config$root)) {
    get_project_root()
  } else {
    normalizePath(config$root, winslash = "/", mustWork = FALSE)
  }

  levels <- unique(tolower(as.character(config$automation$analysis_levels)))
  scopes <- unique(tolower(as.character(config$automation$dataset_scopes)))
  fm_values <- unique(tolower(as.character(config$automation$fm_values)))
  rotations <- unique(tolower(as.character(config$automation$rotations)))
  ks <- sort(unique(as.integer(config$automation$k_values)))
  ks <- ks[is.finite(ks) & !is.na(ks) & ks >= 1]

  if (!all(levels %in% c("items", "subscales"))) {
    stop("automation$analysis_levels may contain only 'items' and 'subscales'.", call. = FALSE)
  }
  if (!all(scopes %in% c("hitop", "complete"))) {
    stop("automation$dataset_scopes may contain only 'hitop' and 'complete'.", call. = FALSE)
  }
  if (!all(fm_values %in% c("minres", "ml", "pa"))) {
    stop("automation$fm_values may contain only 'minres', 'ml', and 'pa'.", call. = FALSE)
  }
  if (!all(rotations %in% c("oblimin", "promax"))) {
    stop("automation$rotations may contain only 'oblimin' and 'promax'.", call. = FALSE)
  }
  if (length(ks) == 0) stop("automation$k_values contains no valid factor numbers.", call. = FALSE)

  jobs <- do.call(
    rbind,
    lapply(levels, function(level) {
      do.call(
        rbind,
        lapply(scopes, function(scope) {
          expand.grid(
            analysis_level = level,
            dataset_scope = scope,
            fm = fm_values,
            rotation = rotations,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
  rownames(jobs) <- NULL
  jobs$job_number <- seq_len(nrow(jobs))
  jobs$job_id <- sprintf(
    "%02d__%s__%s__%s__%s",
    jobs$job_number, jobs$analysis_level, jobs$dataset_scope, jobs$fm, jobs$rotation
  )

  input_dir <- file.path(root, config$input$dir_rel)
  required_inputs <- unique(vapply(
    seq_len(nrow(jobs)),
    function(i) resolve_configured_input_filename(jobs$analysis_level[i], jobs$dataset_scope[i]),
    character(1)
  ))
  missing_inputs <- file.path(input_dir, required_inputs)
  missing_inputs <- missing_inputs[!file.exists(missing_inputs)]
  if (length(missing_inputs) > 0) {
    stop(
      "Automation was not started because input files are missing:\n",
      paste(missing_inputs, collapse = "\n"),
      call. = FALSE
    )
  }

  if (isTRUE(config$stratification$use)) {
    strat_path <- file.path(root, config$stratification$path_rel)
    if (!file.exists(strat_path)) {
      stop(
        "Automation was not started because the stratification file is missing: ",
        strat_path,
        call. = FALSE
      )
    }
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d__%H-%M-%S")
  log_dir <- file.path(root, config$automation$log_dir_rel, timestamp)
  safe_mkdir(log_dir)

  # Freeze exactly the code currently visible in the RStudio source editor when
  # possible. This prevents child jobs from running an older on-disk version
  # when the user sourced unsaved edits. Outside RStudio, copy the saved file.
  frozen_script <- file.path(log_dir, "_frozen_automation_script.R")
  wrote_editor_buffer <- FALSE
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && length(ctx$contents) > 0) {
      tryCatch({
        writeLines(ctx$contents, frozen_script, useBytes = TRUE)
        wrote_editor_buffer <- file.exists(frozen_script)
      }, error = function(e) NULL)
    }
  }
  if (!wrote_editor_buffer) {
    copied <- file.copy(script_path, frozen_script, overwrite = TRUE)
    if (!isTRUE(copied) || !file.exists(frozen_script)) {
      stop("Could not create frozen script copy for child processes.", call. = FALSE)
    }
  }
  child_script_path <- normalizePath(frozen_script, winslash = "/", mustWork = TRUE)

  status <- jobs
  status$input_file <- vapply(
    seq_len(nrow(status)),
    function(i) resolve_configured_input_filename(status$analysis_level[i], status$dataset_scope[i]),
    character(1)
  )
  status$correlation_type <- ifelse(status$analysis_level == "items", "polychoric", "pearson")
  status$k_values <- paste(ks, collapse = ",")
  status$status <- "pending"
  status$exit_code <- NA_integer_
  status$started_at <- NA_character_
  status$finished_at <- NA_character_
  status$runtime_minutes <- NA_real_
  status$log_file <- file.path(log_dir, paste0(status$job_id, ".log"))
  status_file <- file.path(log_dir, "automation_job_status.csv")
  utils::write.csv(status, status_file, row.names = FALSE)

  rscript_bin <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  if (!file.exists(rscript_bin)) {
    stop("Rscript executable not found: ", rscript_bin, call. = FALSE)
  }

  show_log_tail <- function(log_file, n = 80L) {
    if (!file.exists(log_file)) {
      message("No child log file was created: ", log_file)
      return(invisible(NULL))
    }
    lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character())
    if (length(lines) == 0) {
      message("Child log is empty: ", log_file)
      return(invisible(NULL))
    }
    n <- max(1L, as.integer(n))
    message("\n----- LAST CHILD-LOG LINES -----")
    message(paste(tail(lines, n), collapse = "\n"))
    message("----- END CHILD LOG -----\n")
    invisible(NULL)
  }

  r_code_literal <- function(x) {
    paste(capture.output(dput(x)), collapse = "")
  }

  run_child <- function(level, scope, fm, rotation, log_file, preflight_only = FALSE) {
    # Do not pass the configuration through system2(env = ...). On Windows,
    # environment values containing spaces (especially R library paths) can make
    # Rscript terminate before it has written anything to the redirected log.
    # Instead, create a tiny launcher R script that sets the environment and
    # library paths from inside the child R process and then sources the frozen
    # analysis script.
    launcher_stem <- paste0(
      tools::file_path_sans_ext(basename(log_file)),
      "__launcher"
    )
    launcher_file <- file.path(log_dir, paste0(launcher_stem, ".R"))

    child_env <- c(
      EFA_AUTOMATION_CHILD = "1",
      EFA_PREFLIGHT_ONLY = if (isTRUE(preflight_only)) "1" else "0",
      EFA_ROOT = root,
      EFA_ANALYSIS_LEVEL = level,
      EFA_DATASET_SCOPE = scope,
      EFA_FM = fm,
      EFA_ROTATE = rotation,
      EFA_K_VALUES = paste(ks, collapse = ","),
      EFA_STRAT_PATH_REL = config$stratification$path_rel
    )

    launcher_lines <- c(
      "options(warn = 1)",
      paste0("parent_libs <- ", r_code_literal(.libPaths())),
      ".libPaths(unique(c(parent_libs[dir.exists(parent_libs)], .libPaths())))",
      paste0("child_env <- ", r_code_literal(child_env)),
      "do.call(Sys.setenv, as.list(child_env))",
      paste0("setwd(", r_code_literal(root), ")"),
      "cat('CHILD LAUNCHER START\\n')",
      "cat('R version: ', R.version.string, '\\n', sep = '')",
      "cat('Working directory: ', getwd(), '\\n', sep = '')",
      paste0(
        "cat('Frozen child script: ', ",
        r_code_literal(child_script_path),
        ", '\\n', sep = '')"
      ),
      "cat('Library paths: ', paste(.libPaths(), collapse = ' | '), '\\n', sep = '')",
      "cat('Variant: ', Sys.getenv('EFA_ANALYSIS_LEVEL'), ' | ', Sys.getenv('EFA_DATASET_SCOPE'), ' | fm=', Sys.getenv('EFA_FM'), ' | rotation=', Sys.getenv('EFA_ROTATE'), '\\n', sep = '')",
      "flush.console()",
      paste0(
        "source(", r_code_literal(child_script_path),
        ", echo = FALSE, chdir = FALSE, encoding = 'UTF-8')"
      )
    )

    writeLines(launcher_lines, launcher_file, useBytes = TRUE)
    launcher_file <- normalizePath(launcher_file, winslash = "/", mustWork = TRUE)

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(root)

    # On Windows, use a short .cmd wrapper for reliable combined stdout/stderr
    # redirection. This also captures failures that happen before R itself starts.
    if (.Platform$OS.type == "windows") {
      cmd_file <- file.path(log_dir, paste0(launcher_stem, ".cmd"))
      rscript_win <- normalizePath(rscript_bin, winslash = "\\", mustWork = TRUE)
      launcher_win <- normalizePath(launcher_file, winslash = "\\", mustWork = TRUE)
      log_win <- normalizePath(log_file, winslash = "\\", mustWork = FALSE)

      cmd_lines <- c(
        "@echo off",
        paste0(
          '"', rscript_win, '" --vanilla "', launcher_win,
          '" > "', log_win, '" 2>&1'
        ),
        "set CHILD_EXIT=%ERRORLEVEL%",
        "exit /b %CHILD_EXIT%"
      )
      writeLines(cmd_lines, cmd_file, useBytes = TRUE)

      comspec <- Sys.getenv("COMSPEC", unset = "cmd.exe")
      exit_code <- tryCatch(
        system2(
          command = comspec,
          args = c("/d", "/c", shQuote(cmd_file, type = "cmd")),
          wait = TRUE
        ),
        error = function(e) {
          cat(
            "Controller could not start Windows child wrapper: ",
            conditionMessage(e), "\\n",
            file = log_file, append = TRUE, sep = ""
          )
          999L
        }
      )
    } else {
      exit_code <- tryCatch(
        system2(
          command = rscript_bin,
          args = c("--vanilla", shQuote(launcher_file)),
          stdout = log_file,
          stderr = log_file,
          wait = TRUE
        ),
        error = function(e) {
          cat(
            "Controller could not start child R process: ",
            conditionMessage(e), "\\n",
            file = log_file, append = TRUE, sep = ""
          )
          999L
        }
      )
    }

    if (is.null(exit_code) || length(exit_code) == 0 || is.na(exit_code)) {
      exit_code <- 999L
    }

    # Never leave an empty log again. If the process died before producing any
    # output, write all information needed to diagnose the launch itself.
    log_size <- if (file.exists(log_file)) file.info(log_file)$size else NA_real_
    if (!identical(as.integer(exit_code), 0L) &&
        (!file.exists(log_file) || is.na(log_size) || log_size == 0)) {
      cat(
        "CHILD PROCESS FAILED BEFORE WRITING OUTPUT\\n",
        "Exit code: ", as.integer(exit_code), "\\n",
        "Rscript: ", rscript_bin, "\\n",
        "Launcher: ", launcher_file, "\\n",
        "Frozen script: ", child_script_path, "\\n",
        "Working directory: ", root, "\\n",
        "Parent library paths: ", paste(.libPaths(), collapse = " | "), "\\n",
        file = log_file,
        append = TRUE,
        sep = ""
      )
    }

    as.integer(exit_code)
  }

  message("\n============================================================")
  message("AUTOMATED EFA GRID")
  message("Jobs: ", nrow(status), " | k values per job: ", paste(ks, collapse = ", "))
  message("Child script snapshot: ", child_script_path)
  message("Progress and errors: ", log_dir)
  message("============================================================\n")

  if (isTRUE(config$automation$preflight_child)) {
    pf_level <- if ("subscales" %in% levels) "subscales" else levels[1]
    pf_scope <- if ("hitop" %in% scopes) "hitop" else scopes[1]
    pf_fm <- if ("minres" %in% fm_values) "minres" else fm_values[1]
    pf_rotation <- if ("oblimin" %in% rotations) "oblimin" else rotations[1]
    pf_log <- file.path(log_dir, "00__child_process_preflight.log")

    message(
      "Child-process preflight: ", pf_level, " | ", pf_scope,
      " | fm=", pf_fm, " | rotation=", pf_rotation
    )
    pf_exit <- run_child(
      level = pf_level,
      scope = pf_scope,
      fm = pf_fm,
      rotation = pf_rotation,
      log_file = pf_log,
      preflight_only = TRUE
    )
    if (is.null(pf_exit) || length(pf_exit) == 0 || is.na(pf_exit)) pf_exit <- 999L

    if (!identical(as.integer(pf_exit), 0L)) {
      show_log_tail(pf_log, config$automation$failed_log_tail_n)
      stop(
        "Automation child-process preflight failed with exit code ",
        as.integer(pf_exit),
        ". The 24-job grid was not started. See: ",
        pf_log,
        call. = FALSE
      )
    }
    message("Child-process preflight completed successfully. Starting full grid.\n")
  }

  for (i in seq_len(nrow(status))) {
    job_start <- Sys.time()
    status$status[i] <- "running"
    status$started_at[i] <- as.character(job_start)
    utils::write.csv(status, status_file, row.names = FALSE)

    message(
      "[", i, "/", nrow(status), "] ", status$analysis_level[i],
      " | ", status$dataset_scope[i],
      " | fm=", status$fm[i],
      " | rotation=", status$rotation[i]
    )

    exit_code <- run_child(
      level = status$analysis_level[i],
      scope = status$dataset_scope[i],
      fm = status$fm[i],
      rotation = status$rotation[i],
      log_file = status$log_file[i],
      preflight_only = FALSE
    )

    if (is.null(exit_code) || length(exit_code) == 0 || is.na(exit_code)) exit_code <- 999L
    job_end <- Sys.time()
    status$exit_code[i] <- as.integer(exit_code)
    status$finished_at[i] <- as.character(job_end)
    status$runtime_minutes[i] <- round(as.numeric(difftime(job_end, job_start, units = "mins")), 2)
    status$status[i] <- if (identical(as.integer(exit_code), 0L)) "completed" else "failed"
    utils::write.csv(status, status_file, row.names = FALSE)

    message(
      "    -> ", status$status[i],
      " in ", status$runtime_minutes[i], " min",
      if (status$status[i] == "failed") paste0(" | see ", status$log_file[i]) else ""
    )

    if (status$status[i] == "failed" && isTRUE(config$automation$print_failed_log_tail)) {
      show_log_tail(status$log_file[i], config$automation$failed_log_tail_n)
    }

    if (status$status[i] == "failed" && !isTRUE(config$automation$continue_on_error)) {
      message("Stopping automation because continue_on_error = FALSE.")
      break
    }
  }

  n_done <- sum(status$status == "completed")
  n_failed <- sum(status$status == "failed")
  message("\nAUTOMATION FINISHED: ", n_done, " completed, ", n_failed, " failed.")
  message("Job status: ", status_file)
  invisible(status)
}


.automation_controller_only <- FALSE
if (isTRUE(config$automation$enabled) && !.automation_child) {
  .automation_controller_only <- TRUE
  run_automation_grid()
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


if (!isTRUE(.automation_controller_only)) {

# ---------------------------
# 2) Resolve paths + read data
# ---------------------------
if (is.null(config$root)) {
  config$root <- get_project_root()
} else {
  config$root <- normalizePath(config$root, winslash = "/", mustWork = FALSE)
}

message("Project ROOT = ", config$root)
message(
  "Run variant: level=", config$analysis_level,
  " | dataset=", config$dataset_scope,
  " | fm=", config$efa$fm,
  " | rotation=", config$efa$rotate
)

input_dir <- file.path(config$root, config$input$dir_rel)

get_input_path <- function() {
  level <- tolower(trimws(as.character(config$analysis_level)))
  scope <- tolower(trimws(as.character(config$dataset_scope)))

  if (!(level %in% c("items", "subscales"))) {
    stop("Unknown analysis_level: ", level, ". Use 'items' or 'subscales'.", call. = FALSE)
  }
  if (!(scope %in% c("hitop", "complete"))) {
    stop("Unknown dataset_scope: ", scope, ". Use 'hitop' or 'complete'.", call. = FALSE)
  }

  filename <- resolve_configured_input_filename(level, scope)
  file.path(input_dir, filename)
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
bootstrap_tag <- paste0(
  "boot", config$bootstrap$n_boot,
  "_seed", config$bootstrap$seed, "_",
  sanitize_tag(paste(config$bootstrap$strata_vars, collapse = "_x_"))
)
threshold_tag <- make_threshold_tag()
analysis_option_tag <- make_analysis_option_tag()

# Keep the run directory deliberately short. Windows/K-drive paths can fail
# around 260 characters even when the directory itself exists.
# The full input filename, seed, strata, thresholds, fm, and rotation are still
# preserved in metadata and in the method-specific filenames.
scope_tag <- sanitize_tag(config$dataset_scope)
bootstrap_short_tag <- paste0(
  "b", as.integer(config$bootstrap$n_boot),
  "s", as.integer(config$bootstrap$seed),
  "_", paste(substr(config$bootstrap$strata_vars, 1, 1), collapse = "")
)

run_tag <- paste(
  run_date,
  config$analysis_level,
  scope_tag,
  kset_tag,
  bootstrap_short_tag,
  sep = "__"
)

file_prefix <- paste(
  run_date,
  config$analysis_level,
  scope_tag,
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

# Fail early with a useful message rather than letting write.csv()/writexl fail
# later with a misleading "No such file or directory" error.
if (.Platform$OS.type == "windows") {
  longest_base_dir <- max(nchar(c(summary_dir, tables_dir, plots_dir)))
  if (longest_base_dir > 210) {
    stop(
      "Output base path is still too long for reliable Windows/K-drive writing (",
      longest_base_dir, " characters before the filename): ", out_dir,
      call. = FALSE
    )
  }
}

shorten_out_stem <- function(stem, max_chars = 45) {
  stem0 <- sanitize_tag(stem)

  # Keep filenames short enough for Windows/K-drive and libxlsxwriter.
  # The full thresholds and settings are stored in run_metadata/config_snapshot;
  # filenames keep only k + extraction/rotation + a compact output name.
  stem_map <- c(
    correlation_matrix_reference_with_codebook_labels = "corr_ref_labels",
    correlation_matrix_reference_label_map = "corr_ref_labelmap",
    correlation_matrix_reference = "corr_ref",
    correlation_heatmap_reference = "corr_heatmap_ref",
    scree_plot_cng_reference = "scree_cng",
    parallel_analysis_reference = "parallel",
    run_metadata = "metadata",
    multi_k_run_summary = "multi_k_summary",
    efa_reference_solution_readable = "efa_solution",
    efa_solution_bundle_for_scoring = "scoring_bundle",
    efa_solution_bundle_for_scoring_README = "scoring_bundle_readme",
    item_assignment_strict_and_interpretive_with_codebook = "item_assignment",
    reliability_and_assignment = "reliability_assignment",
    decision_summary = "decision_summary",
    loading_heatmap_readable_interpretive = "loadings_interpretive",
    loading_heatmap_readable_strict = "loadings_strict",
    loading_heatmap_readable_all = "loadings_all",
    factor_correlation_matrix = "factor_corr",
    factor_correlation_matrix_heatmap = "factor_corr_heatmap",
    bootstrap_tucker_by_factor = "boot_tucker_factor_plot",
    bootstrap_tucker_overall = "boot_tucker_overall_plot",
    bootstrap_assignment_probability_heatmap = "boot_assignment_heatmap",
    bootstrap_least_stable_items = "boot_item_stability_plot",
    bootstrap_tucker_factor_summary = "boot_tucker_factor",
    bootstrap_tucker_overall_summary = "boot_tucker_overall",
    bootstrap_item_stability = "boot_item_stability",
    bootstrap_assignment_probabilities = "boot_assignment_probs",
    bootstrap_factor_matching_frequencies = "boot_factor_matching",
    bootstrap_tucker_raw = "boot_tucker_raw",
    bootstrap_tucker_raw_overall = "boot_tucker_raw_overall",
    bootstrap_stability_summary = "boot_stability_summary",
    bootstrap_factor_correlation_summary = "boot_factor_corr",
    bootstrap_factor_correlations_raw = "boot_factor_corr_raw",
    bootstrap_factor_correlation_plot = "boot_factor_corr_plot",
    manifest_variables_selected_reference = "selected_vars_reference"
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

# Dataset-level diagnostics depend only on the analysis level and indicator set,
# not on extraction method or rotation. The run directory already identifies
# items/subscales and HiTOP/complete, so these filenames intentionally omit
# fm/rotation and are written only once across the six method combinations.
make_dataset_outfile <- function(stem, ext, dir = tables_dir) {
  stem_tag <- shorten_out_stem(stem)
  outfile <- file.path(dir, paste0("dataset__", stem_tag, ".", ext))
  safe_mkdir(dirname(outfile))
  outfile
}

write_once <- function(path, writer) {
  if (file.exists(path)) {
    message("Dataset-level diagnostic already exists; skipping duplicate: ", path)
    return(invisible(FALSE))
  }
  writer(path)
  invisible(TRUE)
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

write_correlation_matrix_with_labels <- function(R, item_info = NULL, label = "reference") {
  xlsx_file <- make_dataset_outfile(
    paste0("correlation_matrix_", label, "_with_codebook_labels"),
    "xlsx",
    dir = tables_dir
  )
  map_file <- make_dataset_outfile(
    paste0("correlation_matrix_", label, "_label_map"),
    "csv",
    dir = tables_dir
  )

  if (file.exists(xlsx_file) && file.exists(map_file)) {
    message("Dataset-level correlation matrix already written; skipping duplicate.")
    return(invisible(FALSE))
  }

  lab_map <- make_variable_label_map(colnames(R), item_info, max_chars = 220)
  labels <- make.unique(lab_map$label, sep = "__")

  R_labeled <- as.data.frame(R)
  names(R_labeled) <- labels
  R_labeled <- tibble::rownames_to_column(R_labeled, "variable")
  R_labeled$variable_label <- labels[match(R_labeled$variable, lab_map$variable)]
  R_labeled <- R_labeled[, c("variable", "variable_label", labels), drop = FALSE]

  if (!file.exists(xlsx_file)) {
    writexl::write_xlsx(
      list(
        correlation_matrix_labeled = R_labeled,
        variable_label_map = lab_map
      ),
      xlsx_file
    )
  }

  if (!file.exists(map_file)) {
    safe_write_csv(lab_map, map_file, row.names = FALSE)
  }
  invisible(TRUE)
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
    stop("Stratification file not found: ", strat_path, call. = FALSE)
  }

  strat <- readxl::read_excel(strat_path) |> as.data.frame()
  message(
    "Loaded stratification info: ", basename(strat_path),
    " | N = ", nrow(strat), " | p = ", ncol(strat)
  )

  method <- config$stratification$merge_method

  duplicate_action <- tolower(trimws(as.character(
    config$stratification$unresolved_duplicate_action %||% "stop"
  )))
  valid_duplicate_actions <- c("stop", "keep_most_complete", "keep_first")
  if (!(duplicate_action %in% valid_duplicate_actions)) {
    stop(
      "config$stratification$unresolved_duplicate_action must be one of: ",
      paste(valid_duplicate_actions, collapse = ", "),
      ". Current value: ", duplicate_action,
      call. = FALSE
    )
  }

  key_has_duplicates <- function(dat, key_cols) {
    any(duplicated(dat[, key_cols, drop = FALSE]))
  }

  duplicate_key_strings <- function(dat, key_cols, max_n = 20L) {
    dup <- duplicated(dat[, key_cols, drop = FALSE]) |
      duplicated(dat[, key_cols, drop = FALSE], fromLast = TRUE)
    if (!any(dup)) return(character())
    vals <- unique(dat[dup, key_cols, drop = FALSE])
    vals <- utils::head(vals, max_n)
    apply(vals, 1, function(z) {
      paste(paste0(key_cols, "=", as.character(z)), collapse = " | ")
    })
  }

  is_effectively_missing <- function(x) {
    is.na(x) | (is.character(x) & trimws(x) == "")
  }

  # Resolve repeated rows only when they are mutually compatible. This is safe
  # for exact duplicates or complementary rows where one row merely contains
  # additional non-missing values. Conflicting non-missing values are never
  # selected or averaged automatically.
  coalesce_compatible_duplicates <- function(dat, key_cols, role) {
    if (!key_has_duplicates(dat, key_cols)) {
      return(list(data = dat, resolved = data.frame(), conflicts = data.frame()))
    }

    key_df <- dat[, key_cols, drop = FALSE]
    key_chr <- lapply(key_df, function(v) {
      z <- as.character(v)
      z[is.na(z)] <- "<NA>"
      z
    })
    group_key <- do.call(paste, c(key_chr, list(sep = "\u001f")))
    group_order <- unique(group_key)
    groups <- split(seq_len(nrow(dat)), factor(group_key, levels = group_order))

    output_rows <- vector("list", length(groups))
    resolved_rows <- list()
    conflict_rows <- list()

    for (g_i in seq_along(groups)) {
      idx <- groups[[g_i]]
      block <- dat[idx, , drop = FALSE]

      if (length(idx) == 1L) {
        output_rows[[g_i]] <- block
        next
      }

      non_key <- setdiff(names(dat), key_cols)
      conflict_cols <- character()

      for (nm in non_key) {
        v <- block[[nm]]
        keep <- !is_effectively_missing(v)
        vals <- unique(as.character(v[keep]))
        if (length(vals) > 1L) conflict_cols <- c(conflict_cols, nm)
      }

      key_label <- paste(
        paste0(key_cols, "=", as.character(block[1, key_cols, drop = TRUE])),
        collapse = " | "
      )

      if (length(conflict_cols) > 0L) {
        # Preserve every conflicting source row for diagnosis, but keep exactly
        # one row in the temporary analysis dataset so the person-level bootstrap
        # never counts a duplicated person twice.
        block_diag <- block
        block_diag$.source_row <- idx
        block_diag$.duplicate_role <- role
        block_diag$.duplicate_key <- key_label
        block_diag$.conflicting_columns <- paste(conflict_cols, collapse = " | ")
        conflict_rows[[length(conflict_rows) + 1L]] <- block_diag

        nonmissing_counts <- vapply(
          seq_len(nrow(block)),
          function(ii) {
            sum(vapply(
              block[ii, names(dat), drop = FALSE],
              function(v) !is_effectively_missing(v)[1L],
              logical(1)
            ))
          },
          integer(1)
        )

        selected_local <- if (identical(duplicate_action, "keep_most_complete")) {
          which.max(nonmissing_counts)
        } else {
          1L
        }

        output_rows[[g_i]] <- block[selected_local, names(dat), drop = FALSE]
        resolved_rows[[length(resolved_rows) + 1L]] <- data.frame(
          role = role,
          key = key_label,
          original_rows = length(idx),
          rows_after = 1L,
          resolution = paste0("conflicting_duplicate_", duplicate_action),
          conflicting_columns = paste(conflict_cols, collapse = " | "),
          selected_source_row = idx[selected_local],
          selected_nonmissing_fields = nonmissing_counts[selected_local],
          stringsAsFactors = FALSE
        )
        next
      }

      merged <- block[1, , drop = FALSE]
      for (nm in non_key) {
        v <- block[[nm]]
        keep <- !is_effectively_missing(v)
        if (any(keep)) merged[[nm]] <- v[which(keep)[1L]]
      }
      output_rows[[g_i]] <- merged

      resolved_rows[[length(resolved_rows) + 1L]] <- data.frame(
        role = role,
        key = key_label,
        original_rows = length(idx),
        rows_after = 1L,
        resolution = "coalesced_compatible_duplicate_rows",
        stringsAsFactors = FALSE
      )
    }

    conflicts <- if (length(conflict_rows) > 0L) {
      dplyr::bind_rows(conflict_rows)
    } else {
      data.frame()
    }
    resolved <- if (length(resolved_rows) > 0L) {
      dplyr::bind_rows(resolved_rows)
    } else {
      data.frame()
    }

    if (nrow(conflicts) > 0L) {
      diag_file <- file.path(
        summary_dir,
        paste0("duplicate_conflicts_", role, ".xlsx")
      )
      writexl::write_xlsx(
        list(conflicting_rows = conflicts),
        diag_file
      )

      if (identical(duplicate_action, "stop")) {
        stop(
          "Conflicting duplicate rows were found in the ", role,
          " data for key ", paste(key_cols, collapse = " + "), ". ",
          "Diagnostic workbook: ", diag_file,
          call. = FALSE
        )
      }

      warning(
        "TEMPORARY EXPLORATORY DUPLICATE HANDLING: conflicting duplicate rows were found in the ",
        role, " data for key ", paste(key_cols, collapse = " + "), ". ",
        "The run continues with unresolved_duplicate_action='", duplicate_action, "'. ",
        "Exactly one row per duplicate key is retained. Review before the final analysis. ",
        "Diagnostic workbook: ", diag_file,
        call. = FALSE
      )
    }

    out <- dplyr::bind_rows(output_rows)
    rownames(out) <- NULL

    if (nrow(resolved) > 0L) {
      message(
        "Collapsed ", nrow(resolved), " duplicate key(s) in ", role,
        " data using key ", paste(key_cols, collapse = " + "),
        ". N: ", nrow(dat), " -> ", nrow(out),
        ". Duplicate action: ", duplicate_action, "."
      )
      diag_file <- file.path(
        summary_dir,
        paste0("duplicate_resolution_log_", role, ".csv")
      )
      utils::write.csv(resolved, diag_file, row.names = FALSE)
    }

    list(data = out, resolved = resolved, conflicts = conflicts)
  }

  shared <- intersect(names(df), names(strat))
  id_like <- shared[
    grepl(
      "(^id$|(^|_)id$|^vp_?id$|^vpid$|^vpcode$|participant|subject|case)",
      shared,
      ignore.case = TRUE
    )
  ]

  if (length(id_like) == 0L) {
    stop(
      "No shared ID-like column exists in the analysis and stratification files.",
      call. = FALSE
    )
  }

  # Treat the person ID as the primary identity key. Compound keys such as
  # vp_id + sample can fail massively when sample labels are coded differently
  # across files. Duplicate IDs are handled explicitly above instead.
  preferred_id <- as.character(config$stratification$preferred_auto_key %||% "")
  if (nzchar(preferred_id) && preferred_id %in% id_like) {
    id_col <- preferred_id
  } else {
    id_col <- id_like[1L]
  }
  identity_key <- id_col

  if (method != "by_row_order") {
    df_res <- coalesce_compatible_duplicates(df, identity_key, "analysis")
    strat_res <- coalesce_compatible_duplicates(strat, identity_key, "stratification")
    df <- df_res$data
    strat <- strat_res$data
  }

  choose_auto_key <- function(df, strat) {
    shared_now <- intersect(names(df), names(strat))
    id_like_now <- shared_now[
      grepl(
        "(^id$|(^|_)id$|^vp_?id$|^vpid$|^vpcode$|participant|subject|case)",
        shared_now,
        ignore.case = TRUE
      )
    ]

    preferred_now <- as.character(config$stratification$preferred_auto_key %||% "")
    if (nzchar(preferred_now) && preferred_now %in% id_like_now) {
      id_like_now <- c(preferred_now, setdiff(id_like_now, preferred_now))
    }

    # Prefer a single person ID. Only try compound keys as a fallback.
    candidate_keys <- lapply(id_like_now, function(z) z)
    extras_now <- intersect(c("sample", "project", "p"), shared_now)
    for (id_now in id_like_now) {
      for (extra in extras_now) {
        candidate_keys[[length(candidate_keys) + 1L]] <- c(id_now, extra)
      }
    }

    for (key_try in candidate_keys) {
      if (all(key_try %in% names(df)) && all(key_try %in% names(strat)) &&
          !key_has_duplicates(df, key_try) && !key_has_duplicates(strat, key_try)) {
        return(key_try)
      }
    }

    id_now <- id_like_now[1L]
    dup_df <- duplicate_key_strings(df, id_now)
    dup_strat <- duplicate_key_strings(strat, id_now)
    stop(
      "No unique merge key remained after safely coalescing compatible duplicate rows.\n",
      "Duplicate keys in analysis file (first values): ",
      if (length(dup_df) == 0) "<none>" else paste(dup_df, collapse = "; "),
      "\nDuplicate keys in stratification file (first values): ",
      if (length(dup_strat) == 0) "<none>" else paste(dup_strat, collapse = "; "),
      "\nThese remaining duplicates need a substantive data-cleaning decision.",
      call. = FALSE
    )
  }

  if (method == "by_row_order") {
    if (nrow(df) != nrow(strat)) {
      stop(
        "Row-order merge requested but nrow(df) != nrow(strat).",
        call. = FALSE
      )
    }
    dup <- intersect(names(df), names(strat))
    if (length(dup) > 0) {
      strat <- strat |> dplyr::select(-dplyr::all_of(dup))
    }
    out <- dplyr::bind_cols(df, strat)
    if (nrow(out) != nrow(df)) {
      stop("Row-order merge unexpectedly changed N.", call. = FALSE)
    }
    return(out)
  }

  if (method == "by_key") {
    key <- config$stratification$key
    if (is.null(key) || length(key) == 0) {
      stop(
        "merge_method='by_key' but config$stratification$key is NULL.",
        call. = FALSE
      )
    }
    key <- as.character(key)
    if (!all(key %in% names(df)) || !all(key %in% names(strat))) {
      stop(
        "Configured merge key column(s) not found in both files: ",
        paste(key, collapse = ", "),
        call. = FALSE
      )
    }
  } else if (method == "auto") {
    if (!is.null(config$stratification$key) &&
        length(config$stratification$key) > 0) {
      key <- as.character(config$stratification$key)
      if (!all(key %in% names(df)) || !all(key %in% names(strat))) {
        stop(
          "Configured stratification key column(s) not found in both files: ",
          paste(key, collapse = ", "),
          call. = FALSE
        )
      }
    } else {
      key <- choose_auto_key(df, strat)
    }
  } else {
    stop(
      "Unknown stratification merge_method: ", method,
      ". Use 'auto', 'by_key', or 'by_row_order'.",
      call. = FALSE
    )
  }

  if (key_has_duplicates(df, key) || key_has_duplicates(strat, key)) {
    dup_df <- duplicate_key_strings(df, key)
    dup_strat <- duplicate_key_strings(strat, key)
    stop(
      "The selected merge key is not unique in both files: ",
      paste(key, collapse = " + "),
      ". A many-to-many join is not allowed.\n",
      "Duplicates in analysis file: ",
      if (length(dup_df) == 0) "<none>" else paste(dup_df, collapse = "; "),
      "\nDuplicates in stratification file: ",
      if (length(dup_strat) == 0) "<none>" else paste(dup_strat, collapse = "; "),
      call. = FALSE
    )
  }

  message("Stratification merge: joining by key = ", paste(key, collapse = " + "))

  requested_strat_cols <- unique(c(
    config$bootstrap$strata_vars,
    config$stratification$strata_var
  ))
  missing_required <- setdiff(requested_strat_cols, names(strat))
  if (length(missing_required) > 0) {
    warning(
      "Some requested stratification columns are absent from the stratification file: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  # Always import requested stratification columns, even when columns with the
  # same names already exist in the analysis file. The previous implementation
  # skipped those columns entirely; therefore pre-existing but mostly empty
  # project/group columns remained empty after the join.
  strat_value_cols <- intersect(requested_strat_cols, names(strat))
  strat_join <- strat[, unique(c(key, strat_value_cols)), drop = FALSE]
  helper_cols <- setdiff(names(strat_join), key)
  helper_names <- paste0(helper_cols, ".__strat")
  names(strat_join)[match(helper_cols, names(strat_join))] <- helper_names
  strat_join$.__strat_match <- TRUE

  n_before <- nrow(df)
  out <- dplyr::left_join(
    df,
    strat_join,
    by = key,
    relationship = "many-to-one"
  )

  if (nrow(out) != n_before) {
    stop(
      "Stratification merge changed N from ", n_before, " to ", nrow(out),
      ". This is not allowed for the person-level bootstrap.",
      call. = FALSE
    )
  }

  unmatched <- is.na(out$.__strat_match)
  if (any(unmatched)) {
    unmatched_file <- file.path(summary_dir, "unmatched_stratification_rows.csv")
    utils::write.csv(
      out[unmatched, intersect(c(key, "sample"), names(out)), drop = FALSE],
      unmatched_file,
      row.names = FALSE
    )
    warning(
      sum(unmatched), " analysis row(s) had no matching stratification row. ",
      "Diagnostic file: ", unmatched_file,
      call. = FALSE
    )
  }
  out$.__strat_match <- NULL

  prefer_strat <- isTRUE(
    config$stratification$prefer_stratification_values %||% TRUE
  )
  conflict_log <- list()

  for (nm in strat_value_cols) {
    helper <- paste0(nm, ".__strat")
    if (!(helper %in% names(out))) next

    new_value <- out[[helper]]
    if (!(nm %in% names(out))) {
      out[[nm]] <- new_value
    } else {
      old_value <- out[[nm]]
      old_missing <- is_effectively_missing(old_value)
      new_missing <- is_effectively_missing(new_value)
      conflict <- !old_missing & !new_missing &
        as.character(old_value) != as.character(new_value)

      if (any(conflict, na.rm = TRUE)) {
        conflict_log[[length(conflict_log) + 1L]] <- data.frame(
          row = which(conflict),
          variable = nm,
          analysis_value = as.character(old_value[conflict]),
          stratification_value = as.character(new_value[conflict]),
          stringsAsFactors = FALSE
        )
      }

      if (prefer_strat) {
        use_new <- !new_missing
        combined <- old_value
        combined[use_new] <- new_value[use_new]
        out[[nm]] <- combined
      } else {
        combined <- old_value
        fill <- old_missing & !new_missing
        combined[fill] <- new_value[fill]
        out[[nm]] <- combined
      }
    }
    out[[helper]] <- NULL
  }

  if (length(conflict_log) > 0L) {
    conflicts <- dplyr::bind_rows(conflict_log)
    conflict_file <- file.path(summary_dir, "stratification_value_conflicts.csv")
    utils::write.csv(conflicts, conflict_file, row.names = FALSE)
    warning(
      "Conflicting analysis-file and stratification-file values were found. ",
      if (prefer_strat) "The stratification-file values were used. " else "The analysis-file values were retained. ",
      "Diagnostic file: ", conflict_file,
      call. = FALSE
    )
  }

  missing_bootstrap_fields <- setdiff(config$bootstrap$strata_vars, names(out))
  if (length(missing_bootstrap_fields) > 0) {
    stop(
      "After the stratification merge, bootstrap strata variable(s) are still missing: ",
      paste(missing_bootstrap_fields, collapse = ", "),
      call. = FALSE
    )
  }

  strata_df <- out[, config$bootstrap$strata_vars, drop = FALSE]
  missing_strata_rows <- apply(
    strata_df,
    1,
    function(z) any(is.na(z) | trimws(as.character(z)) == "")
  )

  if (any(missing_strata_rows)) {
    n_missing <- sum(missing_strata_rows)
    prop_missing <- n_missing / nrow(out)
    missing_file <- file.path(summary_dir, "missing_bootstrap_strata_after_merge.csv")
    utils::write.csv(
      out[missing_strata_rows,
          intersect(c(key, "sample", config$bootstrap$strata_vars), names(out)),
          drop = FALSE],
      missing_file,
      row.names = FALSE
    )

    # Report which field is actually missing. The previous generic message could
    # not distinguish, for example, 304 missing group values from 304 unmatched
    # projects. These are substantively very different problems.
    missing_by_variable <- data.frame(
      variable = config$bootstrap$strata_vars,
      n_missing = vapply(
        config$bootstrap$strata_vars,
        function(nm) {
          v <- out[[nm]]
          sum(is.na(v) | trimws(as.character(v)) == "")
        },
        integer(1)
      ),
      stringsAsFactors = FALSE
    )
    missing_by_variable$proportion <- missing_by_variable$n_missing / nrow(out)
    missing_count_file <- file.path(summary_dir, "missing_bootstrap_strata_counts.csv")
    utils::write.csv(missing_by_variable, missing_count_file, row.names = FALSE)

    pattern_df <- as.data.frame(lapply(
      out[, config$bootstrap$strata_vars, drop = FALSE],
      function(v) is.na(v) | trimws(as.character(v)) == ""
    ))
    pattern_label <- apply(
      pattern_df,
      1,
      function(z) {
        missing_names <- names(pattern_df)[as.logical(z)]
        if (length(missing_names) == 0L) "complete" else paste(missing_names, collapse = " + ")
      }
    )
    pattern_summary <- as.data.frame(table(pattern_label), stringsAsFactors = FALSE)
    names(pattern_summary) <- c("missing_pattern", "n")
    pattern_summary <- pattern_summary[pattern_summary$missing_pattern != "complete", , drop = FALSE]
    pattern_file <- file.path(summary_dir, "missing_bootstrap_strata_patterns.csv")
    utils::write.csv(pattern_summary, pattern_file, row.names = FALSE)

    message(
      "Missing bootstrap strata after merge: ",
      paste0(missing_by_variable$variable, "=", missing_by_variable$n_missing, collapse = "; ")
    )

    action <- tolower(trimws(as.character(
      config$stratification$unmatched_strata_action %||% "stop"
    )))
    # Backward-compatible alias used in earlier script versions.
    if (identical(action, "keep_as_missing")) action <- "explicit_level"

    max_n <- as.integer(config$stratification$max_unmatched_drop_n %||% 0L)
    max_prop <- as.numeric(
      config$stratification$max_unmatched_drop_prop %||% 0
    )

    if (identical(action, "drop") &&
        n_missing <= max_n &&
        prop_missing <= max_prop) {
      warning(
        "Dropping ", n_missing,
        " row(s) with unresolved bootstrap strata for this exploratory run. ",
        "Diagnostics: ", missing_file, " and ", missing_count_file,
        call. = FALSE
      )
      out <- out[!missing_strata_rows, , drop = FALSE]
      rownames(out) <- NULL
    } else if (identical(action, "explicit_level")) {
      configured_labels <- config$stratification$missing_strata_labels %||% character()
      replacement_log <- list()

      for (nm in config$bootstrap$strata_vars) {
        v <- as.character(out[[nm]])
        miss <- is.na(v) | trimws(v) == ""
        if (!any(miss)) {
          out[[nm]] <- v
          next
        }

        label <- if (nm %in% names(configured_labels)) {
          as.character(configured_labels[[nm]])
        } else {
          paste0("<UNCLASSIFIED_", toupper(nm), ">")
        }
        if (!nzchar(label)) label <- paste0("<UNCLASSIFIED_", toupper(nm), ">")

        v[miss] <- label
        out[[nm]] <- v
        replacement_log[[length(replacement_log) + 1L]] <- data.frame(
          variable = nm,
          replacement_level = label,
          replacement_reason = if (identical(nm, "group") && identical(label, "HC")) {
            "dataset coding rule: missing group means healthy control"
          } else {
            "explicit placeholder for unresolved stratum"
          },
          n_replaced = sum(miss),
          stringsAsFactors = FALSE
        )
      }

      replacement_df <- dplyr::bind_rows(replacement_log)
      replacement_file <- file.path(summary_dir, "exploratory_missing_strata_replacements.csv")
      utils::write.csv(replacement_df, replacement_file, row.names = FALSE)

      group_n_replaced <- if ("group" %in% replacement_df$variable) {
        replacement_df$n_replaced[match("group", replacement_df$variable)]
      } else {
        0L
      }
      project_n_replaced <- if ("project" %in% replacement_df$variable) {
        replacement_df$n_replaced[match("project", replacement_df$variable)]
      } else {
        0L
      }

      warning(
        "STRATIFICATION RECODING: ", group_n_replaced,
        " missing group value(s) were recoded to 'HC' according to the dataset coding rule. ",
        project_n_replaced,
        " missing project value(s) were retained as an explicit placeholder level. ",
        "Diagnostics: ", missing_file, "; ", missing_count_file, "; ", pattern_file,
        call. = FALSE
      )
    } else {
      stop(
        "Missing project/group values remain for ", n_missing,
        " row(s) after the corrected stratification merge. ",
        "Counts by variable: ",
        paste0(missing_by_variable$variable, "=", missing_by_variable$n_missing, collapse = "; "),
        ". This exceeds the configured exploratory drop limit or action='stop'. ",
        "Diagnostics: ", missing_file, " and ", missing_count_file,
        call. = FALSE
      )
    }
  }

  out
}
df <- merge_stratification(df)


# ---------------------------
# 3) Full-sample reference data
# ---------------------------
# The reference EFA is fitted to the complete original sample. Bootstrap
# resamples are drawn from this same empirical sample to estimate sampling
# stability. There is no train/test split in the primary pipeline.
df_reference <- df

message("Reference/full-sample N = ", nrow(df_reference))


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
    config$bootstrap$strata_vars,
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

clean_manifest_matrix <- function(X, label = "reference", out_dir = NULL) {
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

X_reference <- select_manifest(df_reference)
X_reference <- clean_manifest_matrix(X_reference, label = "reference_full_sample", out_dir = out_dir)

safe_write_csv(
  data.frame(
    variable = names(X_reference),
    analysis_level = config$analysis_level,
    dataset_scope = config$dataset_scope,
    subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
    include_global_scores = if (identical(config$analysis_level, "subscales")) config$subscales$include_global_scores else NA_character_,
    sample = "reference_full_sample",
    row.names = NULL
  ),
  make_outfile("manifest_variables_selected_reference", "csv", dir = summary_dir),
  row.names = FALSE
)

N <- nrow(X_reference)
p <- ncol(X_reference)
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
    pc <- suppressWarnings(psych::polychoric(Xo, correct = 0, smooth = FALSE, progress = FALSE))
    return(pc$rho)
  }

  if (corr_type == "tetrachoric") {
    Xo <- prep_for_ordinal_corr(X)
    tc <- suppressWarnings(psych::tetrachoric(Xo, correct = 0, smooth = FALSE))
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

sanitize_correlation_matrix <- function(R, X = NULL, label = "reference", out_dir = NULL) {
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
    if (!isTRUE(config$correlation$smooth)) {
      stop(
        "Correlation matrix for ", label,
        " is not positive definite and smoothing is disabled.",
        call. = FALSE
      )
    }

    message(
      "Correlation matrix for ", label,
      " is not positive definite; applying psych::cor.smooth()."
    )
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

make_reference_correlation_cache_signature <- function(X) {
  data_md5 <- if (file.exists(data_path)) unname(tools::md5sum(data_path)) else NA_character_
  strat_md5 <- if (file.exists(strat_path_full)) unname(tools::md5sum(strat_path_full)) else NA_character_
  signature_lines <- c(
    "reference_cache_schema=2026-06-22-v1",
    paste0("data_md5=", data_md5),
    paste0("strat_md5=", strat_md5),
    paste0("n=", nrow(X)),
    paste0("variables=", paste(names(X), collapse = "|")),
    paste0("corr_type=", corr_type),
    paste0("pairwise=", config$correlation$use_pairwise),
    paste0("smooth=", config$correlation$smooth),
    paste0("psych_version=", as.character(utils::packageVersion("psych"))),
    paste0("R_version=", R.version.string)
  )
  tmp <- tempfile(fileext = ".txt")
  writeLines(signature_lines, tmp)
  on.exit(unlink(tmp), add = TRUE)
  unname(tools::md5sum(tmp))
}

prepare_reference_correlation <- function(X) {
  cache_dir <- file.path(config$root, config$bootstrap$cache_dir_rel)
  safe_mkdir(cache_dir)
  cache_key <- make_reference_correlation_cache_signature(X)
  cache_file <- file.path(cache_dir, paste0("reference_correlation_", cache_key, ".rds"))

  if (isTRUE(config$bootstrap$reuse_cache) && file.exists(cache_file)) {
    message("Loading cached full-sample correlation matrix: ", cache_file)
    cached <- readRDS(cache_file)
    if (!identical(cached$cache_key, cache_key)) {
      stop("Reference correlation cache signature mismatch.", call. = FALSE)
    }
    X_kept <- X[, cached$kept_variables, drop = FALSE]
    return(list(R = cached$R, X = X_kept, dropped = cached$dropped, cache_file = cache_file))
  }

  message("Computing full-sample ", corr_type, " correlation matrix...")
  R_raw <- compute_R_named(X, label = "reference_full_sample")
  san <- sanitize_correlation_matrix(
    R_raw,
    X = X,
    label = "reference_full_sample",
    out_dir = out_dir
  )

  cached <- list(
    cache_key = cache_key,
    created_at = as.character(Sys.time()),
    R = san$R,
    kept_variables = names(san$X),
    dropped = san$dropped
  )
  if (isTRUE(config$bootstrap$cache_correlations)) {
    saveRDS(cached, cache_file, compress = FALSE)
  }
  list(R = san$R, X = san$X, dropped = san$dropped, cache_file = cache_file)
}

reference_correlation <- prepare_reference_correlation(X_reference)
R_reference <- reference_correlation$R
X_reference <- reference_correlation$X
p <- ncol(X_reference)

if (write_debug_outputs) {
  write.csv(R_reference, make_outfile("correlation_matrix_reference", "csv", dir = debug_dir), row.names = TRUE)
}
write_correlation_matrix_with_labels(R_reference, item_info, label = "reference")


# ---------------------------
# 5b) Improved correlation heatmap
# ---------------------------
plot_correlation_heatmap <- function(R, label = "reference") {
  outfile <- make_dataset_outfile(
    paste0("correlation_heatmap_", label),
    "png",
    dir = plots_dir
  )
  if (file.exists(outfile)) {
    message("Dataset-level correlation heatmap already written; skipping duplicate.")
    return(invisible(FALSE))
  }

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
    outfile,
    width = config$plots$correlation_heatmap$width,
    height = config$plots$correlation_heatmap$height,
    dpi = config$plots$correlation_heatmap$dpi
  )
}

plot_correlation_heatmap(R_reference, label = "reference")


# ---------------------------
# 6) Adequacy checks + scree/CNG
# ---------------------------
N_eff <- nrow(X_reference)

kmo <- tryCatch(psych::KMO(R_reference), error = function(e) {
  append_log(Sys.time(), " | KMO failed: ", conditionMessage(e))
  NULL
})

bart <- tryCatch(psych::cortest.bartlett(R_reference, n = N_eff), error = function(e) {
  append_log(Sys.time(), " | Bartlett failed: ", conditionMessage(e))
  NULL
})

message("\n--- Adequacy checks (full-sample reference) ---")
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

eigs <- eigen(R_reference, symmetric = TRUE, only.values = TRUE)$values
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
    title = "Scree plot (full-sample reference)",
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

scree_plot_file <- make_dataset_outfile(
  "scree_plot_cng_reference",
  "png",
  dir = plots_dir
)
if (!file.exists(scree_plot_file)) {
  save_plot_png(
    scree_plot,
    scree_plot_file,
    width = 10,
    height = 7,
    dpi = 300
  )
} else {
  message("Dataset-level scree plot already written; skipping duplicate.")
}

k_suggest_parallel <- NA_integer_
if (config$retention$method == "parallel") {
  message("\nRunning parallel analysis (full-sample reference)...")
  cor_arg <- if (corr_type %in% c("polychoric", "tetrachoric")) "poly" else "cor"

  pa <- psych::fa.parallel(
    X_reference,
    fm = config$efa$fm,
    fa = config$retention$parallel$fa,
    n.iter = config$retention$parallel$n_iter,
    cor = cor_arg,
    plot = TRUE
  )

  k_suggest_parallel <- tryCatch({
    if (!is.null(pa$nfact)) as.integer(pa$nfact) else as.integer(pa$nfactors)
  }, error = function(e) NA_integer_)

  parallel_plot_file <- make_dataset_outfile("parallel_analysis_reference", "png", dir = plots_dir)
  safe_mkdir(dirname(parallel_plot_file))
  if (!file.exists(parallel_plot_file)) {
    png(parallel_plot_file, width = 1800, height = 1200, res = 220)
    psych::fa.parallel(
      X_reference,
      fm = config$efa$fm,
      fa = config$retention$parallel$fa,
      n.iter = config$retention$parallel$n_iter,
      cor = cor_arg,
      plot = TRUE
    )
    dev.off()
  } else {
    message("Dataset-level parallel-analysis plot already written; skipping duplicate.")
  }
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

  message("\nScree plot saved to: ", make_dataset_outfile("scree_plot_cng_reference", "png", dir = plots_dir))
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

too_large <- k_values >= ncol(R_reference)
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

writeLines(
  capture.output(str(config)),
  make_outfile("config_snapshot", "txt", dir = summary_dir)
)

write.csv(
  data.frame(
    run_date = run_date,
    analysis_level = config$analysis_level,
    dataset_scope = config$dataset_scope,
    automation_child = .automation_child,
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
    bootstrap_enabled = config$bootstrap$enabled,
    bootstrap_n = config$bootstrap$n_boot,
    bootstrap_seed = config$bootstrap$seed,
    bootstrap_strata = paste(config$bootstrap$strata_vars, collapse = " x "),
    unresolved_duplicate_action = config$stratification$unresolved_duplicate_action,
    subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
    subscale_z_prefix = if (identical(config$analysis_level, "subscales")) config$subscales$z_prefix else NA_character_,
    n_reference = nrow(X_reference),
    p_reference_after_cleaning = ncol(X_reference),
    correlation_type = corr_type,
    efa_engine = "psych",
    efa_fm = config$efa$fm,
    efa_rotate = config$efa$rotate,
    efa_n_rotations = config$efa$n_rotations,
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
    n.rotations = config$efa$n_rotations,
    scores = "none",
    n.obs = n_obs
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

  outfile <- make_outfile("factor_diagram_reference_debug", "png", k_val = k, dir = debug_dir)
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
      main = paste0("EFA factor diagram (full-sample reference, k = ", k, ")")
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
        title = paste0("Top marker loadings (full-sample reference, k = ", k, ", ", f_label, ")"),
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
      title = paste0("Readable loading heatmap (full-sample reference, k = ", k, ")"),
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
      title = paste0("Factor correlation matrix (full-sample reference, k = ", k, ")"),
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
  cong <- as.matrix(cong)
  nr <- nrow(cong)
  nc <- ncol(cong)
  if (nr == 0 || nc == 0) return(data.frame())

  k <- min(nr, nc)

  if (k > 8) {
    warning(
      "k > 8: using greedy one-to-one factor matching to avoid factorial runtime.",
      call. = FALSE
    )

    remaining_rows <- seq_len(nr)
    remaining_cols <- seq_len(nc)
    pairs <- list()

    while (length(remaining_rows) > 0 && length(remaining_cols) > 0) {
      sub_abs <- abs(cong[remaining_rows, remaining_cols, drop = FALSE])
      pos <- which(sub_abs == max(sub_abs, na.rm = TRUE), arr.ind = TRUE)[1, ]
      r <- remaining_rows[pos[1]]
      c <- remaining_cols[pos[2]]
      pairs[[length(pairs) + 1]] <- c(r, c)
      remaining_rows <- setdiff(remaining_rows, r)
      remaining_cols <- setdiff(remaining_cols, c)
    }

    pairs <- do.call(rbind, pairs)
    row_idx <- pairs[, 1]
    col_idx <- pairs[, 2]
    total_abs <- sum(abs(cong[cbind(row_idx, col_idx)]), na.rm = TRUE)
  } else {
    perms <- permute_vec(seq_len(nc))
    if (ncol(perms) > k) perms <- perms[, seq_len(k), drop = FALSE]

    scores <- apply(
      perms,
      1,
      function(p) sum(abs(cong[cbind(seq_len(k), p)]), na.rm = TRUE)
    )

    best <- perms[which.max(scores), ]
    row_idx <- seq_len(k)
    col_idx <- best
    total_abs <- max(scores, na.rm = TRUE)
  }

  reference_raw <- rownames(cong)[row_idx]
  candidate_raw <- colnames(cong)[col_idx]
  vals <- as.numeric(cong[cbind(row_idx, col_idx)])

  data.frame(
    reference_factor = factor_raw_to_factor(reference_raw),
    reference_factor_raw = reference_raw,
    reference_factor_label = factor_raw_to_label(reference_raw),
    matched_candidate_factor = factor_raw_to_factor(candidate_raw),
    matched_candidate_factor_raw = candidate_raw,
    matched_candidate_factor_label = factor_raw_to_label(candidate_raw),
    congruence = vals,
    abs_congruence = abs(vals),
    sign_flip = vals < 0,
    one_to_one_total_abs = total_abs,
    row.names = NULL
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
# 8b) Stratified bootstrap stability
# ---------------------------

quiet_try <- function(expr, quiet = TRUE) {
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

make_joint_bootstrap_strata <- function(df_in, strata_vars) {
  strata_vars <- as.character(strata_vars)
  missing_vars <- setdiff(strata_vars, names(df_in))

  if (length(missing_vars) > 0) {
    stop(
      "Bootstrap stratification variable(s) missing from the merged dataset: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  strata_df <- df_in[, strata_vars, drop = FALSE]

  missing_strata_rows <- apply(
    strata_df,
    1,
    function(z) any(is.na(z) | trimws(as.character(z)) == "")
  )

  if (any(missing_strata_rows) && !isTRUE(config$bootstrap$allow_missing_strata)) {
    stop(
      "Missing values were found in bootstrap stratification variables ",
      paste(strata_vars, collapse = ", "),
      " for ", sum(missing_strata_rows), " row(s). ",
      "Resolve these values or explicitly set bootstrap$allow_missing_strata = TRUE.",
      call. = FALSE
    )
  }

  for (nm in names(strata_df)) {
    v <- as.character(strata_df[[nm]])
    v[is.na(v) | trimws(v) == ""] <- "<NA>"
    strata_df[[nm]] <- v
  }

  stratum <- do.call(
    interaction,
    c(strata_df, list(drop = TRUE, lex.order = TRUE, sep = " x "))
  )
  tab <- as.data.frame(table(stratum), stringsAsFactors = FALSE)
  names(tab) <- c("stratum", "n")

  if (any(tab$n < config$bootstrap$min_stratum_n_warn)) {
    warning(
      "Small joint bootstrap strata detected (< ",
      config$bootstrap$min_stratum_n_warn,
      " persons): ",
      paste0(tab$stratum[tab$n < config$bootstrap$min_stratum_n_warn],
             " (n=", tab$n[tab$n < config$bootstrap$min_stratum_n_warn], ")",
             collapse = "; "),
      ". These cells will retain their exact size, but very small strata contribute little resampling variability.",
      call. = FALSE
    )
  }

  if (any(tab$n == 1)) {
    warning(
      "At least one project x group stratum contains only one person. ",
      "That person will appear in every bootstrap sample, so this stratum contributes no sampling variability: ",
      paste(tab$stratum[tab$n == 1], collapse = "; "),
      call. = FALSE
    )
  }

  list(stratum = stratum, table = tab)
}

draw_stratified_bootstrap_indices <- function(stratum, n_boot, seed) {
  set.seed(as.integer(seed))
  idx_by_stratum <- split(seq_along(stratum), stratum)

  lapply(seq_len(n_boot), function(b) {
    unlist(
      lapply(idx_by_stratum, function(idx) sample(idx, size = length(idx), replace = TRUE)),
      use.names = FALSE
    )
  })
}

bootstrap_cor_worker <- function(
  i,
  X,
  indices_list,
  corr_type,
  use_pairwise,
  smooth
) {
  tryCatch({
    Xb <- as.data.frame(X[indices_list[[i]], , drop = FALSE])

    n_nonmiss <- vapply(Xb, function(v) sum(!is.na(v)), integer(1))
    n_unique <- vapply(Xb, function(v) length(unique(v[!is.na(v)])), integer(1))
    sd_val <- vapply(Xb, function(v) {
      suppressWarnings(stats::sd(as.numeric(v), na.rm = TRUE))
    }, numeric(1))

    drop_manifest <- n_nonmiss < 2 | n_unique < 2 | !is.finite(sd_val) | sd_val == 0
    dropped <- names(Xb)[drop_manifest]

    if (any(drop_manifest)) {
      Xb <- Xb[, !drop_manifest, drop = FALSE]
    }

    if (ncol(Xb) < 2) {
      return(list(
        ok = FALSE,
        stage = "manifest_screening",
        detail = "fewer than two variables remained",
        dropped = dropped
      ))
    }

    if (corr_type == "pearson") {
      use <- if (isTRUE(use_pairwise)) "pairwise.complete.obs" else "complete.obs"
      R <- suppressWarnings(stats::cor(Xb, use = use))
    } else {
      Xo <- as.data.frame(Xb)
      for (nm in names(Xo)) {
        v <- Xo[[nm]]
        if (is.factor(v)) {
          Xo[[nm]] <- as.ordered(v)
        } else if (is.numeric(v)) {
          u <- unique(v[!is.na(v)])
          if (length(u) <= 10 && all(abs(u - round(u)) < 1e-8)) {
            Xo[[nm]] <- as.ordered(factor(v, levels = sort(unique(v), na.last = NA)))
          }
        }
      }

      if (corr_type == "polychoric") {
        R <- suppressWarnings(psych::polychoric(Xo, correct = 0, smooth = FALSE, progress = FALSE)$rho)
      } else if (corr_type == "tetrachoric") {
        R <- suppressWarnings(psych::tetrachoric(Xo, correct = 0, smooth = FALSE)$rho)
      } else {
        stop("Unknown correlation type: ", corr_type)
      }
    }

    R <- as.matrix(R)
    if (nrow(R) != ncol(Xb) || ncol(R) != ncol(Xb)) {
      return(list(
        ok = FALSE,
        stage = "correlation_dimension_mismatch",
        detail = paste0("dim(R)=", paste(dim(R), collapse = "x"),
                        "; p=", ncol(Xb)),
        dropped = dropped
      ))
    }

    rownames(R) <- names(Xb)
    colnames(R) <- names(Xb)
    storage.mode(R) <- "double"

    repeat {
      bad <- which(!is.finite(R), arr.ind = TRUE)
      if (nrow(bad) == 0) break

      bad_vars <- c(rownames(R)[bad[, 1]], colnames(R)[bad[, 2]])
      bad_counts <- sort(table(bad_vars), decreasing = TRUE)
      drop_var <- names(bad_counts)[1]

      dropped <- unique(c(dropped, drop_var))
      keep <- setdiff(colnames(R), drop_var)
      R <- R[keep, keep, drop = FALSE]

      if (ncol(R) < 2) {
        return(list(
          ok = FALSE,
          stage = "bad_correlation_cleanup",
          detail = "fewer than two variables remained",
          dropped = dropped
        ))
      }
    }

    keep_names <- colnames(R)
    R <- (R + t(R)) / 2
    rownames(R) <- keep_names
    colnames(R) <- keep_names
    diag(R) <- 1

    smoothed <- FALSE
    eig <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    if (any(eig <= 1e-8)) {
      if (!isTRUE(smooth)) {
        return(list(
          ok = FALSE,
          stage = "non_positive_definite",
          detail = "matrix smoothing disabled",
          dropped = dropped
        ))
      }

      R <- psych::cor.smooth(R)
      R <- as.matrix(R)
      rownames(R) <- keep_names
      colnames(R) <- keep_names
      R <- (R + t(R)) / 2
      diag(R) <- 1
      smoothed <- TRUE
    }

    list(
      ok = TRUE,
      R = R,
      n = nrow(Xb),
      p = ncol(R),
      dropped = dropped,
      smoothed = smoothed
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      stage = "worker_error",
      detail = conditionMessage(e),
      dropped = character()
    )
  })
}

make_bootstrap_cache_signature <- function(
  data_path,
  strat_path,
  X,
  corr_type,
  bootstrap_cfg,
  stratum_table,
  stratum_vector
) {
  data_md5 <- if (file.exists(data_path)) unname(tools::md5sum(data_path)) else NA_character_
  strat_md5 <- if (!is.null(strat_path) && file.exists(strat_path)) unname(tools::md5sum(strat_path)) else NA_character_

  signature_lines <- c(
    "cache_schema_version=2026-06-22-v1",
    paste0("data_md5=", data_md5),
    paste0("strat_md5=", strat_md5),
    paste0("n=", nrow(X)),
    paste0("variables=", paste(names(X), collapse = "|")),
    paste0("corr_type=", corr_type),
    paste0("pairwise=", config$correlation$use_pairwise),
    paste0("smooth=", config$correlation$smooth),
    paste0("psych_version=", as.character(utils::packageVersion("psych"))),
    paste0("R_version=", R.version.string),
    paste0("n_boot=", bootstrap_cfg$n_boot),
    paste0("seed=", bootstrap_cfg$seed),
    paste0("strata=", paste(bootstrap_cfg$strata_vars, collapse = "|")),
    paste0("stratum_counts=", paste(stratum_table$stratum, stratum_table$n, sep = ":", collapse = "|")),
    paste0("stratum_assignments=", paste(as.character(stratum_vector), collapse = "|"))
  )

  tmp <- tempfile(fileext = ".txt")
  writeLines(signature_lines, tmp)
  on.exit(unlink(tmp), add = TRUE)
  unname(tools::md5sum(tmp))
}

prepare_bootstrap_correlation_cache <- function(X_reference, df_reference) {
  if (!isTRUE(config$bootstrap$enabled)) return(NULL)

  strata <- make_joint_bootstrap_strata(
    df_reference,
    config$bootstrap$strata_vars
  )

  cache_key <- make_bootstrap_cache_signature(
    data_path = data_path,
    strat_path = strat_path_full,
    X = X_reference,
    corr_type = corr_type,
    bootstrap_cfg = config$bootstrap,
    stratum_table = strata$table,
    stratum_vector = strata$stratum
  )

  cache_dir <- file.path(config$root, config$bootstrap$cache_dir_rel)
  safe_mkdir(cache_dir)

  cache_file <- file.path(
    cache_dir,
    paste0("bootstrap_correlations_", cache_key, ".rds")
  )

  if (
    isTRUE(config$bootstrap$cache_correlations) &&
    isTRUE(config$bootstrap$reuse_cache) &&
    file.exists(cache_file)
  ) {
    message("Loading cached bootstrap correlation matrices: ", cache_file)
    cache <- readRDS(cache_file)

    if (!identical(cache$cache_key, cache_key)) {
      stop("Bootstrap cache signature mismatch.", call. = FALSE)
    }

    return(cache)
  }

  message(
    "\nPreparing ", config$bootstrap$n_boot,
    " stratified bootstrap correlation matrices.",
    "\nJoint strata: ", paste(config$bootstrap$strata_vars, collapse = " x "),
    "\nThis is the expensive step; the result is cached and can be reused across k/fm/rotation."
  )

  indices_list <- draw_stratified_bootstrap_indices(
    strata$stratum,
    n_boot = config$bootstrap$n_boot,
    seed = config$bootstrap$seed
  )

  workers <- suppressWarnings(as.integer(config$bootstrap$workers))
  if (!is.finite(workers) || workers < 1L) workers <- 1L
  workers <- min(workers, as.integer(config$bootstrap$n_boot))

  start_time <- Sys.time()

  if (workers > 1L) {
    message("Using ", workers, " parallel workers for bootstrap correlations.")
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages(library(psych))
      options(mc.cores = 1)
      NULL
    })

    parallel::clusterExport(
      cl,
      varlist = c(
        "bootstrap_cor_worker",
        "X_reference",
        "indices_list",
        "corr_type",
        "config"
      ),
      envir = environment()
    )

    results <- parallel::parLapplyLB(
      cl,
      seq_len(config$bootstrap$n_boot),
      function(i) {
        bootstrap_cor_worker(
          i = i,
          X = X_reference,
          indices_list = indices_list,
          corr_type = corr_type,
          use_pairwise = config$correlation$use_pairwise,
          smooth = config$correlation$smooth
        )
      }
    )

    parallel::stopCluster(cl)
    on.exit(NULL, add = FALSE)
  } else {
    results <- vector("list", config$bootstrap$n_boot)

    for (i in seq_len(config$bootstrap$n_boot)) {
      if (
        i == 1L ||
        i == config$bootstrap$n_boot ||
        i %% config$bootstrap$progress_every == 0L
      ) {
        message("Bootstrap correlation ", i, "/", config$bootstrap$n_boot)
      }

      results[[i]] <- bootstrap_cor_worker(
        i = i,
        X = X_reference,
        indices_list = indices_list,
        corr_type = corr_type,
        use_pairwise = config$correlation$use_pairwise,
        smooth = config$correlation$smooth
      )
    }
  }

  diagnostics <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
    res <- results[[i]]
    data.frame(
      bootstrap = i,
      success = isTRUE(res$ok),
      stage = if (isTRUE(res$ok)) "ok" else res$stage,
      detail = if (isTRUE(res$ok)) NA_character_ else res$detail,
      n = if (isTRUE(res$ok)) res$n else NA_integer_,
      p = if (isTRUE(res$ok)) res$p else NA_integer_,
      n_dropped = length(res$dropped),
      dropped_variables = paste(res$dropped, collapse = " | "),
      smoothed = if (isTRUE(res$ok)) isTRUE(res$smoothed) else NA,
      row.names = NULL
    )
  }))

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

  cache <- list(
    cache_key = cache_key,
    cache_schema_version = "2026-06-22-v1",
    created_at = as.character(Sys.time()),
    runtime_minutes = elapsed,
    config = list(
      n_boot = config$bootstrap$n_boot,
      seed = config$bootstrap$seed,
      strata_vars = config$bootstrap$strata_vars,
      corr_type = corr_type,
      use_pairwise = config$correlation$use_pairwise,
      smooth = config$correlation$smooth
    ),
    stratum_table = strata$table,
    indices = indices_list,
    results = results,
    diagnostics = diagnostics
  )

  if (isTRUE(config$bootstrap$cache_correlations)) {
    message("Saving bootstrap correlation cache: ", cache_file)
    saveRDS(cache, cache_file, compress = FALSE)
  }

  message(
    "Bootstrap correlation preparation finished in ", elapsed, " min. ",
    "Successful matrices: ", sum(diagnostics$success), "/", nrow(diagnostics)
  )

  cache
}

align_bootstrap_loadings <- function(L_reference, L_bootstrap) {
  common_vars <- intersect(rownames(L_reference), rownames(L_bootstrap))
  min_common <- ceiling(
    config$bootstrap$min_common_variable_prop * nrow(L_reference)
  )

  if (length(common_vars) < min_common) {
    return(list(
      ok = FALSE,
      stage = "too_few_common_loading_rows",
      detail = paste0(length(common_vars), " common variables; required ", min_common)
    ))
  }

  L_ref_sub <- L_reference[common_vars, , drop = FALSE]
  L_boot_sub <- L_bootstrap[common_vars, , drop = FALSE]

  cong <- psych::factor.congruence(L_ref_sub, L_boot_sub, digits = 10)
  cong <- as.matrix(cong)

  match_df <- one_to_one_factor_match(cong)
  if (nrow(match_df) != ncol(L_reference)) {
    return(list(
      ok = FALSE,
      stage = "factor_matching_failed",
      detail = paste0("matched ", nrow(match_df), " of ", ncol(L_reference), " factors")
    ))
  }

  reference_order <- colnames(L_reference)
  match_df <- match_df[match(reference_order, match_df$reference_factor_raw), , drop = FALSE]

  matched_col_idx <- match(
    match_df$matched_candidate_factor_raw,
    colnames(cong)
  )
  matched_row_idx <- match(
    match_df$reference_factor_raw,
    rownames(cong)
  )

  match_df$matched_abs_congruence <- abs(match_df$congruence)
  match_df$second_best_row_abs_congruence <- vapply(
    seq_len(nrow(match_df)),
    function(i) {
      row_values <- abs(cong[matched_row_idx[i], , drop = TRUE])
      row_values <- row_values[-matched_col_idx[i]]
      if (length(row_values) == 0) return(NA_real_)
      max(row_values, na.rm = TRUE)
    },
    numeric(1)
  )
  match_df$matching_margin <- (
    match_df$matched_abs_congruence -
      match_df$second_best_row_abs_congruence
  )

  boot_order <- match_df$matched_candidate_factor_raw
  signs <- ifelse(match_df$congruence < 0, -1, 1)

  L_aligned <- L_bootstrap[, boot_order, drop = FALSE]
  L_aligned <- sweep(L_aligned, 2, signs, `*`)
  colnames(L_aligned) <- reference_order

  list(
    ok = TRUE,
    loadings = L_aligned,
    match = match_df,
    candidate_order = boot_order,
    signs = signs,
    common_vars = common_vars,
    congruence = abs(match_df$congruence)
  )
}

plot_bootstrap_outputs <- function(
  factor_rows,
  overall_rows,
  item_summary,
  assignment_probabilities,
  phi_rows = NULL,
  k
) {
  good <- config$bootstrap$tucker_cutoffs$good
  excellent <- config$bootstrap$tucker_cutoffs$excellent

  factor_order <- sort_factor_names(unique(factor_rows$reference_factor))
  factor_rows$reference_factor <- factor(
    factor_rows$reference_factor,
    levels = factor_order
  )

  p_factor <- ggplot(
    factor_rows,
    aes(x = reference_factor, y = abs_congruence)
  ) +
    geom_violin(trim = FALSE, alpha = 0.25) +
    geom_boxplot(width = 0.16, outlier.shape = NA) +
    geom_jitter(width = 0.10, alpha = 0.12, size = 0.8) +
    geom_hline(yintercept = good, linetype = "dashed") +
    geom_hline(yintercept = excellent, linetype = "dotdash") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste0("Bootstrap stability of reference factors (k = ", k, ")"),
      subtitle = paste0(
        config$bootstrap$n_boot,
        " stratified person-level bootstrap samples; factors aligned by permutation and sign reflection"
      ),
      x = "Reference factor",
      y = "Absolute Tucker congruence"
    )

  save_plot_png(
    p_factor,
    make_outfile("bootstrap_tucker_by_factor", "png", k_val = k, dir = plots_dir),
    width = config$plots$bootstrap_congruence$width,
    height = config$plots$bootstrap_congruence$height,
    dpi = config$plots$bootstrap_congruence$dpi
  )

  overall_long <- tidyr::pivot_longer(
    overall_rows,
    cols = c(mean_abs_congruence, min_abs_congruence),
    names_to = "summary_type",
    values_to = "value"
  )
  overall_long$summary_type <- factor(
    overall_long$summary_type,
    levels = c("mean_abs_congruence", "min_abs_congruence"),
    labels = c("Mean across factors", "Minimum across factors")
  )

  p_overall <- ggplot(overall_long, aes(x = summary_type, y = value)) +
    geom_violin(trim = FALSE, alpha = 0.25) +
    geom_boxplot(width = 0.16, outlier.shape = NA) +
    geom_jitter(width = 0.10, alpha = 0.12, size = 0.8) +
    geom_hline(yintercept = good, linetype = "dashed") +
    geom_hline(yintercept = excellent, linetype = "dotdash") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste0("Bootstrap stability of the complete k = ", k, " solution"),
      x = NULL,
      y = "Absolute Tucker congruence"
    )

  save_plot_png(
    p_overall,
    make_outfile("bootstrap_tucker_overall", "png", k_val = k, dir = plots_dir),
    width = 8.5,
    height = 6,
    dpi = config$plots$bootstrap_congruence$dpi
  )

  item_order_df <- item_summary |>
    dplyr::arrange(
      factor_number(reference_factor),
      dplyr::desc(reference_primary_loading_abs)
    )

  label_map <- setNames(item_order_df$item_label, item_order_df$variable)
  assignment_probabilities$item_label <- unname(label_map[assignment_probabilities$variable])
  assignment_probabilities$item_label <- factor(
    assignment_probabilities$item_label,
    levels = rev(item_order_df$item_label)
  )
  assignment_probabilities$assigned_factor <- factor(
    assignment_probabilities$assigned_factor,
    levels = factor_order
  )

  heat_height <- max(
    config$plots$bootstrap_assignment_heatmap$min_height,
    nrow(item_order_df) * config$plots$bootstrap_assignment_heatmap$height_per_item
  )

  p_heat <- ggplot(
    assignment_probabilities,
    aes(x = assigned_factor, y = item_label, fill = probability)
  ) +
    geom_tile(color = "grey92") +
    scale_fill_gradient(
      low = "white",
      high = "#2166AC",
      limits = c(0, 1),
      oob = scales::squish,
      name = "Assignment\nprobability"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = config$plots$bootstrap_assignment_heatmap$axis_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = paste0("Bootstrap primary-factor assignment probabilities (k = ", k, ")"),
      subtitle = "Rows are ordered by the full-sample reference assignment and loading strength.",
      x = "Aligned reference factor",
      y = NULL
    )

  save_plot_png(
    p_heat,
    make_outfile("bootstrap_assignment_probability_heatmap", "png", k_val = k, dir = plots_dir),
    width = config$plots$bootstrap_assignment_heatmap$width,
    height = heat_height,
    dpi = config$plots$bootstrap_assignment_heatmap$dpi
  )

  top_n <- min(config$plots$bootstrap_item_stability$top_n, nrow(item_summary))
  unstable <- item_summary |>
    dplyr::arrange(prop_same_primary_factor, prop_keep_strict) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::select(
      variable, item_label, reference_factor,
      prop_same_primary_factor, prop_keep_strict, prop_keep_interpretive
    ) |>
    tidyr::pivot_longer(
      cols = c(prop_same_primary_factor, prop_keep_strict, prop_keep_interpretive),
      names_to = "metric",
      values_to = "proportion"
    )

  unstable$metric <- factor(
    unstable$metric,
    levels = c("prop_same_primary_factor", "prop_keep_strict", "prop_keep_interpretive"),
    labels = c("Same primary factor", "Meets strict rule", "Meets interpretive rule")
  )
  unstable$item_label <- factor(
    unstable$item_label,
    levels = rev(unique(unstable$item_label))
  )

  p_items <- ggplot(
    unstable,
    aes(x = item_label, y = proportion, shape = metric)
  ) +
    geom_point(position = position_dodge(width = 0.45), size = 2) +
    coord_flip(ylim = c(0, 1)) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank()) +
    labs(
      title = paste0("Least stable items across bootstrap samples (k = ", k, ")"),
      subtitle = paste0("The ", top_n, " items with the lowest primary-factor assignment stability."),
      x = NULL,
      y = "Bootstrap proportion",
      shape = NULL
    )

  item_height <- max(
    config$plots$bootstrap_item_stability$min_height,
    top_n * config$plots$bootstrap_item_stability$height_per_item
  )

  save_plot_png(
    p_items,
    make_outfile("bootstrap_least_stable_items", "png", k_val = k, dir = plots_dir),
    width = config$plots$bootstrap_item_stability$width,
    height = item_height,
    dpi = config$plots$bootstrap_item_stability$dpi
  )

  if (!is.null(phi_rows) && nrow(phi_rows) > 0) {
    phi_order <- unique(phi_rows$factor_pair)
    phi_rows$factor_pair <- factor(phi_rows$factor_pair, levels = phi_order)

    phi_reference <- phi_rows |>
      dplyr::distinct(factor_pair, reference_correlation)

    p_phi_boot <- ggplot(
      phi_rows,
      aes(x = factor_pair, y = bootstrap_correlation)
    ) +
      geom_violin(trim = FALSE, alpha = 0.25) +
      geom_boxplot(width = 0.16, outlier.shape = NA) +
      geom_point(
        data = phi_reference,
        aes(x = factor_pair, y = reference_correlation),
        inherit.aes = FALSE,
        size = 2.3
      ) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste0("Bootstrap stability of factor correlations (k = ", k, ")"),
        subtitle = "Violin/box = bootstrap distribution; point = full-sample reference correlation.",
        x = "Reference factor pair",
        y = "Aligned factor correlation"
      )

    save_plot_png(
      p_phi_boot,
      make_outfile("bootstrap_factor_correlation_plot", "png", k_val = k, dir = plots_dir),
      width = config$plots$bootstrap_factor_correlations$width,
      height = config$plots$bootstrap_factor_correlations$height,
      dpi = config$plots$bootstrap_factor_correlations$dpi
    )
  }
}

fit_align_one_bootstrap <- function(
  b,
  cache_res,
  k,
  L_reference,
  Phi_reference,
  reference_vars,
  reference_factors
) {
  tryCatch({
    min_required_p <- ceiling(
      config$bootstrap$min_common_variable_prop * nrow(L_reference)
    )
    if (cache_res$p < min_required_p) {
      return(list(
        ok = FALSE,
        bootstrap = b,
        stage = "too_few_variables_before_efa",
        detail = paste0(cache_res$p, " variables; required ", min_required_p)
      ))
    }

    efa_b <- quiet_try(
      fit_efa_psych(cache_res$R, n_obs = cache_res$n, k = k),
      quiet = TRUE
    )
    if (is.null(efa_b)) {
      return(list(ok = FALSE, bootstrap = b, stage = "efa_fit_failed", detail = NA_character_))
    }

    L_b <- as_loading_matrix(efa_b, fallback_names = colnames(cache_res$R))
    aligned <- quiet_try(align_bootstrap_loadings(L_reference, L_b), quiet = TRUE)
    if (is.null(aligned) || !isTRUE(aligned$ok)) {
      return(list(
        ok = FALSE,
        bootstrap = b,
        stage = if (is.null(aligned)) "alignment_error" else aligned$stage,
        detail = if (is.null(aligned)) NA_character_ else aligned$detail
      ))
    }

    p <- length(reference_vars)
    q <- length(reference_factors)
    L_full <- matrix(
      NA_real_,
      nrow = p,
      ncol = q,
      dimnames = list(reference_vars, reference_factors)
    )
    common <- intersect(reference_vars, rownames(aligned$loadings))
    L_full[common, ] <- aligned$loadings[common, reference_factors, drop = FALSE]

    assign_b <- assign_items_to_factors(L_full[common, , drop = FALSE])$assignment
    rownames(assign_b) <- assign_b$variable

    primary <- setNames(rep(NA_character_, p), reference_vars)
    strict <- setNames(rep(NA, p), reference_vars)
    interpretive <- setNames(rep(NA, p), reference_vars)
    primary[common] <- assign_b[common, "primary_factor_raw"]
    strict[common] <- assign_b[common, "keep_strict"]
    interpretive[common] <- assign_b[common, "keep_interpretive"]

    factor_row <- data.frame(
      k = k,
      bootstrap = b,
      reference_factor = reference_factors,
      matched_bootstrap_factor = aligned$match$matched_candidate_factor_raw,
      signed_congruence_before_reflection = aligned$match$congruence,
      abs_congruence = aligned$congruence,
      sign_reflected = aligned$match$sign_flip,
      matching_margin = aligned$match$matching_margin,
      n_common_variables = length(aligned$common_vars),
      row.names = NULL
    )

    overall_row <- data.frame(
      k = k,
      bootstrap = b,
      mean_abs_congruence = mean(aligned$congruence, na.rm = TRUE),
      min_abs_congruence = min(aligned$congruence, na.rm = TRUE),
      n_common_variables = length(aligned$common_vars),
      row.names = NULL
    )

    match_row <- data.frame(
      k = k,
      bootstrap = b,
      reference_factor = reference_factors,
      original_bootstrap_factor = aligned$match$matched_candidate_factor_raw,
      sign_reflected = aligned$match$sign_flip,
      row.names = NULL
    )

    phi_row <- data.frame()
    if (!is.null(Phi_reference) && !is.null(efa_b$Phi)) {
      Phi_b <- as.matrix(efa_b$Phi)
      candidate_order <- aligned$candidate_order
      if (is.null(rownames(Phi_b))) rownames(Phi_b) <- colnames(L_b)
      if (is.null(colnames(Phi_b))) colnames(Phi_b) <- colnames(L_b)

      if (
        all(candidate_order %in% rownames(Phi_b)) &&
        all(candidate_order %in% colnames(Phi_b))
      ) {
        Phi_aligned <- Phi_b[candidate_order, candidate_order, drop = FALSE]
        D <- diag(aligned$signs, nrow = length(aligned$signs))
        Phi_aligned <- D %*% Phi_aligned %*% D
        rownames(Phi_aligned) <- reference_factors
        colnames(Phi_aligned) <- reference_factors
        upper_idx <- which(upper.tri(Phi_reference), arr.ind = TRUE)

        phi_row <- data.frame(
          k = k,
          bootstrap = b,
          factor_1 = rownames(Phi_reference)[upper_idx[, 1]],
          factor_2 = colnames(Phi_reference)[upper_idx[, 2]],
          factor_pair = paste0(
            rownames(Phi_reference)[upper_idx[, 1]],
            " - ",
            colnames(Phi_reference)[upper_idx[, 2]]
          ),
          reference_correlation = Phi_reference[upper_idx],
          bootstrap_correlation = Phi_aligned[upper_idx],
          row.names = NULL
        )
      }
    }

    list(
      ok = TRUE,
      bootstrap = b,
      loadings = L_full,
      primary = primary,
      strict = strict,
      interpretive = interpretive,
      factor_row = factor_row,
      overall_row = overall_row,
      match_row = match_row,
      phi_row = phi_row
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      bootstrap = b,
      stage = "worker_error",
      detail = conditionMessage(e)
    )
  })
}

run_bootstrap_stability <- function(
  k,
  efa_reference,
  X_reference,
  bootstrap_cache,
  item_info = NULL
) {
  if (!isTRUE(config$bootstrap$enabled) || is.null(bootstrap_cache)) return(NULL)

  L_reference <- as_loading_matrix(
    efa_reference,
    fallback_names = names(X_reference)
  )
  reference_factors <- colnames(L_reference)
  reference_vars <- rownames(L_reference)
  Phi_reference <- if (!is.null(efa_reference$Phi)) {
    Phi_ref_tmp <- as.matrix(efa_reference$Phi)
    if (is.null(rownames(Phi_ref_tmp))) rownames(Phi_ref_tmp) <- reference_factors
    if (is.null(colnames(Phi_ref_tmp))) colnames(Phi_ref_tmp) <- reference_factors
    Phi_ref_tmp[reference_factors, reference_factors, drop = FALSE]
  } else {
    NULL
  }

  ref_assign_obj <- assign_items_to_factors(L_reference)
  ref_assignment <- ref_assign_obj$assignment

  successful_cache <- which(vapply(
    bootstrap_cache$results,
    function(x) isTRUE(x$ok),
    logical(1)
  ))

  B <- length(successful_cache)
  if (B == 0) {
    warning("No successful bootstrap correlation matrices are available.", call. = FALSE)
    return(NULL)
  }

  p <- nrow(L_reference)
  q <- ncol(L_reference)

  aligned_loading_array <- array(
    NA_real_,
    dim = c(p, q, B),
    dimnames = list(reference_vars, reference_factors, paste0("b", successful_cache))
  )

  primary_factor_matrix <- matrix(
    NA_character_,
    nrow = p,
    ncol = B,
    dimnames = list(reference_vars, paste0("b", successful_cache))
  )
  keep_strict_matrix <- matrix(
    NA,
    nrow = p,
    ncol = B,
    dimnames = list(reference_vars, paste0("b", successful_cache))
  )
  keep_interpretive_matrix <- keep_strict_matrix

  factor_rows <- list()
  overall_rows <- list()
  match_rows <- list()
  phi_rows <- list()
  fit_failures <- list()

  start_time <- Sys.time()

  fit_workers <- suppressWarnings(as.integer(config$bootstrap$fit_workers))
  if (!is.finite(fit_workers) || fit_workers < 1L) fit_workers <- 1L
  fit_workers <- min(fit_workers, B)

  run_one <- function(b) {
    fit_align_one_bootstrap(
      b = b,
      cache_res = bootstrap_cache$results[[b]],
      k = k,
      L_reference = L_reference,
      Phi_reference = Phi_reference,
      reference_vars = reference_vars,
      reference_factors = reference_factors
    )
  }

  if (fit_workers > 1L) {
    message(
      "Bootstrap EFA k=", k, ": using ", fit_workers,
      " parallel workers for ", B, " correlation matrices."
    )
    cl <- parallel::makeCluster(fit_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages(library(psych))
      options(mc.cores = 1)
      NULL
    })
    parallel::clusterExport(
      cl,
      varlist = c(
        "config", "fit_align_one_bootstrap", "fit_efa_psych", "quiet_try",
        "as_loading_matrix", "align_bootstrap_loadings",
        "one_to_one_factor_match", "permute_vec", "factor_raw_to_factor",
        "factor_raw_to_label", "assign_items_to_factors", "make_factor_map",
        "factor_number", "sort_factor_names", "bootstrap_cache", "k",
        "L_reference", "Phi_reference", "reference_vars", "reference_factors"
      ),
      envir = environment()
    )
    worker_results <- parallel::parLapplyLB(
      cl,
      successful_cache,
      function(b) {
        fit_align_one_bootstrap(
          b = b,
          cache_res = bootstrap_cache$results[[b]],
          k = k,
          L_reference = L_reference,
          Phi_reference = Phi_reference,
          reference_vars = reference_vars,
          reference_factors = reference_factors
        )
      }
    )
    parallel::stopCluster(cl)
    on.exit(NULL, add = FALSE)
  } else {
    worker_results <- vector("list", B)
    for (j in seq_along(successful_cache)) {
      if (
        j == 1L || j == B ||
        j %% config$bootstrap$progress_every == 0L
      ) {
        message("Bootstrap EFA k=", k, ": ", j, "/", B)
      }
      worker_results[[j]] <- run_one(successful_cache[j])
    }
  }

  for (j in seq_along(worker_results)) {
    res <- worker_results[[j]]
    if (!isTRUE(res$ok)) {
      fit_failures[[length(fit_failures) + 1]] <- data.frame(
        bootstrap = res$bootstrap,
        stage = res$stage,
        detail = res$detail,
        row.names = NULL
      )
      next
    }

    aligned_loading_array[, , j] <- res$loadings
    primary_factor_matrix[, j] <- unname(res$primary[reference_vars])
    keep_strict_matrix[, j] <- unname(res$strict[reference_vars])
    keep_interpretive_matrix[, j] <- unname(res$interpretive[reference_vars])
    factor_rows[[length(factor_rows) + 1]] <- res$factor_row
    overall_rows[[length(overall_rows) + 1]] <- res$overall_row
    match_rows[[length(match_rows) + 1]] <- res$match_row
    if (!is.null(res$phi_row) && nrow(res$phi_row) > 0) {
      phi_rows[[length(phi_rows) + 1]] <- res$phi_row
    }
  }

  factor_rows <- dplyr::bind_rows(factor_rows)
  overall_rows <- dplyr::bind_rows(overall_rows)
  match_rows <- dplyr::bind_rows(match_rows)
  phi_rows <- if (length(phi_rows) > 0) {
    dplyr::bind_rows(phi_rows)
  } else {
    data.frame()
  }
  fit_failures <- if (length(fit_failures) > 0) {
    dplyr::bind_rows(fit_failures)
  } else {
    data.frame(bootstrap = integer(), stage = character(), detail = character())
  }

  if (nrow(factor_rows) == 0) {
    warning("No bootstrap EFA solutions could be aligned for k = ", k, call. = FALSE)
    return(NULL)
  }

  used_bootstrap_ids <- sort(unique(factor_rows$bootstrap))
  used_cols <- match(paste0("b", used_bootstrap_ids), colnames(primary_factor_matrix))

  aligned_loading_array <- aligned_loading_array[, , used_cols, drop = FALSE]
  primary_factor_matrix <- primary_factor_matrix[, used_cols, drop = FALSE]
  keep_strict_matrix <- keep_strict_matrix[, used_cols, drop = FALSE]
  keep_interpretive_matrix <- keep_interpretive_matrix[, used_cols, drop = FALSE]

  factor_summary <- factor_rows |>
    dplyr::group_by(k, reference_factor) |>
    dplyr::summarise(
      n_successful = dplyr::n(),
      mean_abs_congruence = mean(abs_congruence, na.rm = TRUE),
      median_abs_congruence = stats::median(abs_congruence, na.rm = TRUE),
      sd_abs_congruence = stats::sd(abs_congruence, na.rm = TRUE),
      min_abs_congruence = min(abs_congruence, na.rm = TRUE),
      q025_abs_congruence = as.numeric(stats::quantile(abs_congruence, 0.025, na.rm = TRUE)),
      q10_abs_congruence = as.numeric(stats::quantile(abs_congruence, 0.10, na.rm = TRUE)),
      q90_abs_congruence = as.numeric(stats::quantile(abs_congruence, 0.90, na.rm = TRUE)),
      q975_abs_congruence = as.numeric(stats::quantile(abs_congruence, 0.975, na.rm = TRUE)),
      prop_ge_good = mean(abs_congruence >= config$bootstrap$tucker_cutoffs$good, na.rm = TRUE),
      prop_ge_excellent = mean(abs_congruence >= config$bootstrap$tucker_cutoffs$excellent, na.rm = TRUE),
      mean_matching_margin = mean(matching_margin, na.rm = TRUE),
      q10_matching_margin = as.numeric(stats::quantile(matching_margin, 0.10, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(factor_number(reference_factor))

  overall_summary <- data.frame(
    k = k,
    requested_bootstraps = config$bootstrap$n_boot,
    successful_correlation_matrices = sum(bootstrap_cache$diagnostics$success),
    successful_aligned_efa_solutions = length(used_bootstrap_ids),
    failed_or_unaligned_efa_solutions =
      sum(bootstrap_cache$diagnostics$success) - length(used_bootstrap_ids),
    mean_of_bootstrap_mean_congruence = mean(overall_rows$mean_abs_congruence, na.rm = TRUE),
    median_of_bootstrap_mean_congruence = stats::median(overall_rows$mean_abs_congruence, na.rm = TRUE),
    q025_of_bootstrap_mean_congruence = as.numeric(stats::quantile(overall_rows$mean_abs_congruence, 0.025, na.rm = TRUE)),
    q975_of_bootstrap_mean_congruence = as.numeric(stats::quantile(overall_rows$mean_abs_congruence, 0.975, na.rm = TRUE)),
    mean_of_bootstrap_min_congruence = mean(overall_rows$min_abs_congruence, na.rm = TRUE),
    median_of_bootstrap_min_congruence = stats::median(overall_rows$min_abs_congruence, na.rm = TRUE),
    q025_of_bootstrap_min_congruence = as.numeric(stats::quantile(overall_rows$min_abs_congruence, 0.025, na.rm = TRUE)),
    q975_of_bootstrap_min_congruence = as.numeric(stats::quantile(overall_rows$min_abs_congruence, 0.975, na.rm = TRUE)),
    prop_all_factors_ge_good = mean(overall_rows$min_abs_congruence >= config$bootstrap$tucker_cutoffs$good, na.rm = TRUE),
    prop_all_factors_ge_excellent = mean(overall_rows$min_abs_congruence >= config$bootstrap$tucker_cutoffs$excellent, na.rm = TRUE),
    row.names = NULL
  )

  ref_assignment <- enrich_with_item_info(ref_assignment, item_info)
  ref_assignment$item_label <- make_item_plot_label(ref_assignment)
  rownames(ref_assignment) <- ref_assignment$variable

  item_rows <- lapply(reference_vars, function(v) {
    ref_factor <- ref_assignment[v, "primary_factor_raw"]
    factor_col <- match(ref_factor, reference_factors)

    boot_primary <- primary_factor_matrix[v, ]
    boot_strict <- keep_strict_matrix[v, ]
    boot_interpretive <- keep_interpretive_matrix[v, ]
    boot_loading_ref_factor <- aligned_loading_array[v, factor_col, ]

    available <- !is.na(boot_primary)

    data.frame(
      variable = v,
      item_label = ref_assignment[v, "item_label"],
      reference_factor = ref_factor,
      reference_primary_loading_signed = ref_assignment[v, "primary_loading_signed"],
      reference_primary_loading_abs = ref_assignment[v, "primary_loading"],
      reference_second_loading_abs = ref_assignment[v, "second_loading"],
      reference_loading_gap = ref_assignment[v, "loading_gap"],
      reference_keep_strict = ref_assignment[v, "keep_strict"],
      reference_keep_interpretive = ref_assignment[v, "keep_interpretive"],
      n_available_bootstraps = sum(available),
      prop_same_primary_factor = mean(boot_primary[available] == ref_factor, na.rm = TRUE),
      prop_keep_strict = mean(boot_strict[available], na.rm = TRUE),
      prop_keep_interpretive = mean(boot_interpretive[available], na.rm = TRUE),
      prop_same_strict_status = mean(
        boot_strict[available] == ref_assignment[v, "keep_strict"],
        na.rm = TRUE
      ),
      prop_same_interpretive_status = mean(
        boot_interpretive[available] == ref_assignment[v, "keep_interpretive"],
        na.rm = TRUE
      ),
      median_loading_on_reference_factor = stats::median(boot_loading_ref_factor, na.rm = TRUE),
      q025_loading_on_reference_factor = as.numeric(stats::quantile(boot_loading_ref_factor, 0.025, na.rm = TRUE)),
      q975_loading_on_reference_factor = as.numeric(stats::quantile(boot_loading_ref_factor, 0.975, na.rm = TRUE)),
      row.names = NULL
    )
  })

  item_summary <- dplyr::bind_rows(item_rows)

  assignment_probabilities <- do.call(
    rbind,
    lapply(reference_vars, function(v) {
      denom <- sum(!is.na(primary_factor_matrix[v, ]))
      data.frame(
        variable = v,
        assigned_factor = reference_factors,
        probability = if (denom > 0) {
          vapply(
            reference_factors,
            function(f) mean(primary_factor_matrix[v, ] == f, na.rm = TRUE),
            numeric(1)
          )
        } else {
          rep(NA_real_, length(reference_factors))
        },
        row.names = NULL
      )
    })
  )

  matching_frequencies <- match_rows |>
    dplyr::count(
      reference_factor,
      original_bootstrap_factor,
      sign_reflected,
      name = "n"
    ) |>
    dplyr::group_by(reference_factor) |>
    dplyr::mutate(proportion = n / sum(n)) |>
    dplyr::ungroup()

  phi_summary <- if (nrow(phi_rows) > 0) {
    phi_rows |>
      dplyr::group_by(
        k,
        factor_1,
        factor_2,
        factor_pair,
        reference_correlation
      ) |>
      dplyr::summarise(
        n_successful = dplyr::n(),
        mean_bootstrap_correlation = mean(bootstrap_correlation, na.rm = TRUE),
        median_bootstrap_correlation = stats::median(bootstrap_correlation, na.rm = TRUE),
        sd_bootstrap_correlation = stats::sd(bootstrap_correlation, na.rm = TRUE),
        q025_bootstrap_correlation = as.numeric(
          stats::quantile(bootstrap_correlation, 0.025, na.rm = TRUE)
        ),
        q975_bootstrap_correlation = as.numeric(
          stats::quantile(bootstrap_correlation, 0.975, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  } else {
    data.frame()
  }

  runtime_minutes <- round(
    as.numeric(difftime(Sys.time(), start_time, units = "mins")),
    1
  )
  overall_summary$bootstrap_efa_runtime_minutes <- runtime_minutes

  safe_write_csv(
    factor_summary,
    make_outfile("bootstrap_tucker_factor_summary", "csv", k_val = k, dir = tables_dir),
    row.names = FALSE
  )
  safe_write_csv(
    overall_summary,
    make_outfile("bootstrap_tucker_overall_summary", "csv", k_val = k, dir = tables_dir),
    row.names = FALSE
  )
  safe_write_csv(
    item_summary,
    make_outfile("bootstrap_item_stability", "csv", k_val = k, dir = tables_dir),
    row.names = FALSE
  )
  safe_write_csv(
    assignment_probabilities,
    make_outfile("bootstrap_assignment_probabilities", "csv", k_val = k, dir = tables_dir),
    row.names = FALSE
  )
  safe_write_csv(
    matching_frequencies,
    make_outfile("bootstrap_factor_matching_frequencies", "csv", k_val = k, dir = tables_dir),
    row.names = FALSE
  )

  if (nrow(phi_summary) > 0) {
    safe_write_csv(
      phi_summary,
      make_outfile("bootstrap_factor_correlation_summary", "csv", k_val = k, dir = tables_dir),
      row.names = FALSE
    )
  }

  if (isTRUE(config$bootstrap$save_raw_congruence_rows)) {
    safe_write_csv(
      factor_rows,
      make_outfile("bootstrap_tucker_raw", "csv", k_val = k, dir = tables_dir),
      row.names = FALSE
    )
    safe_write_csv(
      overall_rows,
      make_outfile("bootstrap_tucker_raw_overall", "csv", k_val = k, dir = tables_dir),
      row.names = FALSE
    )

    if (nrow(phi_rows) > 0) {
      safe_write_csv(
        phi_rows,
        make_outfile("bootstrap_factor_correlations_raw", "csv", k_val = k, dir = tables_dir),
        row.names = FALSE
      )
    }
  }

  bootstrap_sheets <- list(
    overall_summary = overall_summary,
    factor_summary = factor_summary,
    item_stability = item_summary,
    assignment_probabilities = assignment_probabilities,
    factor_matching_frequencies = matching_frequencies,
    correlation_diagnostics = bootstrap_cache$diagnostics,
    efa_alignment_failures = fit_failures,
    strata = bootstrap_cache$stratum_table
  )

  if (nrow(phi_summary) > 0) {
    bootstrap_sheets$factor_correlation_summary <- phi_summary
  }

  writexl::write_xlsx(
    bootstrap_sheets,
    make_outfile("bootstrap_stability_summary", "xlsx", k_val = k, dir = summary_dir)
  )

  if (isTRUE(config$bootstrap$save_aligned_loading_array)) {
    saveRDS(
      aligned_loading_array,
      make_outfile("bootstrap_aligned_loadings", "rds", k_val = k, dir = raw_dir),
      compress = FALSE
    )
  }

  plot_bootstrap_outputs(
    factor_rows = factor_rows,
    overall_rows = overall_rows,
    item_summary = item_summary,
    assignment_probabilities = assignment_probabilities,
    phi_rows = phi_rows,
    k = k
  )

  list(
    overall_summary = overall_summary,
    factor_summary = factor_summary,
    item_summary = item_summary,
    assignment_probabilities = assignment_probabilities,
    matching_frequencies = matching_frequencies,
    phi_summary = phi_summary,
    phi_rows = phi_rows,
    factor_rows = factor_rows,
    overall_rows = overall_rows,
    failures = fit_failures
  )
}


export_solution_for_scoring <- function(
  efa_obj,
  k,
  X_reference,
  R_reference,
  assignment = NULL,
  factor_map = NULL,
  item_info = NULL
) {
  L <- as_loading_matrix(
    efa_obj,
    fallback_names = colnames(R_reference)
  )

  bundle <- list(
    purpose = "Full-sample EFA solution bundle for a separate per-person scoring script",
    k = k,
    created_at = as.character(Sys.time()),
    analysis_level = config$analysis_level,
    dataset_scope = config$dataset_scope,
    subscale_score_version = if (identical(config$analysis_level, "subscales")) config$subscales$score_version else NA_character_,
    subscale_z_prefix = if (identical(config$analysis_level, "subscales")) config$subscales$z_prefix else NA_character_,
    input_file = data_path,
    stratification_path = if (file.exists(strat_path_full)) strat_path_full else NA_character_,
    correlation_type = corr_type,
    efa_engine = "psych",
    efa_fm = config$efa$fm,
    efa_rotate = config$efa$rotate,
    efa_n_rotations = config$efa$n_rotations,
    reference_sample = "complete_original_sample",
    training_variables = colnames(X_reference),
    n_training = nrow(X_reference),
    p_training = ncol(X_reference),
    efa_object = efa_obj,
    loadings = L,
    factor_correlations = if (!is.null(efa_obj$Phi)) efa_obj$Phi else NULL,
    assignment = assignment,
    factor_map = factor_map,
    item_info = item_info,
    scoring_note = paste(
      "The legacy field names training_variables/n_training are retained for compatibility.",
      "They refer to the complete full-sample reference solution in this bootstrap pipeline.",
      "Align a scoring dataset to training_variables and use psych::factor.scores()."
    )
  )

  saveRDS(
    bundle,
    make_outfile("efa_solution_bundle_for_scoring", "rds", k_val = k, dir = summary_dir)
  )

  scoring_readme <- data.frame(
    field = c(
      "purpose", "k", "analysis_level", "correlation_type",
      "efa_engine", "efa_fm", "efa_rotate", "efa_n_rotations",
      "reference_sample", "n_reference", "p_reference", "important_note"
    ),
    value = c(
      bundle$purpose,
      as.character(k),
      config$analysis_level,
      corr_type,
      "psych",
      config$efa$fm,
      config$efa$rotate,
      as.character(config$efa$n_rotations),
      "complete_original_sample",
      as.character(nrow(X_reference)),
      as.character(ncol(X_reference)),
      "Per-person scores are computed separately from this full-sample reference solution."
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
# 9) Run one k on the full-sample reference
# ---------------------------
run_single_k <- function(
  k,
  R_reference,
  X_reference,
  bootstrap_cache = NULL
) {
  message("\n============================================================")
  message("Running full-sample reference EFA for k = ", k)
  message("============================================================")

  k_summary <- list(
    k = k,
    status = "started",
    bootstrap_status = NA_character_
  )

  if (k >= ncol(R_reference)) {
    msg <- paste0(
      "Skipping k = ", k,
      " because k must be smaller than p = ",
      ncol(R_reference), "."
    )
    message(msg)
    append_log(Sys.time(), " | ", msg)
    k_summary$status <- "skipped_k_too_large"
    return(k_summary)
  }

  efa_reference <- tryCatch(
    fit_efa_psych(
      R_reference,
      n_obs = nrow(X_reference),
      k = k
    ),
    error = function(e) {
      msg <- paste0(
        "Reference EFA failed for k = ", k, ": ",
        conditionMessage(e)
      )
      message(msg)
      append_log(Sys.time(), " | ", msg)
      NULL
    }
  )

  if (is.null(efa_reference)) {
    k_summary$status <- "reference_efa_failed"
    return(k_summary)
  }

  k_summary$status <- "reference_efa_ok"

  if (write_debug_outputs) {
    saveRDS(
      efa_reference,
      make_outfile("efa_reference_fit", "rds", k_val = k, dir = raw_dir)
    )
  }

  L_reference <- as_loading_matrix(
    efa_reference,
    fallback_names = colnames(R_reference)
  )

  loadings_reference <- extract_loadings_psych(efa_reference)
  loadings_reference <- enrich_with_item_info(
    loadings_reference,
    item_info
  )

  fit_reference <- extract_fit_psych(
    efa_reference,
    k = k
  )

  assign_obj <- assign_items_to_factors(L_reference)
  assignment <- assign_obj$assignment
  factor_map <- assign_obj$factor_map

  assignment_enriched <- enrich_with_item_info(
    assignment,
    item_info
  )
  assignment_enriched$item_label <- make_item_plot_label(
    assignment_enriched
  )

  loadings_wide_info <- make_loadings_wide_with_info(
    L_reference,
    item_info
  )
  loadings_long_info <- make_loadings_long(
    L_reference,
    factor_map,
    item_info
  )
  marker_table <- make_marker_table(
    assignment,
    item_info,
    rule = "interpretive",
    top_n = config$plots$loading_marker_plot$top_n_per_factor
  )

  comm <- tibble::tibble(
    variable = names(efa_reference$communality),
    communality = unname(efa_reference$communality)
  ) |>
    enrich_with_item_info(item_info)

  uniq <- tibble::tibble(
    variable = names(efa_reference$uniquenesses),
    uniqueness = unname(efa_reference$uniquenesses)
  ) |>
    enrich_with_item_info(item_info)

  Phi_df <- if (!is.null(efa_reference$Phi)) {
    as.data.frame(efa_reference$Phi)
  } else {
    NULL
  }

  if (!is.null(Phi_df)) {
    plot_factor_correlation_heatmap(Phi_df, k = k)

    safe_write_csv(
      tibble::rownames_to_column(Phi_df, "factor_raw"),
      make_outfile("factor_correlation_matrix", "csv", k_val = k, dir = tables_dir),
      row.names = FALSE
    )
  }

  sheets <- list(
    fit = fit_reference,
    factor_map_ordered = factor_map,
    item_assignment = assignment_enriched,
    top_marker_items = marker_table,
    loadings_wide_with_codebook = loadings_wide_info,
    loadings_long_with_codebook = loadings_long_info,
    communalities = comm,
    uniquenesses = uniq
  )

  if (!is.null(Phi_df)) {
    sheets$factor_correlations <- tibble::rownames_to_column(
      Phi_df,
      "factor_raw"
    )
  }

  writexl::write_xlsx(
    sheets,
    make_outfile(
      "efa_reference_solution_readable",
      "xlsx",
      k_val = k,
      dir = tables_dir
    )
  )

  plot_factor_diagram(efa_reference, k = k)
  plot_loading_marker_items(marker_table, k = k)
  plot_loading_heatmap_readable(
    L_reference,
    assignment,
    factor_map,
    k = k,
    item_info = item_info
  )

  export_solution_for_scoring(
    efa_obj = efa_reference,
    k = k,
    X_reference = X_reference,
    R_reference = R_reference,
    assignment = assignment_enriched,
    factor_map = factor_map,
    item_info = item_info
  )

  # Optional hierarchy analysis.
  if (config$hierarchy$method == "bass_ackwards") {
    max_f <- config$hierarchy$bass_ackwards$max_factors
    message(
      "Running bassAckward decomposition up to ",
      max_f, " factors for k = ", k, "..."
    )

    ba <- tryCatch(
      psych::bassAckward(
        r = R_reference,
        nfactors = max_f,
        fm = config$efa$fm,
        rotate = config$efa$rotate,
        n.obs = nrow(X_reference)
      ),
      error = function(e) NULL
    )

    if (!is.null(ba)) {
      if (write_debug_outputs) {
        saveRDS(
          ba,
          make_outfile("bassAckward_reference", "rds", k_val = k, dir = raw_dir)
        )
      }
      sink(
        make_outfile(
          "bassAckward_reference_summary",
          "txt",
          k_val = k,
          dir = debug_dir
        )
      )
      print(ba)
      sink()
    } else {
      message("bassAckward failed for k = ", k, ".")
    }
  }

  # Stratified bootstrap stability.
  bootstrap_result <- run_bootstrap_stability(
    k = k,
    efa_reference = efa_reference,
    X_reference = X_reference,
    bootstrap_cache = bootstrap_cache,
    item_info = item_info
  )

  if (is.null(bootstrap_result)) {
    k_summary$bootstrap_status <- if (isTRUE(config$bootstrap$enabled)) {
      "bootstrap_failed_or_unavailable"
    } else {
      "bootstrap_disabled"
    }
  } else {
    k_summary$bootstrap_status <- "bootstrap_ok"

    bs <- bootstrap_result$overall_summary
    k_summary$bootstrap_successful_solutions <-
      bs$successful_aligned_efa_solutions[1]
    k_summary$bootstrap_mean_congruence <-
      bs$mean_of_bootstrap_mean_congruence[1]
    k_summary$bootstrap_median_congruence <-
      bs$median_of_bootstrap_mean_congruence[1]
    k_summary$bootstrap_mean_min_congruence <-
      bs$mean_of_bootstrap_min_congruence[1]
    k_summary$bootstrap_prop_all_ge_good <-
      bs$prop_all_factors_ge_good[1]
    k_summary$bootstrap_prop_all_ge_excellent <-
      bs$prop_all_factors_ge_excellent[1]
  }

  # Reliability based on the reference-sample assignment.
  rel_reference <- reliability_report(
    X_reference,
    assignment,
    label = "reference_full_sample",
    k = k,
    rule = config$assignment$reliability_rule
  )

  message("\n--- Reliability summary, k = ", k, " ---")
  print(rel_reference)

  safe_write_csv(
    assignment_enriched,
    make_outfile(
      "item_assignment_strict_and_interpretive_with_codebook",
      "csv",
      k_val = k,
      dir = tables_dir
    ),
    row.names = FALSE
  )

  safe_write_csv(
    rel_reference,
    make_outfile(
      "reliability_summary_reference",
      "csv",
      k_val = k,
      dir = tables_dir
    ),
    row.names = FALSE
  )

  assignment_summary <- make_assignment_summary(
    assignment,
    k = k
  )

  factor_size_summary <- assignment |>
    dplyr::group_by(
      primary_factor,
      primary_factor_raw,
      primary_factor_label,
      primary_factor_order
    ) |>
    dplyr::summarise(
      n_primary_all = dplyr::n(),
      n_keep_strict = sum(keep_strict, na.rm = TRUE),
      n_keep_interpretive = sum(keep_interpretive, na.rm = TRUE),
      max_abs_loading = max(primary_loading, na.rm = TRUE),
      median_abs_loading = stats::median(
        primary_loading,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(primary_factor_order)

  decision_summary <- data.frame(
    k = k,
    status = k_summary$status,
    bootstrap_status = k_summary$bootstrap_status,
    n_reference = nrow(X_reference),
    n_variables_reference = ncol(X_reference),
    n_keep_strict = assignment_summary$n_keep_strict,
    n_keep_interpretive = assignment_summary$n_keep_interpretive,
    n_drop_strict_low_loading =
      assignment_summary$n_drop_strict_low_loading,
    n_drop_strict_crossloading =
      assignment_summary$n_drop_strict_crossloading,
    n_drop_interpretive_low_loading =
      assignment_summary$n_drop_interpretive_low_loading,
    n_drop_interpretive_crossloading =
      assignment_summary$n_drop_interpretive_crossloading,
    bootstrap_successful_solutions =
      ifelse(
        is.null(k_summary$bootstrap_successful_solutions),
        NA_integer_,
        k_summary$bootstrap_successful_solutions
      ),
    bootstrap_mean_congruence =
      ifelse(
        is.null(k_summary$bootstrap_mean_congruence),
        NA_real_,
        k_summary$bootstrap_mean_congruence
      ),
    bootstrap_median_congruence =
      ifelse(
        is.null(k_summary$bootstrap_median_congruence),
        NA_real_,
        k_summary$bootstrap_median_congruence
      ),
    bootstrap_mean_min_congruence =
      ifelse(
        is.null(k_summary$bootstrap_mean_min_congruence),
        NA_real_,
        k_summary$bootstrap_mean_min_congruence
      ),
    bootstrap_prop_all_ge_good =
      ifelse(
        is.null(k_summary$bootstrap_prop_all_ge_good),
        NA_real_,
        k_summary$bootstrap_prop_all_ge_good
      ),
    bootstrap_prop_all_ge_excellent =
      ifelse(
        is.null(k_summary$bootstrap_prop_all_ge_excellent),
        NA_real_,
        k_summary$bootstrap_prop_all_ge_excellent
      ),
    RMSR = fit_reference$RMSR[1],
    RMSEA = fit_reference$RMSEA[1],
    TLI = fit_reference$TLI[1],
    BIC = fit_reference$BIC[1],
    row.names = NULL
  )

  summary_sheets <- list(
    decision_summary = decision_summary,
    factor_map_ordered = factor_map,
    factor_size_summary = factor_size_summary,
    assignment_summary = assignment_summary,
    reliability = rel_reference
  )

  if (!is.null(Phi_df)) {
    summary_sheets$factor_correlations_raw <-
      tibble::rownames_to_column(Phi_df, "factor_raw")
  }

  if (!is.null(bootstrap_result)) {
    summary_sheets$bootstrap_overall <-
      bootstrap_result$overall_summary
    summary_sheets$bootstrap_by_factor <-
      bootstrap_result$factor_summary
  }

  writexl::write_xlsx(
    summary_sheets,
    make_outfile(
      "decision_summary",
      "xlsx",
      k_val = k,
      dir = summary_dir
    )
  )

  writexl::write_xlsx(
    list(
      reliability = rel_reference,
      assignment = assignment_enriched,
      factor_map = factor_map
    ),
    make_outfile(
      "reliability_and_assignment",
      "xlsx",
      k_val = k,
      dir = tables_dir
    )
  )

  sink(
    make_outfile(
      "reliability_summary_reference",
      "txt",
      k_val = k,
      dir = tables_dir
    )
  )
  cat("Reliability summary based on the full-sample reference assignment\n")
  cat("k = ", k, "\n", sep = "")
  cat(
    "Reliability assignment rule: ",
    config$assignment$reliability_rule,
    "\n",
    sep = ""
  )
  cat(
    "Strict rule: |primary loading| >= ",
    config$assignment$strict$primary_abs_min,
    " and 2nd-highest |loading| < ",
    config$assignment$strict$second_abs_max,
    "\n",
    sep = ""
  )
  cat(
    "Interpretive rule: |primary loading| >= ",
    config$assignment$interpretive$primary_abs_min,
    " and primary-vs-second loading gap >= ",
    config$assignment$interpretive$gap_min,
    "\n\n",
    sep = ""
  )
  print(rel_reference)
  sink()

  k_summary
}


# ---------------------------
# 10) Preflight, prepare bootstrap cache once, then run all k values
# ---------------------------
preflight_k <- k_values[1]
preflight_fit <- tryCatch(
  fit_efa_psych(
    R_reference,
    n_obs = nrow(X_reference),
    k = preflight_k
  ),
  error = function(e) {
    stop(
      "Preflight EFA failed for k = ", preflight_k,
      ". Bootstrap correlations were not started. Error: ",
      conditionMessage(e),
      call. = FALSE
    )
  }
)
rm(preflight_fit)

if (isTRUE(.automation_child) && isTRUE(.automation_preflight_only)) {
  if (isTRUE(config$bootstrap$enabled)) {
    preflight_strata <- make_joint_bootstrap_strata(
      df_reference,
      config$bootstrap$strata_vars
    )
    message(
      "Bootstrap-strata preflight OK | strata=",
      nrow(preflight_strata$table),
      " | N=", length(preflight_strata$stratum)
    )
    rm(preflight_strata)
  }

  message(
    "AUTOMATION CHILD PREFLIGHT OK | level=", config$analysis_level,
    " | dataset=", config$dataset_scope,
    " | fm=", config$efa$fm,
    " | rotation=", config$efa$rotate,
    " | N=", nrow(X_reference),
    " | p=", ncol(X_reference),
    " | correlation=", corr_type
  )
  quit(save = "no", status = 0, runLast = FALSE)
}

bootstrap_cache <- prepare_bootstrap_correlation_cache(
  X_reference = X_reference,
  df_reference = df_reference
)

all_results <- lapply(k_values, function(k) {
  run_single_k(
    k = k,
    R_reference = R_reference,
    X_reference = X_reference,
    bootstrap_cache = bootstrap_cache
  )
})

run_summary <- dplyr::bind_rows(lapply(all_results, function(x) {
  as.data.frame(x, stringsAsFactors = FALSE)
}))

safe_write_csv(
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
message("Bootstrap correlation cache directory: ",
        file.path(config$root, config$bootstrap$cache_dir_rel))
if (write_debug_outputs) message("Debug/raw outputs in: ", debug_dir)
message("Run summary:")
print(run_summary)

} # end single-run pipeline guard
