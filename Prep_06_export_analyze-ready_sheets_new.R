#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Export COMPLETE + HiTOP analysis inputs
# Rewritten to enforce a simple pipeline:
#
#   1) Read clean masters (adults, adolescents)
#   2) Build COMPLETE item set
#   3) Force 0 into protected hidden adolescent items
#   4) Drop participants with too much missingness based on COMPLETE only
#   5) Impute remaining COMPLETE missing values pooled across samples
#   6) Save COMPLETE outputs (sample-wise + combined)
#   7) Derive HiTOP strictly as subset of imputed COMPLETE
#   8) Save HiTOP outputs (sample-wise + combined)
#
# Variants:
#   - full
#   - lt030   (items flagged by loading helper are removed first,
#              except protected hidden items)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# ---- Packages ----------------------------------------------------------------
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "readr", "writexl", "janitor", "dplyr", "stringr", "tibble",
  "purrr", "fs", "glue", "rstudioapi", "jsonlite", "mice"
))

# ---- Config ------------------------------------------------------------------
CFG <- list(
  samples = c("adults", "adolescents"),
  loading_threshold = 0.30,
  missing_threshold = 0.20,
  
  questionnaire_scales = c(
    "IDAS","CAPE","AQ","SUQ","ASRS-5","BISBAS","IUS","APS","TICS","CTQ","MAP-SR"
  ),
  
  protected_hidden_items = c("and2", "and3", "hal2", "hal3", "opi2", "opi3"),
  
  mice_m = 1,
  mice_maxit = 2,
  mice_seed = 1234,
  mice_donors = 3,
  
  out_dir_name = "03_analysis_input"
)

# ---- Helpers -----------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x) && !is.na(x) && nzchar(x)) x else y

script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

normalize_sample_case <- function(sample) {
  stringr::str_replace_all(stringr::str_to_title(sample), "_", " ")
}

normalize_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_replace_all("\\[|\\]", "") %>%
    stringr::str_replace_all("[^a-z0-9_]+", "")
}

safe_score_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

is_nonempty <- function(x) {
  x0 <- trimws(as.character(x))
  !is.na(x0) & nzchar(x0) & tolower(x0) != "na"
}

pick_first <- function(cands, nms) {
  hit <- cands[cands %in% nms]
  if (length(hit)) hit[1] else NA_character_
}

choose_id_col <- function(nms) {
  pick_first(c("vp_id","vp","vpid","participant_id","participantid","id"), nms)
}

extract_date <- function(x) {
  d <- stringr::str_match(x, "(\\d{4}-\\d{2}-\\d{2})")[,2]
  suppressWarnings(as.Date(d))
}

latest_file_by_pattern <- function(dir, pattern, recurse = FALSE) {
  files_all <- fs::dir_ls(dir, type = "file", recurse = recurse, fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  if (all(is.na(dts))) {
    info <- file.info(files)
    return(files[order(info$mtime, decreasing = TRUE)][1])
  }
  files[order(dts, decreasing = TRUE)][1]
}

latest_clean_master_for_sample <- function(root, sample) {
  folder <- fs::path(root, "02_cleaned", sample)
  cand <- fs::path(folder, paste0(sample, "_clean_master.csv"))
  if (fs::file_exists(cand)) return(cand)
  
  files <- fs::dir_ls(folder, regexp = "clean_master\\.csv$", type = "file", fail = FALSE)
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  files[order(dts, decreasing = TRUE)][1]
}

latest_iteminfo_for_sample <- function(root, sample) {
  info_dir <- fs::path(root, "information")
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(info_dir, patt)
}

resolve_iteminfo_with_fallback <- function(root, sample, fallback_sample = "adults") {
  ii_path <- latest_iteminfo_for_sample(root, sample)
  if (!is.na(ii_path) && fs::file_exists(ii_path)) return(ii_path)
  
  fb <- latest_iteminfo_for_sample(root, fallback_sample)
  if (!is.na(fb) && fs::file_exists(fb)) return(fb)
  
  NA_character_
}

latest_scoring <- function(root) {
  info_dir <- fs::path(root, "information")
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(info_dir, patt)
}

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

sample_tag <- function(samples) {
  paste(samples, collapse = "_")
}

read_master_csv_robust <- function(master_csv, default_delim = ";") {
  first_line <- readr::read_lines(master_csv, n_max = 1)
  delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
    sub("^sep=", "", first_line, ignore.case = TRUE)
  } else default_delim
  
  skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L
  
  suppressMessages(
    readr::read_delim(
      master_csv,
      delim = delim,
      skip = skip_n,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
  ) %>% tibble::as_tibble()
}

read_scoring <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

# ---- Logging -----------------------------------------------------------------
ROOT <- script_dir()
OUT_DIR <- fs::path(ROOT, CFG$out_dir_name)
DIR_LOGS <- fs::path(ROOT, "logs")

fs::dir_create(OUT_DIR)
fs::dir_create(DIR_LOGS)

logfile <- fs::path(
  DIR_LOGS,
  glue::glue("export_analysis_inputs_rewrite_{format(Sys.time(), '%Y-%m-%d_%H%M%S')}.log")
)

log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- Load metadata -----------------------------------------------------------
ITEM_INFO <- resolve_iteminfo_with_fallback(ROOT, "adults", fallback_sample = "adults")
if (is.na(ITEM_INFO) || !fs::file_exists(ITEM_INFO)) {
  stop("Could not find Item Information file.", call. = FALSE)
}

SCORING_PATH <- latest_scoring(ROOT)
if (is.na(SCORING_PATH) || !fs::file_exists(SCORING_PATH)) {
  stop("Could not find Scoring file.", call. = FALSE)
}

ii <- readxl::read_excel(ITEM_INFO) %>%
  janitor::clean_names()

if (!"item" %in% names(ii))  stop("Item Information missing 'item' column.", call. = FALSE)
if (!"scale" %in% names(ii)) stop("Item Information missing 'scale' column.", call. = FALSE)
if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_

ii <- ii %>%
  dplyr::mutate(
    item     = normalize_id(.data$item),
    scale    = as.character(.data$scale),
    subscale = as.character(.data$subscale)
  ) %>%
  dplyr::filter(is_nonempty(.data$item), is_nonempty(.data$scale))

scoring_df <- read_scoring(SCORING_PATH)

# ---- Read keys / masters -----------------------------------------------------
read_one_sample_master <- function(sample) {
  master_path <- latest_clean_master_for_sample(ROOT, sample)
  if (is.na(master_path) || !fs::file_exists(master_path)) {
    stop("Missing clean master for sample '", sample, "'.", call. = FALSE)
  }
  
  keys_path <- fs::path(ROOT, "02_cleaned", "keys", paste0(sample, "_keys.rds"))
  if (!fs::file_exists(keys_path)) {
    stop("Missing keys for sample '", sample, "'.", call. = FALSE)
  }
  
  d <- read_master_csv_robust(master_path)
  id_col_orig <- choose_id_col(names(d))
  if (is.na(id_col_orig)) {
    stop("Could not find ID column in master for sample '", sample, "'.", call. = FALSE)
  }
  
  names(d) <- normalize_id(names(d))
  id_col <- normalize_id(id_col_orig)
  
  list(
    sample = sample,
    d = d,
    id_col = id_col,
    keys = readRDS(keys_path),
    master_path = master_path
  )
}

masters <- purrr::map(CFG$samples, read_one_sample_master)
names(masters) <- CFG$samples

# ---- Hidden-item zero fill ---------------------------------------------------
fill_hidden_items_for_adolescents <- function(df, sample, items = CFG$protected_hidden_items, fill_value = 0) {
  if (tolower(sample) != "adolescents") return(df)
  
  item_map <- tibble::tibble(
    orig = names(df),
    norm = normalize_id(names(df))
  ) %>% dplyr::distinct(.data$norm, .keep_all = TRUE)
  
  for (it in normalize_id(items)) {
    hit <- item_map$orig[item_map$norm == it]
    
    if (!length(hit)) {
      df[[it]] <- fill_value
      log_msg("Created missing protected hidden item column '", it, "' and filled with 0.")
    } else {
      cc <- hit[1]
      df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
      n_na <- sum(is.na(df[[cc]]))
      if (n_na > 0) df[[cc]][is.na(df[[cc]])] <- fill_value
      log_msg("Filled ", n_na, " NA values in protected hidden item '", cc, "' with 0.")
    }
  }
  
  names(df) <- normalize_id(names(df))
  df
}

for (nm in names(masters)) {
  masters[[nm]]$d <- fill_hidden_items_for_adolescents(masters[[nm]]$d, nm)
}

# ---- Score helpers -----------------------------------------------------------
get_scale_scoring_mode <- function(scoring_df, scale, default = "mean") {
  if (toupper(trimws(scale)) == "SUQ") return("sum")
  
  if (is.null(scoring_df) || !"scale" %in% names(scoring_df)) return(default)
  
  cand_cols <- intersect(
    names(scoring_df),
    c("mode","scoring","method","aggregation","score_type","score_method","compute")
  )
  if (!length(cand_cols)) return(default)
  
  sc_key <- toupper(trimws(scale))
  s_key  <- toupper(trimws(as.character(scoring_df$scale)))
  idx    <- which(s_key == sc_key)
  if (!length(idx)) return(default)
  
  for (cc in cand_cols) {
    v <- scoring_df[[cc]][idx[1]]
    if (is.na(v)) next
    v0 <- tolower(trimws(as.character(v)))
    if (grepl("sum|total", v0)) return("sum")
    if (grepl("mean|avg|average", v0)) return("mean")
  }
  
  default
}

get_scale_min_prop <- function(scoring_df, scale, default = 0.50) {
  if (is.null(scoring_df) || !"scale" %in% names(scoring_df)) return(default)
  
  cand_cols <- intersect(names(scoring_df), c("min_prop_items","min_prop","minprop","min_prop_item"))
  if (!length(cand_cols)) return(default)
  
  sc_key <- toupper(trimws(scale))
  s_key  <- toupper(trimws(as.character(scoring_df$scale)))
  idx    <- which(s_key == sc_key)
  if (!length(idx)) return(default)
  
  v <- suppressWarnings(as.numeric(scoring_df[[cand_cols[1]]][idx[1]]))
  if (is.na(v)) default else v
}

score_items_wide <- function(d, items, agg = c("mean","sum"), min_prop = 0.50) {
  agg <- match.arg(agg)
  
  item_cols <- intersect(normalize_id(items), normalize_id(names(d)))
  if (!length(item_cols)) return(rep(NA_real_, nrow(d)))
  
  col_map <- tibble::tibble(orig = names(d), norm = normalize_id(names(d))) %>%
    dplyr::distinct(.data$norm, .keep_all = TRUE)
  
  orig_cols <- col_map$orig[match(item_cols, col_map$norm)]
  orig_cols <- orig_cols[!is.na(orig_cols)]
  if (!length(orig_cols)) return(rep(NA_real_, nrow(d)))
  
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  
  n_nonmiss <- rowSums(!is.na(as.matrix(M)))
  thresh <- ceiling(min_prop * ncol(M))
  
  out <- if (agg == "sum") rowSums(M, na.rm = TRUE) else rowMeans(M, na.rm = TRUE)
  out[n_nonmiss < thresh] <- NA_real_
  out
}

recompute_scores_from_keys <- function(df, keys_obj, scoring_df, scales_keep) {
  out <- df
  
  scales_keep <- unique(as.character(scales_keep))
  scales_keep <- scales_keep[is_nonempty(scales_keep)]
  
  if (!length(scales_keep)) return(out)
  
  if (!is.null(keys_obj$items_by_scale) && nrow(keys_obj$items_by_scale)) {
    scale_tbl <- keys_obj$items_by_scale %>%
      dplyr::filter(.data$scale %in% scales_keep)
    
    for (i in seq_len(nrow(scale_tbl))) {
      sc <- as.character(scale_tbl$scale[i])
      items <- scale_tbl$items[[i]]
      mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean")
      mp_i   <- get_scale_min_prop(scoring_df, sc, default = CFG$min_prop_items_default)
      out[[paste0("score_", safe_score_name(sc))]] <- score_items_wide(out, items, agg = mode_i, min_prop = mp_i)
    }
  }
  
  if (!is.null(keys_obj$items_by_subscale) && nrow(keys_obj$items_by_subscale)) {
    sub_tbl <- keys_obj$items_by_subscale %>%
      dplyr::filter(.data$scale %in% scales_keep) %>%
      dplyr::filter(is_nonempty(.data$subscale))
    
    for (i in seq_len(nrow(sub_tbl))) {
      sc <- as.character(sub_tbl$scale[i])
      sub <- as.character(sub_tbl$subscale[i])
      items <- sub_tbl$items[[i]]
      mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean")
      mp_i   <- get_scale_min_prop(scoring_df, sc, default = CFG$min_prop_items_default)
      out[[paste0("score_", safe_score_name(sc), "__", safe_score_name(sub))]] <-
        score_items_wide(out, items, agg = mode_i, min_prop = mp_i)
    }
  }
  
  out
}

drop_flagged_items_from_keys <- function(keys, flagged_items_norm) {
  if (!length(flagged_items_norm)) return(keys)
  
  prune_vec <- function(x) {
    x <- as.character(x)
    x[!(normalize_id(x) %in% flagged_items_norm)]
  }
  
  if (!is.null(keys$items_by_scale) && nrow(keys$items_by_scale)) {
    keys$items_by_scale <- keys$items_by_scale %>%
      dplyr::mutate(items = purrr::map(.data$items, prune_vec))
  }
  
  if (!is.null(keys$items_by_subscale) && nrow(keys$items_by_subscale)) {
    keys$items_by_subscale <- keys$items_by_subscale %>%
      dplyr::mutate(items = purrr::map(.data$items, prune_vec))
  }
  
  if (!is.null(keys$items_by_higher_order) && nrow(keys$items_by_higher_order)) {
    keys$items_by_higher_order <- keys$items_by_higher_order %>%
      dplyr::mutate(items = purrr::map(.data$items, prune_vec))
  }
  
  if (!is.null(keys$nested) && nrow(keys$nested)) {
    keys$nested <- keys$nested %>%
      dplyr::mutate(
        higher_order = purrr::map(.data$higher_order, function(ho_tbl) {
          if (is.null(ho_tbl) || !nrow(ho_tbl)) return(ho_tbl)
          ho_tbl %>%
            dplyr::mutate(
              subscales = purrr::map(.data$subscales, function(sub_tbl) {
                if (is.null(sub_tbl) || !nrow(sub_tbl)) return(sub_tbl)
                sub_tbl %>%
                  dplyr::mutate(items = purrr::map(.data$items, prune_vec))
              })
            )
        })
      )
  }
  
  keys
}

# ---- Flag helper -------------------------------------------------------------
latest_flag_helper <- function(root, combined_label, threshold_tag) {
  base_dir <- fs::path(
    root, "out", "internal_data_analysis",
    "distribution_of_backbone_scores_and_internal_consistency"
  )
  patt <- paste0("^", combined_label, "_flagged_items_", threshold_tag, "\\.xlsx$")
  latest_file_by_pattern(base_dir, patt, recurse = TRUE)
}

read_flagged_items <- function(path, dataset_label, threshold_value) {
  if (is.na(path) || !fs::file_exists(path)) return(tibble::tibble())
  
  x <- suppressMessages(readxl::read_excel(path, sheet = "flagged_items")) %>%
    janitor::clean_names()
  
  req <- c("dataset", "item", "loading")
  if (!all(req %in% names(x))) return(tibble::tibble())
  
  x %>%
    dplyr::mutate(
      dataset = as.character(.data$dataset),
      item = normalize_id(.data$item),
      loading = suppressWarnings(as.numeric(.data$loading))
    ) %>%
    dplyr::filter(
      .data$dataset == dataset_label,
      !is.na(.data$loading),
      abs(.data$loading) < threshold_value
    )
}

# ---- Metadata item sets ------------------------------------------------------
hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)

ii_hitop <- NULL
if (length(hitop_cols)) {
  mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
  ii_hitop <- ii[mapped_rows, , drop = FALSE]
}

ii_complete <- ii %>%
  dplyr::filter(.data$scale %in% CFG$questionnaire_scales)

if (!nrow(ii_complete)) {
  stop("No COMPLETE rows found in Item Information.", call. = FALSE)
}

# ---- Item export helper ------------------------------------------------------
extract_item_table <- function(df, id_col, item_names_norm) {
  col_map <- tibble::tibble(
    orig = names(df),
    norm = normalize_id(names(df))
  ) %>% dplyr::distinct(.data$norm, .keep_all = TRUE)
  
  item_cols <- col_map %>%
    dplyr::filter(.data$norm %in% item_names_norm) %>%
    dplyr::pull(.data$orig) %>%
    unique()
  
  if (!length(item_cols)) return(NULL)
  
  df %>%
    dplyr::transmute(
      sample = NA_character_,
      !!id_col := as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))
    )
}

# ---- Pooled imputation -------------------------------------------------------
impute_pooled_items <- function(df_combined, item_cols, label) {
  item_cols <- intersect(item_cols, names(df_combined))
  if (!length(item_cols)) stop("No item columns passed to imputation.", call. = FALSE)
  
  id_col <- choose_id_col(names(df_combined))
  if (is.na(id_col)) stop("Could not find ID column in pooled data.", call. = FALSE)
  
  dup_idx <- duplicated(df_combined[, c("sample", id_col), drop = FALSE])
  if (any(dup_idx)) {
    log_msg("Detected ", sum(dup_idx), " duplicate rows in ", label, ". Keeping first occurrence.")
    df_combined <- df_combined[!dup_idx, , drop = FALSE]
  }
  
  X <- df_combined[, item_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  row_missing <- rowMeans(is.na(X))
  drop_idx <- row_missing > CFG$missing_threshold
  
  if (any(drop_idx)) {
    dropped <- df_combined[drop_idx, c("sample", id_col), drop = FALSE]
    dropped$missing_prop <- row_missing[drop_idx]
    
    log_msg("Dropping ", nrow(dropped), " participants from ", label,
            " because COMPLETE missingness > ", CFG$missing_threshold)
    
    for (i in seq_len(nrow(dropped))) {
      log_msg("Dropped: sample=", dropped$sample[i],
              ", id=", dropped[[id_col]][i],
              ", missing_prop=", round(dropped$missing_prop[i], 3))
    }
  }
  
  df_keep <- df_combined[!drop_idx, , drop = FALSE]
  X <- df_keep[, item_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  if (!anyNA(X)) {
    log_msg(label, ": no missing values left after participant exclusion.")
    df_keep[, item_cols] <- X
    return(df_keep)
  }
  
  miss_cols_initial <- names(X)[colSums(is.na(X)) > 0]
  obs_cols <- setdiff(names(X), miss_cols_initial)
  
  fully_missing_cols <- miss_cols_initial[colSums(!is.na(X[, miss_cols_initial, drop = FALSE])) == 0]
  if (length(fully_missing_cols)) {
    stop(
      paste0(label, ": fully missing columns cannot be imputed: ",
             paste(fully_missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }
  
  one_value_cols <- miss_cols_initial[
    vapply(X[, miss_cols_initial, drop = FALSE], function(z) {
      length(unique(z[!is.na(z)])) == 1
    }, logical(1))
  ]
  
  if (length(one_value_cols)) {
    for (cc in one_value_cols) {
      fill_val <- unique(X[[cc]][!is.na(X[[cc]])])[1]
      X[[cc]][is.na(X[[cc]])] <- fill_val
    }
    log_msg(label, ": filled ", length(one_value_cols),
            " columns deterministically because they had only one observed value.")
  }
  
  miss_cols <- names(X)[colSums(is.na(X)) > 0]
  if (!length(miss_cols)) {
    df_keep[, item_cols] <- X
    log_msg(label, ": all missingness resolved before mice.")
    return(df_keep)
  }
  
  imp_data <- X[, c(miss_cols, obs_cols), drop = FALSE]
  
  pred <- mice::quickpred(
    imp_data,
    mincor = 0.10,
    minpuc = 0.25,
    include = obs_cols
  )
  
  meth <- rep("", ncol(imp_data))
  names(meth) <- names(imp_data)
  meth[miss_cols] <- "pmm"
  if (length(obs_cols)) pred[obs_cols, ] <- 0
  
  log_msg(label, ": running mice on ", length(miss_cols), " columns.")
  
  suppressMessages({
    imp <- mice::mice(
      data = imp_data,
      m = CFG$mice_m,
      maxit = CFG$mice_maxit,
      method = meth,
      predictorMatrix = pred,
      donors = CFG$mice_donors,
      seed = CFG$mice_seed,
      printFlag = FALSE
    )
  })
  
  completed <- mice::complete(imp, 1)
  X[, names(completed)] <- completed
  
  remaining_cols <- names(X)[colSums(is.na(X)) > 0]
  if (length(remaining_cols)) {
    for (cc in remaining_cols) {
      z <- suppressWarnings(as.numeric(X[[cc]]))
      fill_val <- suppressWarnings(stats::median(z, na.rm = TRUE))
      if (!is.finite(fill_val)) fill_val <- unique(z[!is.na(z)])[1] %||% 0
      X[[cc]][is.na(X[[cc]])] <- fill_val
    }
    log_msg(label, ": final deterministic fallback applied to ", length(remaining_cols), " columns.")
  }
  
  df_keep[, item_cols] <- X
  log_msg(label, ": remaining NAs after imputation = ",
          sum(is.na(df_keep[, item_cols, drop = FALSE])))
  
  df_keep
}

# ---- Variant runner ----------------------------------------------------------
run_variant <- function(variant_name = c("full", "lt030")) {
  variant_name <- match.arg(variant_name)
  
  combined_label <- sample_tag(CFG$samples)
  threshold_tag <- make_threshold_tag(CFG$loading_threshold)
  
  protected_norm <- normalize_id(CFG$protected_hidden_items)
  
  flagged_norm <- character(0)
  if (variant_name == "lt030") {
    helper <- latest_flag_helper(ROOT, combined_label, threshold_tag)
    if (is.na(helper) || !fs::file_exists(helper)) {
      stop("Could not find flagged-items helper for lt030 variant.", call. = FALSE)
    }
    
    flagged_tbl <- read_flagged_items(helper, combined_label, CFG$loading_threshold)
    flagged_norm <- setdiff(unique(normalize_id(flagged_tbl$item)), protected_norm)
    log_msg("lt030: flagged items removed = ", length(flagged_norm))
  }
  
  ii_complete_use <- ii_complete
  if (length(flagged_norm)) {
    ii_complete_use <- ii_complete_use %>%
      dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm))
  }
  
  complete_item_set <- unique(normalize_id(ii_complete_use$item))
  complete_scales_use <- unique(ii_complete_use$scale)
  
  complete_list <- list()
  kept_keys <- list()
  
  for (nm in names(masters)) {
    obj <- masters[[nm]]
    tab <- extract_item_table(obj$d, obj$id_col, complete_item_set)
    if (!is.null(tab)) {
      tab$sample <- nm
      tab <- tab %>% dplyr::relocate(.data$sample)
      complete_list[[nm]] <- tab
    }
    
    keys_use <- if (length(flagged_norm)) {
      drop_flagged_items_from_keys(obj$keys, flagged_norm)
    } else {
      obj$keys
    }
    kept_keys[[nm]] <- keys_use
  }
  
  complete_combined_raw <- dplyr::bind_rows(complete_list)
  id_col_combined <- choose_id_col(names(complete_combined_raw))
  complete_item_cols <- setdiff(names(complete_combined_raw), c("sample", id_col_combined))
  
  complete_combined_imp <- impute_pooled_items(
    complete_combined_raw,
    item_cols = complete_item_cols,
    label = paste0("COMPLETE_", variant_name)
  )
  
  complete_split <- split(complete_combined_imp, complete_combined_imp$sample)
  complete_split$combined <- complete_combined_imp
  
  complete_items_sheets <- list()
  complete_scores_sheets <- list()
  
  for (nm in names(complete_split)) {
    dd <- complete_split[[nm]]
    if (nm == "combined") {
      complete_items_sheets[[nm]] <- dd
      next
    }
    
    id_col <- choose_id_col(names(dd))
    dd_no_sample <- dd %>% dplyr::select(-.data$sample)
    
    scored <- recompute_scores_from_keys(
      dd_no_sample,
      keys_obj = kept_keys[[nm]],
      scoring_df = scoring_df,
      scales_keep = complete_scales_use
    ) %>%
      dplyr::mutate(sample = nm, .before = 1)
    
    complete_items_sheets[[nm]] <- dd
    complete_scores_sheets[[nm]] <- scored
  }
  
  complete_scores_sheets$combined <- dplyr::bind_rows(complete_scores_sheets)
  
  # HiTOP is now STRICTLY derived from imputed COMPLETE
  ii_hitop_use <- ii_hitop
  if (length(flagged_norm) && !is.null(ii_hitop_use) && nrow(ii_hitop_use)) {
    ii_hitop_use <- ii_hitop_use %>%
      dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm))
  }
  
  hitop_items_sheets <- list()
  hitop_scores_sheets <- list()
  
  if (!is.null(ii_hitop_use) && nrow(ii_hitop_use)) {
    hitop_item_set <- unique(normalize_id(ii_hitop_use$item))
    hitop_scales_use <- unique(ii_hitop_use$scale)
    
    for (nm in names(complete_split)) {
      dd <- complete_split[[nm]]
      id_col <- choose_id_col(names(dd))
      
      keep_cols <- c("sample", id_col, intersect(hitop_item_set, names(dd)))
      keep_cols <- unique(keep_cols)
      
      dd_hitop <- dd %>% dplyr::select(dplyr::any_of(keep_cols))
      hitop_items_sheets[[nm]] <- dd_hitop
      
      if (nm != "combined") {
        dd_no_sample <- dd_hitop %>% dplyr::select(-.data$sample)
        scored <- recompute_scores_from_keys(
          dd_no_sample,
          keys_obj = kept_keys[[nm]],
          scoring_df = scoring_df,
          scales_keep = hitop_scales_use
        ) %>%
          dplyr::mutate(sample = nm, .before = 1)
        
        hitop_scores_sheets[[nm]] <- scored
      }
    }
    
    hitop_scores_sheets$combined <- dplyr::bind_rows(hitop_scores_sheets)
  }
  
  suffix <- if (variant_name == "full") "" else paste0("_", threshold_tag)
  
  out_complete_items <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_items{suffix}.xlsx"))
  out_complete_scores <- fs::path(OUT_DIR, glue::glue("{combined_label}_complete_subscales{suffix}.xlsx"))
  out_hitop_items <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_items{suffix}.xlsx"))
  out_hitop_scores <- fs::path(OUT_DIR, glue::glue("{combined_label}_HiTOP_subscales{suffix}.xlsx"))
  
  writexl::write_xlsx(complete_items_sheets, out_complete_items)
  writexl::write_xlsx(complete_scores_sheets, out_complete_scores)
  if (length(hitop_items_sheets)) writexl::write_xlsx(hitop_items_sheets, out_hitop_items)
  if (length(hitop_scores_sheets)) writexl::write_xlsx(hitop_scores_sheets, out_hitop_scores)
  
  log_msg("Wrote COMPLETE items: ", out_complete_items)
  log_msg("Wrote COMPLETE scores: ", out_complete_scores)
  log_msg("Wrote HiTOP items: ", out_hitop_items)
  log_msg("Wrote HiTOP scores: ", out_hitop_scores)
  
  invisible(list(
    complete = complete_combined_imp,
    hitop = hitop_items_sheets
  ))
}

# ---- Stratification export ---------------------------------------------------
make_strat <- function(obj) {
  d <- obj$d
  id_col <- obj$id_col
  sample <- obj$sample
  
  proj_col   <- pick_first(c("project", "p"), names(d))
  age_col    <- pick_first(c("age_years", "age"), names(d))
  gender_col <- pick_first(c("gender"), names(d))
  group_col  <- pick_first(c("group"), names(d))
  
  tibble::tibble(
    sample  = sample,
    !!id_col := as.character(d[[id_col]]),
    project = if (!is.na(proj_col)) as.character(d[[proj_col]]) else NA_character_,
    age     = if (!is.na(age_col)) suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
    gender  = if (!is.na(gender_col)) as.character(d[[gender_col]]) else NA_character_,
    group   = if (!is.na(group_col)) as.character(d[[group_col]]) else NA_character_
  )
}

strat_combined <- purrr::map(masters, make_strat) %>% dplyr::bind_rows()
out_strat <- fs::path(OUT_DIR, glue::glue("{sample_tag(CFG$samples)}_stratification_info.xlsx"))
writexl::write_xlsx(list(strat_all = strat_combined), out_strat)
log_msg("Wrote stratification info: ", out_strat)

# ---- Run both variants -------------------------------------------------------
log_msg("Running FULL variant")
run_variant("full")

log_msg("Running LT030 variant")
run_variant("lt030")

log_msg("Done.")