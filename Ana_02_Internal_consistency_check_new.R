# --- analyze_backbone_scales_omega.R (refined v3: all-polychoric) ------------
# - Only scales present in Scoring.xlsx (minus blacklist)
# - CAPE Positive: ω from CAPE *frequency* items only (polychoric)
# - BISBAS total ω: BAS-only (Drive, FunSeeking, RewardResponsiveness)
# - ALL ω use POLYCHORIC correlations
# - Warn (print + log) if any ω < .60
# - Write ω table (sheet "omega") and item loadings (sheet "loadings") to XLSX

# ===== Pre-flight =====
rm(list = ls(all.names = TRUE)); invisible(gc())
try(cat("\014"), silent = TRUE)

# ===== Setup =====
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}
ensure_packages(c(
  "readr","jsonlite","tibble","dplyr","purrr","stringr","fs","glue",
  "janitor","psych","rprojroot","readxl","tidyr","writexl"
))

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
project_root <- function() {
  env <- Sys.getenv("BACKBONE_ROOT", unset = NA)
  if (!is.na(env) && dir.exists(env)) return(normalizePath(env, winslash = "/"))
  sd <- script_dir(); if (dir.exists(sd)) return(sd)
  root <- tryCatch(rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root),
                   error = function(e) NA)
  if (!is.na(root)) return(normalizePath(root, winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}

# ===== CONFIG =====
ROOT <- project_root()
DIR_INFO           <- fs::path(ROOT, "information")
DIR_QUESTIONNAIRES <- fs::path(ROOT, "01_project_data", "all_projects_backbone", "questionnaires")
DIR_EXPORT         <- fs::path(ROOT, "02_cleaned")
DIR_KEYS           <- fs::path(DIR_EXPORT, "keys")
DIR_FUNCTIONS      <- fs::path(ROOT, "functions")
DIR_LOGS           <- fs::path(ROOT, "logs")

SAMPLE               <- "adults"
DELIM                <- ";"
MIN_PROP_ITEMS       <- 0.50
CAPE_OMEGA_MODE      <- "frequency_only"   # or "skip"
OMEGA_WARN_THRESHOLD <- 0.60

# ===== Source helpers =====
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))
source(file.path(DIR_FUNCTIONS, "extract_date_string.R"))
source(file.path(DIR_FUNCTIONS, "latest_questionnaire_for_sample.R"))
source(file.path(DIR_FUNCTIONS, "safe_var.R"))
source(file.path(DIR_FUNCTIONS, "setup_logging.R"))

# ===== Paths =====
master_csv <- fs::path(DIR_EXPORT, SAMPLE, glue::glue("{SAMPLE}_clean_master.csv"))
keys_json  <- fs::path(DIR_KEYS, glue::glue("{SAMPLE}_keys.json"))
stopifnot(fs::file_exists(master_csv), fs::file_exists(keys_json))

latest_scoring <- function(info_dir = DIR_INFO) {
  files <- fs::dir_ls(info_dir, glob = "*_Scoring.xlsx", fail = FALSE)
  if (!length(files)) return(NA_character_)
  dt <- stringr::str_match(basename(files), "^(\\d{4}-\\d{2}-\\d{2})_")[,2]
  files[order(as.Date(dt), decreasing = TRUE)][1]
}
q_file   <- latest_questionnaire_for_sample(SAMPLE, DIR_QUESTIONNAIRES)
date_str <- if (!is.na(q_file)) extract_date_string(q_file) else format(Sys.Date(), "%Y-%m-%d")

OUT_BASE <- fs::path(ROOT, "out", "internal_data_analysis",
                     "distribution_of_backbone_scores_and_internal_consistency",
                     paste0(date_str, "_", SAMPLE))
fs::dir_create(OUT_BASE); fs::dir_create(DIR_LOGS)

# ===== Logger =====
log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_data_analysis_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

# ===== Read data =====
first_line <- readr::read_lines(master_csv, n_max = 1)
delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
  sub("^sep=", "", first_line, ignore.case = TRUE)
} else DELIM
skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L

df <- suppressMessages(
  readr::read_delim(
    master_csv, delim = delim, skip = skip_n, show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
) %>% janitor::clean_names()

if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  df$group <- factor(df$group, levels = c("HC","patient"))
}
proj_col <- get_project_col(df)

col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id(names(df)))

# ===== Keys & Scoring workbook =====
keys_raw <- jsonlite::read_json(keys_json, simplifyVector = FALSE)
items_by_scale <- tibble::tibble(
  scale = purrr::map_chr(keys_raw$items_by_scale, ~ .x$scale %||% NA_character_),
  items = purrr::map(keys_raw$items_by_scale, ~ as.character(.x$items %||% character(0)))
)
items_by_subscale <- tibble::tibble(
  scale    = purrr::map_chr(keys_raw$items_by_subscale, ~ .x$scale %||% NA_character_),
  subscale = purrr::map_chr(keys_raw$items_by_subscale, ~ .x$subscale %||% NA_character_),
  items    = purrr::map(keys_raw$items_by_subscale, ~ as.character(.x$items %||% character(0)))
)
items_by_higher <- try({
  tibble::tibble(
    higher_order_subscale = purrr::map_chr(keys_raw$items_by_higher_order, ~ .x$higher_order_subscale %||% NA_character_),
    items = purrr::map(keys_raw$items_by_higher_order, ~ as.character(.x$items %||% character(0)))
  )
}, silent = TRUE)
if (inherits(items_by_higher, "try-error")) items_by_higher <- tibble::tibble()

SCORING_PATH <- latest_scoring(DIR_INFO)
scoring_df <- if (!is.na(SCORING_PATH)) {
  suppressMessages(readxl::read_excel(SCORING_PATH)) %>% janitor::clean_names()
} else {
  logger$write("⚠️ Scoring workbook not found; cannot filter scales to Scoring.xlsx.")
  tibble::tibble()
}

# ===== Allowlist from Scoring.xlsx + blacklist (ω only) ======================
norm <- function(x) normalize_id(as.character(x))
scales_in_scoring_norm <- if (nrow(scoring_df) && "scale" %in% names(scoring_df)) {
  unique(na.omit(norm(scoring_df$scale)))
} else character(0)

blacklist_norm <- c(
  "suq","substanceusequestionnaire","substance_use_questionnaire",
  "fhsfamilytree","fhs_familytree","fhs-familytree","familyhistory","family_history",
  "time","timeitems","time_block","timequestions"
)
allowed_norm <- setdiff(scales_in_scoring_norm, blacklist_norm)

if (!length(allowed_norm)) {
  logger$write("ℹ️ No allowed scales after filtering by Scoring.xlsx and blacklist — skipping ω.")
  items_by_scale    <- items_by_scale[0,]
  items_by_subscale <- items_by_subscale[0,]
} else {
  items_by_scale    <- items_by_scale    %>% dplyr::filter(norm(.data$scale) %in% allowed_norm)
  items_by_subscale <- items_by_subscale %>% dplyr::filter(norm(.data$scale) %in% allowed_norm)
}

# ===== Utilities =============================================================
match_items <- function(items) {
  items_norm <- normalize_id(items)
  hit <- intersect(items_norm, col_map$item_norm)
  list(n_in_key = length(items_norm), n_found = length(hit), found = hit)
}

# build numeric items df (drop zero-variance)
prepare_item_df <- function(d, keep_norm) {
  if (!length(keep_norm)) return(NULL)
  orig <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  vv <- vapply(X, function(v) {
    v <- as.numeric(v); v <- v[is.finite(v)]
    if (!length(v)) return(FALSE)
    stats::var(v, na.rm = TRUE) > 0
  }, logical(1))
  X <- X[, vv, drop = FALSE]
  if (!ncol(X)) return(NULL)
  X
}

# ---- omega from a correlation matrix (and overall 1-factor loadings) --------
omega_from_matrix <- function(R, n_obs) {
  out <- try(psych::omega(R, n.obs = n_obs, sl = FALSE, plot = FALSE, warnings = FALSE), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) return(list(omega = NA_real_, loadings = NULL))
  om <- NA_real_
  for (nm in c("omega.t","omega.tot","omega.totale","omega.totals","omega.totalscore")) {
    if (!is.null(out[[nm]]) && is.numeric(out[[nm]])) { om <- as.numeric(out[[nm]]); break }
  }
  if (is.na(om) && !is.null(out$omega) && is.data.frame(out$omega) && "omega.tot" %in% names(out$omega)) {
    om <- as.numeric(out$omega[,"omega.tot", drop = TRUE])
  }
  fl <- try(psych::fa(r = R, nfactors = 1, fm = "minres"), silent = TRUE)
  load_tbl <- NULL
  if (!inherits(fl, "try-error") && !is.null(fl$loadings)) {
    load_tbl <- tibble::tibble(item = rownames(fl$loadings), loading = as.numeric(fl$loadings[,1]))
  }
  list(omega = om, loadings = load_tbl)
}

# ---- UNIVERSAL polychoric ω calculator --------------------------------------
compute_omega_and_loadings <- function(items_df) {
  if (is.null(items_df) || !ncol(items_df)) return(list(omega=NA_real_, loadings=NULL, method="polychoric"))
  # per-row completeness threshold
  thresh <- ceiling(MIN_PROP_ITEMS * ncol(items_df))
  n_nonmiss <- rowSums(is.finite(as.matrix(items_df)))
  X <- items_df[n_nonmiss >= thresh, , drop = FALSE]
  if (nrow(X) < 10L || ncol(X) < 2L) return(list(omega=NA_real_, loadings=NULL, method="polychoric"))
  pc <- try(psych::polychoric(X, correct = 0), silent = TRUE)
  if (inherits(pc, "try-error") || is.null(pc$rho)) return(list(omega=NA_real_, loadings=NULL, method="polychoric"))
  res <- omega_from_matrix(pc$rho, n_obs = nrow(X))
  list(omega = res$omega, loadings = res$loadings, method = "polychoric")
}

# ----- CAPE helpers (Positive frequency-only) --------------------------------
is_cape <- function(scale) norm(scale) == "cape"
is_positive <- function(subscale) norm(subscale) %in% c("positive","positivesymptoms","positive_symptoms","pos","positive_scale")
select_cape_frequency_norms <- function(item_names) {
  want <- normalize_id(item_names)
  present <- intersect(want, col_map$item_norm)
  if (!length(present)) return(character(0))
  pres_orig <- col_map$orig[match(present, col_map$item_norm)]
  pres_norm <- normalize_id(pres_orig)
  keep <- grepl("^capefreq", pres_norm, ignore.case = TRUE)
  col_map$item_norm[col_map$orig %in% pres_orig[keep]]
}
omega_for_cape_positive <- function(d, items_from_keys) {
  if (CAPE_OMEGA_MODE == "skip") return(list(omega = NA_real_, loadings = NULL, method = "skipped"))
  freq_norms <- select_cape_frequency_norms(items_from_keys)
  if (length(freq_norms) < 2L) {
    logger$write("⚠️ CAPE Positive: <2 frequency items present; ω skipped.")
    return(list(omega = NA_real_, loadings = NULL, method = "polychoric"))
  }
  X <- prepare_item_df(d, freq_norms)
  compute_omega_and_loadings(X)  # polychoric
}

# ----- BISBAS BAS-only (Drive, FunSeeking, RewardResponsiveness) -------------
bisbas_bas_item_norms <- function() {
  if (nrow(items_by_higher)) {
    bas <- items_by_higher %>% dplyr::filter(tolower(.data$higher_order_subscale) == "bas")
    if (nrow(bas)) {
      it <- unique(unlist(bas$items, use.names = FALSE))
      return(intersect(normalize_id(it), col_map$item_norm))
    }
  }
  pat <- "(drive|fun[_ ]?seeking|reward[_ ]?respons(e|iveness)|rr)$"
  rows <- items_by_subscale %>% dplyr::filter(norm(.data$scale) == "bisbas" & grepl(pat, normalize_id(.data$subscale)))
  it <- unique(unlist(rows$items, use.names = FALSE))
  intersect(normalize_id(it), col_map$item_norm)
}

# ===== MAIN ==================================================================
results <- list()
loadings_rows <- list()  # for Excel sheet 2

# --- SCALES (including BISBAS total handled as BAS-only) ---------------------
if (nrow(items_by_scale)) {
  message("== Scales ==")
  for (i in seq_len(nrow(items_by_scale))) {
    sc    <- items_by_scale$scale[i]
    items <- unlist(items_by_scale$items[[i]], use.names = FALSE)
    
    if (norm(sc) == "bisbas") {
      bas_norms <- bisbas_bas_item_norms()
      if (length(bas_norms) < 2L) { logger$write("⚠️ BISBAS BAS-only: <2 items present; ω skipped."); next }
      X <- prepare_item_df(df, bas_norms)
      res <- compute_omega_and_loadings(X)
    } else if (is_cape(sc)) {
      res <- omega_for_cape_positive(df, items)
    } else {
      mchk <- match_items(items)
      if (mchk$n_found < 2L) { logger$write(glue::glue("⚠️ Skipping ω for scale '{sc}': <2 items present.")); next }
      keep_norm <- intersect(normalize_id(items), col_map$item_norm)
      X <- prepare_item_df(df, keep_norm)
      res <- compute_omega_and_loadings(X)
    }
    
    if (is.finite(res$omega) && !is.na(res$omega) && res$omega < OMEGA_WARN_THRESHOLD) {
      msg <- glue::glue("⚠️ ω < .60 for scale '{sc}': inacceptably low internal consistency — please check.")
      warning(msg, call. = FALSE); logger$write(msg)
    }
    
    results[[paste0("scale__", sc)]] <- list(
      level = "scale", name = sc, project = "OVERALL",
      omega_overall = res$omega
    )
    if (!is.null(res$loadings) && nrow(res$loadings)) {
      loadings_rows[[length(loadings_rows)+1]] <- res$loadings %>%
        dplyr::mutate(level = "scale", name = sc, method = res$method, .before = 1)
    }
  }
}

# --- SUBSCALES ---------------------------------------------------------------
if (nrow(items_by_subscale)) {
  message("== Subscales ==")
  for (i in seq_len(nrow(items_by_subscale))) {
    sc    <- items_by_subscale$scale[i]
    ssc   <- items_by_subscale$subscale[i]
    items <- unlist(items_by_subscale$items[[i]], use.names = FALSE)
    label <- paste(sc, ssc, sep = " / ")
    
    if (is_cape(sc) && is_positive(ssc)) {
      res <- omega_for_cape_positive(df, items)
    } else {
      mchk <- match_items(items)
      if (mchk$n_found < 2L) { logger$write(glue::glue("⚠️ Skipping ω for subscale '{label}': <2 items present.")); next }
      keep_norm <- intersect(normalize_id(items), col_map$item_norm)
      X <- prepare_item_df(df, keep_norm)
      res <- compute_omega_and_loadings(X)
    }
    
    if (is.finite(res$omega) && !is.na(res$omega) && res$omega < OMEGA_WARN_THRESHOLD) {
      msg <- glue::glue("⚠️ ω < .60 for subscale '{label}': inacceptably low internal consistency — please check.")
      warning(msg, call. = FALSE); logger$write(msg)
    }
    
    results[[paste0("subscale__", sc, "__", ssc)]] <- list(
      level = "subscale", name = label, project = "OVERALL",
      omega_overall = res$omega
    )
    if (!is.null(res$loadings) && nrow(res$loadings)) {
      loadings_rows[[length(loadings_rows)+1]] <- res$loadings %>%
        dplyr::mutate(level = "subscale", name = label, method = res$method, .before = 1)
    }
  }
}

# ===== Flatten & write Excel =================================================
omega_rows <- purrr::imap_dfr(results, function(res, key) {
  tibble::tibble(level = res$level, name = res$name, project = "OVERALL",
                 omega_overall = as.numeric(res$omega_overall))
})

loadings_tbl <- if (length(loadings_rows)) dplyr::bind_rows(loadings_rows) else {
  tibble::tibble(level=character(), name=character(), method=character(),
                 item=character(), loading=double())
}

out_xlsx <- fs::path(OUT_BASE, glue::glue("{SAMPLE}_omega_results.xlsx"))
writexl::write_xlsx(list(omega = omega_rows, loadings = loadings_tbl), out_xlsx)

message("Omega XLSX: ", out_xlsx)
message("Log file  : ", log_path)
try(logger$close(), silent = TRUE)
