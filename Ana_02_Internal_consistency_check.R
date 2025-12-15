# --- analyze_backbone_scales.R -----------------------------------------------
# INTERNAL CONSISTENCY ONLY:
# - Computes McDonald's omega (ωt) for all scales + subscales
# - ALSO computes omega for "scale totals" even if the keys define only subscales
#   (scale total = union of that scale's subscale items, unless an explicit scale definition exists)
# - Exports an XLSX with two sheets:
#     1) omega    (overall + by-project + by group)
#     2) loadings (1-factor FA loadings for overall sample; no splits)
# - Logs skipped participants (too many missing items) to ROOT/logs/<DATE>_data_analysis_log.txt
#
# NOTE: This script intentionally does NOT create plots.

# ===== Pre-flight: clear console + workspace =====
rm(list = ls(all.names = TRUE)); invisible(gc())
try(cat("\014"), silent = TRUE)

# ===== Setup =====
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readr","jsonlite","tibble","dplyr","tidyr","purrr","stringr",
  "fs","glue","janitor","psych","rprojroot","readxl","openxlsx"
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
  root <- tryCatch(
    rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root),
    error = function(e) NA
  )
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

SAMPLE         <- "adults"
DELIM          <- ";"          # German Excel
MIN_PROP_ITEMS <- 0.50         # >= 50% items required to score omega/loadings
OMEGA_TIMEOUT_SEC <- 20

# ===== Source helpers =====
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))
source(file.path(DIR_FUNCTIONS, "extract_date_string.R"))
source(file.path(DIR_FUNCTIONS, "latest_questionnaire_for_sample.R"))
source(file.path(DIR_FUNCTIONS, "setup_logging.R"))

# ===== Input / Output paths =====
master_csv <- fs::path(DIR_EXPORT, SAMPLE, glue::glue("{SAMPLE}_clean_master.csv"))
keys_json  <- fs::path(DIR_KEYS, glue::glue("{SAMPLE}_keys.json"))
stopifnot(fs::file_exists(master_csv), fs::file_exists(keys_json))

q_file   <- latest_questionnaire_for_sample(SAMPLE, DIR_QUESTIONNAIRES)
date_str <- if (!is.na(q_file)) extract_date_string(q_file) else format(Sys.Date(), "%Y-%m-%d")

OUT_BASE <- fs::path(
  ROOT, "out", "internal_data_analysis",
  "distribution_of_backbone_scores_and_internal_consistency",
  paste0(date_str, "_", SAMPLE)
)
fs::dir_create(OUT_BASE)
fs::dir_create(DIR_LOGS)

# ===== Logger =====
log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_data_analysis_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

# ===== Read master CSV robustly (handles "sep=" header + BOM) =====
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

# Normalize group: missing -> HC
if ("group" %in% names(df)) {
  df$group <- as.character(df$group)
  df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
  df$group <- factor(df$group, levels = c("HC","patient"))
}

proj_col <- get_project_col(df) # "project" or "p" or NULL

# ===== Column map for items =====
col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id(names(df)))

match_items <- function(items) {
  items_norm <- normalize_id(items)
  hit <- intersect(items_norm, col_map$item_norm)
  list(n_in_key = length(items_norm), n_found = length(hit), found = hit)
}

# ===== Read & flatten the keys JSON safely =====
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

# ===== Build scale totals even if keys only define subscales =====
.as_chr0 <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) == 1 && is.na(x)) return(character(0))
  as.character(x)
}

scale_from_subscales <- items_by_subscale %>%
  dplyr::filter(!is.na(.data$scale)) %>%
  dplyr::group_by(.data$scale) %>%
  dplyr::summarise(
    items_sub = list(unique(unlist(.data$items, use.names = FALSE))),
    .groups = "drop"
  )

items_by_scale_all <- items_by_scale %>%
  dplyr::rename(items_scale = .data$items) %>%
  dplyr::full_join(scale_from_subscales, by = "scale") %>%
  dplyr::mutate(
    items = purrr::map2(.data$items_scale, .data$items_sub, ~ {
      a <- .as_chr0(.x); b <- .as_chr0(.y)
      if (length(a)) a else b
    }),
    total_source = dplyr::case_when(
      purrr::map_int(.data$items_scale, ~ length(.as_chr0(.x))) > 0 ~ "keys_scale",
      TRUE ~ "union_subscales"
    )
  ) %>%
  dplyr::select(.data$scale, .data$items, .data$total_source) %>%
  dplyr::arrange(.data$scale)

# ===== Utility: participant id =====
get_id_vec <- function(z) {
  if ("vpid" %in% names(z)) z$vpid
  else if ("vp" %in% names(z)) z$vp
  else if ("participantid" %in% names(z)) z$participantid
  else rep(NA_character_, nrow(z))
}

# ===== Omega =====
compute_omega_safe <- function(items_df, timeout_sec = OMEGA_TIMEOUT_SEC) {
  items_df[] <- lapply(items_df, function(z) suppressWarnings(as.numeric(z)))
  
  # drop empty + zero-variance columns
  keep <- vapply(items_df, function(z) {
    z <- z[is.finite(z)]
    length(z) > 0 && length(unique(z)) > 1
  }, logical(1))
  items_df <- items_df[, keep, drop = FALSE]
  
  if (ncol(items_df) < 2L || nrow(items_df) < 10L) return(NA_real_)
  
  setTimeLimit(elapsed = timeout_sec, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  
  out <- tryCatch({
    capture.output(
      res <- psych::omega(items_df, nfactors = 1, sl = FALSE, plot = FALSE, warnings = FALSE),
      file = NULL
    )
    res
  }, error = function(e) NULL)
  
  if (is.null(out)) return(NA_real_)
  
  cand <- c("omega.t","omega.tot","omega.totale","omega.totals","omega.totalscore")
  for (nm in cand) if (!is.null(out[[nm]]) && is.numeric(out[[nm]])) return(as.numeric(out[[nm]]))
  
  if (!is.null(out$omega) && is.data.frame(out$omega) && "omega.tot" %in% names(out$omega)) {
    return(as.numeric(out$omega[,"omega.tot", drop = TRUE]))
  }
  NA_real_
}

omega_for <- function(d, items, min_prop = MIN_PROP_ITEMS,
                      scale_label = NULL, sub_label = NULL) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (length(keep_norm) < 2L) return(NA_real_)
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  n_nonmiss <- rowSums(!is.na(as.matrix(X)))
  thresh    <- ceiling(min_prop * ncol(X))
  skip      <- n_nonmiss < thresh
  
  if (any(skip)) {
    ids <- get_id_vec(d)
    lbl <- paste0(scale_label %||% "?", if (!is.null(sub_label)) paste0(" / ", sub_label) else "")
    for (j in which(skip)) {
      logger$write(glue::glue("⚠️ Omega: skipping participant {ids[j]} for {lbl}: missing items >= {100*(1-min_prop)}%"))
    }
  }
  
  X <- X[!skip, , drop = FALSE]
  if (nrow(X) < 10L) return(NA_real_)
  compute_omega_safe(X)
}

# ===== Loadings (1-factor FA, minres) =====
compute_loadings_safe <- function(items_df, timeout_sec = OMEGA_TIMEOUT_SEC) {
  items_df[] <- lapply(items_df, function(z) suppressWarnings(as.numeric(z)))
  
  keep <- vapply(items_df, function(z) {
    z <- z[is.finite(z)]
    length(z) > 0 && length(unique(z)) > 1
  }, logical(1))
  items_df <- items_df[, keep, drop = FALSE]
  
  if (ncol(items_df) < 2L || nrow(items_df) < 20L) return(NULL)
  
  setTimeLimit(elapsed = timeout_sec, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  
  fa_out <- tryCatch(
    suppressWarnings(suppressMessages(psych::fa(items_df, nfactors = 1, rotate = "none", fm = "minres"))),
    error = function(e) NULL
  )
  if (is.null(fa_out)) return(NULL)
  
  L <- as.matrix(fa_out$loadings)
  if (!ncol(L)) return(NULL)
  
  tibble::tibble(
    item = rownames(L),
    loading = as.numeric(L[, 1]),
    communality = if (!is.null(fa_out$communality)) as.numeric(fa_out$communality) else NA_real_,
    uniqueness  = if (!is.null(fa_out$uniquenesses)) as.numeric(fa_out$uniquenesses) else NA_real_,
    n_used = nrow(items_df),
    method = "fa(minres, 1)"
  )
}

loadings_for <- function(d, items, min_prop = MIN_PROP_ITEMS,
                         level = "scale",
                         scale = NA_character_, subscale = NA_character_,
                         total_source = NA_character_) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (length(keep_norm) < 2L) return(tibble::tibble())
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  n_nonmiss <- rowSums(!is.na(as.matrix(X)))
  thresh <- ceiling(min_prop * ncol(X))
  X <- X[n_nonmiss >= thresh, , drop = FALSE]
  
  L <- compute_loadings_safe(X)
  if (is.null(L) || !nrow(L)) return(tibble::tibble())
  
  dplyr::mutate(
    L,
    level = level,
    scale = scale,
    subscale = subscale,
    total_source = total_source,
    .before = 1
  )
}

# ===== MAIN LOOPS =====
results <- list()

# ---- SCALE TOTALS (includes union-of-subscales totals where needed)
if (nrow(items_by_scale_all)) {
  message("== Scale totals (incl. union-of-subscales where needed) ==")
  
  for (i in seq_len(nrow(items_by_scale_all))) {
    sc    <- items_by_scale_all$scale[i]
    items <- unlist(items_by_scale_all$items[[i]], use.names = FALSE)
    src   <- items_by_scale_all$total_source[i]
    
    mchk <- match_items(items)
    message(glue::glue("[Scale {i}/{nrow(items_by_scale_all)}] {sc} (source={src}): {mchk$n_found}/{mchk$n_in_key} items found"))
    if (mchk$n_found == 0) {
      warning(glue::glue("No matching item columns for scale '{sc}'. Skipping omega."), call. = FALSE)
      next
    }
    
    om_overall <- omega_for(df, items, scale_label = sc)
    
    om_by_proj <- NULL
    if (!is.null(proj_col)) {
      om_by_proj <- split(df, df[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items, scale_label = sc))
    }
    
    om_hc <- om_pat <- NA_real_
    if ("group" %in% names(df) && any(df$group %in% c("HC","patient"), na.rm = TRUE)) {
      om_hc  <- omega_for(dplyr::filter(df, .data$group %in% "HC"), items, scale_label = sc, sub_label = "HC")
      om_pat <- omega_for(dplyr::filter(df, .data$group %in% "patient"), items, scale_label = sc, sub_label = "patient")
    }
    
    om_by_proj_hc <- om_by_proj_pat <- NULL
    if (!is.null(proj_col) && "group" %in% names(df)) {
      d_hc  <- dplyr::filter(df, .data$group %in% "HC")
      d_pat <- dplyr::filter(df, .data$group %in% "patient")
      if (nrow(d_hc))  om_by_proj_hc  <- split(d_hc,  d_hc[[proj_col]])  |> purrr::map_dbl(~ omega_for(.x, items, scale_label = sc, sub_label = "HC"))
      if (nrow(d_pat)) om_by_proj_pat <- split(d_pat, d_pat[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items, scale_label = sc, sub_label = "patient"))
    }
    
    results[[paste0("scale__", sc)]] <- list(
      level = "scale",
      name  = sc,
      total_source = src,
      omega_overall = om_overall,
      omega_by_project = om_by_proj,
      omega_overall_HC = om_hc,
      omega_overall_patient = om_pat,
      omega_by_project_HC = om_by_proj_hc,
      omega_by_project_patient = om_by_proj_pat
    )
  }
}

# ---- SUBSCALES
if (nrow(items_by_subscale)) {
  message("== Subscales ==")
  
  for (i in seq_len(nrow(items_by_subscale))) {
    sc    <- items_by_subscale$scale[i]
    ssc   <- items_by_subscale$subscale[i]
    items <- unlist(items_by_subscale$items[[i]], use.names = FALSE)
    
    mchk <- match_items(items)
    message(glue::glue("[Subscale {i}/{nrow(items_by_subscale)}] {sc} — {ssc}: {mchk$n_found}/{mchk$n_in_key} items found"))
    if (mchk$n_found == 0) {
      warning(glue::glue("No matching item columns for subscale '{sc} — {ssc}'. Skipping omega."), call. = FALSE)
      next
    }
    
    om_overall <- omega_for(df, items, scale_label = sc, sub_label = ssc)
    
    om_by_proj <- NULL
    if (!is.null(proj_col)) {
      om_by_proj <- split(df, df[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items, scale_label = sc, sub_label = ssc))
    }
    
    om_hc <- om_pat <- NA_real_
    if ("group" %in% names(df) && any(df$group %in% c("HC","patient"), na.rm = TRUE)) {
      om_hc  <- omega_for(dplyr::filter(df, .data$group %in% "HC"), items, scale_label = sc, sub_label = paste0(ssc, " / HC"))
      om_pat <- omega_for(dplyr::filter(df, .data$group %in% "patient"), items, scale_label = sc, sub_label = paste0(ssc, " / patient"))
    }
    
    om_by_proj_hc <- om_by_proj_pat <- NULL
    if (!is.null(proj_col) && "group" %in% names(df)) {
      d_hc  <- dplyr::filter(df, .data$group %in% "HC")
      d_pat <- dplyr::filter(df, .data$group %in% "patient")
      if (nrow(d_hc))  om_by_proj_hc  <- split(d_hc,  d_hc[[proj_col]])  |> purrr::map_dbl(~ omega_for(.x, items, scale_label = sc, sub_label = paste0(ssc, " / HC")))
      if (nrow(d_pat)) om_by_proj_pat <- split(d_pat, d_pat[[proj_col]]) |> purrr::map_dbl(~ omega_for(.x, items, scale_label = sc, sub_label = paste0(ssc, " / patient")))
    }
    
    results[[paste0("subscale__", sc, "__", ssc)]] <- list(
      level = "subscale",
      name  = paste(sc, ssc, sep = " / "),
      total_source = "subscale",
      omega_overall = om_overall,
      omega_by_project = om_by_proj,
      omega_overall_HC = om_hc,
      omega_overall_patient = om_pat,
      omega_by_project_HC = om_by_proj_hc,
      omega_by_project_patient = om_by_proj_pat
    )
  }
}

# ===== Flatten omega results =====
flatten_named <- function(x, prefix) {
  if (is.null(x) || length(x) == 0) return(tibble::tibble())
  nm <- names(x); if (is.null(nm)) nm <- paste0("split_", seq_along(x))
  tibble::tibble(project_or_split = nm, value = as.numeric(unname(x)), kind = prefix)
}

omega_rows <- purrr::imap_dfr(results, function(res, key) {
  base <- tibble::tibble(
    level = res$level,
    name  = res$name,
    total_source = res$total_source %||% NA_character_,
    omega_overall = as.numeric(res$omega_overall),
    omega_overall_HC = as.numeric(res$omega_overall_HC),
    omega_overall_patient = as.numeric(res$omega_overall_patient)
  )
  
  by_proj         <- flatten_named(res$omega_by_project,        "all")
  by_proj_hc      <- flatten_named(res$omega_by_project_HC,     "HC")
  by_proj_patient <- flatten_named(res$omega_by_project_patient,"patient")
  long_by_proj <- dplyr::bind_rows(by_proj, by_proj_hc, by_proj_patient)
  
  if (!nrow(long_by_proj)) {
    base %>% dplyr::mutate(project = "OVERALL") %>%
      dplyr::relocate(project, .after = total_source)
  } else {
    long_by_proj %>%
      tidyr::pivot_wider(id_cols = "project_or_split", names_from = kind, values_from = value) %>%
      dplyr::rename(project = project_or_split) %>%
      dplyr::mutate(
        level = res$level,
        name  = res$name,
        total_source = res$total_source %||% NA_character_,
        .before = 1
      ) %>%
      dplyr::bind_rows(base %>% dplyr::mutate(project = "OVERALL")) %>%
      dplyr::relocate(project, .after = total_source)
  }
})

# ===== Factor loadings table (overall; no project/group splits) =====
loadings_rows <- dplyr::bind_rows(
  # scale totals (use items_by_scale_all; ignore total_source arg in pmap by selecting cols explicitly)
  purrr::pmap_dfr(
    items_by_scale_all %>% dplyr::select(scale, items, total_source),
    function(scale, items, total_source) {
      loadings_for(
        df,
        unlist(items, use.names = FALSE),
        level = "scale",
        scale = scale,
        subscale = NA_character_,
        total_source = total_source
      )
    }
  ),
  # subscales
  purrr::pmap_dfr(
    items_by_subscale,
    function(scale, subscale, items) {
      loadings_for(
        df,
        unlist(items, use.names = FALSE),
        level = "subscale",
        scale = scale,
        subscale = subscale,
        total_source = "subscale"
      )
    }
  )
)

# ===== Write XLSX =====
xlsx_path <- fs::path(OUT_BASE, glue::glue("{SAMPLE}_omega_and_loadings.xlsx"))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "omega")
openxlsx::writeDataTable(wb, "omega", omega_rows, withFilter = TRUE)

openxlsx::addWorksheet(wb, "loadings")
openxlsx::writeDataTable(wb, "loadings", loadings_rows, withFilter = TRUE)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

message("== Summary ==")
message("Omega+Loadings XLSX: ", xlsx_path)
message("Log file:            ", log_path)

try(logger$close(), silent = TRUE)
