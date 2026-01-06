#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Separate Backbone Data by Project  — Cleaning & Export Pipeline
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com) &
#          Antonia Bott (antonia.bott@uni-hamburg.de)
# First edited: 2025-08-08 (SW)
#
# Description:
# This script reads questionnaire data (LimeSurvey, PsyToolkit), fixes known
# VP-ID issues, removes test/empty entries, separates data per project, exports
# cleaned datasets (and discarded rows) to disk, and manages logging.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Clean up R environment -------------------------------------------------------
rm(list = ls())
cat("\014")

# Install/load packages --------------------------------------------------------
if (!require("dplyr"))     { install.packages("dplyr")     }; library(dplyr)
if (!require("tidyr"))     { install.packages("tidyr")     }; library(tidyr)
if (!require("writexl"))   { install.packages("writexl")   }; library(writexl)
if (!require("rstudioapi")){ install.packages("rstudioapi")}; library(rstudioapi)
if (!require("readxl"))    { install.packages("readxl")}; library(readxl)
if (!require("purrr")){ install.packages("purrr")}; library(purrr)
if (!require("stringr")){ install.packages("stringr")}; library(stringr)
if (!require("rlang")){ install.packages("rlang")}; library(rlang)
if (!require("readr")){ install.packages("readr")}; library(readr)


# Ensure proper number display -------------------------------------------------
options(scipen = 999)  # disable scientific notation globally

# Get Today -------------------------------------------------------------------
today <- format(Sys.Date(), "%Y-%m-%d")

# Set working directory -----------------------------------------------
whoami <- Sys.info()[["nodename"]]
if (is.null(whoami) || is.na(whoami)) whoami <- Sys.info()[[4]]
print(whoami)

# If you are not in this list, a sensible default is used
name <- switch(
  whoami,
  "KOPSY-D-033080" = "K:/vol2011/bba3488/",
  "UN-LAP-015977"  = {
    if (rstudioapi::isAvailable()) {
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else {
      getwd()
    }
  },
  # default fallback
  getwd()
)

setwd(name)

in_path          <- file.path("raw_data")
out_path         <- file.path("01_project_data")
function_path    <- file.path("functions")
psytool_path     <- file.path("raw_data", "psytoolkit")
cogtest_out_path <- file.path(out_path, "experiment_data")

# Specify the folder path
discarded_path <- file.path(out_path, "discarded")

# Check if the folder exists
if (!dir.exists(discarded_path)) {
  dir.create(discarded_path, recursive = TRUE)
}

# ---------- shared helpers ----------
# robust CSV reader for both ';' (questionnaires) and ',' (PsyToolkit)
safe_read_csv <- function(path, tz = "Europe/Berlin") {
  stopifnot(file.exists(path))
  first <- readr::read_lines(path, n_max = 1)
  delim <- ifelse(stringr::str_count(first, ";") > stringr::str_count(first, ","), ";", ",")
  df <- readr::read_delim(
    path, delim = delim,
    na = c("", "NA", "NaN", "null"),
    locale = readr::locale(tz = tz, decimal_mark = ".", grouping_mark = ","),
    trim_ws = TRUE, show_col_types = FALSE
  )
  # strip BOM + normalize names
  nm <- names(df); nm[1] <- sub("^\ufeff", "", nm[1]); nm <- gsub("\\s+", "_", trimws(nm)); names(df) <- nm
  # parse TIME_* if present with the exact pattern we saw: %Y-%m-%d-%H-%M
  parse_hyphen_dt <- function(x) {
    y <- trimws(as.character(x)); y[y %in% c("", "NA","NaN","null")] <- NA
    suppressWarnings(as.POSIXct(y, format = "%Y-%m-%d-%H-%M", tz = tz))
  }
  if ("TIME_start" %in% names(df)) {
    df$TIME_start <- parse_hyphen_dt(df$TIME_start)
  }
  if ("TIME_end" %in% names(df)) {
    df$TIME_end <- parse_hyphen_dt(df$TIME_end)
  }
  df
}


.detect_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  idx <- grep("^--file=", args)
  if (length(idx)) {
    p <- sub("^--file=", "", args[idx[length(idx)]])
    if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
  }
  if (requireNamespace("knitr", quietly = TRUE)) {
    p <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p, mustWork = FALSE)))
  }
  ""
}
.sanitize_base <- function(p) {
  if (is.null(p)) return(NULL)
  p2 <- normalizePath(p, winslash = "/", mustWork = FALSE)
  tail <- tolower(basename(p2))
  if (tail %in% c("experiment_data", "questionnaires")) dirname(p2) else p2
}
.normalize_sample <- function(x) {
  x <- tolower(x)
  if (grepl("parents_p6", x)) return("parents_p6")
  if (grepl("children_p6", x)) return("children_p6")
  if (grepl("children_parents", x)) return("children_parents")
  if (grepl("(?:^|[_\\.-])(children)(?:$|[_\\.-])", x)) return("children_parents")
  if (grepl("(?:^|[_\\.-])(adolescents)(?:$|[_\\.-])", x)) return("adolescents")
  if (grepl("(?:^|[_\\.-])(adults)(?:$|[_\\.-])", x)) return("adults")
  "unknown"
}
.infer_sample_from_call <- function(df) {
  txt1 <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
  s1 <- .normalize_sample(txt1); if (s1 != "unknown") return(s1)
  mc <- tryCatch(match.call(expand.dots = FALSE)$df, error = function(e) NULL)
  txt2 <- tryCatch(if (!is.null(mc)) paste(deparse(mc), collapse = "") else "", error = function(e) "")
  s2 <- .normalize_sample(txt2); if (s2 != "unknown") return(s2)
  pf <- parent.frame()
  nms <- ls(envir = pf, all.names = TRUE)
  cand <- nms[grepl("adults|adolescents|children_parents|children|parents_p6|children_p6", tolower(nms))]
  for (nm in cand) { s3 <- .normalize_sample(nm); if (s3 != "unknown") return(s3) }
  "unknown"
}
.ensure_dir <- function(path, dry_run) if (!dir.exists(path) && !dry_run) dir.create(path, recursive = TRUE, showWarnings = FALSE)
.is_empty_df <- function(x) is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
.parse_pid <- function(lbl) { s <- trimws(as.character(lbl)); d <- gsub("\\D+", "", s); if (nzchar(d)) d else "unknown" }

## Source required functions ---------------------------------------------------
source(file.path(function_path, "separate_by_project.R"))
source(file.path(function_path, "remove_test_rows.R"))
source(file.path(function_path, "copy_psytool_files.R"))
source(file.path(function_path, "extract_pilot_by_vpid.R"))
source(file.path(function_path, "resolve_duplicates.R"))
source(file.path(function_path, "correct_child_vpids.R"))
source(file.path(function_path, "check_vpid_forms.R"))
source(file.path(function_path, "find_pilot_ids.R"))
source(file.path(function_path, "compare_vpcodes.R"))
source(file.path(function_path, "partition_empty_obs_psytoolkit.R"))
source(file.path(function_path, "collect_ids_to_excel.R"))
source(file.path(function_path, "move_old_backbones.R"))
source(file.path(function_path, "setup_logging.R"))
source(file.path(function_path, "qc_ranges_and_missing.R"))
source(file.path(function_path, "aggregate_scales.R"))
source(file.path(function_path, "extract_scales.R"))
source(file.path(function_path, "prepare_project_slices.R"))
source(file.path(function_path, "write_project_slices.R"))
source(file.path(function_path, "analyze_rushing.R"))


## Move old Data ---------------------------------------------------------------
move_old_backbones(out_path, dry_run = FALSE)

## Setup Logging ---------------------------------------------------------------
logger <- setup_logging("logs/all_action_points.log")

## Backbone surveys ------------------------------------------------------------
file_adults            <- "results-survey564757_remids_translated.csv"
file_adolescents       <- "results-survey585676.csv"
file_children_parents  <- "results-survey798916_remids_translated.csv"
file_parents_p6        <- "results-survey191355.csv"
file_children_p6       <- "results-survey518972.csv"

## Load data -------------------------------------------------------------------
# Questionnaires
dat_adults           <- safe_read_csv(file.path(name, in_path, file_adults))
dat_adolescents      <- safe_read_csv(file.path(name, in_path, file_adolescents))
dat_children_parents <- safe_read_csv(file.path(name, in_path, file_children_parents))
dat_parents_p6       <- safe_read_csv(file.path(name, in_path, file_parents_p6))
dat_children_p6      <- safe_read_csv(file.path(name, in_path, file_children_p6))

# Get metadata
quest_info <- file.info(file.path(name, in_path, file_adults))
quest_info$sample <- "adults"
quest_info[2, ]   <- c(file.info(file.path(name, in_path, file_adolescents)),      "adolescents")
quest_info[3, ]   <- c(file.info(file.path(name, in_path, file_children_parents)), "children_parents")
quest_info[4, ]   <- c(file.info(file.path(name, in_path, file_parents_p6)),       "parents_p6")
quest_info[5, ]   <- c(file.info(file.path(name, in_path, file_children_p6)),      "children_p6")

# Data Overview
file_general <- "results-survey415148.csv"
dat_general  <- safe_read_csv(file.path(name, in_path, file_general))

# PsyToolkit Tests
file_psytool_info <- "data.csv"

psytool_folder_name_adults = dir(file.path(name, psytool_path), pattern = "^PsyToolkitData_RU5389_BB_adults_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$")
psytool_folder_name_children = dir(file.path(name, psytool_path), pattern = "^PsyToolkitData_RU5389_BB_children_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$")
psytool_folder_name_adolescents = dir(file.path(name, psytool_path), pattern = "^PsyToolkitData_RU5389_BB_adolescents_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$")

# Build full paths to the data.csv that actually exist
cog_paths <- c(
  if (length(psytool_folder_name_adults)) 
    file.path(name, psytool_path, psytool_folder_name_adults, file_psytool_info) else NA_character_,
  if (length(psytool_folder_name_adolescents)) 
    file.path(name, psytool_path, psytool_folder_name_adolescents, file_psytool_info) else NA_character_,
  if (length(psytool_folder_name_children)) 
    file.path(name, psytool_path, psytool_folder_name_children, file_psytool_info) else NA_character_
)

# Get metadata
cogtest_info <- file.info(cog_paths)
cogtest_info$sample <- c("adults","adolescents","children_parents")[seq_len(nrow(cogtest_info))]

psytool_info_adults      <- safe_read_csv(cog_paths[1])
psytool_info_children    <- safe_read_csv(cog_paths[3])
psytool_info_adolescents <- safe_read_csv(cog_paths[2])

# Get item info

scoring_info <- read_excel(file.path("information", "2025-10-28_Scoring.xlsx"))
item_info_adults <- read_excel(file.path("information", "2025-10-28_Item_Information_Adults.xlsx"))

# Remove Test Datasets from all Project data ----------------------------------
dat_adults           <- remove_test_rows(dat_adults,           "Adults",      dat_general)
dat_adolescents      <- remove_test_rows(dat_adolescents,      "Adolescents", dat_general)
dat_children_parents <- remove_test_rows(dat_children_parents, "Children",    dat_general)
dat_parents_p6       <- remove_test_rows(dat_parents_p6,       "Parents",     dat_general)
dat_children_p6      <- remove_test_rows(dat_children_p6,      "Children",    dat_general)

psytool_info_adults      <- remove_test_rows(psytool_info_adults,      "Adults",      dat_general)
psytool_info_adolescents <- remove_test_rows(psytool_info_adolescents, "Adolescents", dat_general)
psytool_info_children    <- remove_test_rows(psytool_info_children,    "Children",    dat_general)

################################################################################
## Data Cleaning for Questionnaire Data ----------------------------------------
################################################################################


# ---- 1) read schema once ----
library(jsonlite)
schema <- read_json(file.path("information", "recommended_schema.json"), simplifyVector = TRUE)

# small converters
as_num_safely <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = FALSE)))
as_int_safely <- function(x) suppressWarnings(as.integer(as_num_safely(x)))

as_time_safely <- function(x, tz = "Europe/Berlin") {
  x <- as.character(x)
  # try several known formats
  for (fmt in c("%Y-%m-%d %H:%M:%S",
                "%Y-%m-%d-%H-%M",
                "%Y-%m-%d",
                "%d.%m.%Y %H:%M:%S",
                "%d.%m.%Y %H:%M",
                "%d.%m.%Y")) {
    out <- try(as.POSIXct(x, format = fmt, tz = tz), silent = TRUE)
    if (!inherits(out, "try-error") && any(!is.na(out))) {
      # fill non-matching with NA, keep vector length
      good <- !is.na(out)
      if (!all(good)) out[!good] <- NA
      return(out)
    }
  }
  # fallback: return NA POSIXct vector
  as.POSIXct(rep(NA, length(x)), origin = "1970-01-01", tz = tz)
}

coerce_by_schema <- function(df, schema) {
  if (is.null(df) || !ncol(df)) return(df)
  for (nm in intersect(names(df), names(schema))) {
    tgt <- schema[[nm]]
    if (is.null(tgt)) next
    # skip if already the right general class
    cur <- class(df[[nm]])[1]
    # perform coercion
    df[[nm]] <- switch(
      tgt,
      "integer"  = as_int_safely(df[[nm]]),
      "double"   = as_num_safely(df[[nm]]),
      "datetime" = as_time_safely(df[[nm]]),
      "string"   = as.character(df[[nm]]),
      df[[nm]]
    )
  }
  df
}


# Set column name variables ----------------------------------------------------
vp_col     <- "vpid"
project_col<- "project"
last_page  <- "lastpage"
link_col   <- "comp"
id_col     <- "id"         # careful - psytoolkit: vpid; questionnaires: unique counter
submit_col <- "submitdate"

# --- Normalize ID column types (questionnaires) ---
for (nm in c("dat_adults","dat_adolescents","dat_children_parents","dat_children_p6","dat_parents_p6")) {
  if (exists(nm)) {
    df <- get(nm)
    if ("vpid"   %in% names(df)) df$vpid   <- suppressWarnings(as.integer(as.character(df$vpid)))
    if ("VPCode" %in% names(df)) df$VPCode <- suppressWarnings(as.integer(as.character(df$VPCode)))
    assign(nm, df)
  }
}

# --- Normalize ID column types (cogtests / PsyToolkit) ---
for (nm in c("psytool_info_adults","psytool_info_adolescents","psytool_info_children")) {
  if (exists(nm)) {
    df <- get(nm)
    if ("id" %in% names(df)) df$id <- suppressWarnings(as.integer(as.character(df$id)))
    assign(nm, df)
  }
}


# Fix issues with project assignment ------------------------------------------
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)]  <- 2
# assuming this is project 9 since project 1 does not collect data and the id starts with a 9.
# also project 9 IDs are actually consecutive and 17 is missing.
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 99017)] <- 9


# Remove empty Rows ------------------------------------------------------------
# ---------- NA & factor SAFE helpers ----------
.as_num <- function(x) {
  if (is.factor(x)) {
    suppressWarnings(as.numeric(as.character(x)))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

.pid_eq <- function(df, project_col, PROJECT) {
  if (!project_col %in% names(df)) return(rep(FALSE, nrow(df)))
  lab <- df[[project_col]]
  if (is.factor(lab)) lab <- as.character(lab)
  lab <- trimws(as.character(lab))
  dig <- gsub("\\D+", "", lab)
  dig == as.character(PROJECT)
}

.comp_is_cogn <- function(df, link_col) {
  if (!link_col %in% names(df)) return(rep(FALSE, nrow(df)))
  comp <- df[[link_col]]
  if (is.factor(comp)) comp <- as.character(comp)
  comp <- tolower(trimws(as.character(comp)))
  !is.na(comp) & comp == "cogn"
}

.lp_is_below <- function(df, last_page, thr) {
  if (!last_page %in% names(df)) return(rep(FALSE, nrow(df)))
  lp <- .as_num(df[[last_page]])
  !is.na(lp) & lp < thr
}

# Build mask: project == PROJECT & (comp == "cogn" OR lastpage < thr)
.drop_mask <- function(df, PROJECT, project_col, link_col, last_page, thr) {
  .pid_eq(df, project_col, PROJECT) &
    ( .comp_is_cogn(df, link_col) | .lp_is_below(df, last_page, thr) )
}

# tiny diag helper
.diag <- function(tag, df, mask) {
  cat(sprintf("%s: n=%d | drop=%d | keep=%d\n",
              tag, nrow(df), sum(mask, na.rm = TRUE), nrow(df) - sum(mask, na.rm = TRUE)))
}

# ------- coerce table types to same format ---------------#
# Example on your loaded tables:
dat_adults           <- coerce_by_schema(dat_adults,           schema)
dat_adolescents      <- coerce_by_schema(dat_adolescents,      schema)
dat_children_parents <- coerce_by_schema(dat_children_parents, schema)
dat_parents_p6       <- coerce_by_schema(dat_parents_p6,       schema)
dat_children_p6      <- coerce_by_schema(dat_children_p6,      schema)
dat_general          <- coerce_by_schema(dat_general,          schema)

# --- ALSO coerce PsyToolkit tables to the same schema --- #
psytool_info_adults      <- coerce_by_schema(psytool_info_adults,      schema)
psytool_info_adolescents <- coerce_by_schema(psytool_info_adolescents, schema)
psytool_info_children    <- coerce_by_schema(psytool_info_children,    schema)

# Be explicit for project + id to remove any ambiguity
if ("p"  %in% names(psytool_info_adults))      psytool_info_adults$p      <- as_int_safely(psytool_info_adults$p)
if ("p"  %in% names(psytool_info_adolescents)) psytool_info_adolescents$p <- as_int_safely(psytool_info_adolescents$p)
if ("p"  %in% names(psytool_info_children))    psytool_info_children$p    <- as_int_safely(psytool_info_children$p)

if ("id" %in% names(psytool_info_adults))      psytool_info_adults$id      <- as_int_safely(psytool_info_adults$id)
if ("id" %in% names(psytool_info_adolescents)) psytool_info_adolescents$id <- as_int_safely(psytool_info_adolescents$id)
if ("id" %in% names(psytool_info_children))    psytool_info_children$id    <- as_int_safely(psytool_info_children$id)


# ---------- Remove empty Rows ----------
LAST_P_EMPTY <- 7

## Project 3
PROJECT <- 3
mask_ad_3 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_3 <- dat_adults[ which(mask_ad_3), , drop = FALSE ]
dat_adults <- dat_adults[ -which(mask_ad_3), , drop = FALSE ]
.diag("P3 adults", dat_adults, mask_ad_3)

mask_ch_3 <- .drop_mask(dat_children_parents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ch_3 <- dat_children_parents[ which(mask_ch_3), , drop = FALSE ]
dat_children_parents <- dat_children_parents[ -which(mask_ch_3), , drop = FALSE ]
.diag("P3 children", dat_children_parents, mask_ch_3)

## Project 4
PROJECT <- 4
mask_ch_4 <- .drop_mask(dat_children_parents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ch_4 <- dat_children_parents[ which(mask_ch_4), , drop = FALSE ]
dat_children_parents <- dat_children_parents[ -which(mask_ch_4), , drop = FALSE ]
.diag("P4 children", dat_children_parents, mask_ch_4)

mask_ad_4 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_4 <- dat_adults[ which(mask_ad_4), , drop = FALSE ]
dat_adults <- dat_adults[ -which(mask_ad_4), , drop = FALSE ]
.diag("P4 adults", dat_adults, mask_ad_4)

## Project 6 (different questionnaire; no project split for _p6 forms)
LAST_P_EMPTY <- 3
mask_ch_6 <- .lp_is_below(dat_children_p6, last_page, LAST_P_EMPTY)
empty_ch_6 <- dat_children_p6[ which(mask_ch_6), , drop = FALSE ]
dat_children_p6 <- dat_children_p6[ -which(mask_ch_6), , drop = FALSE ]
.diag("P6 children_p6", dat_children_p6, mask_ch_6)

mask_p_6 <- .lp_is_below(dat_parents_p6, last_page, LAST_P_EMPTY)
empty_p_6 <- dat_parents_p6[ which(mask_p_6), , drop = FALSE ]
dat_parents_p6 <- dat_parents_p6[ -which(mask_p_6), , drop = FALSE ]
.diag("P6 parents_p6", dat_parents_p6, mask_p_6)

## Project 7
PROJECT <- 7
LAST_P_EMPTY <- 7

mask_ad_7 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_7 <- dat_adults[ which(mask_ad_7), , drop = FALSE ]
dat_adults <- dat_adults[ -which(mask_ad_7), , drop = FALSE ]
.diag("P7 adults", dat_adults, mask_ad_7)

mask_adlsc_7 <- .drop_mask(dat_adolescents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_adlsc_7 <- dat_adolescents[ which(mask_adlsc_7), , drop = FALSE ]
dat_adolescents <- dat_adolescents[ -which(mask_adlsc_7), , drop = FALSE ]
.diag("P7 adolescents", dat_adolescents, mask_adlsc_7)

## Project 8
PROJECT <- 8
mask_ad_8 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_8 <- dat_adults[ which(mask_ad_8), , drop = FALSE ]
dat_adults <- dat_adults[ -which(mask_ad_8), , drop = FALSE ]
.diag("P8 adults", dat_adults, mask_ad_8)

mask_ch_8 <- .drop_mask(dat_children_parents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ch_8 <- dat_children_parents[ which(mask_ch_8), , drop = FALSE ]
dat_children_parents <- dat_children_parents[ -which(mask_ch_8), , drop = FALSE ]
.diag("P8 children", dat_children_parents, mask_ch_8)

## Project 9
PROJECT <- 9
mask_ad_9 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_9 <- dat_adults[ which(mask_ad_9), , drop = FALSE ]
dat_adults <- dat_adults[ -which(mask_ad_9), , drop = FALSE ]
.diag("P9 adults", dat_adults, mask_ad_9)
# ---- collect "empty" rows in the old shape so the next code stays flush ----

# helper: an empty data.frame with the same columns as a template df
.empty_like <- function(df) df[0, , drop = FALSE]

# Ensure each per-project "empty_*" object exists (even if 0 rows)
if (!exists("empty_ad_3"))      empty_ad_3      <- .empty_like(dat_adults)
if (!exists("empty_ch_3"))      empty_ch_3      <- .empty_like(dat_children_parents)
if (!exists("empty_ch_4"))      empty_ch_4      <- .empty_like(dat_children_parents)
if (!exists("empty_ad_4"))      empty_ad_4      <- .empty_like(dat_adults)
if (!exists("empty_ch_6"))      empty_ch_6      <- .empty_like(dat_children_p6)
if (!exists("empty_p_6"))       empty_p_6       <- .empty_like(dat_parents_p6)
if (!exists("empty_ad_7"))      empty_ad_7      <- .empty_like(dat_adults)
if (!exists("empty_adlsc_7"))   empty_adlsc_7   <- .empty_like(dat_adolescents)
if (!exists("empty_ad_8"))      empty_ad_8      <- .empty_like(dat_adults)
if (!exists("empty_ch_8"))      empty_ch_8      <- .empty_like(dat_children_parents)
if (!exists("empty_ad_9"))      empty_ad_9      <- .empty_like(dat_adults)

# Recreate the legacy lists the downstream code expects
ads <- list(empty_ad_3, empty_ad_4, empty_ad_7, empty_ad_8, empty_ad_9)
ch  <- list(empty_ch_3, empty_ch_4, empty_ch_8)

# Keep only the non-empty ones
ads_non_empty <- ads[vapply(ads, nrow, integer(1)) > 0]
chs_non_empty <- ch[vapply(ch,  nrow, integer(1)) > 0]

# Bind them together (preserve columns; if none, create a truly empty like-template)
all_empty_ad <- if (length(ads_non_empty) > 0) {
  dplyr::bind_rows(ads_non_empty)
} else {
  .empty_like(dat_adults)
}

all_empty_ch <- if (length(chs_non_empty) > 0) {
  dplyr::bind_rows(chs_non_empty)
} else {
  .empty_like(dat_children_parents)
}

# Tag reason so later write-out works the same
all_empty_ad$.__reason__.   <- "empty"
all_empty_ch$.__reason__.   <- "empty"
empty_adlsc_7$.__reason__.  <- "empty"

# Fix ID naming issues ---------------------------------------------------------
# Project 2
PROJECT <- 2
# wrong entry
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 20035)] <- 20036
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4    & dat_adults[[project_col]] == PROJECT)] <- 20004
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 6    & dat_adults[[project_col]] == PROJECT)] <- 20006
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 15   & dat_adults[[project_col]] == PROJECT)] <- 20015
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2023 & dat_adults[[project_col]] == PROJECT)] <- 20023
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 26   & dat_adults[[project_col]] == PROJECT)] <- 20026
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 35   & dat_adults[[project_col]] == PROJECT)] <- 20035
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2041 & dat_adults[[project_col]] == PROJECT)] <- 20041
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2044 & dat_adults[[project_col]] == PROJECT)] <- 20044
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2046 & dat_adults[[project_col]] == PROJECT)] <- 20046
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2048 & dat_adults[[project_col]] == PROJECT)] <- 20048
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2051 & dat_adults[[project_col]] == PROJECT)] <- 20051
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2052 & dat_adults[[project_col]] == PROJECT)] <- 20052

# Project 3
PROJECT <- 3
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 1 & dat_adults[[project_col]] == PROJECT)] <- 30001
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 3 & dat_adults[[project_col]] == PROJECT)] <- 30003
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 8 & dat_adults[[project_col]] == PROJECT)] <- 30008
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9 & dat_adults[[project_col]] == PROJECT)] <- 30009
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 104 & dat_adults[[project_col]] == PROJECT)] <- 30104

# assuming the wrong initial number was given...
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10002 & dat_adults[[project_col]] == PROJECT)] <- 30002
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10005 & dat_adults[[project_col]] == PROJECT)] <- 30005
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10006 & dat_adults[[project_col]] == PROJECT)] <- 30006
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10007 & dat_adults[[project_col]] == PROJECT)] <- 30007
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 40019 & dat_adults[[project_col]] == PROJECT)] <- 30019

# falsely named datasets
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 227 & dat_adults[[project_col]] == PROJECT)] <- 30047
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 316 & dat_adults[[project_col]] == PROJECT)] <- 30057
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 579 & dat_adults[[project_col]] == PROJECT)] <- 30100
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 606 & dat_adults[[project_col]] == PROJECT)] <- 30101
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 708 & dat_adults[[project_col]] == PROJECT)] <- 30112
# info on who to rename to what comes from Hendrik

# Project 4
PROJECT <- 4
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4001 & dat_adults[[project_col]] == PROJECT)] <- 40001
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4002 & dat_adults[[project_col]] == PROJECT)] <- 40002
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4003 & dat_adults[[project_col]] == PROJECT)] <- 40003

# falesly named datasets
dat_adults[[vp_col]][which(dat_adults$id == 630 & dat_adults[[project_col]] == PROJECT)] <- 40016

# Project 8
PROJECT <- 8
# assuming a 0 (or many) 0s are missing
dat_children_parents[[vp_col]][which(dat_adults[[vp_col]] == 80418)] <- 80518

# Project 9
PROJECT <- 9
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9901)] <- 99001

# Special Case Project 8: Remap VPIDs so children and adults have unique IDs --
dat_children_parents <- correct_child_vpids(
  dat_children_parents, vpid_col = "vpid", project_col = "project", startdate_col = "startdate",
  mapping_file = file.path("information", "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx")
)

# Gather Pilot Participant IDs -------------------------------------------------
pilots_ad_auto <- find_pilot_ids(dat_general, dat_adults)
pilots_asc_auto <- find_pilot_ids(dat_general, dat_adolescents)
pilots_ch_auto <- find_pilot_ids(dat_general, dat_children_parents)

pilot_ad_2 <- c(20004)
pilot_ad_9 <- c()
pilot_ad_8 <- c(80350)
pilot_asc_7 <- c()
pilot_ch_6 <- c(62973, 62980, 62998, 62992, 62987, 62989, 62994, 62970)
pilot_ch_8 <- c(80350)

pilot_ad_all <- c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto)
pilot_asc_all <- c(pilots_asc_auto)
pilots_ch_all <- c(pilots_ch_auto, pilot_ch_6, pilot_ch_8)

# --- SAVE P9 (and all) pilot rows BEFORE extracting them out ------------------
pilot_quest_adults  <- dplyr::filter(dat_adults, .data[[vp_col]] %in% pilot_ad_all)

# Move to separate file and from original dataset -----------------------------
dat_adults <- extract_pilot_by_vpid(
  dat_adults, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_ad_all, sample = "adults", vpid_col = "vpid"
)
dat_adolescents <- extract_pilot_by_vpid(
  dat_adolescents, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_asc_all, sample = "adolescents", vpid_col = "vpid"
)
dat_children_parents <- extract_pilot_by_vpid(
  dat_children_parents, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "children_parents", vpid_col = "vpid"
)
dat_children_p6 <- extract_pilot_by_vpid(
  dat_children_p6, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "children_p6", vpid_col = "VPCode"
)
dat_parents_p6 <- extract_pilot_by_vpid(
  dat_parents_p6, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "parents_p6", vpid_col = "VPCode"
)

# Handle duplicate IDs ---------------------------------------------------------
# Delete not needed, incomplete or faulty datasets
vp_col = "vpid"
project_col = "project"
# Project 3
del_id_ad <- c(59, 80) # Hendrik said they can be deleted as they are incomplete
dat_adults <- dat_adults %>%
  dplyr::filter(!id %in% del_id_ad)

# Project 6
keep_row_id <- dat_children_parents %>%
  mutate(start_dt = as.POSIXct(startdate), .row = row_number()) %>%
  filter(vpid == 62128, form == "C") %>%
  arrange(start_dt, .row) %>%
  slice_head(n = 1) %>%
  pull(.row)

dat_children_parents <- dat_children_parents %>%
  mutate(.row = row_number()) %>%
  filter(.row == keep_row_id | !(vpid == 62128 & form == "C")) %>%
  select(-.row)

# Project 7
# participants filled out questionnaires twice
drop_ids_p7 <- c(70076L,70072L,70062L)
dat_adults <- dat_adults %>% group_by(vpid, project) %>% arrange(submitdate, .by_group=TRUE) %>% filter(!(project==7 & vpid %in% drop_ids_p7 & n()>1 & row_number()==n())) %>% ungroup()

# Project 8
dat_children_parents <- dat_children_parents %>%
  mutate(startdate = as.Date(startdate)) %>%
  group_by(vpid, form) %>%
  filter(!(vpid == 80505 & form == "P" & startdate == max(startdate))) %>%
  ungroup()

# Leo says they are not complete and cannot be salvaged:
dat_adults <- dat_adults[!(dat_adults$vpid == 80018 & dat_adults$project == 8), ]
dat_adults <- dat_adults[!(dat_adults$vpid == 80009 & dat_adults$project == 8), ]


# Project 9
PROJECT = 9;
dat_adults = dat_adults %>%
  filter(!(vpid == 90002 & lastpage == 11))

dat_adults <- dat_adults %>% 
  group_by(vpid, project) %>% 
  arrange(dplyr::coalesce(submitdate, as.POSIXct("1900-01-01", tz="Europe/Berlin")), .by_group=TRUE) %>% 
  filter(!(project==9 & vpid==90004L & n()>1 & row_number()==n())) %>% 
  ungroup()


# Remove all rows with zero non-admin answers
drop_answer_empty <- function(df, admin_like = c(
  "vpid","vp_id","id","project","p","proj","comp","submitdate","startdate",
  "datestamp","remid","remidcheck","warning","consent","end","seed","startlanguage"
)) {
  admin_rx <- paste0("^(?:", paste(admin_like, collapse="|"), ")$", collapse="")
  is_admin <- tolower(names(df)) %in% tolower(admin_like)
  # consider non-admin, numeric/character with actual values
  sub <- df[!is_admin]
  if (!ncol(sub)) return(df)
  non_empty <- rowSums(!vapply(sub, function(x) {
    if (is.character(x)) is.na(x) | trimws(x) == "" else is.na(x)
  }, logical(nrow(df)))) > 0
  df[non_empty, , drop = FALSE]
}

dat_adults            <- drop_answer_empty(dat_adults)
dat_adolescents       <- drop_answer_empty(dat_adolescents)
dat_children_parents  <- drop_answer_empty(dat_children_parents)
dat_children_p6       <- drop_answer_empty(dat_children_p6)
dat_parents_p6        <- drop_answer_empty(dat_parents_p6)


# move to other sample

# --- Move from Adults -> Adolescents (Questionnaires + Cogtests) ---
ids <- 70090L

# Questionnaires (key: vpid)
dat_adolescents <- dplyr::bind_rows(dat_adolescents, dplyr::filter(dat_adults, vpid %in% ids))
dat_adults      <- dplyr::filter(dat_adults, !vpid %in% ids)

# Cogtests / PsyToolkit (key: id)
psytool_info_adolescents <- dplyr::bind_rows(psytool_info_adolescents, dplyr::filter(psytool_info_adults, id %in% ids))
psytool_info_adults      <- dplyr::filter(psytool_info_adults, !id %in% ids)




# Auto-remove and check for remaining duplicates
# Adults
res_adults <- resolve_duplicates(dat_adults, vp_col, submit_col,
                                 dataset_name = "adults", data_type = "questionnaire",
                                 project_col, logger = logger)
dat_adults    <- res_adults$cleaned
trash_adults  <- res_adults$trash_bin


# Adolescents
res_adolescents <- resolve_duplicates(dat_adolescents, vp_col, submit_col,
                                      dataset_name = "adolescents", data_type = "questionnaire",
                                      project_col, logger = logger)
dat_adolescents   <- res_adolescents$cleaned
trash_adolescents <- res_adolescents$trash_bin


# Children/Parents
res_children_parents <- resolve_duplicates(dat_children_parents, vp_col, submit_col,
                                           dataset_name = "children_parents", data_type = "questionnaire",
                                           project_col, logger = logger)
dat_children_parents  <- res_children_parents$cleaned
trash_children_parents<- res_children_parents$trash_bin

# Project 6 children parents
vp_col <- "VPCode"
res_children_p6 <- resolve_duplicates(dat_children_p6, vp_col, submit_col,
                                      dataset_name = "children_p6", data_type = "questionnaire",
                                      project_col, lastpage_threshold = 13, logger = logger)
dat_children_p6 <- res_children_p6$cleaned
trash_children_p6 <- res_children_p6$trash_bin

res_parents_p6 <- resolve_duplicates(dat_parents_p6, vp_col, submit_col,
                                     dataset_name = "parents_p6", data_type = "questionnaire",
                                     project_col, lastpage_threshold = 13, logger = logger)
dat_parents_p6 <- res_parents_p6$cleaned
trash_parents_p6 <- res_parents_p6$trash_bin

# Special Case Project 8: Check C, P and A entries -----------------------------
check_vpid_forms(dat_children_parents, logger = logger)

# Save the Trash just to be safe -----------------------------------------------
all_trash_adults      <- dplyr::bind_rows(all_empty_ad,      trash_adults)
all_trash_children    <- dplyr::bind_rows(all_empty_ch,      trash_children_parents)
all_trash_adolescents <- dplyr::bind_rows(empty_adlsc_7,     trash_adolescents)

write_xlsx(all_trash_adults,      file.path(out_path, "discarded", sprintf("deleted-rows_%s_adults.xlsx", today)))
write_xlsx(all_trash_children,    file.path(out_path, "discarded", sprintf("deleted-rows_%s_children.xlsx", today)))
write_xlsx(all_trash_adolescents, file.path(out_path, "discarded", sprintf("deleted-rows_%s_adolescents.xlsx", today)))

# Separate the data by project and store on disk -------------------------------

samples <- list(
  adults          = dat_adults,
  adolescents     = dat_adolescents,
  children_parents= dat_children_parents,
  children_p6     = dat_children_p6,
  parents_p6      = dat_parents_p6
)

# 1) prepare all preps first
preps <- lapply(names(samples), function(s) {
  prepare_project_slices(samples[[s]], out_path = out_path, sample = s,
                         data_type = "questionnaires", metadata_info = quest_info)
})
names(preps) <- names(samples)

# 2) pooled rushing for adults + adolescents (single PNG, shared cutoff)
pooled <- analyze_rushing(preps[c("adults","adolescents")])
preps["adults"]      <- pooled["adults"]
preps["adolescents"] <- pooled["adolescents"]

# 3) rushing for the other samples individually (optional; now minutes-based)
for (s in setdiff(names(preps), c("adults","adolescents"))) {
  preps[[s]] <- analyze_rushing(preps[[s]])
}

# 4) now write everything
lapply(names(preps), function(s) write_project_slices(preps[[s]]))



##########################################################################
## Data Cleaning for Cognitive Test Data ---------------------------------
##########################################################################

# Set column name variables ----------------------------------------------------
vp_col     <- "id"
project_col<- "p"
# last_page <- "lastpage"
link_col   <- "comp"
id_col     <- NA    # psytoolkit info sheets: vpid; questionnaires: unique increment counter
submit_col <- "TIME_end"
start_col  <- "TIME_start"

# Fix issues with project assignment ------------------------------------------
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)]  <- 2
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)] <- 9

# Remove empty Rows ------------------------------------------------------------
list_output           <- partition_empty_obs_psytoolkit(psytool_info_adults)
psytool_info_adults   <- list_output$kept
no_id_ad              <- list_output$no_id
empty_rows_ad         <- list_output$empty
# TODO: need to understand how it is possible to generate entries without ID - and possibly reconstruct?

list_output               <- partition_empty_obs_psytoolkit(psytool_info_adolescents)
psytool_info_adolescents  <- list_output$kept
no_id_adlsc               <- list_output$no_id
empty_rows_adlsc          <- list_output$empty

list_output            <- partition_empty_obs_psytoolkit(psytool_info_children)
psytool_info_children  <- list_output$kept
no_id_ch               <- list_output$no_id
empty_rows_ch          <- list_output$empty

# Fix ID naming issues ---------------------------------------------------------
# Project 2
PROJECT <- 2
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 20035)] <- 20036
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 4    & psytool_info_adults[[project_col]] == PROJECT)] <- 20004
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 6    & psytool_info_adults[[project_col]] == PROJECT)] <- 20006
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 15   & psytool_info_adults[[project_col]] == PROJECT)] <- 20015
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2023 & psytool_info_adults[[project_col]] == PROJECT)] <- 20023
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 26   & psytool_info_adults[[project_col]] == PROJECT)] <- 20026
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 35   & psytool_info_adults[[project_col]] == PROJECT)] <- 20035
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2041 & psytool_info_adults[[project_col]] == PROJECT)] <- 20041
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2044 & psytool_info_adults[[project_col]] == PROJECT)] <- 20044
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2046 & psytool_info_adults[[project_col]] == PROJECT)] <- 20046
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2048 & psytool_info_adults[[project_col]] == PROJECT)] <- 20048
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2051 & psytool_info_adults[[project_col]] == PROJECT)] <- 20051
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2052 & psytool_info_adults[[project_col]] == PROJECT)] <- 20052

# Project 3
PROJECT <- 3
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 1 & psytool_info_adults[[project_col]] == PROJECT)] <- 30001
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 3 & psytool_info_adults[[project_col]] == PROJECT)] <- 30003
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 8 & psytool_info_adults[[project_col]] == PROJECT)] <- 30008
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 9 & psytool_info_adults[[project_col]] == PROJECT)] <- 30009

psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10002 & psytool_info_adults[[project_col]] == PROJECT)] <- 30002
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10005 & psytool_info_adults[[project_col]] == PROJECT)] <- 30005
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10006 & psytool_info_adults[[project_col]] == PROJECT)] <- 30006
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10007 & psytool_info_adults[[project_col]] == PROJECT)] <- 30007
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 40019 & psytool_info_adults[[project_col]] == PROJECT)] <- 30019
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 104 & psytool_info_adults[[project_col]] == PROJECT)] <- 30104

psytool_info_adults[[vp_col]][which(psytool_info_adults[[id_col]] == 579 & psytool_info_adults[[project_col]] == PROJECT)] <- 30100
psytool_info_adults[[vp_col]][which(psytool_info_adults[[id_col]] == 606 & psytool_info_adults[[project_col]] == PROJECT)] <- 30101
psytool_info_adults[[vp_col]][which(psytool_info_adults[[id_col]] == 708 & psytool_info_adults[[project_col]] == PROJECT)] <- 30112
# info on who to rename to what comes from Hendrik

# Falsely named datasets -----------------------------
psytool_info_adults$id <- suppressWarnings(as.integer(psytool_info_adults$id))
psytool_info_adults <- psytool_info_adults %>%
  group_by(id) %>%
  mutate(
    id = dplyr::case_when(
      id == 30048L & p == 3 & TIME_start == max(TIME_start) ~ 30047L,
      id == 30058L & p == 3 & TIME_start == max(TIME_start) ~ 30057L,
      id == 30099L & p == 3 & TIME_start == min(TIME_start) ~ 30100L,
      id == 30101L & p == 3 & TIME_start == max(TIME_start) ~ 30102L,
      id == 30111L & p == 3 & TIME_start == min(TIME_start) ~ 30112L,
      TRUE ~ id
    )
  ) %>%
  ungroup()

psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 219 & psytool_info_adults[[project_col]] == PROJECT)] <- 30002

# Project 4
PROJECT <- 4
# assuming a 0 (or many) 0s are missing
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 4001 & psytool_info_adults[[project_col]] == PROJECT)] <- 40001
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 4002 & psytool_info_adults[[project_col]] == PROJECT)] <- 40002
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 4003 & psytool_info_adults[[project_col]] == PROJECT)] <- 40003

# Project 8
PROJECT <- 8
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 800028 & psytool_info_adults[[project_col]] == PROJECT)] <- 80028

# Project 9
PROJECT <- 9
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 9901)] <- 99001

# Special Case Project 8: Remap VPIDs so children and adults have unique IDs --
psytool_info_children <- correct_child_vpids(
  psytool_info_children, vpid_col = "id", project_col = "p",  startdate_col = "TIME_start",
  mapping_file = file.path("information", "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx")
)


# Gather Pilot Participant IDs -------------------------------------------------
pilots_ad_auto  <- find_pilot_ids(dat_general, psytool_info_adults,      vpid_col_df2 = vp_col)
pilots_asc_auto <- find_pilot_ids(dat_general, psytool_info_adolescents, vpid_col_df2 = vp_col)
pilots_ch_auto  <- find_pilot_ids(dat_general, psytool_info_children,    vpid_col_df2 = vp_col)

pilot_ad_2  <- c(20004)
pilot_ad_9  <- c()
pilot_ad_8  <- c(80350)
pilot_asc_7 <- c()
pilot_ch_6  <- c(62973, 62980, 62998, 62992, 62987, 62989, 62994, 62970)

pilot_ad_all  <- c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto)
pilot_asc_all <- c(pilots_asc_auto)
pilots_ch_all <- c(pilots_ch_auto, pilot_ch_6)

# --- save P9 (and all) pilot rows before extracting them out ------------------
pilot_psytool_adults <- dplyr::filter(psytool_info_adults, .data[[vp_col]] %in% pilot_ad_all)

# Move to separate file and from original dataset -----------------------------
psytool_info_adults <- extract_pilot_by_vpid(
  psytool_info_adults, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_ad_all, sample = "psytool_adults", vpid_col = vp_col
)
psytool_info_adolescents <- extract_pilot_by_vpid(
  psytool_info_adolescents, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_asc_all, sample = "psytool_adolescents", vpid_col = vp_col
)
psytool_info_children <- extract_pilot_by_vpid(
  psytool_info_children, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "psytool_children", vpid_col = vp_col
)

# Handle duplicate IDs ---------------------------------------------------------
# Delete not needed, incomplete or faulty datasets -----------------------------
# Project 3 — Hendrik said they can be deleted
psytool_info_adults <- psytool_info_adults %>%
  group_by(.data[[vp_col]]) %>%
  filter(!(.data[[vp_col]] == 30009 & .data[[start_col]] != max(.data[[start_col]]))) %>%
  ungroup()

psytool_info_adults <- psytool_info_adults[!(psytool_info_adults$id == 80009 & psytool_info_adults$p == 8), ]


# Adults
res_adults <- resolve_duplicates(psytool_info_adults, vp_col, submit_col,
                                 dataset_name = "adults", data_type = "experiment_data",
                                 project_col, logger = logger)
psytool_info_adults <- res_adults$cleaned
trash_adults        <- res_adults$trash_bin

# Adolescents
res_adolescents <- resolve_duplicates(psytool_info_adolescents, vp_col, submit_col,
                                      dataset_name = "adolescents", data_type = "experiment_data",
                                      project_col, logger = logger)
psytool_info_adolescents <- res_adolescents$cleaned
trash_adolescents        <- res_adolescents$trash_bin

# Children
res_children <- resolve_duplicates(psytool_info_children, vp_col, submit_col,
                                   dataset_name = "children", data_type = "experiment_data",
                                   project_col, logger = logger)
psytool_info_children <- res_children$cleaned
trash_children        <- res_children$trash_bin

# Separate the data by project and store on disk -------------------------------
# Cognitive Tests

samples <- list(
  adults          = psytool_info_adults,
  adolescents     = psytool_info_adolescents,
  children_parents= psytool_info_children
)

# Cogtests
variable_output_paths = lapply(names(samples), function(s) {
  prep <- prepare_project_slices(samples[[s]], out_path = out_path, sample = s, data_type = "experiment_data", metadata_info = cogtest_info)
  write_project_slices(prep)
})
names(variable_output_paths) <- names(samples)


# ================= Pilot exception for Project 9 ==============================
# Save pilot rows that belong to project 9 into '<pid>_backbone/pilot_data/'
# using separate_by_project(..., pilot_mode = TRUE).

write_p9_pilots <- function(df, sample_label, data_type, metadata_info) {
  if (is.null(df) || !NROW(df)) return(invisible(NULL))
  # Detect the project column robustly
  proj_candidates <- c("project", "Projekt...Project", "Projekt.", "Projekt",
                       "projekt", "p", "proj", "Proj")
  lower_names <- tolower(names(df))
  hit <- match(tolower(proj_candidates), lower_names)
  hit <- hit[!is.na(hit)][1]
  if (is.na(hit)) return(invisible(NULL))
  proj_col <- names(df)[hit]
  sub <- df[trimws(as.character(df[[proj_col]])) %in% c("9","P9","Project 9","9."), , drop = FALSE]
  if (!NROW(sub)) return(invisible(NULL))
  
  separate_by_project(
    sub,
    out_path      = out_path,
    sample        = sample_label,
    export_csv    = FALSE,
    data_type     = data_type,
    metadata_info = metadata_info,
    pilot_mode    = TRUE,
    verbose       = TRUE
  )
}

# --- Questionnaires (use quest_info) -----------------------------------------
write_p9_pilots(pilot_quest_adults, "adults", "questionnaires", quest_info)

# --- PsyToolkit (cognitive; use cogtest_info) --------------------------------
write_p9_pilots(pilot_psytool_adults, "adults", "experiment_data", cogtest_info)


# Build logger dest map robustly from the file paths we just wrote
build_dest_dirs <- function(paths_vec) {
  # Flatten + drop empties
  paths_vec <- as.character(unlist(paths_vec, use.names = FALSE))
  paths_vec <- paths_vec[nzchar(paths_vec)]
  if (!length(paths_vec)) stop("No output paths to derive per-project destinations from.")
  
  find_backbone_root <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    if (!nzchar(p)) return(NA_character_)
    # if it's a file, start at its directory
    cur <- if (file.exists(p) && !dir.exists(p)) dirname(p) else p
    # climb until we hit X_backbone
    for (i in 1:10) {
      base <- basename(cur)
      if (grepl("^[0-9]+_backbone$", base)) return(cur)
      parent <- dirname(cur)
      if (identical(parent, cur)) break
      cur <- parent
    }
    NA_character_
  }
  
  roots <- vapply(paths_vec, find_backbone_root, character(1))
  roots <- unique(roots[!is.na(roots)])
  if (!length(roots)) stop("No per-project folders (<digits>_backbone) found among the given paths.")
  
  proj_keys <- sub("^([0-9]+).*", "\\1", basename(roots))
  stats::setNames(roots, proj_keys)
}


# Use adults (any sample works; they all produce the same project set)
dest_dirs <- build_dest_dirs(variable_output_paths$adults)

# (Optional) clear old .log files in those project folders
for (dir in dest_dirs) {
  if (dir.exists(dir)) {
    log_files <- list.files(dir, pattern = "\\.log$", full.names = TRUE)
    if (length(log_files)) {
      message("Removing existing log files in: ", dir)
      file.remove(log_files)
    }
  }
}

# Split the main log into the per-project folders
logger$split(dest_dirs)

# --- REPLACE up to here --------------------------------------------------------


# 4️⃣ Close when done
logger$close()

## Get the Experimental Data Sets Associated with the project ------------------

# copy TXT files for all per-project cogtests AND mirror into all_projects_backbone
log_copy <- copy_psytool_files(
  env_objects       = ls(pattern = "^data_.*_p_[0-9]+_cogtest$"),
  cogtest_out_path  = out_path,          # "01_project_data"
  meta_env_name     = "cogtest_info",
  test_cols         = NULL,              # let it auto-detect
  allowed_projects  = as.character(2:9),
  middle_subdir     = NULL,
  purge_old_dated   = TRUE,              # delete old *_cogtest_data before copying
  write_all_projects= TRUE               # also write ALL_<date>_*_cogtest_data
)

# Expose the P9 adults pilot subset under the name copy_psytool_files() expects
if (exists("pilot_psytool_adults") && NROW(pilot_psytool_adults)) {
  data_adults_p_9_cogtest <- pilot_psytool_adults
}

# Copy P9 adults pilot experiment files under 01_project_data/9_backbone/pilot_data/experiment_data/...
copy_psytool_files(
  env_objects      = "data_adults_p_9_cogtest",
  cogtest_out_path = out_path,
  meta_env_name    = "cogtest_info",
  allowed_projects = "9",
  middle_subdir    = "pilot_data"   # <<< puts results under pilot_data/experiment_data
)


## Print the final list of IDs to Disk -----------------------------------------

collect_ids_to_excel(
  meta_data = quest_info,
  dat_adults,
  dat_adolescents,
  dat_children_parents,
  dat_children_p6,
  dat_parents_p6,
  submit_date_col = "startdate",
  id_col = "vpid",
  project_col = "project",
  data_type = "questionnaire"
)

collect_ids_to_excel(
  meta_data = quest_info,
  dat_children_p6,
  dat_parents_p6,
  submit_date_col = "startdate",
  id_col = "VPCode",
  project_col = NULL,
  data_type = "questionnaire_p6"
)

collect_ids_to_excel(
  meta_data = cogtest_info,
  psytool_info_adults,
  psytool_info_adolescents,
  psytool_info_children,
  submit_date_col = "TIME_start",
  id_col = "id",
  project_col = "p",
  data_type = "cogtest"
)


## Data Sanity Check -----------------------------------------



## ------------- helpers -------------

# Canonical key: lowercase + remove all non-alphanumerics
.canon_key <- function(x) {
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x, perl = TRUE)
}

# Normalize item/scale mapping to columns: item, scale, item_key
.normalize_item_info <- function(item_info_adults) {
  ii <- item_info_adults %>%
    rename_with(~ "item",  dplyr::matches("(?i)^item$")) %>%
    rename_with(~ "scale", dplyr::matches("(?i)^scale$")) %>%
    mutate(
      item     = as.character(item),
      scale    = as.character(scale),
      item_key = .canon_key(item)
    ) %>%
    distinct(item, scale, item_key)
  
  # warn if different item names collapse to same key
  dup_keys <- ii %>% count(item_key) %>% filter(n > 1)
  if (nrow(dup_keys) > 0) {
    warning("Multiple Item names collapse to the same canonical key: ",
            paste0(dup_keys$item_key, collapse = ", "),
            ". Disambiguate Item names if this is unintended.")
  }
  ii
}

# Normalize scoring to columns: scale, min, max
.normalize_scoring_info <- function(scoring_info) {
  scoring_info %>%
    rename_with(~ "scale", dplyr::matches("(?i)^scale$")) %>%
    rename_with(~ "min",   dplyr::matches("(?i)^min$")) %>%
    rename_with(~ "max",   dplyr::matches("(?i)^max$")) %>%
    mutate(scale = as.character(scale)) %>%
    select(scale, min, max)
}

# Link mapping items to actual data columns (handles brackets/dots via canonical keys)
# Returns: list(link, present, missing)
.build_item_link <- function(dat, item_info_adults) {
  ii <- .normalize_item_info(item_info_adults) %>%
    mutate(item_key = .canon_key(item))
  
  dat_cols <- tibble(
    data_col = names(dat),
    data_key = .canon_key(names(dat))
  )
  
  # join item_key (mapping) -> data_key (actual columns)
  link <- ii %>%
    left_join(dat_cols, by = c("item_key" = "data_key")) %>%
    select(item, scale, item_key, data_col)
  
  # for QC we only care about true scales, not admin fields
  present <- link %>% filter(!is.na(data_col), !is.na(scale))
  missing <- link %>% filter(is.na(data_col), !is.na(scale))
  
  # warn if data columns canonicalize to the same key (rare)
  dup_dat_keys <- dat_cols %>% count(data_key) %>% filter(n > 1)
  if (nrow(dup_dat_keys) > 0) {
    warning("Multiple data columns collapse to the same canonical key: ",
            paste0(dup_dat_keys$data_key, collapse = ", "),
            ". Consider cleaning column names to avoid ambiguity.")
  }
  
  list(link = link, present = present, missing = missing)
}


# expect objects named data_adults_p_1_questionnaire ... _p_9_questionnaire
dataset_names <- sprintf("data_adults_p_%d_questionnaire", 1:9)
dataset_names <- dataset_names[dataset_names %in% ls(envir = .GlobalEnv)]
datasets <- mget(dataset_names, envir = .GlobalEnv)

# run QC on each dataset
qc_results <- imap(datasets, function(dat, nm) {
  message("\n--- QC for ", nm, " ---")
  
  # quick mapping counts (optional; useful for sanity)
  lp <- .build_item_link(dat, item_info_adults)
  message("Mapped items: ", nrow(lp$present), "  |  Unmapped (in mapping but not in data): ", nrow(lp$missing))
  
  res <- qc_ranges_and_missing(
    dat                  = dat,
    item_info_adults     = item_info_adults,
    scoring_info         = scoring_info,
    id_col               = "vpid",                 # this attaches vpids to outputs
    all_or_nothing_scales= c("FHSfamilytree", "CAPE", "SUQ", "health", "demographics", "times"),
    exclude_scales_from_qc = c("id")               # <-- ignore the ID scale in QC
  )
  
  
  # concise console output
  message("Range violations: ", nrow(res$violations))
  if (nrow(res$per_scale_summary) > 0) {
    message("Scales with any FULL missing participants:")
    print(res$per_scale_summary %>% filter(n_full_missing > 0) %>% arrange(desc(n_full_missing)))
    message("Scales with any PARTIAL missing participants (excl. all-or-nothing):")
    print(res$per_scale_summary %>% filter(n_partial_missing > 0) %>% arrange(desc(n_partial_missing)))
  } else {
    message("No mapped scales found (skipped).")
  }
  
  invisible(res)
})

