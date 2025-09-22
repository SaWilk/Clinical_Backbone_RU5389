#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: translate remids
# Author: Saskia Wilken (saskia.wilken@uni-hamburg.de) 
# 2025-08-26
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script
# - reads the "children/parents" and "adults" questionnaire data
# - loads Remote ↔ Participant ID mappings from the Excel sheet "Combined"
# - fixes known data-entry issues before any checks
# - requires remid == remidcheck (except when remid == "XXXXX")
# - if remid == "XXXXX", uses remidcheck as the lookup key (via mapping), logs these rows
# - for valid rows, maps Remote_ID → Participant_ID, writes to vpid, and clears remid/remidcheck
# - saves the updated data as "..._remids_translated.csv"

# clean up R environment
rm(list = ls())
cat("\014")

# install & load required packages --------------------------------------------
pkg <- c("dplyr", "readxl", "rstudioapi")
to_install <- pkg[!sapply(pkg, require, character.only = TRUE)]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkg, library, character.only = TRUE))

# number display ---------------------------------------------------------------
options(scipen = 999)

# paths ------------------------------------------------------------------------
script_dir <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path),
                       error = function(e) getwd())

in_path      <- file.path(script_dir, "raw_data")
info_path    <- file.path(script_dir, "information")

# optional log dir for Project 8 special-case notes
log_dir <- file.path(script_dir, "01_project_data", "logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

file_children_parents <- "results-survey798916.csv"
file_adults           <- "results-survey564757.csv"

out_children_parents  <- "results-survey798916_remids_translated.csv"
out_adults            <- "results-survey564757_remids_translated.csv"

xlsx_file   <- "2025-08-20_Remote-IDs_Projekt_8_Extended_SW.xlsx"
xlsx_sheet  <- "Combined"

# load data --------------------------------------------------------------------
dat_children_parents <- read.csv(file.path(in_path, file_children_parents),
                                 sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
dat_adults <- read.csv(file.path(in_path, file_adults),
                       sep = ";", stringsAsFactors = FALSE, check.names = FALSE)

# load mapping (Participant_ID, Remote_ID) from Excel --------------------------
map_ids <- readxl::read_excel(file.path(info_path, xlsx_file), sheet = xlsx_sheet) |>
  dplyr::select(Participant_ID, Remote_ID) |>
  dplyr::mutate(
    Participant_ID = as.character(Participant_ID),
    Remote_ID      = as.character(Remote_ID)
  )

# fix known data-entry errors --------------------------------------------------
# 1) Extend mapping with special cases (all map to vpid 99999)
special_map <- data.frame(
  Participant_ID = rep("99999", 8),
  Remote_ID      = c("99", "999", "9999", "99999", "999999", "9999999", "R70999", "10"),
  stringsAsFactors = FALSE
)
map_ids <- dplyr::bind_rows(map_ids, special_map) |>
  dplyr::distinct(Remote_ID, .keep_all = TRUE)

# 2) Correct mis-typed remid (and remidcheck) in raw survey data (805199 → 80519)
fix_mistyped_both <- function(df) {
  if (is.numeric(df$remid)) {
    df$remid[df$remid == 805199] <- 80519
  } else {
    df$remid[df$remid == "805199"] <- "80519"
  }
  if ("remidcheck" %in% names(df)) {
    if (is.numeric(df$remidcheck)) {
      df$remidcheck[df$remidcheck == 805199] <- 80519
    } else {
      df$remidcheck[df$remidcheck == "805199"] <- "80519"
    }
  }
  df
}
dat_children_parents <- fix_mistyped_both(dat_children_parents)
dat_adults           <- fix_mistyped_both(dat_adults)

# utilities --------------------------------------------------------------------
is_blank <- function(x) is.na(x) | x == ""

# Identity check before any mapping (except for the "XXXXX" rows) --------------
flag_and_report_mismatches <- function(df, dataset_name) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  
  xmask <- !is.na(remid_chr) & toupper(remid_chr) == "XXXXX"
  both_present <- !is_blank(remid_chr) & !is_blank(check_chr)
  mismatch <- both_present & (remid_chr != check_chr) & !xmask
  
  if (any(mismatch)) {
    idx <- which(mismatch)
    cat("\n⚠️ [", dataset_name, "] remid vs remidcheck mismatch rows (", length(idx), "):\n", sep = "")
    for (i in idx) {
      cat("   ⚠️ row ", i, ": remid='", remid_chr[i], "', remidcheck='", check_chr[i], "' -> MANUAL CHECK NEEDED (skipped)\n", sep = "")
    }
  }
  
  eligible <- !mismatch
  eligible
}

# Unmapped check ---------------------------------------------------------------
find_unmapped <- function(df, remote_ids, eligible_rows) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  
  xmask      <- !is.na(remid_chr) & toupper(remid_chr) == "XXXXX"
  key        <- ifelse(xmask & !is_blank(check_chr), check_chr, remid_chr)
  
  use_mask   <- eligible_rows & !is_blank(key)
  candidates <- key[use_mask]
  
  is_numeric <- grepl("^[0-9]+$", candidates)
  whitelist  <- candidates %in% c("R70999")
  candidates <- candidates[is_numeric | whitelist]
  
  setdiff(candidates, as.character(remote_ids))
}

# translate + log function -----------------------------------------------------
translate_and_log <- function(df, map_tbl, eligible_rows, dataset_name) {
  remid_chr  <- as.character(df$remid)
  check_chr  <- as.character(df$remidcheck)
  xmask      <- !is.na(remid_chr) & toupper(remid_chr) == "XXXXX"
  
  key <- ifelse(xmask & !is_blank(check_chr), check_chr, remid_chr)
  df$key_for_mapping <- key
  
  df <- df |>
    dplyr::left_join(map_tbl, by = c("key_for_mapping" = "Remote_ID"))
  
  can_translate <- eligible_rows & !is_blank(df$key_for_mapping) & !is.na(df$Participant_ID)
  
  df$vpid[can_translate] <- df$Participant_ID[can_translate]
  
  df$remid[can_translate]      <- NA
  df$remidcheck[can_translate] <- NA
  
  x_rows <- which(xmask & eligible_rows)
  log_df <- NULL
  if (length(x_rows) > 0) {
    log_df <- data.frame(
      dataset                = dataset_name,
      row_index              = x_rows,
      fallback_key_from_remidcheck = df$key_for_mapping[x_rows],
      mapped_participant_ID  = df$Participant_ID[x_rows],
      stringsAsFactors = FALSE
    )
  }
  
  df <- dplyr::select(df, -key_for_mapping, -Participant_ID)
  list(df = df, log = log_df)
}

# PROCESS: children/parents ----------------------------------------------------
eligible_cp <- flag_and_report_mismatches(dat_children_parents, "children/parents")

unmapped_children <- find_unmapped(dat_children_parents, map_ids$Remote_ID, eligible_cp)
if (length(unmapped_children) > 0) {
  cat("\n⚠️ [children/parents] remids without mapping (n=", length(unmapped_children), "):\n", sep = "")
  print(unmapped_children)
  stop("⚠️ Unmapped remids detected (ignoring letter-only and using remidcheck when 'XXXXX'). Please update the Excel mapping and re-run.")
}

res_cp <- translate_and_log(dat_children_parents, map_ids, eligible_cp, "children_parents")
dat_children_parents_out <- res_cp$df
log_cp <- res_cp$log

# PROCESS: adults --------------------------------------------------------------
eligible_ad <- flag_and_report_mismatches(dat_adults, "adults")

unmapped_adults <- find_unmapped(dat_adults, map_ids$Remote_ID, eligible_ad)
if (length(unmapped_adults) > 0) {
  cat("\n⚠️ [adults] remids without mapping (n=", length(unmapped_adults), "):\n", sep = "")
  print(unmapped_adults)
  stop("⚠️ Unmapped remids detected (ignoring letter-only and using remidcheck when 'XXXXX'). Please update the Excel mapping and re-run.")
}

res_ad <- translate_and_log(dat_adults, map_ids, eligible_ad, "adults")
dat_adults_out <- res_ad$df
log_ad <- res_ad$log

# Write Project 8 log for 'XXXXX' fallbacks ------------------------------------
log_all <- dplyr::bind_rows(log_cp, log_ad)
if (!is.null(log_all) && nrow(log_all) > 0) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0("project8_xxxxx_fallback_log_", ts, ".csv"))
  write.table(log_all, file = log_file, sep = ";", dec = ".", row.names = FALSE, qmethod = "double")
  cat("\n⚠️ Project 8 'XXXXX' fallback log written to:\n", log_file, "\n")
}

# save results -----------------------------------------------------------------
write.table(dat_children_parents_out,
            file = file.path(in_path, out_children_parents),
            sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

write.table(dat_adults_out,
            file = file.path(in_path, out_adults),
            sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

cat("\n✅ Saved translated data to:\n",
    file.path(in_path, out_children_parents), "\n",
    file.path(in_path, out_adults), "\n", sep = "")
