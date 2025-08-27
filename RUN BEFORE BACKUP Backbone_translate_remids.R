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
# - for rows with a non-empty remid, looks up Remote_ID and writes the matching Participant_ID into vpid
# - saves the updated data as "..._remids_translated.csv"
#
# Note:
# - remid and vpid are numeric-only by design (but may be stored as character).
# - Columns remid and vpid exist in the survey exports; we do not create or standardize them.
# - Before making changes, we check for remids that have no correspondence in the Excel sheet.
#   We now IGNORE any remids containing letters (e.g., 'xxxxx') in that check.

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
# Script directory (works in RStudio; falls back to current working directory)
script_dir <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path),
                       error = function(e) getwd())

in_path      <- file.path(script_dir, "raw_data")
info_path    <- file.path(script_dir, "information")

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

# 2) Correct mis-typed remid in raw survey data (805199 → 80519) in-memory only
fix_mistyped_remid <- function(df) {
  if (is.numeric(df$remid)) {
    df$remid[df$remid == 805199] <- 80519
  } else {
    df$remid[df$remid == "805199"] <- "80519"
  }
  df
}
dat_children_parents <- fix_mistyped_remid(dat_children_parents)
dat_adults           <- fix_mistyped_remid(dat_adults)

# helper: find unmapped remids (ignoring any with letters) ---------------------
find_unmapped <- function(df, remote_ids) {
  # take non-empty remids
  remids <- unique(df$remid[!is.na(df$remid) & df$remid != ""])
  remids_chr <- as.character(remids)
  
  # keep only numeric remids OR known allowed special tokens (like "R70999")
  is_numeric <- grepl("^[0-9]+$", remids_chr)
  whitelist  <- remids_chr %in% c("R70999")
  
  candidates <- remids_chr[is_numeric | whitelist]
  
  setdiff(candidates, as.character(remote_ids))
}

# pre-check for unmapped remids (no modification yet) --------------------------
unmapped_children <- find_unmapped(dat_children_parents, map_ids$Remote_ID)
unmapped_adults   <- find_unmapped(dat_adults,           map_ids$Remote_ID)

if (length(unmapped_children) > 0 || length(unmapped_adults) > 0) {
  if (length(unmapped_children) > 0) {
    cat("\n[children/parents] remids without mapping (n=", length(unmapped_children), "):\n", sep = "")
    print(unmapped_children)
  }
  if (length(unmapped_adults) > 0) {
    cat("\n[adults] remids without mapping (n=", length(unmapped_adults), "):\n", sep = "")
    print(unmapped_adults)
  }
  stop("\nUnmapped remids detected (ignoring any entries containing letters). Please update the Excel mapping and re-run.")
}

# translate function -----------------------------------------------------------
translate_remids <- function(df, map_tbl) {
  df |>
    dplyr::mutate(remid_chr = as.character(remid)) |>
    dplyr::left_join(map_tbl, by = c("remid_chr" = "Remote_ID")) |>
    dplyr::mutate(
      vpid  = ifelse(!is.na(remid) & as.character(remid) != "" & !is.na(Participant_ID),
                     Participant_ID,
                     vpid),
      remid = ifelse(!is.na(remid) & as.character(remid) != "" & !is.na(Participant_ID),
                     NA,  # clear remid after successful translation
                     remid)
    ) |>
    dplyr::select(-remid_chr, -Participant_ID)
}
# translate and save -----------------------------------------------------------
dat_children_parents_out <- translate_remids(dat_children_parents, map_ids)
dat_adults_out           <- translate_remids(dat_adults,           map_ids)

write.table(dat_children_parents_out,
            file = file.path(in_path, out_children_parents),
            sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

write.table(dat_adults_out,
            file = file.path(in_path, out_adults),
            sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

cat("\nSaved translated data to:\n",
    file.path(in_path, out_children_parents), "\n",
    file.path(in_path, out_adults), "\n", sep = "")
