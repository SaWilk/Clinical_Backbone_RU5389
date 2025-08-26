#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: translate remids
# Author: Saskia Wilken (saskia.wilken@uni-hamburg.de) 
# 2025-08-26
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script
# - reads the "children/parents" questionnaire data
# - loads Remote ↔ Participant ID mappings from the Excel sheet "Combined"
# - for rows with a non-empty remid, looks up Remote_ID and writes the matching Participant_ID into vpid
# - saves the updated data as "results-survey798916_remids_translated.csv"

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
file_out               <- "results-survey798916_remids_translated.csv"
xlsx_file              <- "2025-08-20_Remote-IDs_Projekt_8_Extended_SW.xlsx"
xlsx_sheet             <- "Combined"

# load data --------------------------------------------------------------------
dat_children_parents <- read.csv(file.path(in_path, file_children_parents),
                                 sep = ";", stringsAsFactors = FALSE, check.names = FALSE)

# load mapping (Participant_ID, Remote_ID) from Excel ------------------------
map_ids_raw <- readxl::read_excel(file.path(info_path, xlsx_file),
                                  sheet = xlsx_sheet)

# keep only needed columns, coerce to character, and standardize for safe joins
map_ids <- map_ids_raw |>
  dplyr::select(Participant_ID, Remote_ID) |>
  dplyr::mutate(
    Participant_ID = as.character(Participant_ID),
    Remote_ID      = as.character(Remote_ID),
    Remote_ID_std  = tolower(trimws(Remote_ID))
  )

# ensure remid/vpid exist; create if missing -----------------------------------
if (!"remid" %in% names(dat_children_parents)) {
  dat_children_parents$remid <- NA_character_
}
if (!"vpid" %in% names(dat_children_parents)) {
  dat_children_parents$vpid <- NA_character_
}

# standardize remid for joining ------------------------------------------------
dat_std <- dat_children_parents |>
  dplyr::mutate(remid_std = tolower(trimws(as.character(remid))))

# join & translate remids -> vpid ----------------------------------------------
dat_joined <- dat_std |>
  dplyr::left_join(map_ids, by = dplyr::join_by(remid_std == Remote_ID_std)) |>
  dplyr::mutate(
    vpid = ifelse(!is.na(remid) & nzchar(remid) & !is.na(Participant_ID),
                  Participant_ID,
                  vpid)
  ) |>
  dplyr::select(-c(remid_std, Remote_ID_std, Remote_ID, Participant_ID))

# save result ------------------------------------------------------------------
out_file_path <- file.path(in_path, file_out)
write.table(dat_joined, file = out_file_path, sep = ";", dec = ".", row.names = FALSE, qmethod = "double")

cat("Saved translated data to:\n", out_file_path, "\n")
