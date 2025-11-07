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

# Helper: safe CSV read ----------------------------------------------
safe_read_csv <- function(path, sep = NULL) {
  if (!file.exists(path)) stop("Missing file: ", path)
  if (is.null(sep)) {
    read.csv(path)
  } else {
    read.csv(path, sep = sep)
  }
}

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
dat_adults           <- safe_read_csv(file.path(name, in_path, file_adults),           sep = ";")
dat_adolescents      <- safe_read_csv(file.path(name, in_path, file_adolescents),      sep = ";")
dat_children_parents <- safe_read_csv(file.path(name, in_path, file_children_parents), sep = ";")
dat_parents_p6       <- safe_read_csv(file.path(name, in_path, file_parents_p6),       sep = ";")
dat_children_p6      <- safe_read_csv(file.path(name, in_path, file_children_p6),      sep = ";")

# Get metadata
quest_info <- file.info(file.path(name, in_path, file_adults))
quest_info$sample <- "adults"
quest_info[2, ]   <- c(file.info(file.path(name, in_path, file_adolescents)),      "adolescents")
quest_info[3, ]   <- c(file.info(file.path(name, in_path, file_children_parents)), "children_parents")
quest_info[4, ]   <- c(file.info(file.path(name, in_path, file_parents_p6)),       "parents_p6")
quest_info[5, ]   <- c(file.info(file.path(name, in_path, file_children_p6)),      "children_p6")

# Data Overview
file_general <- "results-survey415148.csv"
dat_general  <- safe_read_csv(file.path(name, in_path, file_general), sep = ";")

# PsyToolkit Tests
file_psytool_info <- "data.csv"
psytool_info_adults      <- safe_read_csv(file.path(name, psytool_path, "adults",      file_psytool_info))
psytool_info_children    <- safe_read_csv(file.path(name, psytool_path, "children",    file_psytool_info))
psytool_info_adolescents <- safe_read_csv(file.path(name, psytool_path, "adolescents", file_psytool_info))

# Get metadata
cogtest_info <- file.info(file.path(name, psytool_path, "adults", file_psytool_info))
cogtest_info$sample <- "adults"
cogtest_info[2, ]   <- c(file.info(file.path(name, psytool_path, "adolescents", file_psytool_info)), "adolescents")
cogtest_info[3, ]   <- c(file.info(file.path(name, psytool_path, "children",    file_psytool_info)), "children_parents")

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

# Set column name variables ----------------------------------------------------
vp_col     <- "vpid"
project_col<- "project"
last_page  <- "lastpage"
link_col   <- "comp"
id_col     <- "id"         # careful - psytoolkit: vpid; questionnaires: unique counter
submit_col <- "submitdate"

# Fix issues with project assignment ------------------------------------------
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)]  <- 2
# assuming this is project 9 since project 1 does not collect data and the id starts with a 9.
# also project 9 IDs are actually consecutive and 17 is missing.
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 99017)] <- 9

# Remove empty Rows ------------------------------------------------------------
LAST_P_EMPTY <- 7
# Project 3
PROJECT <- 3
# adult
empty_ad_3  <- dat_adults[which(dat_adults[[last_page]] < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT), ]
dat_adults  <- dat_adults[!(dat_adults[[project_col]] == PROJECT & dat_adults[[last_page]] < LAST_P_EMPTY), ]
# children
empty_ch_3  <- dat_children_parents[which(dat_children_parents[[last_page]] < LAST_P_EMPTY & dat_children_parents[[project_col]] == PROJECT), ]
dat_children_parents <- dat_children_parents[!(dat_children_parents[[last_page]] < LAST_P_EMPTY & dat_children_parents[[project_col]] == PROJECT), ]

# Project 4
PROJECT <- 4
# children
empty_ch_4  <- dat_children_parents[which(dat_children_parents[[last_page]] < LAST_P_EMPTY & dat_children_parents[[project_col]] == PROJECT), ]
dat_children_parents <- dat_children_parents[!(dat_children_parents[[last_page]] < LAST_P_EMPTY & dat_children_parents[[project_col]] == PROJECT), ]
# adult
empty_ad_4  <- dat_adults[which(dat_adults[[last_page]] < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT), ]
dat_adults  <- dat_adults[!(dat_adults[[project_col]] == PROJECT & dat_adults[[last_page]] < LAST_P_EMPTY), ]

# Project 6
LAST_P_EMPTY <- 3 # different questionnaire
# children
empty_ch_6  <- dat_children_p6[which(dat_children_p6[[last_page]] < LAST_P_EMPTY), ]
dat_children_p6 <- dat_children_p6[!(dat_children_p6[[last_page]] < LAST_P_EMPTY), ]
# parents
empty_p_6   <- dat_parents_p6[which(dat_parents_p6[[last_page]] < LAST_P_EMPTY), ]
dat_parents_p6   <- dat_parents_p6[!(dat_parents_p6[[last_page]] < LAST_P_EMPTY), ]

# Project 7
PROJECT <- 7
LAST_P_EMPTY <- 7 # back to original questionnaire
# adult
empty_ad_7 <- dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), ]
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                             (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ]
# adolescent
empty_adlsc_7 <- dat_adolescents[which((dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT), ]
dat_adolescents <- dat_adolescents[!(dat_adolescents[[project_col]] == PROJECT &
                                       (dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY)), ]

# Project 8
PROJECT <- 8
# adult
empty_ad_8 <- dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), ]
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                             (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ]
# children
empty_ch_8 <- dat_children_parents[which((dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY) & dat_children_parents[[project_col]] == PROJECT), ]
dat_children_parents <- dat_children_parents[!(dat_children_parents[[project_col]] == PROJECT &
                                                 (dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY)), ]

# Project 9
PROJECT <- 9
empty_ad_9 <- dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), ]
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                             (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ]

# Put them into a list
ads <- list(empty_ad_3, empty_ad_7, empty_ad_8, empty_ad_9)
ch  <- list(empty_ch_3, empty_ch_8)

# Keep only the non-empty ones
ads_non_empty <- ads[lengths(ads) > 0]
chs_non_empty <- ch[lengths(ch)   > 0]

# Bind them together (if any left)
all_empty_ad <- if (length(ads_non_empty) > 0) do.call(rbind, ads_non_empty) else data.frame()
all_empty_ch <- if (length(chs_non_empty) > 0) do.call(rbind, chs_non_empty)  else data.frame()

# Saving it so I can write to disk later on in the script
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

# assuming the wrong initial number was given...
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10002 & dat_adults[[project_col]] == PROJECT)] <- 30002
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10005 & dat_adults[[project_col]] == PROJECT)] <- 30005
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10006 & dat_adults[[project_col]] == PROJECT)] <- 30006
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10007 & dat_adults[[project_col]] == PROJECT)] <- 30007
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 40019 & dat_adults[[project_col]] == PROJECT)] <- 30019

# falsely named datasets
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 227 & dat_adults[[project_col]] == PROJECT)] <- 30047
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 316 & dat_adults[[project_col]] == PROJECT)] <- 30057

# Project 4
PROJECT <- 4
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4001 & dat_adults[[project_col]] == PROJECT)] <- 40001
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4002 & dat_adults[[project_col]] == PROJECT)] <- 40002
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4003 & dat_adults[[project_col]] == PROJECT)] <- 40003

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

pilot_ad_all <- c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto)
pilot_asc_all <- c(pilots_asc_auto)
pilots_ch_all <- c(pilots_ch_auto, pilot_ch_6)

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

# Project 8
dat_children_parents <- dat_children_parents %>%
  mutate(startdate = as.Date(startdate)) %>%
  group_by(vpid, form) %>%
  filter(!(vpid == 80505 & form == "P" & startdate == max(startdate))) %>%
  ungroup()

dat_adults <- dat_adults[!(dat_adults$vpid == 80009 & dat_adults$project == 8), ]
# Project 9
PROJECT = 9;
dat_adults = dat_adults %>%
  filter(!(vpid == 90002 & lastpage == 11))

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
all_trash_adults      <- rbind(all_empty_ad, trash_adults)
all_trash_children    <- rbind(all_empty_ch, trash_children_parents)
all_trash_adolescents <- rbind(empty_adlsc_7, trash_adolescents)

write_xlsx(all_trash_adults,      file.path(out_path, "discarded", sprintf("deleted-rows_%s_adults.xlsx", today)))
write_xlsx(all_trash_children,    file.path(out_path, "discarded", sprintf("deleted-rows_%s_children.xlsx", today)))
write_xlsx(all_trash_adolescents, file.path(out_path, "discarded", sprintf("deleted-rows_%s_adolescents.xlsx", today)))

# Separate the data by project and store on disk -------------------------------
# Questionnaires
separate_by_project(dat_adults,           out_path, "adults",          data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_adolescents,      out_path, "adolescents",     data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_children_parents, out_path, "children",        data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_children_p6,      out_path, "children_p6",     data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_parents_p6,       out_path, "parents_p6",      data_type = "questionnaires", metadata_info = quest_info)

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

# Falsely named datasets -----------------------------
psytool_info_adults$id <- suppressWarnings(as.integer(psytool_info_adults$id))
psytool_info_adults <- psytool_info_adults %>%
  group_by(id) %>%
  mutate(
    id = dplyr::case_when(
      id == 30048L & p == 3 & TIME_start == max(TIME_start) ~ 30047L,
      id == 30058L & p == 3 & TIME_start == max(TIME_start) ~ 30057L,
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
adult_paths      <- separate_by_project(psytool_info_adults,      cogtest_out_path, "adults",
                                        data_type = "experiment_data", metadata_info = cogtest_info)
children_paths   <- separate_by_project(psytool_info_children,    cogtest_out_path, "children_parents",
                                        data_type = "experiment_data", metadata_info = cogtest_info)


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


path_components     <- unlist(strsplit(adult_paths[1], .Platform$file.sep))
path_length         <- length(path_components)
all_path_components <- unlist(strsplit(adult_paths, .Platform$file.sep))

# given: all_path_components (character vector), path_length (integer)
n <- floor(length(all_path_components) / path_length)
stopifnot(n > 0)

# reshape components into rows of length `path_length`
m <- matrix(all_path_components[seq_len(n * path_length)], nrow = n, byrow = TRUE)

# everything except the last element of each row
proj_folders <- m[, 1:(path_length - 1), drop = FALSE]

# first character of the (path_length-1)-th element in each row
proj_numbers <- substr(m[, path_length - 1], 1, 1)

# 1) Collapse each row of components into a full folder path
project_folder <- apply(proj_folders, 1, function(parts) do.call(file.path, as.list(parts)))

# 2) Make sure the first-digit keys are character
proj_keys <- as.character(proj_numbers)

# 3) Build the named character vector for logger$split()
dest_dirs <- stats::setNames(project_folder, proj_keys)

# Step 4: remove existing .log files if they exist
for (dir in dest_dirs) {
  if (dir.exists(dir)) {
    log_files <- list.files(dir, pattern = "\\.log$", full.names = TRUE)
    if (length(log_files) > 0) {
      message("Removing existing log files in: ", dir)
      file.remove(log_files)
    }
  }
}

logger$split(dest_dirs)

# 4️⃣ Close when done
logger$close()

## Get the Experimental Data Sets Associated with the project ------------------

copy_psytool_files(cogtest_out_path = out_path, meta_env_name = "cogtest_info")

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
  id_col = "vpid",
  project_col = "project",
  data_type = "questionnaire"
)

collect_ids_to_excel(
  meta_data = quest_info,
  dat_children_p6,
  dat_parents_p6,
  id_col = "VPCode",
  project_col = NULL,
  data_type = "questionnaire_p6"
)

collect_ids_to_excel(
  meta_data = cogtest_info,
  psytool_info_adults,
  psytool_info_adolescents,
  psytool_info_children,
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



## ---- Combine data, log-transform, 2 SD rule on log scale, and plot ----

# 1) Bind all datasets together while keeping dataset name
all_dt <- do.call(
  rbind,
  lapply(names(datasets), function(nm) {
    df <- datasets[[nm]]
    needed <- intersect(c("vpid", "interviewtime"), names(df))
    if (length(needed) < 2) {
      stop(sprintf("Dataset '%s' is missing 'vpid' or 'interviewtime'.", nm))
    }
    out <- df[needed]
    out$.dataset <- nm
    out
  })
)

# Coerce interviewtime to numeric
all_dt$interviewtime <- suppressWarnings(as.numeric(all_dt$interviewtime))

# Keep only positive times for log transform; warn about drops
valid_idx <- !is.na(all_dt$interviewtime) & all_dt$interviewtime > 0
if (!all(valid_idx)) {
  message(sprintf(
    "Excluding %d rows with non-positive or missing interviewtime before log-transform.",
    sum(!valid_idx)
  ))
}
dt_valid <- all_dt[valid_idx, , drop = FALSE]

# 2) Compute mean and SD on the log scale (natural log)
log_times <- log(dt_valid$interviewtime)
log_mean  <- mean(log_times)
log_sd    <- sd(log_times)

# Cutoff on log scale and back-transform to seconds
cutoff_log     <- log_mean - 2 * log_sd
center_seconds <- exp(log_mean)
cutoff_seconds <- exp(cutoff_log)

cat(sprintf("Mean (log scale): %.4f\n", log_mean))
cat(sprintf("SD (log scale): %.4f\n", log_sd))
cat(sprintf("Cutoff (log): mean - 2*SD = %.4f\n", cutoff_log))
cat(sprintf("Center on original scale (exp(mean_log)) = %.2f s\n", center_seconds))
cat(sprintf("2 SD cutoff on original scale (exp(mean_log - 2*SD)) = %.2f s\n", cutoff_seconds))

# 3) Flag participants: log(interviewtime) < log_mean - 2*SD
dt_valid$log_interviewtime <- log_times
flagged <- subset(
  dt_valid,
  log_interviewtime < cutoff_log
)[, c(".dataset", "vpid", "interviewtime")]

flagged <- flagged[order(flagged$interviewtime), ]

if (nrow(flagged) == 0) {
  message("No participants found below the 2 SD cutoff on the log scale.")
} else {
  message("Participants more than 2 SD below the mean on the log scale:")
  print(flagged, row.names = FALSE)
}

# ---- 4a) Histogram on original seconds scale ----
op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)

hist(
  dt_valid$interviewtime,
  breaks = "FD",
  main   = "Interview Time (seconds) — Log-scale 2 SD Rule",
  xlab   = "Interview time (seconds)"
)
abline(v = center_seconds, lwd = 2)        # exp(mean_log)
abline(v = cutoff_seconds, lwd = 2, lty = 2)  # exp(mean_log - 2*SD_log)

legend("topright",
       legend = c(
         sprintf("exp(mean_log) = %.2f s", center_seconds),
         sprintf("Cutoff = %.2f s", cutoff_seconds)
       ),
       lwd = c(2, 2), lty = c(1, 2), bty = "n")

if (nrow(flagged) > 0) {
  rug(flagged$interviewtime, ticksize = 0.05)
}

# ---- 4b) Histogram on log-transformed scale ----
hist(
  log_times,
  breaks = "FD",
  main   = "Log(Interview Time) — 2 SD Rule",
  xlab   = "log(Interview time)"
)
abline(v = log_mean, lwd = 2)       # mean on log scale
abline(v = cutoff_log, lwd = 2, lty = 2)  # mean - 2*SD cutoff

legend("topright",
       legend = c(
         sprintf("Mean(log) = %.3f", log_mean),
         sprintf("Cutoff(log) = %.3f", cutoff_log)
       ),
       lwd = c(2, 2), lty = c(1, 2), bty = "n")

rug(log(flagged$interviewtime), ticksize = 0.05)

# ---- 5) Package results ----
results <- list(
  log_mean = log_mean,
  log_sd = log_sd,
  cutoff_log = cutoff_log,
  center_seconds = center_seconds,
  cutoff_seconds = cutoff_seconds,
  flagged_participants = flagged,
  combined_data_valid = dt_valid
)

# Inspect:
# results$flagged_participants
