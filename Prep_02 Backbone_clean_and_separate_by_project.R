#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: Backbone Separate Data by Project 
# Author: Saskia Wilken (saskia.wilken@uni-hamburg.de) & Antonia Bott (antonia.bott@uni-hamburg.de)
# 2025-08-08 (Date initially edited by SW)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script 
# (1) reads questionnaire data exported from LimeSurvey and PsyToolkit

# (2) Fixes Issues with VP ID assignment

# (3) Creates project-specific data files in the environment as well as on disk


# clean up R environment
rm(list=ls())
cat("\014")

# install packages
if(!require("dplyr")){install.packages("dplyr")};library(dplyr)
if(!require("tidyr")){install.packages("tidyr")};library(tidyr)
if(!require("writexl")){install.packages("writexl")};library(writexl)
if(!require("openxlsx")){install.packages("openxlsx")};library(openxlsx)


# Esure proper number display -------------------------------------------------

options(scipen = 999)  # disable scientific notation globally

# Get Today ----------------------------------------------------------------

today <- format(Sys.Date(), "%Y-%m-%d")

## Set working directory -------------------------------------------------------

whoami<- Sys.info()[[4]]; print(whoami)

# if you are not in this list add your computer here

switch(whoami,
       "KOPSY-D-033080" = {name <- "K:/vol2011/bba3488/";
       path <- "FOR/"},
       "UN-LAP-015977" = {name <- dirname(rstudioapi::getSourceEditorContext()$path);
       }
)

setwd(name);
in_path = file.path("raw_data");
out_path = file.path("01_project_data");
survey_out_path = file.path(out_path, "survey_data");
function_path = file.path("functions");
psytool_path = file.path("raw_data", "psytoolkit");
cogtest_out_path = file.path(out_path, "experiment_data");
# Specify the folder path
discarded_path <- file.path(out_path, "discarded")

# Check if the folder exists
if (!dir.exists(discarded_path)) {
  dir.create(discarded_path, recursive = TRUE)
}


## Setup Logging ---------------------------------------------------------------

create_logger <- function(log_path,
                          append = TRUE,
                          with_timestamp = TRUE,
                          enforce_code = TRUE) {
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  con <- file(log_path, open = if (append) "a" else "w", blocking = TRUE)
  
  write_line <- function(text) {
    if (enforce_code && !grepl("\\b\\d{5}\\b", text)) {
      stop("Each log line must contain a 5-digit code (e.g., 01234). Found: ", text)
    }
    ts <- if (with_timestamp) paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ") else ""
    line <- paste0(ts, text)
    writeLines(line, con = con)
    flush(con)
    invisible(line)
  }
  
  split_into_sublogs <- function(dest_map,
                                 pattern = "\\b\\d{5}\\b",
                                 file_namer = function(d) sprintf("sublog_%s.log", d),
                                 overwrite = TRUE) {
    if (is.null(names(dest_map)) || any(!names(dest_map) %in% as.character(0:9))) {
      stop("dest_map must be a named vector/list with names '0'..'9'")
    }
    if (!file.exists(log_path)) stop("Log file not found: ", log_path)
    
    lines <- readLines(log_path, warn = FALSE)
    buckets <- setNames(vector("list", 10), as.character(0:9))
    
    for (ln in lines) {
      m <- regexpr(pattern, ln)
      if (m[1] > 0) {
        code <- substr(ln, m[1], m[1] + attr(m, "match.length") - 1)
        first_digit <- substr(code, 1, 1)
        if (!is.null(dest_map[[first_digit]])) {
          buckets[[first_digit]] <- c(buckets[[first_digit]], ln)
        }
      }
    }
    
    out_paths <- list()
    for (d in names(dest_map)) {
      shard_lines <- buckets[[d]]
      if (length(shard_lines)) {
        dir.create(dest_map[[d]], recursive = TRUE, showWarnings = FALSE)
        out_file <- file.path(dest_map[[d]], file_namer(d))
        if (file.exists(out_file) && !overwrite) stop("File exists: ", out_file)
        writeLines(shard_lines, out_file)
        out_paths[[d]] <- out_file
      }
    }
    invisible(out_paths)
  }
  
  close_logger <- function() close(con)
  
  list(
    path = normalizePath(log_path, mustWork = FALSE),
    write = write_line,
    split = split_into_sublogs,
    close = close_logger
  )
}



# 1️⃣ Create logger
logger <- create_logger("logs/all_action_points.log")


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
source(file.path(function_path, "remove_empty_obs_psytoolkit.R"))
source(file.path(function_path, "collect_ids_to_excel.R"))
source(file.path(function_path, "move_old_backbones.R"))


## Move old Data ---------------------------------------------------------------

move_old_backbones(out_path, dry_run = FALSE)  


## Backbone surveys ------------------------------------------------------------

file_adults <- "results-survey564757_remids_translated.csv";
file_adolescents <- "results-survey585676.csv";
file_children_parents <- "results-survey798916_remids_translated.csv";
file_parents_p6 <- "results-survey191355.csv";
file_children_p6 <- "results-survey518972.csv";


## Load data -------------------------------------------------------------------

# Questionnaires
dat_adults <- read.csv(file.path(name,in_path, file_adults), sep = ";");
dat_adolescents <- read.csv(file.path(name,in_path, file_adolescents), sep = ";");
dat_children_parents <- read.csv(file.path(name,in_path, file_children_parents), sep = ";");
dat_parents_p6 <- read.csv(file.path(name,in_path, file_parents_p6), sep = ";");
dat_children_p6 <- read.csv(file.path(name,in_path, file_children_p6), sep = ";");

# get metadata
quest_info <- file.info(file.path(name,in_path, file_adults));
quest_info$sample = "adults";
quest_info[2, ] <- c(file.info(file.path(name,in_path, file_adolescents)), "adolescents");
quest_info[3, ] <- c(file.info(file.path(name,in_path, file_children_parents)), "children_parents");
quest_info[4, ] <- c(file.info(file.path(name,in_path, file_parents_p6)), "parents_p6");
quest_info[5, ] <- c(file.info(file.path(name,in_path, file_children_p6)), "children_p6");


# Data Overview
file_general <- "results-survey415148.csv"
dat_general <- read.csv(file.path(name,in_path, file_general), sep = ";")


# Psytoolkit Tests
file_psytool_info = "data.csv";

psytool_info_adults <- read.csv(file.path(name, psytool_path, "adults", file_psytool_info))
psytool_info_children <- read.csv(file.path(name, psytool_path, "children", file_psytool_info))
#psytool_info_adults_remote <- read.csv(file.path(name, psytool_path, "adults_remote", file_psytool_info))
# seems unimportant, only test data. 
psytool_info_adolescents <- read.csv(file.path(name, psytool_path, "adolescents", file_psytool_info))


# get metadata
cogtest_info <- file.info(file.path(name, psytool_path, "adults", file_psytool_info));
cogtest_info$sample = "adults";
cogtest_info[2, ] <- c(file.info(file.path(name, psytool_path, "adolescents", file_psytool_info)), "adolescents");
cogtest_info[3, ] <- c(file.info(file.path(name, psytool_path, "children", file_psytool_info)), "children_parents");


# remove Test Datasets from all Project data -----------------------------------

dat_adults <- remove_test_rows(dat_adults, "Adults", dat_general)
dat_adolescents <- remove_test_rows(dat_adolescents, "Adolescents", dat_general)
dat_children_parents <- remove_test_rows(dat_children_parents, "Children", dat_general)
dat_parents_p6 <- remove_test_rows(dat_parents_p6, "Children", dat_general)
dat_children_p6 <- remove_test_rows(dat_children_p6, "Children", dat_general)


psytool_info_adults <- remove_test_rows(psytool_info_adults, "Adults", dat_general)
psytool_info_adolescents <- remove_test_rows(psytool_info_adolescents, "Adolescents", dat_general)
psytool_info_children <- remove_test_rows(psytool_info_children, "Children", dat_general)


################################################################################
## Data Cleaning for Questionnaire Data ----------------------------------------
################################################################################

# Set column name variables ----------------------------------------------------

vp_col <- "vpid"
project_col <- "project"
last_page <- "lastpage"
link_col <- "comp"
id_col = "id" # careful - in psytoolkit information sheets, this is the vpid, in the 
# questionnaire data this is an unqiue incrementing number counting the datasets
submit_col = "submitdate";


# Fix issues with project assignment -------------------------------------------

dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)];
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)] = 2;

dat_adults[[project_col]][which(dat_adults[[vp_col]] == 99017)];
# assuming this is project 9 since project 1 does not collect data and the id 
# starts with a 9. also project 9 IDs are actually consecutive and 17 is missing. 
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 99017)] = 9;


# Remove empty Rows --------------------------------------------------------

LAST_P_EMPTY = 6;
# empty entries did not get pat page LAST_P_EMPTY. I assume this is a technical issue usually 
# and I remove it. I also remove when only cognitive tests were performed sicne thre is no quest data. 
# I do not remove incomplete entries in this step

# Project 3
PROJECT = 3;
# adult
empty_ad_3 = dat_adults[which(dat_adults[[last_page]] < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT), ];
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & dat_adults[[last_page]] < LAST_P_EMPTY), ];

# Project 6
# these are different questionnaires
LAST_P_EMPTY = 3;
# children
empty_ch_6 = dat_children_p6[which(dat_children_p6[[last_page]] < LAST_P_EMPTY), ];
dat_children_p6 <- dat_children_p6[!(dat_children_p6[[last_page]] < LAST_P_EMPTY), ];
LAST_P_EMPTY = 3;
# parents
empty_p_6 = dat_parents_p6[which(dat_parents_p6[[last_page]] < LAST_P_EMPTY), ];
dat_parents_p6 <- dat_parents_p6[!(dat_parents_p6[[last_page]] < LAST_P_EMPTY), ];

# Project 7
PROJECT = 7;
# adult
empty_ad_7 = dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), ];
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                             (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ];
# adolescent
empty_adlsc_7 = dat_adolescents[which((dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT), ]
dat_adolescents <- dat_adolescents[!(dat_adolescents[[project_col]] == PROJECT &
                                       (dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY)), ];

# Project 8
PROJECT = 8;
# adult
empty_ad_8 = dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), ];
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                            (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ];
# children
empty_ch_8 = dat_children_parents[which((dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY) & dat_children_parents[[project_col]] == PROJECT), ]
dat_children_parents <- dat_children_parents[!(dat_children_parents[[project_col]] == PROJECT &
                             (dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY)), ];

# Project 9
PROJECT = 9;
empty_ad_9 = dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), ];
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ];


# put them into a list
ads <- list(empty_ad_3, empty_ad_7, empty_ad_8, empty_ad_9);

# keep only the non-empty ones
ads_non_empty <- ads[lengths(ads) > 0];

# bind them together (if any left)
all_empty_ad <- if (length(ads_non_empty) > 0) {
  do.call(rbind, ads_non_empty);
} else {
  data.frame()  # return empty df if all were empty
}

# saving it so I can write to disk later on in the script
all_empty_ad$.__reason__. = "empty";
empty_ch_8$.__reason__. = "empty";
empty_adlsc_7$.__reason__. = "empty";


# Fix ID naming issues --------------------------------------------------------

# Project 2
PROJECT = 2;
# wrong entry
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 20035)] = 20036; 
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 4 & dat_adults[[project_col]] == PROJECT)] = 20004;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 6 & dat_adults[[project_col]] == PROJECT)] = 20006;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 15 & dat_adults[[project_col]] == PROJECT)] = 20015;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2023 & dat_adults[[project_col]] == PROJECT)] = 20023;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 26 & dat_adults[[project_col]] == PROJECT)] = 20026;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 35 & dat_adults[[project_col]] == PROJECT)] = 20035;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2041 & dat_adults[[project_col]] == PROJECT)] = 20041;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2044 & dat_adults[[project_col]] == PROJECT)] = 20044;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2046 & dat_adults[[project_col]] == PROJECT)] = 20046;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2048 & dat_adults[[project_col]] == PROJECT)] = 20048;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2051 & dat_adults[[project_col]] == PROJECT)] = 20051;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 2052 & dat_adults[[project_col]] == PROJECT)] = 20052;
# Project 3
PROJECT = 3;
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 1 & dat_adults[[project_col]] == PROJECT)] = 30001;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 3 & dat_adults[[project_col]] == PROJECT)] = 30003;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 8 & dat_adults[[project_col]] == PROJECT)] = 30008;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9 & dat_adults[[project_col]] == PROJECT)] = 30009;

# assuming the wrong initial number was given since the projects in question do not collect data (yet)
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10002 & dat_adults[[project_col]] == PROJECT)] = 30002;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10005 & dat_adults[[project_col]] == PROJECT)] = 30005;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10006 & dat_adults[[project_col]] == PROJECT)] = 30006;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 10007 & dat_adults[[project_col]] == PROJECT)] = 30007;
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 40019 & dat_adults[[project_col]] == PROJECT)] = 30019;

# falsely named datasets
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 227 & dat_adults[[project_col]] == PROJECT)] = 30047;
dat_adults[[vp_col]][which(dat_adults[[id_col]] == 316 & dat_adults[[project_col]] == PROJECT)] = 30057;

# Project 9
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9901)] = 99001;


# Special Case Project 8: Remap VPIDs so children and adults have unqiue IDs -------------

dat_children_parents <- correct_child_vpids(dat_children_parents, 
                                            mapping_file =file.path("information", "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx"));


# Gather Pilot Participant IDs -------------------------------------------------

pilots_ad_auto = find_pilot_ids(dat_general, dat_adults)
pilots_asc_auto = find_pilot_ids(dat_general, dat_adolescents)
pilots_ch_auto = find_pilot_ids(dat_general, dat_children_parents)


pilot_ad_2 = c(20004);
pilot_ad_9 = c();
pilot_ad_8 = c(80350)

pilot_asc_7 = c();

pilot_ch_6 = c(62973,
               62980, 
               62998,
               62992,
               62987,
               62989,
               62994,
               62970
)

pilot_ad_all = c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto);
pilot_asc_all = c(pilots_asc_auto);
pilots_ch_all = c(pilots_ch_auto, pilot_ch_6);

# Move to separate file and from original dataset

dat_adults <- extract_pilot_by_vpid(
  dat_adults,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilot_ad_all,
  sample = "adults",
  vpid_col = "vpid"
);
dat_adolescents <- extract_pilot_by_vpid(
  dat_adolescents,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilot_asc_all,
  sample = "adolescents",
  vpid_col = "vpid"
);
dat_children_parents <- extract_pilot_by_vpid(
  dat_children_parents,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilots_ch_all,
  sample = "children_parents",
  vpid_col = "vpid"
);
dat_children_p6 <- extract_pilot_by_vpid(
  dat_children_p6,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilots_ch_all,
  sample = "children_p6",
  vpid_col = "VPCode"
);
dat_parents_p6 <- extract_pilot_by_vpid(
  dat_parents_p6,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilots_ch_all,
  sample = "parents_p6",
  vpid_col = "VPCode"
);

# Handle duplicate IDs ---------------------------------------------------------

# list of duplicates
# sort(unique(dat_adults[[vp_col]][duplicated(dat_adults[[vp_col]])]))
# sort(unique(dat_adolescents[[vp_col]][duplicated(dat_adolescents[[vp_col]])]))
# sort(unique(dat_children_parents[[vp_col]][duplicated(dat_children_parents[[vp_col]])]))

# Delete not needed, incomplete or faulty datasets 

# Project 3
# using list of "ids" that can be deleted
# these are the entries in the output file, not the vp identifiers. 

del_id_ad = c(59, 80) # Hendrik said they can be deleted as they are incomplete
dat_adults <- dat_adults %>%
  dplyr::filter(!id %in% del_id_ad);

# Project 6
# removing all the datasets that were not recorded on teh same day as the test data for 62128. 
keep_row_id <- dat_children_parents %>%
  mutate(start_dt = as.POSIXct(startdate),
         .row = row_number()) %>%
  filter(vpid == 62128, form == "C") %>%
  arrange(start_dt, .row) %>%
  slice_head(n = 1) %>%
  pull(.row)

dat_children_parents <- dat_children_parents %>%
  mutate(.row = row_number()) %>%
  filter(.row == keep_row_id | !(vpid == 62128 & form == "C")) %>%
  select(-.row)

# Project 8
# removing the newer 80505 P questionnaire, following Johannes' suggestion
dat_children_parents <- dat_children_parents %>%
  mutate(startdate = as.Date(startdate)) %>%
  group_by(vpid, form) %>%
  filter(!(vpid == 80505 & form == "P" & startdate == max(startdate))) %>%
  ungroup()

# auto-remove and check for remaining duplicates 

# Adults
res_adults <- resolve_duplicates(dat_adults, vp_col, submit_col, dataset_name = "adults", data_type = "questionnaire", project_col, logger = logger);
dat_adults <- res_adults$cleaned;
trash_adults <- res_adults$trash_bin;

# [adults] Multiple complete datasets for vpid=80009 — please resolve manually.
# [adults] Multiple complete datasets for vpid=80011 — please resolve manually.
# Leo fragen, waiting for response...

# Adolescents
res_adolescents <- resolve_duplicates(dat_adolescents, vp_col, submit_col, dataset_name = "adolescents", data_type = "questionnaire", project_col, logger = logger);
dat_adolescents <- res_adolescents$cleaned;
trash_adolescents <- res_adolescents$trash_bin;

# [adolescents] Multiple complete datasets for vpid=70076 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70072 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70062 — please resolve manually.
# Waiting for resonse from Ibrahim.... 

# Children/Parents
res_children_parents <- resolve_duplicates(dat_children_parents, vp_col, submit_col, dataset_name = "children_parents", data_type = "questionnaire", project_col, logger = logger);
dat_children_parents <- res_children_parents$cleaned;
trash_children_parents <- res_children_parents$trash_bin;

# Project 6 children parents
vp_col = "VPCode";
res_children_p6 <- resolve_duplicates(dat_children_p6, vp_col, submit_col, dataset_name = "children_p6", data_type = "questionnaire", project_col, 13, logger = logger);
dat_children_p6 <- res_children_p6$cleaned;
trash_children_p6 <- res_children_p6$trash_bin
res_parents_p6 <- resolve_duplicates(dat_parents_p6, vp_col, submit_col, dataset_name = "parents_p6", data_type = "questionnaire", project_col, 13, logger = logger);
dat_parents_p6 <- res_parents_p6$cleaned;
trash_parents_p6 <- res_parents_p6$trash_bin

# Special Case Project 8: Check if all children_parents questionnaire sets have C, P and A entries ----

check_vpid_forms(dat_children_parents)


# Save the Trash just to be safe -----------------------------------------------

# build combined dfs
all_trash_adults       <- rbind(all_empty_ad, trash_adults)
all_trash_children     <- rbind(empty_ch_8, trash_children_parents)
all_trash_adolescents  <- rbind(empty_adlsc_7, trash_adolescents)


# write each one to disk
write_xlsx(all_trash_adults,      file.path(out_path, "discarded", sprintf("deleted-rows_%s_adults.xlsx", today)))
write_xlsx(all_trash_children,    file.path(out_path, "discarded", sprintf("deleted-rows_%s_children.xlsx", today)))
write_xlsx(all_trash_adolescents, file.path(out_path, "discarded", sprintf("deleted-rows_%s_adolescents.xlsx", today)))


# Separate the data by project and store on disk ------------------------------

# Questionnaires
separate_by_project(dat_adults, out_path, "adults", data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_adolescents, out_path, "adolescents", data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_children_parents, out_path, "children", data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_children_p6, out_path, "children_p6", data_type = "questionnaires", metadata_info = quest_info)
separate_by_project(dat_parents_p6, out_path, "parents_p6", data_type = "questionnaires", metadata_info = quest_info)


##########################################################################
## Data Cleaning for Cognitive Test Data ---------------------------------
##########################################################################

# Set column name variables ----------------------------------------------------

vp_col <- "id";
project_col <- "p";
# last_page <- "lastpage";
link_col <- "comp";
id_col = NA; # careful - in psytoolkit information sheets, this is the vpid, in the 
# questionnaire data this is an unqiue incrementing numbercoutning the datasets. 
# deleting it here to avoid confusion - there is also no equivalent in the psytoolkit 
# output
submit_col = "TIME_end";
start_col = "TIME_start";

# Fix issues with project assignment -------------------------------------------

psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)];
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)] = 2;

psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)];
# assuming this is project 9 since project 1 does not collect data and the id 
# starts with a 9. also project 9 IDs are actually consecutive and 17 is missing. 
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)] = 9;


# Remove empty Rows ------------------------------------------------------------


list_output <- remove_empty_obs_psytoolkit(psytool_info_adults)
psytool_info_adults = list_output$kept;    # cleaned dataframe
no_id_ad = list_output$no_id;       # id missing
empty_rows_ad = list_output$empty; # rows that were dropped
# TODO: need to understand how it is possible to generate entries without ID - and possibly reconstruct?

list_output <- remove_empty_obs_psytoolkit(psytool_info_adolescents)
psytool_info_adolescents = list_output$kept;    # cleaned dataframe
no_id_adlsc = list_output$no_id;       # id missing
empty_rows_adlsc = list_output$empty; # rows that were dropped

list_output <- remove_empty_obs_psytoolkit(psytool_info_children)
psytool_info_children = list_output$kept;    # cleaned dataframe
no_id_ch = list_output$no_id;       # id missing
empty_rows_ch = list_output$empty; # rows that were dropped


# Fix ID naming issues ---------------------------------------------------------

# Project 2
PROJECT = 2;
# wrong entry
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 20035)] = 20036; 
# assuming a 0 (or many) 0s are missing
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 4 & psytool_info_adults[[project_col]] == PROJECT)] = 20004;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 6 & psytool_info_adults[[project_col]] == PROJECT)] = 20006;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 15 & psytool_info_adults[[project_col]] == PROJECT)] = 20015;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2023 & psytool_info_adults[[project_col]] == PROJECT)] = 20023;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 26 & psytool_info_adults[[project_col]] == PROJECT)] = 20026;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 35 & psytool_info_adults[[project_col]] == PROJECT)] = 20035;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2041 & psytool_info_adults[[project_col]] == PROJECT)] = 20041;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2044 & psytool_info_adults[[project_col]] == PROJECT)] = 20044;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2046 & psytool_info_adults[[project_col]] == PROJECT)] = 20046;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2048 & psytool_info_adults[[project_col]] == PROJECT)] = 20048;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2051 & psytool_info_adults[[project_col]] == PROJECT)] = 20051;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 2052 & psytool_info_adults[[project_col]] == PROJECT)] = 20052;
# Project 3
PROJECT = 3;
# assuming a 0 (or many) 0s are missing
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 1 & psytool_info_adults[[project_col]] == PROJECT)] = 30001;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 3 & psytool_info_adults[[project_col]] == PROJECT)] = 30003;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 8 & psytool_info_adults[[project_col]] == PROJECT)] = 30008;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 9 & psytool_info_adults[[project_col]] == PROJECT)] = 30009;

# assuming the wrong initial number was given since the projects in question do not collect data (yet)
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10002 & psytool_info_adults[[project_col]] == PROJECT)] = 30002;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10005 & psytool_info_adults[[project_col]] == PROJECT)] = 30005;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10006 & psytool_info_adults[[project_col]] == PROJECT)] = 30006;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 10007 & psytool_info_adults[[project_col]] == PROJECT)] = 30007;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 40019 & psytool_info_adults[[project_col]] == PROJECT)] = 30019;

# falsely named datasets
psytool_info_adults <- psytool_info_adults %>%
  group_by(id) %>%
  mutate(
    id = case_when(
      id == "30048" & p == 3 & TIME_start == max(TIME_start) ~ "30047",
      id == "30058" & p == 3 & TIME_start == max(TIME_start) ~ "30057",
      TRUE ~ id
    )
  ) %>%
  ungroup()



# Project 8
PROJECT = 8;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 219 & psytool_info_adults[[project_col]] == PROJECT)] = 30002;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 800028 & psytool_info_adults[[project_col]] == PROJECT)] = 80028;


# Project 9
# assuming a 0 (or many) 0s are missing
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 9901)] = 99001


# Gather Pilot Participant IDs -------------------------------------------------

pilots_ad_auto = find_pilot_ids(dat_general, psytool_info_adults, vpid_col_df2 = vp_col)
pilots_asc_auto = find_pilot_ids(dat_general, psytool_info_adolescents, vpid_col_df2 = vp_col)
pilots_ch_auto = find_pilot_ids(dat_general, psytool_info_children, vpid_col_df2 = vp_col)


pilot_ad_2 = c(20004);
pilot_ad_9 = c();
pilot_ad_8 = c(80350)

pilot_asc_7 = c();

pilot_ch_6 = c(62973,
               62980, 
               62998,
               62992,
               62987,
               62989,
               62994,
               62970
)

pilot_ad_all = c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto);
pilot_asc_all = c(pilots_asc_auto);
pilots_ch_all = c(pilots_ch_auto, pilot_ch_6);

# Move to separate file and from original dataset

psytool_info_adults <- extract_pilot_by_vpid(
  psytool_info_adults,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilot_ad_all,
  sample = "psytool_adults",
  vpid_col = vp_col
);
psytool_info_adolescents <- extract_pilot_by_vpid(
  psytool_info_adolescents,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilot_asc_all,
  sample = "psytool_adolescents",
  vpid_col = vp_col
);
psytool_info_children <- extract_pilot_by_vpid(
  psytool_info_children,
  out_path = file.path(out_path, "pilots"),
  export_csv = FALSE,
  pilot_ids = pilots_ch_all,
  sample = "psytool_children",
  vpid_col = vp_col
);


# Handle duplicate IDs ---------------------------------------------------------

# Delete not needed, incomplete or faulty datasets -----------------------------

# Project 3
# Hendrik said they can be deleted 
psytool_info_adults <- psytool_info_adults %>%
  # Step 2: for id 30009, keep only the latest TIME_start
  group_by(.data[[vp_col]]) %>%
  filter(!(.data[[vp_col]] == 30009 & .data[[start_col]] != max(.data[[start_col]]))) %>%
  ungroup()


# Adults
res_adults <- resolve_duplicates(psytool_info_adults, vp_col, submit_col, dataset_name = "adults", data_type = "experiment_data", project_col, logger = logger);
psytool_info_adults <- res_adults$cleaned;
trash_adults <- res_adults$trash_bin;
# ⚠️ [adults] Multiple complete datasets for id=30099 — please resolve manually.


# Adolescents
res_adolescents <- resolve_duplicates(psytool_info_adolescents, vp_col, submit_col, dataset_name = "adolescents", data_type = "experiment_data", project_col, logger = logger);
psytool_info_adolescents <- res_adolescents$cleaned;
trash_adolescents <- res_adolescents$trash_bin;

# Children
res_children <- resolve_duplicates(psytool_info_children, vp_col, submit_col, dataset_name = "children", data_type = "experiment_data", project_col, logger = logger);
psytool_info_children <- res_children$cleaned;
trash_children <- res_children$trash_bin;


# Separate the data by project and store on disk -------------------------------

# Cognitive Tests  
adult_paths = separate_by_project(psytool_info_adults, cogtest_out_path, "adults", data_type = "experiment_data", metadata_info = cogtest_info)
children_paths = separate_by_project(psytool_info_children, cogtest_out_path, "children_parents", data_type = "experiment_data", metadata_info = cogtest_info)
adolescents_paths = separate_by_project(psytool_info_adolescents, cogtest_out_path, "adolescents", data_type = "experiment_data", metadata_info = cogtest_info)
# TODO: cogtest_out_path isn't used for anything, right?

path_components = unlist(strsplit(adult_paths[1], .Platform$file.sep))
path_length = length(path_components);
all_path_components = unlist(strsplit(adult_paths, .Platform$file.sep))

# given: all_path_components (character vector), path_length (integer)

n <- floor(length(all_path_components) / path_length)
stopifnot(n > 0)

# reshape components into rows of length `path_length`
m <- matrix(all_path_components[seq_len(n * path_length)],
            nrow = n, byrow = TRUE)

# everything except the last element of each row
proj_folders <- m[, 1:(path_length - 1), drop = FALSE]

# first character of the (path_length-1)-th element in each row
proj_numbers <- substr(m[, path_length - 1], 1, 1)

# 1) Collapse each row of components into a full folder path
project_folder <- apply(
  proj_folders, 1,
  function(parts) do.call(file.path, as.list(parts))
)

# 2) Make sure the first-digit keys are character
proj_keys <- as.character(proj_numbers)

# 3) Build the named character vector for logger$split()
dest_dirs <- stats::setNames(project_folder, proj_keys)

# Step 4: remove existing .log files if they exist
for (dir in dest_dirs) {
  if (dir.exists(dir)) {
    # find all files ending with .log
    log_files <- list.files(dir, pattern = "\\.log$", full.names = TRUE)
    if (length(log_files) > 0) {
      message("Removing existing log files in: ", dir)
      file.remove(log_files)
    }
  }
}

# 5) Your existing write and split calls
writexl::write_xlsx(psytool_info_adults, file.path(getwd(), paste0("test", ".xlsx")))
logger$split(dest_dirs)

# 4️⃣ Close when done
logger$close()


## Get the Experimental Data Sets Associated with the project ------------------

copy_psytool_files(cogtest_out_path = out_path, meta_env_name = "cogtest_info")


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
# TODO: why is there a project 3 observation in children_parents?

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
# TODO: find out why so many adults are missing from the cognitive tests

