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
if(!require("writexl")){install.packages("writexl")};library(tidyr)
if(!require("openxlsx")){install.packages("openxlsx")};library(openxlsx)


# Esure proper number display -------------------------------------------------

options(scipen = 999)  # disable scientific notation globally

# Get Today ----------------------------------------------------------------


today <- format(Sys.Date(), "%Y-%m-%d")  # "2025-08-28"

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

## Source required functions --------------------

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


## Backbone surveys ------------------------------------------------------------

file_adults <- "results-survey564757_remids_translated.csv";
file_adolescents <- "results-survey585676.csv";
file_children_parents <- "results-survey798916_remids_translated.csv";
file_parents_p6 <- "results-survey191355.csv";
file_children_p6 <- "results-survey518972.csv";


## Load data -------------------------------------------------------------------

# Questionnaires
dat_adults <- read.csv(file.path(name,in_path, file_adults), sep = ";")
dat_adolescents <- read.csv(file.path(name,in_path, file_adolescents), sep = ";")
dat_children_parents <- read.csv(file.path(name,in_path, file_children_parents), sep = ";")
dat_parents_p6 <- read.csv(file.path(name,in_path, file_parents_p6), sep = ";")
dat_children_p6 <- read.csv(file.path(name,in_path, file_children_p6), sep = ";")


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


# remove Test Datasets from all Project data -----------------------------------


dat_adults <- remove_test_rows(dat_adults, "Adults", dat_general)
dat_adolescents <- remove_test_rows(dat_adolescents, "Adolescents", dat_general)
dat_children_parents <- remove_test_rows(dat_children_parents, "Children", dat_general)
dat_parents_p6 <- remove_test_rows(dat_parents_p6, "Children", dat_general)
dat_children_p6 <- remove_test_rows(dat_children_p6, "Children", dat_general)


psytool_info_adults <- remove_test_rows(psytool_info_adults, "Adults", dat_general)
psytool_info_adolescents <- remove_test_rows(psytool_info_adolescents, "Adolescents", dat_general)
psytool_info_children <- remove_test_rows(psytool_info_children, "Children", dat_general)



##########################################################################
## Data Cleaning for Questionnaire Data ----------------------------------
##########################################################################

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


# Adults
res_adults <- resolve_duplicates(dat_adults, vp_col, submit_col, dataset_name = "adults");
dat_adults <- res_adults$cleaned;
trash_adults <- res_adults$trash_bin;

# [adults] Multiple complete datasets for vpid=80009 — please resolve manually.
# [adults] Multiple complete datasets for vpid=80011 — please resolve manually.
# Leo fragen, waiting for response...

# Adolescents
res_adolescents <- resolve_duplicates(dat_adolescents, vp_col, submit_col, dataset_name = "adolescents");
dat_adolescents <- res_adolescents$cleaned;
trash_adolescents <- res_adolescents$trash_bin;

# [adolescents] Multiple complete datasets for vpid=70076 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70072 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70062 — please resolve manually.
# Waiting for resonse from Ibrahim.... 

# Children/Parents
res_children_parents <- resolve_duplicates(dat_children_parents, vp_col, submit_col, dataset_name = "children_parents");
dat_children_parents <- res_children_parents$cleaned;
trash_children_parents <- res_children_parents$trash_bin;

# [children_parents] Multiple incomplete datasets for vpid=62128, form=C — please resolve manually.
# TODO: Harry fragen
# [children_parents] Multiple complete datasets for vpid=80505, form=P — please resolve manually.
# Waiting for response from Johannes
# TODO: remove the newer 80505

# Project 6 children parents
vp_col = "VPCode";
res_children_p6 <- resolve_duplicates(dat_children_p6, vp_col, dataset_name = "children_p6");
dat_children_p6 <- res_children_p6$cleaned;
trash_children_p6 <- res_children_p6$trash_bin
res_parents_p6 <- resolve_duplicates(dat_parents_p6, vp_col, dataset_name = "children_p6");
dat_parents_p6 <- res_parents_p6$cleaned;
trash_parents_p6 <- res_parents_p6$trash_bin

# Special Case Project 8: Check if all children_parents questionnaire sets have C, P and A entries ----

check_vpid_forms(dat_children_parents)

# ⚠️ vpid 80350:
#   - Missing forms: A - PILOT
# ⚠️ vpid 80505:
#   - Duplicate forms: P
# ⚠️ vpid 80516:
#   - Missing forms: A
# might simply not yet have been recorded. 


# Save the Trash just to be safe ------------------------------------

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
separate_by_project(dat_adults, out_path, "adults", data_type = "questionnaires")
separate_by_project(dat_adolescents, out_path, "adolescents", data_type = "questionnaires")
separate_by_project(dat_children_parents, out_path, "children", data_type = "questionnaires")
separate_by_project(dat_children_p6, out_path, "children_p6", data_type = "questionnaires")
separate_by_project(dat_parents_p6, out_path, "parents_p6", data_type = "questionnaires")


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

# Fix issues with project assignment -------------------------------------------

psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)];
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)] = 2;

psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)];
# assuming this is project 9 since project 1 does not collect data and the id 
# starts with a 9. also project 9 IDs are actually consecutive and 17 is missing. 
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)] = 9;


# Remove empty Rows --------------------------------------------------------


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


# Fix ID naming issues --------------------------------------------------------

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
psytool_info_adults[[vp_col]][which(psytool_info_adults[[id_col]] == 227 & psytool_info_adults[[project_col]] == PROJECT)] = 30047;
psytool_info_adults[[vp_col]][which(psytool_info_adults[[id_col]] == 316 & psytool_info_adults[[project_col]] == PROJECT)] = 30057;

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

# Adults
res_adults <- resolve_duplicates(psytool_info_adults, vp_col, submit_col, dataset_name = "adults");
psytool_info_adults <- res_adults$cleaned;
trash_adults <- res_adults$trash_bin;
# ⚠️ [adults] Multiple complete datasets for id=30009 — please resolve manually.
# ⚠️ [adults] Multiple complete datasets for id=30099 — please resolve manually.
# ⚠️ [adults] Multiple complete datasets for id=30048 — please resolve manually.
# ⚠️ [adults] Multiple complete datasets for id=30058 — please resolve manually.

# Adolescents
res_adolescents <- resolve_duplicates(psytool_info_adolescents, vp_col, submit_col, dataset_name = "adults");
psytool_info_adolescents <- res_adolescents$cleaned;
trash_adolescents <- res_adolescents$trash_bin;

# Children
res_children <- resolve_duplicates(psytool_info_children, vp_col, submit_col, dataset_name = "adults");
psytool_info_children <- res_children$cleaned;
trash_children <- res_children$trash_bin;

names(psytool_info_children)
sum(length(psytool_info_children$p == 6))

# Delete not needed, incomplete or faulty datasets ------------------------------------------
# using list of "ids" that can be deleted
# these are the entries in the output file, not the vp identifiers. 

del_id_ad = c(59, 80) # Hendrik Heinbockel said they can be deleted as they are incomplete
psytool_info_adults <- psytool_info_adults %>%
  dplyr::filter(!id %in% del_id_ad)


# Check if the VP IDs in Psytool (cognitive tests) align with the IDs of the questionnaire data (questionnaires) ------------

all_sub_quest = c(psytool_info_adults$vpid, 
                  psytool_info_children$vpid,
                  psytool_info_adolescents$vpid);

all_sub_quest <- all_sub_quest[!(is.na(all_sub_quest) | all_sub_quest == "")]

idx_vec = which(!(all_sub_quest %in% dat_general$vpid))

no_general_ids = all_sub_quest[idx_vec]
# no IDs inconsistent

all_sub_test = c(psytool_info_adults$id, 
                 psytool_info_children$id,
                 psytool_info_adolescents$id);
all_sub_test <- all_sub_test[!(is.na(all_sub_test) | all_sub_test == "")]

idx_vec_test = which(!(all_sub_test %in% all_sub_quest))
no_quest_ids = all_sub_test[idx_vec_test]


no_quest_ids # <- these have to be fixed - they seem to have no questionnaire data.
# [1] "8008"     "219"      "30019"    "1001_A"   "1001"     "P7_100"   "30055"    "42"       "P7_14"    "P7_100"   "30080"   
# [12] "1001"     "P7"       "42"       "P7_001_A" "1001"     "p8_1001"  "62102"    "62989"    "62006"    "62110"    "62020"   
# [23] "62009"    "62023"    "62998"    "62012"    "62103"    "62003"    "62102"    "62992"    "79999"     


idx_vec_quest = which(!(all_sub_quest %in% all_sub_test ))
no_test_ids = all_sub_quest[idx_vec_quest]

no_test_ids # <- also these - they seem to have no cognitive test data
# [1] "79001"  "79002"  "79003"  "79004"  "79005"  "79006"  "79007"  "79008"  "79009"  "79010"  "79011"  "79012"  "79013"  "79014" 
# [15] "79015"  "50201"  "50001"  "50002"  "50003"  "50004"  "50005"  "50006"  "50007"  "50008"  "50009"  "50010"  "50011"  "50012" 
# [29] "50013"  "50014"  "50015"  "9901"   "99017"  "50202"  "50204"  "70101"  "70101"  "70101"  "2051"   "30086"  "30085"  "{vpid}"
# [43] "30087"  "{vpid}" "62044"  "62124"  "78050"  "70001"  "70001"  "70087"  "70087" 


# Separate the data by project and store on disk ------------------------------

# Cognitive Tests  
separate_by_project(psytool_info_adults, cogtest_out_path, "adults", data_type = "experiment_data")
separate_by_project(psytool_info_children, cogtest_out_path, "children_parents", data_type = "experiment_data")
separate_by_project(psytool_info_adolescents, cogtest_out_path, "adolescents", data_type = "experiment_data")


## Get the Experimental Data Sets Associated with the project ------------------

copy_psytool_files(cogtest_out_path = out_path)

