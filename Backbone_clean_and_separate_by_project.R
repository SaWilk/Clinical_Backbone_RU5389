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

# Esure proper number display -------------------------------------------------

options(scipen = 999)  # disable scientific notation globally


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


## Source required functions --------------------

source(file.path(function_path, "separate_by_project.R"))
# source(file.path(function_path, "separate_by_project_cog.R"))
source(file.path(function_path, "remove_test_rows.R"))
source(file.path(function_path, "copy_psytool_files.R"))
source(file.path(function_path, "extract_pilot_by_vpid.R"))
source(file.path(function_path, "resolve_duplicates.R"))
source(file.path(function_path, "correct_child_vpids.R"))
source(file.path(function_path, "check_vpid_forms.R"))


## Backbone surveys ------------------------------------------------------------

#file_adults <-"results_adults_07042025.csv" # SW: not sure why this ID, the survey has a different ID
file_adults <- "results-survey564757_remids_translated.csv"
file_adolescents <- "results-survey585676.csv"
file_children_parents <- "results-survey798916_remids_translated.csv"


## Load data -------------------------------------------------------------------

# Questionnaires
dat_adults <- read.csv(file.path(name,in_path, file_adults), sep = ";")
dat_adolescents <- read.csv(file.path(name,in_path, file_adolescents), sep = ";")
dat_children_parents <- read.csv(file.path(name,in_path, file_children_parents), sep = ";")

names(dat_adults)[1:15]


# Data Overview
file_general <- "results-survey415148.csv"
dat_general <- read.csv(file.path(name,in_path, file_general), sep = ";")

names(dat_general)[1:15]


# Psytoolkit Tests
file_psytool_info = "data.csv";

psytool_info_adults <- read.csv(file.path(name, psytool_path, "adults", file_psytool_info))
psytool_info_children <- read.csv(file.path(name, psytool_path, "children", file_psytool_info))
psytool_info_adults_remote <- read.csv(file.path(name, psytool_path, "adults_remote", file_psytool_info))
# seems unimportant, only test data. 
psytool_info_adolescents <- read.csv(file.path(name, psytool_path, "adolescents", file_psytool_info))


# remove Test Datasets from all Project data -----------------------------------


dat_adults <- remove_test_rows(dat_adults, "Adults")
dat_adolescents <- remove_test_rows(dat_adolescents, "Adolescents")
dat_children_parents <- remove_test_rows(dat_children_parents, "Children")

psytool_info_adults <- remove_test_rows(psytool_info_adults, "Adults")
psytool_info_adolescents <- remove_test_rows(psytool_info_adolescents, "Adolescents")
psytool_info_children <- remove_test_rows(psytool_info_children, "Children")


##########################################################################
## Data Cleaning for Questionnaire Data ----------------------------------
##########################################################################

# Set column name variables ----------------------------------------------------

vp_col <- "vpid"
project_col <- "project"
last_page <- "lastpage"
link_col <- "comp"
id_col = "id" # careful - in psytoolkit information sheets, this is the vpid, in the 
# questionnaire data this is an unqiue incrementing numbercoutning the datasets


# Fix issues with project assignment -------------------------------------------

dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)]
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)] = 2;

dat_adults[[project_col]][which(dat_adults[[vp_col]] == 99017)]
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
sum((dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY) 
    & dat_children_parents[[project_col]] == PROJECT, na.rm = TRUE)
empty_ch_3 = dat_children_parents[which(
  (dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY) 
  & dat_children_parents[[project_col]] == PROJECT), vp_col]
# 30017
dat_children_parents <- dat_children_parents[!(dat_children_parents[[project_col]] == PROJECT & 
                                 (dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY)), ];
# child
sum(dat_adults$last_page < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_3 = dat_adults[which(dat_adults[[last_page]] < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT), vp_col]
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & dat_adults[[last_page]] < LAST_P_EMPTY), ];

# Project 7
PROJECT = 7;
# adult
sum((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_7 = dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                             (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ];
# adolescent
sum((dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT, na.rm = TRUE)
empty_adlsc_7 = dat_adolescents[which((dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT), vp_col]
#  79019  77001  77001  77001  78050  70002  70008  70002  70016  70015  70017  70023  70027  70026  70034  70039  70038  70033
#  70031  70042  70044  70045  70037  70032  70036  70047  70046  70048  70043  70050  70051  70052  70050  70063  70053  70068
#  70066  70057  70069  70075  70064  70065  70022  70077  70076  70058  70073  70078  70070  70056  70074  70072  70071  70084
#  70062  70088  70085  70090  70086  70089  70093  70092  70100  70098  70099
dat_adolescents <- dat_adolescents[!(dat_adolescents[[project_col]] == PROJECT &
                                       (dat_adolescents[[link_col]] == "cogn" | dat_adolescents[[last_page]] < LAST_P_EMPTY)), ];

# Project 8
PROJECT = 8;
# adult
sum((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_8 = dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
 # 79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT &
                            (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ];
# children
sum((dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY) & dat_children_parents[[project_col]] == PROJECT, na.rm = TRUE)
empty_ch_8 = dat_children_parents[which((dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY) & dat_children_parents[[project_col]] == PROJECT), vp_col]
# 79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
dat_children_parents <- dat_children_parents[!(dat_children_parents[[project_col]] == PROJECT &
                             (dat_children_parents[[link_col]] == "cogn" | dat_children_parents[[last_page]] < LAST_P_EMPTY)), ];

# Project 9
PROJECT = 9;
sum((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_9 = dat_adults[which((dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  99003 99020 99009 99021 99027 99023 99006 99010 99025 99025 99007 99024 99018 99012 99037 99034 99036
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & (dat_adults[[link_col]] == "cogn" | dat_adults[[last_page]] < LAST_P_EMPTY)), ];


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
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9901)] = 99001


# Special Case Project 8: Remap VPIDs so children and adults have unqiue IDs -------------

dat_children_parents <- correct_child_vpids(dat_children_parents)


# Handle duplicate IDs ---------------------------------------------------------

# list of duplicates
# sort(unique(dat_adults[[vp_col]][duplicated(dat_adults[[vp_col]])]))
# sort(unique(dat_adolescents[[vp_col]][duplicated(dat_adolescents[[vp_col]])]))
# sort(unique(dat_children_parents[[vp_col]][duplicated(dat_children_parents[[vp_col]])]))

# Delete not needed, incomplete or faulty datasets 

# using list of "ids" that can be deleted
# these are the entries in the output file, not the vp identifiers. 

del_id_ad = c(59, 80) # Hendrik Heinbockel said they can be deleted as they are incomplete
dat_adults <- dat_adults %>%
  dplyr::filter(!id %in% del_id_ad)


# --- Example usage with your three datasets ---
# Assume vp_col is a string like "vp_id"
# Adults
res_adults <- resolve_duplicates(dat_adults, vp_col, dataset_name = "adults")
dat_adults <- res_adults$cleaned
trash_adults <- res_adults$trash_bin

# [adults] Multiple complete datasets for vpid=80009 — please resolve manually.
# [adults] Multiple complete datasets for vpid=80011 — please resolve manually.
# [adults] Multiple complete datasets for vpid=99001 — please resolve manually.

# Adolescents
res_adolescents <- resolve_duplicates(dat_adolescents, vp_col, dataset_name = "adolescents")
dat_adolescents <- res_adolescents$cleaned
trash_adolescents <- res_adolescents$trash_bin

# [adolescents] Multiple complete datasets for vpid=70076 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70072 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70084 — please resolve manually.
# [adolescents] Multiple complete datasets for vpid=70062 — please resolve manually.
# Waiting for resonse from Ibrahim.... 

# Children/Parents
res_children_parents <- resolve_duplicates(dat_children_parents, vp_col, dataset_name = "children_parents")
dat_children_parents <- res_children_parents$cleaned
trash_children_parents <- res_children_parents$trash_bin

# [children_parents] Multiple incomplete datasets for vpid=62128, form=C — please resolve manually.
# [children_parents] Multiple complete datasets for vpid=80505, form=P — please resolve manually.

# Special Case Project 8: Check if all children_parents questionnaire sets have C, P and A entries ----

check_vpid_forms(dat_children_parents)



#  One combined trash bin for easy cross-checking:
# TODO: save as extra variable (like pilots) and also include empty rows


# Gather Pilot Participant IDs WIP -------------------------------------------------

# TODO: use the general_info to obtain the info which type of data collection was performed. 

pilot_ad_2 = c(20002, 20001, 20003, 20004);
pilot_ad_9 = c();
pilot_ad_7 = c(79001, 79002, 79003, 79004, 79005, 79006, 79007, 79008, 79009, 79010, 79011, 79012, 79013, 79014, 79015, 79016);

pilot_asc_7 = c(79019, 77001);

pilot_ad_all = c(pilot_ad_2, pilot_ad_7, pilot_ad_9);
pilot_asc_all = c(pilot_asc_7);

# Move to separate file and from original dataset

dat_adults <- extract_pilot_by_vpid(
  dat_adults,
  out_path = out_path,
  export_csv = FALSE,
  pilot_ids = pilot_ad_all,
  sample = "adults",
  vpid_col = "vpid"
);
psytool_info_adults <- extract_pilot_by_vpid(
  psytool_info_adults,
  out_path = cogtest_out_path,
  export_csv = FALSE,
  pilot_ids = pilot_ad_all,
  sample = "adults",
  vpid_col = "id"
);
dat_adolescents <- extract_pilot_by_vpid(
  dat_adolescents,
  out_path = out_path,
  export_csv = FALSE,
  pilot_ids = pilot_asc_all,
  sample = "adolescents",
  vpid_col = "vpid"
);


# Separate the data by project and store on disk ------------------------------

# Questionnaires
separate_by_project(dat_adults, out_path, "adults")
separate_by_project(dat_adolescents, out_path, "adolescents")
separate_by_project(dat_children_parents, out_path, "children")


##########################################################################
## Data Cleaning for Cognitive Test Data ---------------------------------
##########################################################################

# Set column name variables ----------------------------------------------------

vp_col <- "id"
project_col <- "p"
# last_page <- "lastpage"
link_col <- "comp"
id_col = "id" # careful - in psytoolkit information sheets, this is the vpid, in the 
# questionnaire data this is an unqiue incrementing numbercoutning the datasets

# Fix issues with project assignment -------------------------------------------

psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)]
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)] = 2;

psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)]
# assuming this is project 9 since project 1 does not collect data and the id 
# starts with a 9. also project 9 IDs are actually consecutive and 17 is missing. 
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)] = 9;


# Remove empty Rows --------------------------------------------------------

LAST_P_EMPTY = 6;
# empty entries did not get pat page LAST_P_EMPTY. I assume this is a technical issue usually 
# and I remove it. I also remove when only cognitive tests were performed sicne thre is no quest data. 
# I do not remove incomplete entries in this step

# Project 3
PROJECT = 3;
# adult
sum((psytool_info_children[[link_col]] == "cogn" | psytool_info_children[[last_page]] < LAST_P_EMPTY) 
    & psytool_info_children[[project_col]] == PROJECT, na.rm = TRUE)
empty_ch_3 = psytool_info_children[which(
  (psytool_info_children[[link_col]] == "cogn" | psytool_info_children[[last_page]] < LAST_P_EMPTY) 
  & psytool_info_children[[project_col]] == PROJECT), vp_col]
# 30017
psytool_info_children <- psytool_info_children[!(psytool_info_children[[project_col]] == PROJECT & 
                                                 (psytool_info_children[[link_col]] == "cogn" | psytool_info_children[[last_page]] < LAST_P_EMPTY)), ];
# child
sum(psytool_info_adults$last_page < LAST_P_EMPTY & psytool_info_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_3 = psytool_info_adults[which(psytool_info_adults[[last_page]] < LAST_P_EMPTY & psytool_info_adults[[project_col]] == PROJECT), vp_col]
psytool_info_adults <- psytool_info_adults[!(psytool_info_adults[[project_col]] == PROJECT & psytool_info_adults[[last_page]] < LAST_P_EMPTY), ];

# Project 7
PROJECT = 7;
# adult
sum((psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY) & psytool_info_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_7 = psytool_info_adults[which((psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY) & psytool_info_adults[[project_col]] == PROJECT), vp_col]
#  79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
psytool_info_adults <- psytool_info_adults[!(psytool_info_adults[[project_col]] == PROJECT & 
                             (psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY)), ];
# adolescent
sum((psytool_info_adolescents[[link_col]] == "cogn" | psytool_info_adolescents[[last_page]] < LAST_P_EMPTY) & psytool_info_adolescents[[project_col]] == PROJECT, na.rm = TRUE)
empty_adlsc_7 = psytool_info_adolescents[which((psytool_info_adolescents[[link_col]] == "cogn" | psytool_info_adolescents[[last_page]] < LAST_P_EMPTY) & psytool_info_adolescents[[project_col]] == PROJECT), vp_col]
#  79019  77001  77001  77001  78050  70002  70008  70002  70016  70015  70017  70023  70027  70026  70034  70039  70038  70033
#  70031  70042  70044  70045  70037  70032  70036  70047  70046  70048  70043  70050  70051  70052  70050  70063  70053  70068
#  70066  70057  70069  70075  70064  70065  70022  70077  70076  70058  70073  70078  70070  70056  70074  70072  70071  70084
#  70062  70088  70085  70090  70086  70089  70093  70092  70100  70098  70099
psytool_info_adolescents <- psytool_info_adolescents[!(psytool_info_adolescents[[project_col]] == PROJECT & 
                                       (psytool_info_adolescents[[link_col]] == "cogn" | psytool_info_adolescents[[last_page]] < LAST_P_EMPTY)), ];

# Project 8
# PROJECT = 8;
# adult
sum((psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY) & psytool_info_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_8 = psytool_info_adults[which((psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY) & psytool_info_adults[[project_col]] == PROJECT), vp_col]
# 79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
psytool_info_adults <- psytool_info_adults[!(psytool_info_adults[[project_col]] == PROJECT &
                             (psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY)), ];


# Project 9
PROJECT = 9;
sum((psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY) & psytool_info_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_9 = psytool_info_adults[which((psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY) & psytool_info_adults[[project_col]] == PROJECT), vp_col]
#  99003 99020 99009 99021 99027 99023 99006 99010 99025 99025 99007 99024 99018 99012 99037 99034 99036
psytool_info_adults <- psytool_info_adults[!(psytool_info_adults[[project_col]] == PROJECT & (psytool_info_adults[[link_col]] == "cogn" | psytool_info_adults[[last_page]] < LAST_P_EMPTY)), ];


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

# Project 9
# assuming a 0 (or many) 0s are missing
psytool_info_adults[[vp_col]][which(psytool_info_adults[[vp_col]] == 9901)] = 99001


# Handle duplicate IDs ---------------------------------------------------------

id_col = "id";

# list of duplicates
sort(unique(psytool_info_adults[[vp_col]][duplicated(psytool_info_adults[[vp_col]])]))
sort(unique(psytool_info_adults[["id"]][duplicated(psytool_info_adults[["id"]])]))


sort(unique(psytool_info_adolescents[[vp_col]][duplicated(psytool_info_adolescents[[vp_col]])]))
sort(unique(psytool_info_children[[vp_col]][duplicated(psytool_info_children[[vp_col]])]))

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
separate_by_project(psytool_info_adults, cogtest_out_path)
# separate_by_project(psytool_info_adults_remote, cogtest_out_path)
separate_by_project(psytool_info_children, cogtest_out_path)
separate_by_project(psytool_info_adolescents, cogtest_out_path)


## Get the Experimental Data Sets Associated with the project ------------------


copy_psytool_files()


