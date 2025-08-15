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
source(file.path(function_path, "separate_by_project_cog.R"))
source(file.path(function_path, "remove_test_rows.R"))
source(file.path(function_path, "copy_psytool_files.R"))


## Backbone surveys ------------------------------------------------------------

#file_adults <-"results_adults_07042025.csv" # SW: not sure why this ID, the survey has a different ID
file_adults <- "results-survey564757.csv"
file_adolescents <- "results-survey585676.csv"
file_children <- "results-survey798916.csv"


## Load data -------------------------------------------------------------------

# Questionnaires
dat_adults <- read.csv(file.path(name,in_path, file_adults))
dat_adolescents <- read.csv(file.path(name,in_path, file_adolescents))
dat_children <- read.csv(file.path(name,in_path, file_children))


# Data Overview
file_general <- "results-survey415148.csv"
dat_general <- read.csv(file.path(name,in_path, file_general))

dat_general <- remove_test_rows(dat_general, "general")


# Psytoolkit Tests
file_psytool_info = "data.csv";

psytool_info_adults <- read.csv(file.path(name, psytool_path, "adults", file_psytool_info))
psytool_info_children <- read.csv(file.path(name, psytool_path, "children", file_psytool_info))
psytool_info_adults_remote <- read.csv(file.path(name, psytool_path, "adults_remote", file_psytool_info))
psytool_info_adolescents <- read.csv(file.path(name, psytool_path, "adolescents", file_psytool_info))


# Check if the VP IDs in General Info align with the IDs of the questionnaire data ------------

idx_vec = which(!(c(dat_adults$Versuchspersonennummer., 
                    dat_children$Versuchspersonennummer.,
                    dat_adolescents$Versuchspersonennummer.) 
                  %in% dat_general$Versuchspersonen.ID...Participant.ID))
all_sub_quest = c(dat_adults$Versuchspersonennummer., 
            dat_children$Versuchspersonennummer.,
            dat_adolescents$Versuchspersonennummer.);
no_general_ids = all_sub_quest[idx_vec]
no_general_ids = no_general_ids[!is.na(no_general_ids)]
no_general_ids = no_general_ids[!no_general_ids == 99999]
no_general_ids = no_general_ids[!no_general_ids == 99998]
no_general_ids = no_general_ids[!no_general_ids == 999999]
no_general_ids = no_general_ids[!no_general_ids == 123456]
no_general_ids = no_general_ids[!no_general_ids == 11111]
no_general_ids = no_general_ids[!no_general_ids == 89999]

sort(no_general_ids)
# they do not align - these IDs are int eh questionnaire data but not in the general info data
# [1]     4     8     9  2048  9901 20001 20002 20003 20005 62966 62967 62968 62974 62982 62986 62991 62993 62995 62996 62997
# [21] 77001 77001 77001 77001 78050 79001 79002 79003 79004 79005 79006 79007 79008 79009 79010 79011 79012 79013 79014 79015
# [41] 79016 79016 79019 79019 79019 99001 99001 99002 99002 99003 99003 99004 99004 99004 99005 99005 99006 99006 99007 99007
# [61] 99009 99009 99010 99010 99011 99012 99012 99013 99014 99014 99014 99015 99015 99016 99016 99017 99018 99018 99019 99019
# [81] 99020 99020 99021 99021 99022 99022 99023 99023 99023 99024 99024 99025 99025 99025 99027 99027 99028 99028 99029 99029
# [101] 99030 99031 99031 99032 99032 99033 99033 99034 99034 99035 99036 99036 99037 99037
# how is this even possible? 
# but I think there is no need to fix it 

all_sub_test = c(psytool_info_adults$id, 
            psytool_info_children$id,
            psytool_info_adolescents$id);
idx_vec_test = which(!(idx_vec_test %in% all_sub_quest))
no_quest_ids = all_sub_test[idx_vec]
no_quest_ids = no_quest_ids[!no_quest_ids == 99999]
no_quest_ids = no_quest_ids[!no_quest_ids == 99998]
no_quest_ids = no_quest_ids[!no_quest_ids == 999999]
no_quest_ids = no_quest_ids[!no_quest_ids == 123456]
no_quest_ids = no_quest_ids[!no_quest_ids == 11111]
no_quest_ids = no_quest_ids[!no_quest_ids == 89999]
no_quest_ids = no_quest_ids[!no_quest_ids == 9999999]
no_quest_ids = no_quest_ids[!no_quest_ids == 9999]
no_quest_ids = no_quest_ids[!no_quest_ids == 999]
no_quest_ids = no_quest_ids[!no_quest_ids == 0]
no_quest_ids = no_quest_ids[!no_quest_ids == "999test"]
no_quest_ids = no_quest_ids[!no_quest_ids == ""]


no_quest_ids # <- these have to be fixed - they seem to have no questionnaire data.
# [1] "8008"     "219"      "30019"    "1001_A"   "1001"     "P7_100"   "30055"    "42"       "P7_14"    "P7_100"   "30080"   
# [12] "1001"     "P7"       "42"       "P7_001_A" "1001"     "p8_1001"  "62102"    "62989"    "62006"    "62125"    "62110"   
# [23] "62020"    "62009"    "62023"    "62998"    "62012"    "62103"    "62003"    "62102"    "62992"    "79999"    


idx_vec_quest = which(!(all_sub_quest %in% all_sub_test ))
no_test_ids = all_sub_quest[idx_vec_quest]
no_test_ids = no_test_ids[!no_test_ids == 99998]
no_test_ids = no_test_ids[!no_test_ids == 11111]
# no_test_ids <- also these - they seem to have no cognitive test data


# remove Test Datasets from all Project data -----------------------------------

dat_adults <- remove_test_rows(dat_adults, "Adults")
dat_adolescents <- remove_test_rows(dat_adolescents, "Adolescents")
dat_children <- remove_test_rows(dat_children, "Children")


# Set column name variables ----------------------------------------------------
vp_col <- "Versuchspersonennummer."
project_col <- "Projekt."


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
# I do not remove incomplete entries 

# Project 3
PROJECT = 3;
# adult
sum((dat_children$Aus.Link.von.Psytoolkit == "cogn" | dat_children$Letzte.Seite < LAST_P_EMPTY) 
    & dat_children[[project_col]] == PROJECT, na.rm = TRUE)
empty_ch_3 = dat_children[which(
  (dat_children$Aus.Link.von.Psytoolkit == "cogn" | dat_children$Letzte.Seite < LAST_P_EMPTY) 
  & dat_children[[project_col]] == PROJECT), vp_col]
# 30017
dat_children <- dat_children[!(dat_children[[project_col]] == PROJECT & 
                                 (dat_children$Aus.Link.von.Psytoolkit == "cogn" | dat_children$Letzte.Seite < LAST_P_EMPTY)), ];
# child
sum(dat_adults$Letzte.Seite < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_3 = dat_adults[which(dat_adults$Letzte.Seite < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT), vp_col]
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & dat_adults$Letzte.Seite < LAST_P_EMPTY), ];

# Project 7
PROJECT = 7;
# adult
sum((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_7 = dat_adults[which((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & 
                             (dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY)), ];
# adolescent
sum((dat_adolescents$Aus.Link.von.Psytoolkit == "cogn" | dat_adolescents$Letzte.Seite < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT, na.rm = TRUE)
empty_adlsc_7 = dat_adolescents[which((dat_adolescents$Aus.Link.von.Psytoolkit == "cogn" | dat_adolescents$Letzte.Seite < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT), vp_col]
#  79019  77001  77001  77001  78050  70002  70008  70002  70016  70015  70017  70023  70027  70026  70034  70039  70038  70033
#  70031  70042  70044  70045  70037  70032  70036  70047  70046  70048  70043  70050  70051  70052  70050  70063  70053  70068
#  70066  70057  70069  70075  70064  70065  70022  70077  70076  70058  70073  70078  70070  70056  70074  70072  70071  70084
#  70062  70088  70085  70090  70086  70089  70093  70092  70100  70098  70099
dat_adolescents <- dat_adolescents[!(dat_adolescents[[project_col]] == PROJECT & 
                                       (dat_adolescents$Aus.Link.von.Psytoolkit == "cogn" | dat_adolescents$Letzte.Seite < LAST_P_EMPTY)), ];

# Project 8
# PROJECT = 8;
# adult
# sum((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
# empty_ad_8 = dat_adults[which((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
# dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & 
#                             (dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY)), ];


# Project 9
PROJECT = 9;
sum((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_9 = dat_adults[which((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  99003 99020 99009 99021 99027 99023 99006 99010 99025 99025 99007 99024 99018 99012 99037 99034 99036
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & (dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY)), ];


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

# Project 9
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9901)] = 99001


# Handle duplicate IDs ---------------------------------------------------------
sort(unique(dat_adults[[vp_col]][duplicated(dat_adults[[vp_col]])]))


# Separate the data by project and store on disk

separate_by_project(dat_adults, out_path)
separate_by_project(dat_adolescents, out_path)
separate_by_project(dat_children, out_path)


## Experimenter Page -------------------------------------------------


# Set column name variables ----------------------------------------------------

vp_col <- "Versuchspersonen.ID...Participant.ID"
project_col <- "Projekt...Project"


# Fix issues with project assignment -------------------------------------------

dat_general[[project_col]][which(dat_general[[vp_col]] == 2048)]
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
dat_general[[project_col]][which(dat_general[[vp_col]] == 2048)] = 2;
# DOES NOT APPEAR FOR SOME REASON... WHY? IS THERE PSYTOOLKIT DATA?

dat_general[[project_col]][which(dat_general[[vp_col]] == 99017)]
# assuming this is project 9 since project 1 does not collect data and the id 
# starts with a 9. also project 9 IDs are actually consecutive and 17 is missing. 
dat_general[[project_col]][which(dat_general[[vp_col]] == 99017)] = 9;
# NEITHER, SAME QUESTION
sort(dat_general[[vp_col]])



# Remove empty Rows --------------------------------------------------------

LAST_P_EMPTY = 2;
# empty entries did not get pat page LAST_P_EMPTY. I assume this is a technical issue usually 
# and I remove it. I also remove when only cognitive tests were performed sicne thre is no quest data. 
# I do not remove incomplete entries 

# Project 2
PROJECT = "P2(Donner & Lincoln)";
# adult
sum((dat_general$Letzte.Seite < LAST_P_EMPTY) 
    & dat_general[[project_col]] == PROJECT, na.rm = TRUE)
empty_2 = dat_general[which(
  (dat_general$Letzte.Seite < LAST_P_EMPTY) 
  & dat_general[[project_col]] == PROJECT), vp_col]
# 30017
dat_general <- dat_general[!(dat_general[[project_col]] == PROJECT & 
                                 (dat_general$Letzte.Seite < LAST_P_EMPTY)), ];


# Project 3
PROJECT = 3;
# adult
sum((dat_children$Aus.Link.von.Psytoolkit == "cogn" | dat_children$Letzte.Seite < LAST_P_EMPTY) 
    & dat_children[[project_col]] == PROJECT, na.rm = TRUE)
empty_ch_3 = dat_children[which(
  (dat_children$Aus.Link.von.Psytoolkit == "cogn" | dat_children$Letzte.Seite < LAST_P_EMPTY) 
  & dat_children[[project_col]] == PROJECT), vp_col]
# 30017
dat_children <- dat_children[!(dat_children[[project_col]] == PROJECT & 
                                 (dat_children$Aus.Link.von.Psytoolkit == "cogn" | dat_children$Letzte.Seite < LAST_P_EMPTY)), ];
# child
sum(dat_adults$Letzte.Seite < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_3 = dat_adults[which(dat_adults$Letzte.Seite < LAST_P_EMPTY & dat_adults[[project_col]] == PROJECT), vp_col]
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & dat_adults$Letzte.Seite < LAST_P_EMPTY), ];

# Project 7
PROJECT = 7;
# adult
sum((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_7 = dat_adults[which((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & 
                             (dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY)), ];
# adolescent
sum((dat_adolescents$Aus.Link.von.Psytoolkit == "cogn" | dat_adolescents$Letzte.Seite < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT, na.rm = TRUE)
empty_adlsc_7 = dat_adolescents[which((dat_adolescents$Aus.Link.von.Psytoolkit == "cogn" | dat_adolescents$Letzte.Seite < LAST_P_EMPTY) & dat_adolescents[[project_col]] == PROJECT), vp_col]
#  79019  77001  77001  77001  78050  70002  70008  70002  70016  70015  70017  70023  70027  70026  70034  70039  70038  70033
#  70031  70042  70044  70045  70037  70032  70036  70047  70046  70048  70043  70050  70051  70052  70050  70063  70053  70068
#  70066  70057  70069  70075  70064  70065  70022  70077  70076  70058  70073  70078  70070  70056  70074  70072  70071  70084
#  70062  70088  70085  70090  70086  70089  70093  70092  70100  70098  70099
dat_adolescents <- dat_adolescents[!(dat_adolescents[[project_col]] == PROJECT & 
                                       (dat_adolescents$Aus.Link.von.Psytoolkit == "cogn" | dat_adolescents$Letzte.Seite < LAST_P_EMPTY)), ];

# Project 8
# PROJECT = 8;
# adult
# sum((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
# empty_ad_8 = dat_adults[which((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  79016 70003 70005 70010 keine VP Nummer 70023 70029 70025 70013 70040 70044 70049 70054 70060 70067 70059 70096 70097
# dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & 
#                             (dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY)), ];


# Project 9
PROJECT = 9;
sum((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT, na.rm = TRUE)
empty_ad_9 = dat_adults[which((dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY) & dat_adults[[project_col]] == PROJECT), vp_col]
#  99003 99020 99009 99021 99027 99023 99006 99010 99025 99025 99007 99024 99018 99012 99037 99034 99036
dat_adults <- dat_adults[!(dat_adults[[project_col]] == PROJECT & (dat_adults$Aus.Link.von.Psytoolkit == "cogn" | dat_adults$Letzte.Seite < LAST_P_EMPTY)), ];


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

# Project 9
# assuming a 0 (or many) 0s are missing
dat_adults[[vp_col]][which(dat_adults[[vp_col]] == 9901)] = 99001


# Handle duplicate IDs ---------------------------------------------------------
sort(unique(dat_adults[[vp_col]][duplicated(dat_adults[[vp_col]])]))


# Split by Project -------------------------------------------------------------

separate_by_project_cog(dat_general, out_path)


## Process Cognitive Task Data from Psytoolkit ---------------------------------

psytool_info_adults <- remove_test_rows(psytool_info_adults, "psytool_info")
psytool_info_children <- remove_test_rows(psytool_info_children, "psytool_info")
psytool_info_adults_remote <- remove_test_rows(psytool_info_adults_remote, "psytool_info")
psytool_info_adolescents <- remove_test_rows(psytool_info_adolescents, "psytool_info")
  
separate_by_project(psytool_info_adults, cogtest_out_path)
separate_by_project(psytool_info_adults_remote, cogtest_out_path)
separate_by_project(psytool_info_children, cogtest_out_path)
separate_by_project(psytool_info_adolescents, cogtest_out_path)


## Get the Experimental Data Sets Associated with the project ------------------


copy_psytool_files()


