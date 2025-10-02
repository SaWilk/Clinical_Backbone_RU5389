#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: Backbone Separate Data by Project 
# Author: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com)
# 2025-10-02 (Date initially edited by SW)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script 
# (1) reads questionnaire data exported from LimeSurvey and PsyToolkit

# (2) Fixes Issues with VP ID assignment

# (3) Creates project-specific data files in the environment as well as on disk



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

