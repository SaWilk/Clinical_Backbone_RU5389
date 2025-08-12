
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: Backbone preprocessing 
# Antonia Bott (antonia.bott@uni-hamburg.de)
# Version: 0.5
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script 
# (1) reads questionnaire data exported from LimeSurvey and calculates sum/mean scores

# (2) reads WCST data and calculates outcome variables 
#     (reversals, trials to first reversal, number of errors, perseveration errors)

# (3) reads BACS data and calculates outcome variables
#      (number of answered items, number of correct answers, number of errors)

# (4) reads LNS data and calculates outcome variables
#     (number of correct answers)



# Note:
# For the script to run you should have a main folder (set as working directory) 
# containing two sub folders called data_questionnaires and 
# data_tasks (containing subfolders wcst, BACS and LNS).
# In order to access the relevant data, the script specifies the specific 
# subfolders within their respective sections.

# You can use the given structure - or you need to adapt the script to your folder structure below.  

# Clean up R environment
rm(list=ls())
cat("\014")


# SETTINGS SECTION--------------------------------------------------------------

## 1) Set your working directory (by adding your Sys.info() output to switch() function)
##    (valuable in case you work on multiple systems and/or collaborate)

### Specify the path to your preferred directory on your system
#path <- '/this/should/lead/to/the/Data/Folder'

### System name
whoami <- Sys.info()[[4]]; print(whoami)

switch(whoami,
       "TASCHENRECHNER" = {name <- "C:/Users/anton/";
       path <- "OneDrive/03_Projects/ESinRCT/"}, 
       "KOPSY-D-033080" = {name <- "C:/Users/Antonia Bott/";
       path <- "OneDrive/03_Projects/ESinRCT/"}, 
       "REAKTOR" = {name <- "C:/Users/Toni/"; 
       path <- "OneDrive/03_Projects/ESinRCT/"},
       "Macbook-Lucia.fritz.box" = {name <- "/Users/luciafrohn/"; 
       path <- "Documents/Psychologie/Klinischer Job/FOR/R_Auswertung/"},
       "dhcp-172-21-5-162.wlan.uni-hamburg.de.local" = {name <- "/Users/luciafrohn/"; 
       path <- "Documents/Psychologie/Klinischer Job/FOR/R_Auswertung/"},
       "UN-LAP-015977" = {name <- dirname(rstudioapi::getSourceEditorContext()$path);
       path = file.path("raw_data")}
)
 
#switch (whoami,
  #      "TASCHENRECHNER" = {setwd(paste0("C:/Users/Toni", path))}, 
  #      "7040-PC" = {setwd(paste0("C:/Users/Antonia Bott", path))},
# );getwd()

### ... or by just setting the working directory :)
# setwd('C:/this/should/lead/to/the/Data')



## 2) Load required packages (install if not yet preinstalled)

if(!require("readxl")){install.packages("readxl")};library(readxl)
if(!require("dplyr")){install.packages("dplyr")};library(dplyr)
if(!require("tibble")){install.packages("tibble")};library(tibble)
if(!require("lubridate")){install.packages("lubridate")};library(lubridate)
if(!require("tidyr")){install.packages("tidyr")};library(tidyr)
if(!require("psych")){install.packages("psych")};library(psych)
if(!require("stats")){install.packages("stats")};library(stats)
if(!require("tidyverse")){install.packages("tidyverse")};library(tidyverse)
if(!require("lubridate")){install.packages("lubridate")};library(lubridate)
#if(!require("haven")){install.packages("haven")};library(haven)


# LOAD QUESTIONNAIRE DATA ------------------------------------------------------
setwd('data_questionnaires') 

# load single data file
dat_raw <- read_csv("results-survey564757_17.05.24.csv")

# load multiple data files 
# list_of_files <- list.files(pattern = ".csv")
# dat_raw <- bind_rows(lapply(list_of_files, read_csv))
# Please make sure that your dataset contains 1080 variables!

# Possibility to remove data according to your criteria
# dat <- dat %>%  filter(insert your criteria)


# QUESTIONNAIRES ---------------------------------------------------------------
# variables
dat_quest <- dat_raw %>% mutate(across(starts_with("IDAS") | starts_with("CAPEfreq") |starts_with("CAPEdistr") | starts_with("AQ") |
                             starts_with("Alk") | starts_with("Tab") | starts_with("Cann") | starts_with("Stim") | 
                             starts_with("Opi") | starts_with("Hal") | starts_with("Inh") | starts_with("Med")  | starts_with("And") |
                             starts_with("ASRS") | starts_with("BISBAS") | starts_with("IUS") | starts_with("APS") | starts_with("TICS") |
                             starts_with("CTQ")  | starts_with("MAP")
                               , as.numeric)) %>% 
  
  mutate(across(c(gender, starts_with("parentsgender"), starts_with("siblingsgender"), starts_with("cildrengender") ),
                ~ factor(.x, levels = c("1", "2", "3", "0", "-oth-"),
                         labels = c("female", "male", "nonbinary", "no gender", "other")))) %>% 
  
  mutate(education = factor(education,
                            levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "-oth-"),
                            labels = c("no school diploma", "primary school", "ESA", "MSA", "AHR", "apprentice", "applied science (Fachhochschulabschluss)",
                                        "diploma", "bachelor", "master", "PhD", "other"))) %>% 
  
  mutate(maritalstat = factor(maritalstat,
                              levels = c("0", "1", "2", "3", "4"),
                              labels = c("single", "relationship", "married", "divorced", "widowed"))) %>% 
  
  mutate(across(c(eyesight, hearing),
                ~ factor(.x, levels = c("0", "1", "2"), 
                         labels = c("normal", "corrected", "not corrected")))) %>% 
  
  mutate(across(c(psychomedication, othermedication, ownpsychdisorder, contains("contact")), 
                ~ factor(.x, levels = c("0", "1"), 
                         labels = c("no", "yes")))) %>% 
  
  rename(., date_birth = age) %>% 
  
  # calculation of age at the time of completing the questionnaires
  mutate(quest_date = as.Date(submitdate),
         years = as.integer(floor(interval(date_birth, quest_date) / years(1))),
         month = as.integer(floor((interval(date_birth, quest_date) %% years(1)) / months(1))),
         age_calc = paste(years, month, sep = ".")) %>% 

  # inverse code items 2 and 22 from BIS BAS scales
  mutate_at(c("BISBAS[002]", "BISBAS[022]"),
            ~ recode(., "1" = 4, "2" = 3, "3" = 2, 
                     "4" = 1)) %>%
  rename_at(c("BISBAS[002]", "BISBAS[022]"), 
            function(x) paste0(x,"_inv")) %>%
  
  # inverse code items 2, 5, 7, 13, 19, 26, 28 from CTQ
  mutate_at(c("CTQ[002]", "CTQ[005]", "CTQ[007]", 
              "CTQ[013]", "CTQ[019]", "CTQ[026]", "CTQ[028]"),
            ~ recode(., "1" = 5, "2" = 4, "3" = 3, 
                     "4" = 2, "5" = 1)) %>%
  rename_at(c("CTQ[002]", "CTQ[005]", "CTQ[007]", 
              "CTQ[013]", "CTQ[019]", "CTQ[026]", "CTQ[028]"), 
            function(x) paste0(x,"_inv")) %>% 
  
  # Minimization scale from CTQ (1-4 = 0 points, 5 = 1 point) item 10, 16 and 22
  mutate_at(c("CTQ[010]", "CTQ[016]", "CTQ[022]"),
            ~ recode(., "1" = 0, "2" = 0, "3" = 0, 
                     "4" = 0, "5" = 1)) %>%
  rename_at(c("CTQ[010]", "CTQ[016]", "CTQ[022]"), 
            function(x) paste0(x,"_recoded")) %>% 
  
  #inverse code item 1 from APS 
  mutate_at(c("APS[001]"),
            ~ recode(., "1" = 5, "2" = 4, "3" = 3, 
                     "4" = 2, "5" = 1)) %>%
  rename_at(c("APS[001]"), 
            function(x) paste0(x,"_inv")) %>% 
  
  # inverse code all items from MAP-SR
  mutate(across(starts_with("MAP") & !contains("Info"),
                ~ recode(., `0` = 4, `1` = 3, `2` = 2, `3` = 1, `4` = 0))) %>% 
  rename_with(~ paste0(.,"_inv"), starts_with("MAP") & !contains("Info")) %>% 
  
  # recode items 27, 28, 31, 32, 36 and 37 from AQ-10 (0-1 = 1 point, 2-3 = 0 points) -> wollen wir diese Art der Auswertung?
  mutate_at(c("AQ[027]", "AQ[028]", "AQ[031]", 
              "AQ[032]", "AQ[036]", "AQ[037]"),
            ~ recode(., "0" = 1, "1" = 1, "2" = 0, 
                     "3" = 0)) %>%
  rename_at(c("AQ[027]", "AQ[028]", "AQ[031]", 
              "AQ[032]", "AQ[036]", "AQ[037]"), 
            function(x) paste0(x,"_recoded")) %>% 
  
  # recode items 5, 20, 41 and 45 from AQ-10 (0-1 = 0 points, 2-3 = 1 point)
  mutate_at(c("AQ[005]", "AQ[020]", "AQ[041]", 
              "AQ[045]"),
            ~ recode(., "0" = 0, "1" = 0, "2" = 1, 
                     "3" = 1)) %>%
  rename_at(c("AQ[005]", "AQ[020]", "AQ[041]", 
              "AQ[045]"), 
            function(x) paste0(x,"_recoded")) %>% 
  
  # recode all items from CAPE (0-3 -> 1-4)
  
  mutate_at(c("CAPEfreq[002]", "CAPEfreq[005]", "CAPEfreq[006]", "CAPEfreq[007]", "CAPEfreq[010]",
              "CAPEfreq[011]", "CAPEfreq[013]", "CAPEfreq[015]", "CAPEfreq[017]", "CAPEfreq[020]",
              "CAPEfreq[022]", "CAPEfreq[024]", "CAPEfreq[026]", "CAPEfreq[028]", "CAPEfreq[030]",
              "CAPEfreq[031]", "CAPEfreq[033]", "CAPEfreq[034]", "CAPEfreq[041]","CAPEfreq[042]",
              "CAPEdistr[002]", "CAPEdistr[005]", "CAPEdistr[006]", "CAPEdistr[007]", "CAPEdistr[010]",
              "CAPEdistr[011]", "CAPEdistr[013]", "CAPEdistr[015]", "CAPEdistr[017]", "CAPEdistr[020]",
              "CAPEdistr[022]", "CAPEdistr[024]", "CAPEdistr[026]", "CAPEdistr[028]", "CAPEdistr[030]",
              "CAPEdistr[031]", "CAPEdistr[033]", "CAPEdistr[034]", "CAPEdistr[041]","CAPEdistr[042]"),
            ~ recode(., "0" = 1, "1" = 2, "2" = 3, "3" = 4)) %>%
  rename_at(c("CAPEfreq[002]", "CAPEfreq[005]", "CAPEfreq[006]", "CAPEfreq[007]", "CAPEfreq[010]",
              "CAPEfreq[011]", "CAPEfreq[013]", "CAPEfreq[015]", "CAPEfreq[017]", "CAPEfreq[020]",
              "CAPEfreq[022]", "CAPEfreq[024]", "CAPEfreq[026]", "CAPEfreq[028]", "CAPEfreq[030]",
              "CAPEfreq[031]", "CAPEfreq[033]", "CAPEfreq[034]", "CAPEfreq[041]","CAPEfreq[042]",
              "CAPEdistr[002]", "CAPEdistr[005]", "CAPEdistr[006]", "CAPEdistr[007]", "CAPEdistr[010]",
              "CAPEdistr[011]", "CAPEdistr[013]", "CAPEdistr[015]", "CAPEdistr[017]", "CAPEdistr[020]",
              "CAPEdistr[022]", "CAPEdistr[024]", "CAPEdistr[026]", "CAPEdistr[028]", "CAPEdistr[030]",
              "CAPEdistr[031]", "CAPEdistr[033]", "CAPEdistr[034]", "CAPEdistr[041]","CAPEdistr[042]"),
            function(x) paste0(x,"_recoded"))



# SCORES 

dat_scores <- dat_quest %>%
  
  rowwise() %>%
  
  # CAPE: Calculating the number of items filled in  
  mutate(CAPEfreq_items = rowSums(!is.na(select(dat_quest, starts_with("CAPEfreq") & ends_with("recoded")))),
         CAPEdistr_items = rowSums(!is.na(select(dat_quest, starts_with("CAPEdistr") & ends_with("recoded"))))) %>% 
  
  mutate(IDAS_total_sum = sum(c_across(starts_with("IDAS") & ends_with("]")), na.rm = T), 
         IDAS_dysphoria_sum = sum(c_across(starts_with("IDAS") & contains(c("002", "005", "008", "009", 
                                                                           "021", "031", "039", "047",
                                                                           "056", "060"))), na.rm = T),
         IDAS_lassitude_sum = sum(c_across(starts_with("IDAS") & contains(c("006", "029", "030", "042", 
                                                                            "053", "054"))), na.rm = T),
         IDAS_insomnia_sum = sum(c_across(starts_with("IDAS") & contains(c("004", "011", "017", "025", 
                                                                            "036", "050"))), na.rm = T),
         IDAS_suicidality_sum = sum(c_across(starts_with("IDAS") & contains(c("013", "022", "033", "037",
                                                                              "045", "051"))), na.rm = T),
         IDAS_appetitegain_sum = sum(c_across(starts_with("IDAS") & contains(c("019", "024", "062"))), na.rm = T),
         IDAS_appetiteloss_sum = sum(c_across(starts_with("IDAS") & contains(c("001", "026", "059"))), na.rm = T), 
         IDAS_wellbeing_sum = sum(c_across(starts_with("IDAS") & contains(c("003", "010", "023", "027", 
                                                                            "049", "052", "058", "063"))), na.rm = T),
         IDAS_illtemper_sum = sum(c_across(starts_with("IDAS") & contains(c("012", "035", "043", "061"))), na.rm = T), 
         IDAS_mania_sum = sum(c_across(starts_with("IDAS") & contains(c("066", "070", "076", "082", "086"))), na.rm = T),
         IDAS_euphoria_sum = sum(c_across(starts_with("IDAS") & contains(c("071", "077", "087", "091", "096"))), na.rm = T),
         IDAS_socialanxiety_sum = sum(c_across(starts_with("IDAS") & contains(c("015", "018", "020", "040",
                                                                              "046", "098"))), na.rm = T),
         IDAS_claustrophobia_sum = sum(c_across(starts_with("IDAS") & contains(c("073", "079", "083", "089",
                                                                               "093"))), na.rm = T),
         IDAS_traumaticintrusions_sum = sum(c_across(starts_with("IDAS") & contains(c("014", "028", "034", "041"))), na.rm = T),
         IDAS_traumaticavoidance_sum = sum(c_across(starts_with("IDAS") & contains(c("072", "078", "088", "092"))), na.rm = T),
         IDAS_ordering_sum = sum(c_across(starts_with("IDAS") & contains(c("064", "068", "081", "084",
                                                                                 "094"))), na.rm = T),
         IDAS_cleaning_sum = sum(c_across(starts_with("IDAS") & contains(c("065", "069", "075", "085",
                                                                           "090", "095", "097"))), na.rm = T),
         IDAS_checking_sum = sum(c_across(starts_with("IDAS") & contains(c("067", "074", "080"))), na.rm = T),
         IDAS_panic_sum = sum(c_across(starts_with("IDAS") & contains(c("007", "016", "032", "038", 
                                                                            "044", "048", "055", "057"))), na.rm = T),
         
         CAPE_freq_sum = sum(c_across(starts_with("CAPEfreq") & ends_with("recoded")), na.rm = T),
         CAPE_distr_sum = sum(c_across(starts_with("CAPEdistr") & ends_with("recoded")), na.rm = T),
         CAPE_freq_mean = CAPE_freq_sum/CAPEfreq_items, 
         CAPE_distr_mean = ifelse(CAPEdistr_items == 0, 0, CAPE_distr_sum/CAPEdistr_items),
         
         AQ_sum = sum(c_across(starts_with("AQ")), na.rm = T),
         
         SUQ_Alk_prod = ifelse(is.na(Alk2) | is.na(Alk3), 0, prod(Alk2, Alk3)),
         SUQ_Tab_prod = ifelse(is.na(Tab2) | is.na(Tab3), 0, prod(Tab2, Tab3)),
         SUQ_Cann_prod = ifelse(is.na(Cann2) | is.na(Cann3), 0, prod(Cann2, Cann3)),
         SUQ_Stim_prod = ifelse(is.na(Stim2) | is.na(Stim3), 0, prod(Stim2, Stim3)),
         SUQ_Opi_prod = ifelse(is.na(Opi2) | is.na(Opi3), 0, prod(Opi2, Opi3)),
         SUQ_Hal_prod = ifelse(is.na(Hal2) | is.na(Hal3), 0, prod(Hal2, Hal3)),
         SUQ_Inh_prod = ifelse(is.na(Inh2) | is.na(Inh3), 0, prod(Inh2, Inh3)),
         SUQ_Med_prod = ifelse(is.na(Med2) | is.na(Med3), 0, prod(Med2, Med3)),
         SUQ_And_prod = ifelse(is.na(And2) | is.na(And3), 0, prod(And2, And3)),
         SUQ_total_sum = sum(SUQ_Alk_prod, SUQ_Tab_prod, SUQ_Cann_prod, SUQ_Stim_prod, SUQ_Opi_prod, SUQ_Hal_prod, SUQ_Inh_prod, SUQ_Med_prod, SUQ_And_prod, na.rm = T),
         
         ASRS5_sum = sum(c_across(starts_with("ASRS") & ends_with("]")), na.rm = T),
         BIS_total_mean = round(mean(c_across(starts_with("BISBAS") & contains(c("002", "008", "013", "016", 
                                                                    "019", "022", "024"))), na.rm = T), 2),
         BAS_total_mean = round(mean(c_across(starts_with("BISBAS") & contains(c("003", "004", "005", "007", 
                                                                           "009", "010", "012", "014",
                                                                           "015", "018", "020", "021", "023"))), na.rm = T),2),
         BAS_drive_mean = mean(c_across(starts_with("BISBAS") & contains(c("003", "009", "012", "021"))), na.rm = T),
         BAS_rewardresp_mean = signif(mean(c_across(starts_with("BISBAS") & contains(c("004", "007", "014", "018", "023"))), na.rm = T), 3),
         BAS_funseeking_mean = mean(c_across(starts_with("BISBAS")& contains(c("005", "010", "015", "020"))), na.rm = T),
         
         IUS_sum = sum(c_across(starts_with("IUS") & ends_with("]")), na.rm = T), 
         
         APS_sum = sum(c_across(starts_with("APS")), na.rm = T), 
         
         TICS9_sum = sum(c_across(starts_with("TICS") & ends_with("]")), na.rm = T),
         
         MAPSR_sum = sum(c_across(starts_with("MAP") & ends_with("inv")), na.rm = T), 
         
         CTQ_total_sum = sum(c_across(starts_with("CTQ") & !contains(c("010", "016", "022")) ), na.rm = T),
         CTQ_emotionalabuse_sum = sum(c_across(starts_with("CTQ") & contains(c("003", "008", "014", "018", "025"))), na.rm = T),
         CTQ_physicalabuse_sum = sum(c_across(starts_with("CTQ") & contains(c("009", "011", "012", "015", "017"))), na.rm = T),
         CTQ_sexualabuse_sum = sum(c_across(starts_with("CTQ") & contains(c("020", "021", "023", "024", "027"))), na.rm = T),
         CTQ_emotionalneglect_sum = sum(c_across(starts_with("CTQ") & contains(c("005", "007", "013", "019", "028"))), na.rm = T),
         CTQ_physicalneglect_sum = sum(c_across(starts_with("CTQ") & contains(c("001", "002", "004", "006", "026"))), na.rm = T),
         CTQ_bagatelization_sum = sum(c_across(starts_with("CTQ") & contains(c("010", "016", "022"))), na.rm = T)) %>% 
         
  
  ungroup() %>%
  
  select(vpid, age_calc, gender,
         IDAS_total_sum, CAPE_freq_sum, CAPE_distr_sum, CAPE_freq_mean, CAPE_distr_mean, AQ_sum, SUQ_Alk_prod, SUQ_Tab_prod,SUQ_Cann_prod, SUQ_Stim_prod,
         SUQ_Opi_prod, SUQ_Hal_prod, SUQ_Inh_prod, SUQ_Med_prod, SUQ_And_prod,SUQ_total_sum, ASRS5_sum, BIS_total_mean, BAS_total_mean, BAS_drive_mean, 
         BAS_rewardresp_mean, BAS_funseeking_mean, IUS_sum, APS_sum, TICS9_sum, CTQ_total_sum, CTQ_emotionalabuse_sum, CTQ_physicalabuse_sum, CTQ_sexualabuse_sum,
         CTQ_emotionalneglect_sum, CTQ_physicalneglect_sum, CTQ_bagatelization_sum, MAPSR_sum)


# descriptive analysis ---------------------------------------------------------
# descriptive analysis of dat_scores
descriptive_analysis <- describe(dat_scores) %>% select(min, max, mean, sd) %>% round(., 2)

# you can add other numeric variables of dat to the descriptive analysis if you like
# for example:
# descriptive_analysis <- bind_rows(describe(dat_scores) %>% select(min, max, mean, sd) %>% round(., 2),
#                          describe(dat[, c("height", "weight")])  %>% select(min, max, mean, sd) %>% round(., 2))

# summary of factor variable level quantities
# If interested, you can create datasets for individual variables using the following code: 
# variable_count <- dat %>%  count(variable) (change "variable" to your variable of interest)

# preparation of multiple choice variables (job and ownpsychdiagnosis)
job_count <- dat_quest %>% summarise(jobsearch = sum(`job[0]` == "Y", na.rm = T),
                         school = sum(`job[1]` == "Y", na.rm = T),
                         apprentice = sum(`job[2]` == "Y", na.rm = T),
                         university = sum(`job[3]` == "Y", na.rm = T),
                         self_employed = sum(`job[4]` == "Y", na.rm = T),
                         minijob = sum(`job[5]` == "Y", na.rm = T),
                         parttime = sum(`job[6]` == "Y", na.rm = T),
                         fulltime = sum(`job[7]` == "Y", na.rm = T),
                         home = sum(`job[8]` == "Y", na.rm = T),
                         support_institution = sum(`job[9]` == "Y", na.rm = T),
                         unable = sum(`job[10]` == "Y", na.rm = T),
                         retirement = sum(`job[11]` == "Y", na.rm = T),
                         other= sum(!is.na(`job[other]`)))

job_count <- job_count %>%  pivot_longer(cols = c(jobsearch, school, apprentice, university, self_employed, minijob, parttime,
                                      fulltime, home, support_institution, unable, retirement, other), names_to = "level", values_to = "quantity")

job_count <- job_count %>% mutate(variable = "job")


diagnosis_count <- dat_quest %>% summarise(depression = sum(`ownpsychdiagn[MDE]` == "Y", na.rm = T),
                               bipolar = sum(`ownpsychdiagn[Bipolar]` == "Y", na.rm = T),
                               OCD = sum(`ownpsychdiagn[OCD]` == "Y", na.rm = T),
                               anxiety = sum(`ownpsychdiagn[Anx]` == "Y", na.rm = T),
                               psychotic = sum(`ownpsychdiagn[Psy]` == "Y", na.rm = T),
                               substance = sum(`ownpsychdiagn[SUD]` == "Y", na.rm = T),
                               eatingdis = sum(`ownpsychdiagn[ED]` == "Y", na.rm = T),
                               idk = sum(`ownpsychdiagn[idk]` == "Y", na.rm = T),
                               other= sum(!is.na(`ownpsychdiagn[other]`)))

diagnosis_count <- diagnosis_count %>%  pivot_longer(cols = c(depression, bipolar, OCD, anxiety, psychotic, substance,
                                                             eatingdis, idk, other), names_to = "level", values_to = "quantity")

diagnosis_count <- diagnosis_count %>% mutate(variable = "diagnosis")

# combined quantities of factor variable levels
quantities <- dat_quest %>%
  pivot_longer(cols = c(gender, eyesight, education, maritalstat, hearing, psychomedication, othermedication, ownpsychdisorder)) %>%
  group_by(name, value) %>%
  summarise(quantity = n(), .groups = "drop") %>%
  rename(variable = name, level = value) %>% 
 bind_rows(diagnosis_count, job_count)

# plot of factor variable level quantities
quantities %>% 
  ggplot(aes(x = level, y = quantity, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable,scales = "free_x", ncol = 2, strip.position = "bottom") +
  scale_fill_discrete() +
  theme(legend.position = "none")


# COGNITIVE TASKS --------------------------------------------------------------
# change working directory to data_tasks folder
setwd('../data_tasks')

# WCST--------------------------------------------------------------------------
# get the file names; insert the correct Data folder and file names here
files_wcst <- as_tibble(list.files(path = "WCST", pattern = ".txt")) %>%
  filter(grepl("WCST", value))

n_wcst <- length(files_wcst$value)

# create empty dummy data frame which will get filled by the loop below
wcst_aggregated <- data.frame(vpid = as.character(rep(NA,n_wcst)), 
                              wcst_duration = as.character(rep(NA,n_wcst)),
                              wcst_reversals_number = as.character(rep(NA,n_wcst)), 
                              wcst_reversals_trialstofirst = as.character(rep(NA,n_wcst)), 
                              wcst_anyerror = as.character(rep(NA,n_wcst)), 
                              wcst_perseverationerror = as.character(rep(NA,n_wcst)))
wcst_raw <- NULL

for(i in 1:length(files_wcst$value)){
  
  # Read data and add column names
  wcst_raw[[i]] <- read.table(file = paste0(getwd(), "/wcst/", files_wcst[i,]))
  names(wcst_raw[[i]])<- c("project", "vpid", "card", "ShapeCorrect", "NumberCorrect", "ColorCorrect", "RT", "STATUS", "answer", "anyerror", "perseverationerror", "nonperseverationerror", "correct_count", "block_count", "trial_count", "time_trialstart")
  
  # Extract VP number
  wcst_aggregated$vpid[i] <- wcst_raw[[i]]$vpid[1]
  
  # Duration (in minutes), including instruction
  wcst_aggregated$wcst_duration[i] <- as.numeric(round(wcst_raw[[i]]$time_trialstart[nrow(wcst_raw[[i]])]/1000/60,2))
  
  # Achieved reversals
  wcst_aggregated$wcst_reversals_number[i] <- max(wcst_raw[[i]]$block_count)
  
  # Trials to first reversal
  wcst_aggregated$wcst_reversals_trialstofirst[i] <- max(wcst_raw[[i]]$trial_count[wcst_raw[[i]]$block_count==1])
  
  # Errors
  wcst_aggregated$wcst_anyerror[i] <- sum(wcst_raw[[i]]$anyerror)
  wcst_aggregated$wcst_perseverationerror[i] <- sum(wcst_raw[[i]]$perseverationerror)
  
}

# gleich löschen!
wcst_aggregated[1,1] <- 219


  
# BACS--------------------------------------------------------------------------
# get the file names; insert the correct Data folder and file names here
files_bacs <- as_tibble(list.files(path = "BACS", pattern = ".txt")) %>%
  filter(grepl("BACS", value))
# files_bacs <- as_tibble(list.files(pattern = ".txt")) %>%
 # filter(grepl("BACS", value))

n_bacs <- length(files_bacs$value)

# create empty dummy data frame which will get filled by the loop below
BACS_aggregated <- data.frame(vpid = as.character(rep(NA,n_bacs)),
                              bacs_duration = as.character(rep(NA,n_bacs)),
                              bacs_trial_count = as.character(rep(NA,n_bacs)), 
                              bacs_correct_count = as.character(rep(NA,n_bacs)), 
                              bacs_error_count = as.character(rep(NA,n_bacs)))

BACS_raw <- NULL

for(i in 1:length(files_bacs$value)){
  
  # Read data and add column names
  BACS_raw[[i]] <- read.table(file = paste0(getwd(),"/BACS/", files_bacs[i,]))
  names(BACS_raw[[i]])<- c("project","vpid","block", "TRIALCOUNT", "NumberCorrect", "STATUS", "answer", "RT", "correct_count", "error_count", "time_trialstart")
  
  # Extract VP number
  BACS_aggregated$vpid[i] <- BACS_raw[[i]]$vpid[1]
  
  # Duration (in minutes), including instruction and training
  BACS_aggregated$bacs_duration[i] <- as.numeric(round(BACS_raw[[i]]$time_trialstart[nrow(BACS_raw[[i]])]/1000/60,2))  #könnte vlt last_row tail function dafür benutzen, sclice und dann n 
  
  # Achieved trials
  BACS_aggregated$bacs_trial_count[i] <- max(BACS_raw[[i]]$TRIALCOUNT) - 7  # 7 training trials
  
  # Number of correct answers
  BACS_aggregated$bacs_correct_count[i] <- max(BACS_raw[[i]]$correct_count[BACS_raw[[i]]$block =="experiment"])
  
  # Errors
  BACS_aggregated$bacs_error_count[i] <- max(BACS_raw[[i]]$error_count[BACS_raw[[i]]$block =="experiment"])

}

# löschen:
BACS_aggregated[1,1] <- 219

# LNS--------------------------------------------------------------------------
# get the file names; insert the correct Data folder and file names here
files_lns <- as_tibble(list.files(path = "LNS", pattern = ".txt")) %>%
  filter(grepl("LNS", value))

n_lns <- length(files_lns$value)


# create empty dummy data frame which will get filled by the loop below
LNS_aggregated <- data.frame(vpid = as.character(rep(NA,n_lns)),
                             lns_duration = as.character(rep(NA,n_lns)),
                             lns_block_count = as.character(rep(NA,n_lns)), 
                             lns_longestnumberspan = as.character(rep(NA, n_lns)),
                             lns_correct_count = as.character(rep(NA,n_lns)))
LNS_raw <- NULL

for(i in 1:length(files_lns$value)){
  
  # Read data and add column names
  LNS_raw[[i]] <- read.table(file = paste0(getwd(), "/LNS/", files_lns[i,]))
  names(LNS_raw[[i]])<- c("project","vpid","block", "block_count", "itemCorrect", "STATUS", "answer", "RT", "correct_count", "time_trialstart")
  
  # Extract VP number
  LNS_aggregated$vpid[i] <- LNS_raw[[i]]$vpid[1]
  
  # Duration (in minutes), including instruction and training
   LNS_aggregated$lns_duration[i] <- as.numeric(round(LNS_raw[[i]]$time_trialstart[nrow(LNS_raw[[i]])]/1000/60, 2)) 
  
  # Achieved block
  LNS_aggregated$lns_block_count[i] <- max(LNS_raw[[i]]$block_count)
  
  # Longest number span
  LNS_aggregated$lns_longestnumberspan[i] <- max(LNS_raw[[i]]$block_count) - 1
  
  # Number of correct answers
  LNS_aggregated$lns_correct_count[i] <- max(LNS_raw[[i]]$correct_count)
  
}

# löschen:
LNS_aggregated[1,1] <- 219

# summary of questionnaires and all cognitive tasks
scores <- merge(dat_scores, wcst_aggregated, by = "vpid", all = TRUE)
scores <- merge(scores, BACS_aggregated, by = "vpid", all = T)
scores <- merge(scores, LNS_aggregated, by = "vpid", all = T)

