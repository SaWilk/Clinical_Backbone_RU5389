#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# FOR: Backbone Status 
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de) & Antonia Bott (antonia.bott@uni-hamburg.de)
# 2025-08-08 (Date initially edited by SW)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script 
# (1) reads questionnaire data exported from LimeSurvey and PsyToolkit

# (2) Creates project-specific data files in the environment as well as on disk

# (3) Counts number of individual IDs in dataset

# (4) Visualizes progress in data collection for each project


# clean up R environment
rm(list=ls())
cat("\014")

# install packages
if(!require("dplyr")){install.packages("dplyr")};library(dplyr)
if(!require("tidyr")){install.packages("tidyr")};library(tidyr)
if(!require("writexl")){install.packages("writexl")};library(tidyr)


## Set working directory -------------------------------------------------------

whoami<- Sys.info()[[4]]; print(whoami)

switch(whoami,
       "KOPSY-D-033080" = {name <- "K:/vol2011/bba3488/";
       path <- "FOR/"},
       "UN-LAP-015977" = {name <- dirname(rstudioapi::getSourceEditorContext()$path);
       setwd(name);
       in_path = file.path("raw_data");
       out_path = file.path("01_project_data");
       survey_out_path = file.path(out_path, "survey_data");
       function_path = file.path("functions");
       psytool_path = file.path("raw_data", "psytoolkit");
       cogtest_out_path = file.path(out_path, "experiment_data");
       }
)


## Source required functions --------------------


source(file.path(function_path, "separate_by_project.R"))
source(file.path(function_path, "separate_by_project_cog.R"))
source(file.path(function_path, "remove_test_rows.R"))


## Backbone surveys ------------------------------------------------------------
#file_adults <-"results_adults_07042025.csv" # SW: not sure why this ID, the survey has a different ID
file_adults <- "results-survey564757.csv"
file_adolescents <- "results-survey585676.csv"
file_children <- "results-survey798916.csv"


## Load data -------------------------------------------------------------------

dat_adults <- read.csv(file.path(name,in_path, file_adults))
dat_adolescents <- read.csv(file.path(name,in_path, file_adolescents))
dat_children <- read.csv(file.path(name,in_path, file_children))
  

# remove Versuchspersonennummer 99999 from all datasets ------------------------


dat_adults <- remove_test_rows(dat_adults, "Adults")
dat_adolescents <- remove_test_rows(dat_adolescents, "Adolescents")
dat_children <- remove_test_rows(dat_children, "Children")


# Separate the data by project and store on disk

separate_by_project(dat_adults, out_path)
separate_by_project(dat_adolescents, out_path)
separate_by_project(dat_children, out_path)


## Psyctoolkit cognitive tasks -------------------------------------------------
# load files with overview of the data (not the actual txt files with the data!)

file_cog <- "results-survey415148.csv"
dat_cog <- read.csv(file.path(name,in_path, file_cog))

dat_cog <- remove_test_rows(dat_cog, "cogtests")

separate_by_project_cog(dat_cog, out_path)


## Read in Cognitive Task Data from Psytoolkit ---------------------------------

file_psytool_info = "data.csv";

psytool_info_adults <- read.csv(file.path(name, psytool_path, "adults", file_psytool_info))
psytool_info_children <- read.csv(file.path(name, psytool_path, "children", file_psytool_info))
psytool_info_adults_remote <- read.csv(file.path(name, psytool_path, "adults_remote", file_psytool_info))
psytool_info_adolescents <- read.csv(file.path(name, psytool_path, "adolescents", file_psytool_info))

  
separate_by_project_cog(psytool_info_adults, cogtest_out_path)





