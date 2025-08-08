# clean up R environment
rm(list=ls())
cat("\014")

# install packages
if(!require("dplyr")){install.packages("dplyr")};library(dplyr)
if(!require("tidyr")){install.packages("tidyr")};library(tidyr)

## Set working directory -------------------------------------------------------

whoami<- Sys.info()[[4]]; print(whoami)

switch(whoami,
       "KOPSY-D-033080" = {name <- "K:/vol2011/bba3488/";
       path <- "FOR/"},
       "UN-LAP-015977" = {name <- dirname(rstudioapi::getSourceEditorContext()$path);
       path = file.path("raw_data")}
)


## Backbone surveys ------------------------------------------------------------
#file_adults <-"results_adults_07042025.csv" # SW: not sure why this ID, the survey has a different ID
file_adults <- "results-survey564757.csv"
file_adolescents <- "results-survey585676.csv"
file_children <- "results-survey798916.csv"

## Load data -------------------------------------------------------------------

dat_adults <- read.csv(file.path(name,path, file_adults))
dat_adolescents <- read.csv(file.path(name,path, file_adolescents))
dat_children <- read.csv(file.path(name,path, file_children))
  

idx_chr <- as.character(c(0:9, 99))


# adult data
# create data sets for each project 
parts_adults <- split(dat_adults, dat_adults$Projekt.)
list2env(
  setNames(parts_adults[idx_chr], paste0("data_adults_p_", idx_chr)),
  .GlobalEnv
)

# adolescent data
# create data sets for each project
parts_adolescents <- split(dat_adolescents, dat_adolescents$Projekt.)
list2env(
  setNames(parts_adolescents[idx_chr], paste0("data_adolescents_p_", idx_chr)),
  .GlobalEnv
)

# chlidren data
# create data sets for each project
parts_children <- split(dat_children, dat_children$Projekt.)
list2env(
  setNames(parts_children[idx_chr], paste0("data_children_p_", idx_chr)),
  .GlobalEnv
)

## Psyctoolkit cognitive tasks -------------------------------------------------
# load files with overview of the data (not the actuall txt files with the data!)
cog_adults <- "info_adults.csv"
cog_adolescents <- "info_adolescents.csv"
cog_children <- "info_children.csv"


## Load data -------------------------------------------------------------------
info_adults_cog <- read.csv(paste0(name,path, cog_adults))
info_adolescents_cog <- read.csv(paste0(name,path, cog_adolescents))
info_children_cog <- read.csv(paste0(name,path, cog_children))


# adults
# create data sets for each project
for (i in c(0:9, 99)) {
  assign(paste0("info_adults_cog_p_", i), subset(info_adults_cog, p == i))
}

# children
# create data sets for each project
for (i in c(6,8, 99)) {
  assign(paste0("info_children_cog_p_", i), subset(info_children_cog, p == i))
}

