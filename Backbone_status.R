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
       path <- "FOR/"}
)


## Backbone surveys ------------------------------------------------------------
file_adults <-"results_adults_07042025.csv"
file_adolescents <- "results-survey585676.csv"
file_children <- "results-survey798916.csv"

## Load data -------------------------------------------------------------------

dat_adults <- read.csv(paste0(name,path, file_adults))
dat_adolescents <- read.csv(paste0(name,path, file_adolescents))
dat_children <- read.csv(paste0(name,path, file_children))
  

# adult data
# create data sets for each project 
for (i in c(0:9, 99)) {
  assign(paste0("data_adults_p_", i), subset(dat_adults, project == i))
}

# adolescent data
# create data sets for each project
for (i in c(0:9, 99)) {
  assign(paste0("data_adolescents_p_", i), subset(dat_adolescents, project == i))
}

# chlidren data
# create data sets for each project
for (i in c(0:9, 99)) {
  assign(paste0("data_children_p_", i), subset(dat_children, project == i))
}


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

