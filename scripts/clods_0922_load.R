# load and clean data from 09/22/2015 field observations

library(readxl)
library(reshape2)
library(dplyr)
source("./scripts/clods_0922_functions.R")

# LOAD AND CLEAN TWB2
obs_df <- read_excel("./data_raw/9-22-2015 observations.xlsx")

clods_df <- select(obs_df, -rs, -fd, -irs, -rh)
clods_df$interior <- sapply(clods_df$interior, function(x) if (x=="F") FALSE else TRUE)
clods_df$observer <- factor(clods_df$observer)

for (i in 2:7){
  clods_df[[i]] <- factorClods(clods_df[[i]])
  clods_df[[i]] <- as.numeric(as.character(clods_df[[i]]))
}

photo_df <- read_excel("./data_raw/photo clods.xlsx")

photo_df$interior <- sapply(photo_df$interior, function(x) if (x=="F") FALSE else TRUE)
photo_df$observer <- factor(photo_df$observer)

save(clods_df, photo_df, file="./data/clods_sept_22.RData")
