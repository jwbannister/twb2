# twb2_jan2016.R -- load and clean data from twb2 Jan 2016 collection
# John Bannister
# Created 01/17/2015 -- see git for revision history
# 

library(readxl)
library(reshape2)
library(dplyr)
source("./scripts/clods_0922_functions.R")

library(jsonlite)
json_data <- fromJSON("./data_raw/query.txt")
df1 <- flatten(json_data$features)
colnames(df1) <- gsub("attributes.", "", colnames(df1))
colnames(df1) <- gsub("geometry.", "", colnames(df1))
colnames(df1) <- tolower(colnames(df1))
colnames(df1) <- gsub("_", ".", colnames(df1))
df1 <- select(df1, date=date.time, site.id, 
              observer=monitoring.initials,
              rs=ridge.spacing, fd=furrom.depth, irs=interrow.spacing, 
              rh=ridge.height, f1.pnt5to3, f1.gt3, f2.pnt5to3, f2.gt3, 
              f3.pnt5to3, f3.gt3, x, y, notes)
df1$date <- df1$date / 1000
df1$date <- sapply(df1$date, 
            function (x) as.character(as.POSIXct(x, origin="1970-01-01")))
df1$date <- as.Date(df1$date, format="%Y-%m-%d %H:%M:%S")
df1$site.id <- toupper(df1$site.id)
df1$site.id <- gsub("-", "", df1$site.id)
df1$site.id <- gsub(" ", "", df1$site.id)
df1$site.id <- gsub("_", "", df1$site.id)
df1 <- filter(df1, date > "2016-01-01" | is.na(date))
df1 <- df1[!is.na(df1$site.id), ]
df1[(df1$site.id=="1639A"), ]$date <-  as.Date("2016-01-15")
df1[(df1$site.id=="1606C"), ]$date <-  as.Date("2016-01-12")
df1[(df1$site.id=="1631A"), ]$date <-  as.Date("2016-01-14")
df1[(df1$site.id=="1631B"), ]$date <-  as.Date("2016-01-14")
df1[(df1$site.id=="1633A"), ]$date <-  as.Date("2016-01-14")
df1[(df1$site.id=="1604"), ]$site.id <-  "1604A"
df1[(df1$site.id=="1614"), ]$site.id <-  "1614A"
df1[(df1$site.id=="638A"), ]$site.id <-  "1638A"

write.csv(df1, file="./output/jan2016_twb2_collection.csv")
