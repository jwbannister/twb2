# twb2_collection_load.R
# John Bannister
# 
# pull twb2 surface roughness collection data from Formation ArcGIS online 
# server, clean and output to csv.
load_all()
load_all("../Rowens")
# library(reshape2)
library(dplyr)
library(jsonlite)
# source("./scripts/clods_0922_functions.R")

df1 <- query_AGOL("OwensLake_TwB2", 
                  "Owens_TwB2_Surface_FieldCollection_Active", 0)
df1 <- flatten(df1$features)
colnames(df1) <- gsub("attributes.", "", colnames(df1))
colnames(df1) <- gsub("geometry.", "", colnames(df1))
colnames(df1) <- tolower(colnames(df1))
colnames(df1) <- gsub("_", ".", colnames(df1))
df1 <- select(df1, date=date.time, site.id, 
              observer=monitoring.initials,
              rs=ridge.spacing, fd=furrom.depth, irs=interrow.spacing, 
              rh=ridge.height, f1.pnt5to3, f1.gt3, f2.pnt5to3, f2.gt3, 
              f3.pnt5to3, f3.gt3, x, y, notes)
df1$date <- convert_ESRI_date(df1$date)
df1$site.id <- toupper(df1$site.id)
df1$site.id <- gsub("-", "", df1$site.id)
df1$site.id <- gsub(" ", "", df1$site.id)
df1$site.id <- gsub("_", "", df1$site.id)
for (i in 8:13){
  df1[ , i] <- factor_clods(df1[ , i])
}

# below this point, change filter date as required and add one-off data point 
# corrections as necessary.
df2 <- filter(df1, date > "2016-02-01")
temp <- filter(df1, is.na(date) & site.id=="1602A")
df2 <- rbind(df2, temp)

# do all site ids match known twb2 sites?
df2$site.id[!(df2$site.id %in% twb2_sites$site.id)]

# what known twb2 sites are missing from this collection?
twb2_sites$site.id[!(twb2_sites$site.id %in% df2$site.id)]
twb2_sites$site.id[!(twb2_sites$site.id %in% df2$site.id)]

write.csv(df2, file=paste0("./output/twb2_", Sys.Date(), ".csv"))
