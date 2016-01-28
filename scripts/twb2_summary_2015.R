# load and clean data from 09/22/2015 field observations

library(readxl)
library(reshape2)
library(dplyr)
source("./scripts/clods_0922_functions.R")

df1 <- read_excel("./data_raw/TwB2 2015 combined.xlsx")
df1$id <- gsub("-", "", df1$id)
df1$date <- as.Date(df1$date, "%m/%d/%y") 
df1 <- rename(df1, f1.pnt5to3=f1.small, f1.gt3=f1.large, f2.pnt5to3=f2.small, f2.gt3=f2.large,
              f3.pnt5to3=f3.small, f3.gt3=f3.large, subplot=area)

shp1 <- rgdal::readOGR("./data_raw/twb2_collection", "twb2_collection", stringsAsFactors=F)
shp1 <- cbind(shp1@data, sp::coordinates(shp1))
shp1 <- select(shp1, -GlobalID, -created_us, -created_da, -last_edite,
                 -last_edi_1, -OBJECTID)
colnames(shp1) <- tolower(colnames(shp1))
colnames(shp1) <- gsub("_", ".", colnames(shp1))
shp1 <- select(shp1, date=date.time, id=site.id, rs=ridge.spac, fd=furrom.dep, irs=interrow.s,
                 rh=ridge.heig, f1.pnt5to3, f1.gt3, f2.pnt5to3, f2.gt3, f3.pnt5to3, f3.gt3,
                 observer=monitoring, x=coords.x1, y=coords.x2, subplot)
shp1$date <- as.Date(shp1$date, "%Y/%m/%d") 
shp1$id <- toupper(shp1$id)
shp1$id <- gsub("-", "", shp1$id)
shp1$id <- gsub(" ", "", shp1$id)
shp1 <- shp1[!is.na(shp1$id), ]
shp1 <- filter(shp1, !(id=="TEST1" | id=="TEST2" | id=="TEST3"))
shp1$f1.pnt5to3 <- factor(shp1$f1.pnt5to3, levels=c(0:11), labels=c(0, seq(5, 95, 10), 100))
shp1$f1.gt3 <- factor(shp1$f1.gt3, levels=c(0:11), labels=c(0, seq(5, 95, 10), 100))
shp1$f2.pnt5to3 <- factor(shp1$f2.pnt5to3, levels=c(0:11), labels=c(0, seq(5, 95, 10), 100))
shp1$f2.gt3 <- factor(shp1$f2.gt3, levels=c(0:11), labels=c(0, seq(5, 95, 10), 100))
shp1$f3.pnt5to3 <- factor(shp1$f3.pnt5to3, levels=c(0:11), labels=c(0, seq(5, 95, 10), 100))
shp1$f3.gt3 <- factor(shp1$f3.gt3, levels=c(0:11), labels=c(0, seq(5, 95, 10), 100))
shp1$subplot <- toupper(shp1$subplot)
shp1$subplot <- gsub("T3NE", "T3-NE", shp1$subplot)
shp1$subplot <- gsub("T3SE", "T3-SE", shp1$subplot)
shp1$subplot <- gsub("T3SW", "T3-SW", shp1$subplot)
shp1[shp1$id=="1602B", ]$subplot <- "T3-SW"
shp1[shp1$id=="1609A", ]$subplot <- "T3-SE"
shp1[shp1$id=="1615A", ]$subplot <- "T2-4"
shp1[shp1$id=="1615B", ]$subplot <- "T2-4"
shp1[shp1$id=="1617A", ]$subplot <- "T3-NE"
shp1[shp1$id=="1617B", ]$subplot <- "T3-NE"
shp1[shp1$id=="1617C", ]$subplot <- "T3-NE"
shp1[shp1$id=="1605A", ]$subplot <- "T3-SE"
shp1$observer <- toupper(shp1$observer)

twb2_df <- rbind(df1, shp1)

site_locations <- select(twb2_df, id, x, y, subplot)
site_locations <- site_locations[!is.na(site_locations$x), ]
site_locations <- site_locations[!duplicated(site_locations$id), ]
site_locations$x <- round(site_locations$x, 2)
site_locations$y <- round(site_locations$y, 2)

twb2_df$rs <- round(twb2_df$rs, 0)
twb2_df$fd <- round(twb2_df$fd, 0)
twb2_df$irs <- round(twb2_df$irs, 0)
twb2_df$rh <- round(twb2_df$rh, 0)
twb2_df <- select(twb2_df, -x, -y, -subplot)
twb2_df$f1.pnt5to3 <- as.numeric(twb2_df$f1.pnt5to3)
twb2_df$f1.gt3 <- as.numeric(twb2_df$f1.gt3)
twb2_df$f2.pnt5to3 <- as.numeric(twb2_df$f2.pnt5to3)
twb2_df$f2.gt3 <- as.numeric(twb2_df$f2.gt3)
twb2_df$f3.pnt5to3 <- as.numeric(twb2_df$f3.pnt5to3)
twb2_df$f3.gt3 <- as.numeric(twb2_df$f3.gt3)
for (i in 1:nrow(twb2_df)){
  twb2_df$mean.pnt5to3[i] <- mean(c(twb2_df$f1.pnt5to3[i],
                                  twb2_df$f2.pnt5to3[i],
                                  twb2_df$f3.pnt5to3[i]))
  twb2_df$mean.gt3[i] <- mean(c(twb2_df$f1.gt3[i],
                              twb2_df$f2.gt3[i],
                              twb2_df$f3.gt3[i]))
  twb2_df$mean.total[i] <- mean(c(twb2_df$f1.pnt5to3[i] + twb2_df$f1.gt3[i],
                                twb2_df$f2.pnt5to3[i] + twb2_df$f2.gt3[i],
                                twb2_df$f3.pnt5to3[i] + twb2_df$f3.gt3[i]))
}

write.csv(site_locations, file="./output/twb2_monitoring_locations.csv", row.names=FALSE)
write.csv(twb2_df, file="./output/twb2_collections_2015.csv", row.names=FALSE)
