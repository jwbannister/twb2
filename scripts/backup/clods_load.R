# load and clean available data from tillage areas for clod analysis

library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)

# LOAD AND CLEAN TWB2
twb2_df <- read_excel("../data/TwB2_Base_Combine_062315_to JB.xlsx")

names(twb2_df) <- tolower(names(twb2_df))
names(twb2_df) <- gsub("%", ".prcnt", names(twb2_df))
names(twb2_df)[9:14] <- gsub(" ", "", names(twb2_df)[9:14])
names(twb2_df) <- gsub("-", "_", names(twb2_df))
names(twb2_df)[9:14] <- gsub("_pnt_0.5_3", ".small", names(twb2_df)[9:14])
names(twb2_df)[9:14] <- gsub("_gt3", ".large", names(twb2_df)[9:14])
names(twb2_df) <- gsub(" ", ".", names(twb2_df))
names(twb2_df) <- gsub("_", ".", names(twb2_df))

twb2_df <- filter(twb2_df, !is.na(site))
twb2_df <- select(twb2_df, c(1, 2, 3, 9:14))
twb2_df$date <- as.Date(twb2_df$date, origin="1899-12-30")
twb2_df$f1.total <- twb2_df$f1.small.prcnt + twb2_df$f1.large.prcnt
twb2_df$f2.total <- twb2_df$f2.small.prcnt + twb2_df$f2.large.prcnt
twb2_df$f3.total <- twb2_df$f3.small.prcnt + twb2_df$f3.large.prcnt

twb2_melt <- melt(twb2_df, id.vars=c("site", "subplot", "date"))
twb2_melt$frame <- substr(twb2_melt$variable, 1, 2)
twb2_melt$clods <- substr(twb2_melt$variable, 4, 8)
twb2_melt <- select(twb2_melt, -variable)
names(twb2_melt)[4] <- "percent"

# LOAD AND CLEAN T12
t12_df <- read_excel("../data/T12 Clod Data_v3.xlsx")
colnames(t12_df) <- t12_df[1, ]
t12_df <- t12_df[-1, ]
t12_df <- t12_df[ , 1:9]
names(t12_df) <- tolower(names(t12_df))
names(t12_df) <- gsub("_", ".", names(t12_df))
names(t12_df) <- gsub(" ", ".", names(t12_df))
names(t12_df) <- gsub("\\+", "", names(t12_df))
t12_df$date <- as.Date(as.numeric(t12_df$date), origin="1899-12-30")
t12_df$pct.clods.0to3 <- as.numeric(t12_df$pct.clods.0to3)
t12_df$pct.clods.3to6 <- as.numeric(t12_df$pct.clods.3to6)
t12_df$pct.clods.6 <- as.numeric(t12_df$pct.clods.6)
t12_df <- t12_df %>% mutate(large.prcnt=pct.clods.3to6 + pct.clods.6, small.prcnt=pct.clods.0to3) %>%
  select(-pct.clods.0to3, -pct.clods.3to6, -pct.clods.6, -season)
t12_df <- t12_df[complete.cases(t12_df), ]
colnames(t12_df)[4] <- "site"
t12_df$site<- substr(t12_df$site, 1, 4)
t12_melt <- melt(t12_df, id.vars=c("area", "type", "collection.id", "site", "date"))
t12_melt$size <- substr(t12_melt$variable, 1, 5)
t12_melt <- select(t12_melt, -variable)
names(t12_melt)[6] <- "percent"
names(t12_melt)[7] <- "clod.size"
t12_melt$subplot <- paste0("T12-", t12_melt$area)
t12_melt <- select(t12_melt, -area)

save(twb2_df, twb2_melt, file="../output/twb2_clod_data.RData")
save(t12_df, t12_melt, file="../output/t12_clod_date.RDate")
