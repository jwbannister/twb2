library(rgdal)
library(dplyr)
library(ggplot2)
library(ggrepel)
load("~/dropbox/analysis/Rowens/data-clean/map_objects.RData")
old_locs <- read.csv("./output/twb2_monitoring_locations.csv")

shapefile_dirs <- c("20150610_20150611_TWB2_SC_Sites",
                    "20150612_TWB2_SC",
                    "20150623_T13_SC_Sites",
                    "20150730_TWB2_SC_Sites",
                    "20150731_TWB2_SC_Sites",
                    "20150803_T24_TWB2_SC_Sites")
locs <- list()
for (i in 1:length(shapefile_dirs)){
       path <- paste0("./data_raw/twb2_trimble_locations/", shapefile_dirs[i])
       locs[[i]] <- readOGR(path, "Point_ge")
}
locs_dfs <- lapply(locs, function(x) cbind(x@data, sp::coordinates(x)) %>% 
                   select(-GPS_Time)) 
df1 <- rbind(locs_dfs[[1]], locs_dfs[[2]], locs_dfs[[3]], locs_dfs[[4]],
             locs_dfs[[5]], locs_dfs[[6]]) 
df1$Comment <- as.character(df1$Comment)
df1$Comment <- toupper(df1$Comment)
df1$Comment <- gsub("-", "", df1$Comment)
new_locs <- rename(df1, site=Comment, trimble.x=coords.x1, trimble.y=coords.x2)
new_locs[new_locs$site=="1605", ]$site <- "1605A"
new_locs[new_locs$site=="1201", ]$site <- "1638B"
new_locs[new_locs$site=="1212", ]$site <- "1640B"
new_locs[new_locs$site=="1207", ]$site <- "1638A"
new_locs[new_locs$site=="1219", ]$site <- "1640A"
new_locs[new_locs$site=="1209", ]$site <- "1637B"
new_locs[new_locs$site=="1221", ]$site <- "1639B"
new_locs[new_locs$site=="1205", ]$site <- "1637A"
new_locs[new_locs$site=="1216", ]$site <- "1639A"
new_locs <- filter(new_locs, site!="1604C", site!="1604D", site!="1603C", 
                   site!="1603D", site!="1605C", site!="1605D",
                   site!="1602C", site!="1602D", site!="1488", site!="1487",
                   site!="1485", site!="1482", site!="1481", site!="1484",
                   site!="1483", site!="1486")
new_locs <- new_locs[!duplicated(new_locs$site), ]
write.csv(new_locs, file="./output/updated_twb2_locations.csv", row.names=FALSE)

question_sites <- filter(new_locs, !(site %in% levels(old_locs$id))) %>%
                  rename(x=trimble.x, y=trimble.y, id=site)
                
areas <- c(levels(dcms_polys$label))
border <- filter(dcms_polys, label %in% areas)
id_labels <- filter(dcms_labels, label %in% areas)

print(
      ggplot(border, aes(x=x, y=y)) +
        geom_path(aes(group=id)) +
#        geom_point(data=old_locs, mapping=aes(x=x, y=y), color="blue", alpha=0.5) +
        geom_point(data=new_locs, mapping=aes(x=trimble.x, y=trimble.y), color="red", alpha=0.5) +
        geom_text(data=id_labels, mapping=aes(x=X1, y=X2, label=label)) +
#        geom_text(data=old_locs, mapping=aes(x=x, y=y, label=id)) +
#        xlim(410000, 414000) + ylim(4020000, 4022500) +
        coord_fixed()
      )
