load_all()
library(dplyr)

df1 <- rgdal::readOGR("./data_raw/surface_roughness_locations", 
                    "surface_roughness_locations")
df1 <- df1@data
df1 <- select(df1, site.id, area=Label_1, x, y)
df1[df1$area=="T3SW", ]$area <- "T3-SW"
df1[df1$area=="T3SE", ]$area <- "T3-SE"
df1[df1$area=="T3NE", ]$area <- "T3-NE"
df1[df1$area=="T24 ADD", ]$area <- "T24-ADD"

twb2_sites <- df1
save(twb2_sites, file="./data/twb2_sites.RData")
