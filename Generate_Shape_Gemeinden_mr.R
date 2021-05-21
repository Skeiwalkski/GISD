library(tidyverse)
library(rgdal)
library(broom)
library(maptools)

BRD_Gemeinden <- readOGR(dsn = path.expand("S:/OE/FG28/205 Regionale Unterschiede/Referenzdaten/Kartendaten/BRD/2012/VG250-EW/vg250-ew_ebenen"), 
                     layer = "vg250_gem")


BRD_Gemeinden <- tidy(BRD_Gemeinden, region = "AGS")

saveRDS(BRD_Gemeinden, "C:/git_projects/GISD/BRD_Gemeinden.rds")

ggplot(BRD_Gemeinden, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()
