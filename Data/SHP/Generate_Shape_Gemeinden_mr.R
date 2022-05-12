library(tidyverse)
library(rgdal)
library(broom)
library(maptools)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


BRD_Gemeinden <- readOGR(dsn = path.expand("S:/OE/FG28/205 Regionale Unterschiede/Referenzdaten/Kartendaten/BRD/Verwaltungsgrenzen/2011_UTM32"), 
                     layer = "VG250_Bundeslaender")



BRD_Gemeinden <- tidy(BRD_Gemeinden, region = "AGS")

saveRDS(BRD_Gemeinden, "C:/git_projects/GISD/Data/SHP/BRD_BuLa.rds")

ggplot(BRD_Gemeinden, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()
