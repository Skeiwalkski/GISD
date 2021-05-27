library(tidyverse)
library(rgdal)
library(broom)
require(sf)

library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


dsn <- "S:/OE/FG28/205 Regionale Unterschiede/Referenzdaten/Kartendaten/BRD/Gemeinden_VZ/vz250_01-01.utm32s.shape/vz250_0101"
lay <- "VZ250_GEM"


BRD_Gemeinden <- readOGR(dsn = dsn,                      
                         layer = lay)

BRD_Gemeinden_tidy <- tidy(BRD_Gemeinden, region = "AGS_G")

ggplot(BRD_Gemeinden, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()



