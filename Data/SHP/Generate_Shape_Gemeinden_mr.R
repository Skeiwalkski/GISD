library(tidyverse)
library(rgdal)
library(broom)
library(maptools)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


BRD_Gemeinden <- readOGR(dsn = path.expand("S:/OE/FG28/205 Regionale Unterschiede/Referenzdaten/Kartendaten/BRD/vz250_01-01.utm32s.shape1/vz250_01-01.utm32s.shape/vz250_0101"), 
                     layer = "VZ250_GEM")



BRD_Gemeinden <- tidy(BRD_Gemeinden, region = "AGS_G")

saveRDS(BRD_Gemeinden, "C:/git_projects/GISD/Data/SHP/BRD_Gemeinden.rds")

ggplot(BRD_Gemeinden, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()
