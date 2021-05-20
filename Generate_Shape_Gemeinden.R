library(tidyverse)
library(rgdal)

BRD_Gemeinden <- readOGR(dsn = path.expand("S:/OE/FG28/205 Regionale Unterschiede/Referenzdaten/Kartendaten/BRD/Gemeinden/vg250_01-01.utm32s.shape.kompakt/vg250_kompakt_0101"), 
                     layer = "VG250_P")

library(broom)
BRD_Gemeinden_tidy <- tidy(BRD_Gemeinden)

ggplot(BRD_Gemeinden, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()