---
title: "Räumliche Gleiderungsebenen - Bevölerungszahlen"
author: "Marvin Reis"
date: "21 1 2022"
output:
  bookdown::html_document2:
    keep_md: true
    code_folding: hide
    number_sections: false
    fig_caption: true
    theme: cerulean
    highlight: tango
---







```r
Gemeinden <- gesamt %>% select(Bevölkerung, Gemeindekennziffer) %>% unique() %>% filter(Bevölkerung > 0)

GVB <- gesamt %>% select(Bevölkerung, GVBKennziffer) %>% group_by(GVBKennziffer) %>% mutate(Bevölkerung = sum(Bevölkerung)) %>% ungroup() %>% unique() %>% filter(Bevölkerung > 0)

Kreis <- gesamt %>% select(Bevölkerung, Kreiskennziffer) %>% group_by(Kreiskennziffer) %>% mutate(Bevölkerung = sum(Bevölkerung)) %>% ungroup() %>% unique() %>% filter(Bevölkerung > 0)

ROR <- gesamt %>% select(Bevölkerung, `Raumordnungsregion Nr`) %>% group_by(`Raumordnungsregion Nr`) %>% mutate(Bevölkerung = sum(Bevölkerung)) %>% ungroup() %>% unique() %>% filter(Bevölkerung > 0)

NUTS2 <- gesamt %>% select(Bevölkerung, NUTS2) %>% group_by(NUTS2) %>% mutate(Bevölkerung = sum(Bevölkerung)) %>% ungroup() %>% unique() %>% filter(Bevölkerung > 0)


Tabelle_Bev <- cbind("Ebene" = "Gemeinden", "Anzahl" = nrow(Gemeinden), "Mittlere Bevölkerungszahl" = round(mean(Gemeinden$Bevölkerung)), "Minimum" = round(min(Gemeinden$Bevölkerung)), "Maximum" = round(max(Gemeinden$Bevölkerung)))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "Gemeindeverbände (GVB)", "Anzahl" = nrow(GVB), "Mittlere Bevölkerungszahl" = round(mean(GVB$Bevölkerung)), "Minimum" = round(min(GVB$Bevölkerung)), "Maximum" = round(max(GVB$Bevölkerung))))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "Kreise und kreisfreie Städte (Kreise)", "Anzahl" = nrow(Kreis), "Mittlere Bevölkerungszahl" = round(mean(Kreis$Bevölkerung)), "Minimum" = round(min(Kreis$Bevölkerung)), "Maximum" = round(max(Kreis$Bevölkerung))))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "Raumordnungsregionen (ROR)", "Anzahl" = nrow(ROR), "Mittlere Bevölkerungszahl" = round(mean(ROR$Bevölkerung)), "Minimum" = round(min(ROR$Bevölkerung)), "Maximum" = round(max(ROR$Bevölkerung))))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "NUTS-2", "Anzahl" = nrow(NUTS2), "Mittlere Bevölkerungszahl" = round(mean(NUTS2$Bevölkerung)), "Minimum" = round(min(NUTS2$Bevölkerung)), "Maximum" = round(max(NUTS2$Bevölkerung))))

kable(Tabelle_Bev, caption = "Räumliche Gliederungsebenen in Deutschland und ihre Bevölkerungszahlen")
```



Table: (\#tab:unnamed-chunk-2)Räumliche Gliederungsebenen in Deutschland und ihre Bevölkerungszahlen

|Ebene                                 |Anzahl |Mittlere Bevölkerungszahl |Minimum |Maximum |
|:-------------------------------------|:------|:-------------------------|:-------|:-------|
|Gemeinden                             |10799  |7701                      |10      |3669491 |
|Gemeindeverbände (GVB)                |4411   |18854                     |324     |3669491 |
|Kreise und kreisfreie Städte (Kreise) |401    |207398                    |34193   |3669491 |
|Raumordnungsregionen (ROR)            |96     |866320                    |194363  |3669491 |
|NUTS-2                                |38     |2188598                   |533113  |5207457 |
