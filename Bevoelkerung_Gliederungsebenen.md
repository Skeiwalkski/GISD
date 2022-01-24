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
GVB <- id_data %>% select(Bevölkerung, GVBKennziffer) %>% group_by(GVBKennziffer) %>% mutate(Bev = sum(Bevölkerung)*100) %>% ungroup() %>% select(-Bevölkerung) %>% unique() %>% filter(Bev > 0)

Kreis <- id_data %>% select(Bevölkerung, Kreiskennziffer) %>% group_by(Kreiskennziffer) %>% mutate(Bev = sum(Bevölkerung)*100) %>% ungroup() %>% select(-Bevölkerung) %>% unique() %>% filter(Bev > 0)

ROR <- id_data %>% select(Bevölkerung, `Raumordnungsregion Nr`) %>% group_by(`Raumordnungsregion Nr`) %>% mutate(Bev = sum(Bevölkerung)*100) %>% ungroup() %>% select(-Bevölkerung) %>% unique() %>% filter(Bev > 0)

NUTS2 <- id_data %>% select(Bevölkerung, NUTS2) %>% group_by(NUTS2) %>% mutate(Bev = sum(Bevölkerung)*100) %>% ungroup() %>% select(-Bevölkerung) %>% unique() %>% filter(Bev > 0)


Tabelle_Bev <- cbind("Ebene" = "Gemeindeverbände (GVB)", "Anzahl" = nrow(GVB), "Mittlere Bevölkerungszahl" = round(mean(GVB$Bev)), "Minimum" = round(min(GVB$Bev)), "Maximum" = round(max(GVB$Bev)))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "Kreise und kreisfreie Städte (Kreise)", "Anzahl" = nrow(Kreis), "Mittlere Bevölkerungszahl" = round(mean(Kreis$Bev)), "Minimum" = round(min(Kreis$Bev)), "Maximum" = round(max(Kreis$Bev))))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "Raumordnungsregionen (ROR)", "Anzahl" = nrow(ROR), "Mittlere Bevölkerungszahl" = round(mean(ROR$Bev)), "Minimum" = round(min(ROR$Bev)), "Maximum" = round(max(ROR$Bev))))

Tabelle_Bev <- rbind(Tabelle_Bev, cbind("Ebene" = "NUTS-2", "Anzahl" = nrow(NUTS2), "Mittlere Bevölkerungszahl" = round(mean(NUTS2$Bev)), "Minimum" = round(min(NUTS2$Bev)), "Maximum" = round(max(NUTS2$Bev))))

kable(Tabelle_Bev, caption = "Räumliche Gliederungsebenen in Deutschland und ihre Bevölkerungszahlen")
```



Table: (\#tab:unnamed-chunk-2)Räumliche Gliederungsebenen in Deutschland und ihre Bevölkerungszahlen

|Ebene                                 |Anzahl |Mittlere Bevölkerungszahl |Minimum |Maximum |
|:-------------------------------------|:------|:-------------------------|:-------|:-------|
|Gemeindeverbände (GVB)                |4411   |18822                     |320     |3644830 |
|Kreise und kreisfreie Städte (Kreise) |401    |207044                    |34210   |3644830 |
|Raumordnungsregionen (ROR)            |96     |864839                    |195780  |3644830 |
|NUTS-2                                |38     |2184858                   |531260  |5202360 |
