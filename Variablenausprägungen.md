---
title: "Variablenausprägungen"
author: "Marvin Reis"
date: "21 4 2022"
output:
  bookdown::html_document2:
    keep_md: true
    code_folding: hide
    toc: true
    toc_float: true
    toc_depth: 2
    number_sections: false
    theme: cerulean
    highlight: tango
---


```r
library("knitr")
```

## Variablenausprägungen


```r
Tab_Auspraegungen <- cbind("Variable" = "Gemeindekennziffer / Kreiskennziffer", "Typ" = "Natürliche Zahl", "Ausprägung" = "größer 0", "Beschreibung" = "Individuelle Identifikationsnummer für jede/n Gemeinde / Landkreis / ... in Deutschland")

Tab_Auspraegungen <- rbind(Tab_Auspraegungen, cbind("Variable" = "GISD_Score", "Typ" = "Natürliche Zahl", "Ausprägung" = "0 bis 1", "Beschreibung" = "Vergebener Score anhand der Sozio-Ökonomischen Deprivation der jeweiligen Raumheinheit"))

Tab_Auspraegungen <- rbind(Tab_Auspraegungen, cbind("Variable" = "GISD_5", "Typ" = "Natürliche Zahl", "Ausprägung" = "1 bis 5", "Beschreibung" = "Vergebenes Quintil nach Platzierung der Raumeinheit auf Score-Verteilung"))

Tab_Auspraegungen <- rbind(Tab_Auspraegungen, cbind("Variable" = "GISD_10", "Typ" = "Natürliche Zahl", "Ausprägungen" = "1 bis 10", "Beschreibung" = "Vergebenes Dezil nach Platzierung der Raumeinheit auf Score-Verteilung"))

Tab_Auspraegungen <- rbind(Tab_Auspraegungen, cbind("Variable" = "Name der Gemeinde / des Kreises / ...", "Typ" = "Text", "Ausprägungen" = "Flensburg, Stadt; Kiel, Landeshauptstadt; ... Windischleuba", "Ausprägungen" = "Name der jeweiligen Raumeinheit"))

Tab_Auspraegungen <- rbind(Tab_Auspraegungen, cbind("Variable" = "Jahr", "Typ" = "Natürliche Zahl", "Ausprägung" = "1998 bis 2019", "Beschreibung" = "Jeweiliges Jahr der Datenentnahme / GISD-Score im jeweiligen Jahr"))

kable(Tab_Auspraegungen)
```



|Variable                              |Typ             |Ausprägung                                                  |Beschreibung                                                                            |
|:-------------------------------------|:---------------|:-----------------------------------------------------------|:---------------------------------------------------------------------------------------|
|Gemeindekennziffer, GVBKennziffer, Kreiskennziffer, NUTS2, Raumordnungsregion Nr, PLZ2-5 |Natürliche Zahl mit führenden Nullen oder String (NUTS-2) |größer 0 bzw. DE01,...                                                   |amtliche Regionalschlüssel für Gemeinden, Gemeindeverbände , Stadt- und Landkreise, NUTS-2, Raumordnungsregion sowie Postleitszahl 2-, 3-, 4-, 5-Steller|
|GISD_Score                            |Dezimalzahl |0 bis 1                                                     |Berechneter Score Sozioökonomischer Deprivation für die jeweilige Raumheinheit  |
|GISD_5                                |Natürliche Zahl |1 bis 5                                                     |Zugewiesenes GISD-Quintil der Raumeinheit gemäß der jährlichen Verteilung auf der jeweiligen räumlichen Ebene             |
|GISD_10                               |Natürliche Zahl |1 bis 10                                                    |Zugewiesenes GISD-Dezil der Raumeinheit gemäß der jährlichen Verteilung auf der jeweiligen räumlichen Ebene                   |
|GISD_k                                |Natürliche Zahl |1 bis 3                                             |1 unterstes Quintil, 2 drei mittlere Qunitile, 3 oberstes Qunitil                 |                      |
|Name der Raumeinheit |Text            |Flensburg, Stadt; Kiel, Landeshauptstadt; ... Windischleuba |Name der Raumeinheit                                                         |
|Jahr                                  |Natürliche Zahl |1998 bis 2019                                               |Beobachtungsjahr                   |                      |
