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
|Gemeindekennziffer / Kreiskennziffer  |Natürliche Zahl |größer 0                                                    |Individuelle Identifikationsnummer für jede/n Gemeinde / Landkreis / ... in Deutschland |
|GISD_Score                            |Natürliche Zahl |0 bis 1                                                     |Vergebener Score anhand der Sozio-Ökonomischen Deprivation der jeweiligen Raumheinheit  |
|GISD_5                                |Natürliche Zahl |1 bis 5                                                     |Vergebenes Quintil nach Platzierung der Raumeinheit auf Score-Verteilung                |
|GISD_10                               |Natürliche Zahl |1 bis 10                                                    |Vergebenes Dezil nach Platzierung der Raumeinheit auf Score-Verteilung                  |
|Name der Gemeinde / des Kreises / ... |Text            |Flensburg, Stadt; Kiel, Landeshauptstadt; ... Windischleuba |Name der jeweiligen Raumeinheit                                                         |
|Jahr                                  |Natürliche Zahl |1998 bis 2019                                               |Jeweiliges Jahr der Datenentnahme / GISD-Score im jeweiligen Jahr                       |
