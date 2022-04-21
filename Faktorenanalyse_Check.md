---
title: "Faktorenanalyse_Check"
author: "Marvin Reis"
date: "18 3 2021"
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

# Info

In diesem Codeblog analysieren wir die Faktorenanalyse des GISD. 







```r
library(tidyverse) # Tidyverse Methods
library(bookdown) 
library(readxl) # Read Excel
library(pastecs) # descriptive stats
library(knitr)
library(ggplot2)

Impdata.imputed <- readRDS("C:/git_projects/GISD/Outfiles/Impdata_check.rds")
```



```r
# Variablenliste für die Faktorenanalyse 
#print(listofdeterminants)
TS_Arbeitswelt <- Impdata.imputed  %>% ungroup() %>% select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst) 
TS_Einkommen   <- Impdata.imputed %>% select(Einkommensteuer,Haushaltseinkommen,Schuldnerquote) 
# für den Vergleich der Ergebnisse wird zunächst ein Datensatz für die Variablenauswahl der Revision 2019 generiert
TS_Bildung <- Impdata.imputed %>% select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 

TS_Bildung_adj <- Impdata.imputed %>% select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss_adj) 

# Check dieser Lösung für das 2014er Sample 
#TS_Bildung_r2014 <- Impdata.imputed %>% filter(Jahr<2015) %>%  #dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 

TS_Bildung_4items <- Impdata.imputed %>% select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss, SchulabgaengermitHochschulreife)

#TS_Bildung_4items_without_BoA <- Impdata.imputed %>% #dplyr::select(BeschaeftigtemitakadAbschluss,SchulabgaengerohneAbschluss, SchulabgaengermitHochschulreife) 
```

# Verschiedene Faktorenanalysen des GISD {.tabset}

## Faktorenanalyse gepoolte Querschnitte

Es werden Hauptkomponentenanalysen für jede der drei Subskalen auf Basis der imputierten Daten geschätzt. 


```r
# PCA für die Arbeitsweltdimension
#TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE)
#TS_Arbeitswelt.pca
	# Option retx erzeugt rotierte Lösung

TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)
# die Option rank erlaubt die Beschränkung der Anzahl an Komponenten (Faktoren)
#TS_Arbeitswelt.pca
# nur die erste Komponente mit Eigenwert über 1
	# (prcomp gibt standardmäßig Sdev statt Varianz aus)
#plot(TS_Arbeitswelt.pca, main = "Varianz der Faktoren für Arbeitswelt")

	# screeplot - bei nur drei Variablen wird ein Balkendiagramm angezeigt
# die Faktorladungen der drei Hauptkomponenten für Arbeitswelt 
# die Ladungen der ersten Komponente enstprechen der Erwartung



# PCA für die Einkommensdimension
#TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE) 
#plot(TS_Einkommen.pca)
TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
#TS_Einkommen.pca



# PCA für die Bildungsdimension
#TS_Bildung.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE) 
#plot(TS_Bildung.pca)
TS_Bildung.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE, rank. =1 ) 
#TS_Bildung.pca

# Alternativ Bildungskomponente mit BeschaeftigtemitakadAbschluss,SchulabgaengermitHochschulreife,SchulabgaengerohneAbschluss
#TS_Bildung_new.pca <- prcomp(TS_Bildung_4items_without_BoA, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 

# für die Bildung deutet die Analyse eher auf zwei Komponenten hin
# die Faktorladung für SchulabgaengerohneAbschluss ist auf dem zweiten Faktor schwach
# es wird die Komponente ausgewählt, bei der Beschaeftigte mit akad Abschluss positiv korreliert und 
# BeschaeftigteohneAbschluss und SchulabgaengerohneAbschluss negativ
# regionale Deprivation als Merkmal geringer Anteile von Akademikern bei gleichzeitigen hohen Anteilen 
# von Beschaeftigten ohne Abschluss und Schulabgaengern ohne Abschluss




# Check der Bildungskomponente in Revision 2018 (Daten für 2014)
#TS_Bildung_r2014.pca <- prcomp(TS_Bildung_r2014, center = TRUE, scale. = TRUE, retx=TRUE) 
#TS_Bildung_r2014.pca
# 
TS_Bildung_4items.pca <- prcomp(TS_Bildung_4items, center = TRUE, scale. = TRUE, retx=TRUE, rank. =1 )
# plot(TS_Bildung_4items.pca)
# TS_Bildung_4items.pca

#tab_Bildung_4items <- cbind("Faktor" = "F", "SD" = TS_Bildung_4items.pca$sdev^2)
#tab_Bildung_4items <- cbind(as.data.frame(tab_Bildung_4items))
#tab_Bildung_4items$Faktor <- c("Faktor 1", "Faktor 2", "Faktor 3")
#colnames(tab_Bildung_4items) <- c("Faktoren für Bildung", "Varianz")

TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. =1 ) 
```

### Eigenwerte der Komponenten


```r
par(mfrow=c(1, 3))
plot(TS_Arbeitswelt.pca, main = "Arbeitswelt (Eigenvektoren)", ylim=c(0,2.2))
plot(TS_Einkommen.pca, main = "Einkommen (Eigenverktoren)", ylim=c(0,2.2))
plot(TS_Bildung.pca, main = "Bildung (Eigenvektoren)", ylim=c(0,2.2))
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-1-1.png)<!-- -->



```r
Varianz_tab <- cbind("F_A" = "Faktor1", "Var_A" = round(TS_Arbeitswelt.pca$sdev^2, digits = 3), "Var_E" = round(TS_Einkommen.pca$sdev^2, digits = 3),  "Var_B" = round(TS_Bildung.pca$sdev^2, digits = 3))

Varianz_tab <- cbind(as.data.frame(Varianz_tab))

Varianz_tab$F_A <- c("Faktor 1", "Faktor 2", "Faktor 3")

colnames(Varianz_tab) <- c("Faktoren", "Varianz Arbeitswelt", "Varianz Einkommen", "Varianz Bildung")

kable(Varianz_tab, caption = "Varianz der Faktoren (Eigenverktoren)")
```



Table: (\#tab:unnamed-chunk-2)Varianz der Faktoren (Eigenverktoren)

|Faktoren |Varianz Arbeitswelt |Varianz Einkommen |Varianz Bildung |
|:--------|:-------------------|:-----------------|:---------------|
|Faktor 1 |1.691               |2.053             |1.422           |
|Faktor 2 |0.863               |0.727             |1.191           |
|Faktor 3 |0.447               |0.221             |0.387           |

Die PCA zeigt drei Hauptkomponenten für die Dimension Arbeitswelt. Nur die erste Komponente hat einen Eigenwert über eins. Die Faktorladungen der drei Variablen (Beschäftigungsquote, Arbeitslosigkeit und Bruttoverdienst) entsprechen dabei den Ertwartungen.

Die PCA zeigt drei Hauptkomponenten für die Dimension Einkommen. Nur die erste Komponente hat einen Eigenwert über zwei. Die Faktorladungen der drei Variablen (Einkommensteuer, Haushaltseinkommen und Schuldnerquote) entsprechen dabei den Ertwartungen.

Die PCA zeigt  drei Hauptkomponenten für die Dimension Bildung. Sowohl die erste al auch die zweite Komponente haben dabei einen Eigenwert über eins. Die Faktorladungen der drei Variablen (Beschäftigte mit Abschluss, Schäftigte ohne Abschlus und Schulabgänger ohne Abschluss) entsprechen somit nicht den Erwartungen.

Fazit: Die Betrachtung der Eigenwerte weist bei der Bildungsdimension auf zwei Hauptkomponenten hin.
Hier sollte eine detailliertere Analyse folgen. 

### Faktorladungen


```r
# Componentoverview
GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Faktorladung"=round((TS_Arbeitswelt.pca$rotation*sqrt(abs(TS_Arbeitswelt.pca$sdev^2))), digits = 3),"Component"=round(TS_Arbeitswelt.pca$rotation, digits = 3))
#cbind erstellt Spaltenvektoren mit den Infos aus Teildimension, den (rotierten) Faktorladungen und den Components

GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Faktorladung"=round((TS_Einkommen.pca$rotation*sqrt(abs(TS_Einkommen.pca$sdev^2))), digits = 3),"Component"=round(TS_Einkommen.pca$rotation, digits = 3)))
# rbind erstellt Zeilenvektoren, diese werden hier in die bereits vorhandenen Spaltenvektoren eingebunden

GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung","Faktorladung"=round((TS_Bildung.pca$rotation*sqrt(abs(TS_Bildung.pca$sdev^2))), digits = 3),"Component"=round(TS_Bildung.pca$rotation, digits = 3)))
# auch für die Teildimension Bildung werden Zeilenvektoren eingebunden

GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))
# als letztes wird die Matrix in einen Dataframe übersetzt

rownames(GISD_Komponents) <- NULL
# die überflüssigen Zeilennamen werden gestrichen

colnames(GISD_Komponents) <- c("Variable","Dimension","Faktorladung","Component")

GISD_Komponents$prop_dem <- round(as.numeric(GISD_Komponents$Component)^2*100,digits=1)
# eine weitere Spalte Proportion wird eingefügt mit prozentualen Anteilswerten (eine Nachkommastelle)

GISD_Komponents$prop_GISD <- round(as.numeric(GISD_Komponents$prop_dem)/3, digits = 1)

colnames(GISD_Komponents) <- c("Variable","Dimension","Faktorladung","Component", "Anteil Teilscore", "Anteil GISD")

GISD_Komponents$Variable <- c("Beschäftigtenquote", "Arbeitslosigkeit", "Bruttoverdienst", "Einkommensteuer", "Haushaltseinkommen", "Schuldnerquote", "Beschäftigte mit Abschluss", "Beshäftigte ohne Abschluss", "Schulabgänger ohne Abschluss")

kable(GISD_Komponents, caption = "Faktorladungen und Anteile an den Teilscores sowie am Index")
```



Table: (\#tab:unnamed-chunk-3)Faktorladungen und Anteile an den Teilscores sowie am Index

|Variable                     |Dimension   |Faktorladung |Component | Anteil Teilscore| Anteil GISD|
|:----------------------------|:-----------|:------------|:---------|----------------:|-----------:|
|Beschäftigtenquote           |Arbeitswelt |0.553        |0.426     |             18.1|         6.0|
|Arbeitslosigkeit             |Arbeitswelt |-0.613       |-0.66     |             43.6|        14.5|
|Bruttoverdienst              |Arbeitswelt |0.414        |0.62      |             38.4|        12.8|
|Einkommensteuer              |Einkommen   |0.903        |0.631     |             39.8|        13.3|
|Haushaltseinkommen           |Einkommen   |0.537        |0.63      |             39.7|        13.2|
|Schuldnerquote               |Einkommen   |-0.213       |-0.454    |             20.6|         6.9|
|Beschäftigte mit Abschluss   |Bildung     |0.485        |0.407     |             16.6|         5.5|
|Beshäftigte ohne Abschluss   |Bildung     |0.539        |0.494     |             24.4|         8.1|
|Schulabgänger ohne Abschluss |Bildung     |-0.478       |-0.769    |             59.1|        19.7|

## Faktoranalyse gepoolte Querschnitte
(Mit adjusteted Bildungskomponente)

### Eigenwerte der Komponenten

```r
par(mfrow=c(1, 3))
plot(TS_Arbeitswelt.pca, main = "Arbeitswelt (Eigenvektoren)", ylim=c(0,2.2))
plot(TS_Einkommen.pca, main = "Einkommen (Eigenverktoren)", ylim=c(0,2.2))
plot(TS_Bildung_adj.pca, main = "Bildung adj. (Eigenvektoren)", ylim=c(0,2.2))
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
Varianz_tab <- cbind("F_A" = "Faktor1", "Var_A" = round(TS_Arbeitswelt.pca$sdev^2, digits = 3), "Var_E" = round(TS_Einkommen.pca$sdev^2, digits = 3),  "Var_B" = round(TS_Bildung_adj.pca$sdev^2, digits = 3))

Varianz_tab <- cbind(as.data.frame(Varianz_tab))

Varianz_tab$F_A <- c("Faktor 1", "Faktor 2", "Faktor 3")

colnames(Varianz_tab) <- c("Faktoren", "Varianz Arbeitswelt", "Varianz Einkommen", "Varianz Bildung (adj.)")

kable(Varianz_tab, caption = "Varianz der Faktoren (Eigenverktoren)")
```



Table: (\#tab:unnamed-chunk-5)Varianz der Faktoren (Eigenverktoren)

|Faktoren |Varianz Arbeitswelt |Varianz Einkommen |Varianz Bildung (adj.) |
|:--------|:-------------------|:-----------------|:----------------------|
|Faktor 1 |1.691               |2.053             |1.406                  |
|Faktor 2 |0.863               |0.727             |1.191                  |
|Faktor 3 |0.447               |0.221             |0.403                  |

### Faktoradungen

```r
# Componentoverview
GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Faktorladung"=round((TS_Arbeitswelt.pca$rotation*sqrt(abs(TS_Arbeitswelt.pca$sdev^2))), digits = 3),"Component"=round(TS_Arbeitswelt.pca$rotation, digits = 3))

GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Faktorladung"=round((TS_Einkommen.pca$rotation*sqrt(abs(TS_Einkommen.pca$sdev^2))), digits = 3),"Component"=round(TS_Einkommen.pca$rotation, digits = 3)))

GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung","Faktorladung"=round((TS_Bildung_adj.pca$rotation*sqrt(abs(TS_Bildung_adj.pca$sdev^2))), digits = 3),"Component"=round(TS_Bildung_adj.pca$rotation, digits = 3)))

GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))

rownames(GISD_Komponents) <- NULL

colnames(GISD_Komponents) <- c("Variable","Dimension","Faktorladung","Component")

GISD_Komponents$prop_dem <- round(as.numeric(GISD_Komponents$Component)^2*100,digits=1)

GISD_Komponents$prop_GISD <- round(as.numeric(GISD_Komponents$prop_dem)/3, digits = 1)

colnames(GISD_Komponents) <- c("Variable","Dimension","Faktorladung","Component", "Anteil Teilscore", "Anteil GISD")

GISD_Komponents$Variable <- c("Beschäftigtenquote", "Arbeitslosigkeit", "Bruttoverdienst", "Einkommensteuer", "Haushaltseinkommen", "Schuldnerquote", "Beschäftigte mit Abschluss", "Beshäftigte ohne Abschluss", "Schulabgänger ohne Abschluss (adj.)")

kable(GISD_Komponents, caption = "Faktorladungen und Anteile an den Teilscores sowie am Index")
```



Table: (\#tab:unnamed-chunk-6)Faktorladungen und Anteile an den Teilscores sowie am Index

|Variable                            |Dimension   |Faktorladung |Component | Anteil Teilscore| Anteil GISD|
|:-----------------------------------|:-----------|:------------|:---------|----------------:|-----------:|
|Beschäftigtenquote                  |Arbeitswelt |0.553        |0.426     |             18.1|         6.0|
|Arbeitslosigkeit                    |Arbeitswelt |-0.613       |-0.66     |             43.6|        14.5|
|Bruttoverdienst                     |Arbeitswelt |0.414        |0.62      |             38.4|        12.8|
|Einkommensteuer                     |Einkommen   |0.903        |0.631     |             39.8|        13.3|
|Haushaltseinkommen                  |Einkommen   |0.537        |0.63      |             39.7|        13.2|
|Schuldnerquote                      |Einkommen   |-0.213       |-0.454    |             20.6|         6.9|
|Beschäftigte mit Abschluss          |Bildung     |0.484        |0.408     |             16.6|         5.5|
|Beshäftigte ohne Abschluss          |Bildung     |0.534        |0.489     |             23.9|         8.0|
|Schulabgänger ohne Abschluss (adj.) |Bildung     |-0.49        |-0.771    |             59.4|        19.8|



## Faktorenanalyse gepoolte  
Querschnitte (4 Items Bildung)

### Eigenwerte der Komponenten

```r
par(mfrow=c(1, 3))
plot(TS_Arbeitswelt.pca, main = "Arbeitswelt (Eigenvektoren)", ylim=c(0,2.2))
plot(TS_Einkommen.pca, main = "Einkommen (Eigenverktoren)", ylim=c(0,2.2))
plot(TS_Bildung_4items.pca, main = "Bildung (Eigenvektoren)", ylim=c(0,2.2))
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
Varianz_B4_tab <- cbind("F_A" = "Faktor1", "Var_A" = round(TS_Arbeitswelt.pca$sdev^2, digits = 3), "Var_E" = round(TS_Einkommen.pca$sdev^2, digits = 3),  "Var_B" = round(TS_Bildung_4items.pca$sdev^2, digits = 3))

Varianz_B4_tab[4,2] = NA
Varianz_B4_tab[4,3] = NA

Varianz_B4_tab <- cbind(as.data.frame(Varianz_B4_tab))

Varianz_B4_tab$F_A <- c("Faktor 1", "Faktor 2", "Faktor 3", "Faktor 4")

colnames(Varianz_B4_tab) <- c("Faktoren", "Varianz Arbeitswelt", "Varianz Einkommen", "Varianz Bildung")

kable(Varianz_B4_tab, caption = "Varianz der Faktoren (Eigenverktoren)")
```



Table: (\#tab:unnamed-chunk-8)Varianz der Faktoren (Eigenverktoren)

|Faktoren |Varianz Arbeitswelt |Varianz Einkommen |Varianz Bildung |
|:--------|:-------------------|:-----------------|:---------------|
|Faktor 1 |1.691               |2.053             |1.83            |
|Faktor 2 |0.863               |0.727             |1.374           |
|Faktor 3 |0.447               |0.221             |0.411           |
|Faktor 4 |NA                  |NA                |0.385           |

### Faktorladungen


```r
# Componentoverview
GISD_Komponents_4 <- cbind("Teildimension"="Arbeitswelt","Faktorladung"=round((TS_Arbeitswelt.pca$rotation*sqrt(abs(TS_Arbeitswelt.pca$sdev^2))), digits = 3),"Component"=round(TS_Arbeitswelt.pca$rotation, digits = 3))
#cbind erstellt Spaltenvektoren mit den Infos aus Teildimension, den (rotierten) Faktorladungen und den Components

GISD_Komponents_4 <- rbind(GISD_Komponents_4,cbind("Teildimension"="Einkommen","Faktorladung"=round((TS_Einkommen.pca$rotation*sqrt(abs(TS_Einkommen.pca$sdev^2))), digits = 3),"Component"=round(TS_Einkommen.pca$rotation, digits = 3)))
# rbind erstellt Zeilenvektoren, diese werden hier in die bereits vorhandenen Spaltenvektoren eingebunden

GISD_Komponents_4 <- rbind(GISD_Komponents_4,cbind("Teildimension"="Bildung","Faktorladung"=round((TS_Bildung_4items.pca$rotation[1:4,1]*sqrt(abs(TS_Bildung_4items.pca$sdev^2))), digits = 3),"Component"=round(TS_Bildung_4items.pca$rotation[1:4,1], digits = 3)))
# auch für die Teildimension Bildung werden Zeilenvektoren eingebunden

GISD_Komponents_4 <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents_4)),as.data.frame(GISD_Komponents_4))
# als letztes wird die Matrix in einen Dataframe übersetzt

rownames(GISD_Komponents_4) <- NULL
# die überflüssigen Zeilennamen werden gestrichen

colnames(GISD_Komponents_4) <- c("Variable","Dimension","Faktorladung","Component")

GISD_Komponents_4$prop_dem <- round(as.numeric(GISD_Komponents_4$Component)^2*100,digits=1)
# eine weitere Spalte Proportion wird eingefügt mit prozentualen Anteilswerten (eine Nachkommastelle)

GISD_Komponents_4$prop_GISD <- round(as.numeric(GISD_Komponents_4$prop_dem)/3, digits = 1)

colnames(GISD_Komponents_4) <- c("Variable","Dimension","Faktorladung","Component", "Anteil Teilscore", "Anteil GISD")

GISD_Komponents_4$Variable <- c("Beschäftigtenquote", "Arbeitslosigkeit", "Bruttoverdienst", "Einkommensteuer", "Haushaltseinkommen", "Schuldnerquote", "Beschäftigte mit Abschluss", "Beshäftigte ohne Abschluss", "Schulabgänger ohne Abschluss", "Schulabgänger mit Hochschulreife")

kable(GISD_Komponents_4, caption = "Faktorladungen und Anteile an den Teilscores sowie am Index")
```



Table: (\#tab:unnamed-chunk-9)Faktorladungen und Anteile an den Teilscores sowie am Index

|Variable                         |Dimension   |Faktorladung |Component | Anteil Teilscore| Anteil GISD|
|:--------------------------------|:-----------|:------------|:---------|----------------:|-----------:|
|Beschäftigtenquote               |Arbeitswelt |0.553        |0.426     |             18.1|         6.0|
|Arbeitslosigkeit                 |Arbeitswelt |-0.613       |-0.66     |             43.6|        14.5|
|Bruttoverdienst                  |Arbeitswelt |0.414        |0.62      |             38.4|        12.8|
|Einkommensteuer                  |Einkommen   |0.903        |0.631     |             39.8|        13.3|
|Haushaltseinkommen               |Einkommen   |0.537        |0.63      |             39.7|        13.2|
|Schuldnerquote                   |Einkommen   |-0.213       |-0.454    |             20.6|         6.9|
|Beschäftigte mit Abschluss       |Bildung     |0.88         |0.65      |             42.3|        14.1|
|Beshäftigte ohne Abschluss       |Bildung     |-0.221       |-0.189    |              3.6|         1.2|
|Schulabgänger ohne Abschluss     |Bildung     |-0.239       |-0.373    |             13.9|         4.6|
|Schulabgänger mit Hochschulreife |Bildung     |0.393        |0.634     |             40.2|        13.4|

## Faktorenanalyse nur für Daten des Jahres 2017

Es werden Hauptkomponentenanalysen für jede der drei Subskalen auf Basis der imputierten Daten geschätzt, hier nur mit den Daten von 2017.


```r
# Variablenliste für die Faktorenanalyse mit Einschränkung 2017 

TS_Arbeitswelt_17 <- Impdata.imputed  %>% filter(Jahr == 2017) %>% ungroup() %>% dplyr::select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst) 

TS_Einkommen_17   <- Impdata.imputed %>% filter(Jahr == 2017) %>% dplyr::select(Einkommensteuer,Haushaltseinkommen,Schuldnerquote) 

TS_Bildung_17 <- Impdata.imputed %>% filter(Jahr == 2017) %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss)


#PCA Arbeitswelt 2017
TS_Arbeitswelt_17.pca <- prcomp(TS_Arbeitswelt_17, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)


#PCA Einkommen 2017
TS_Einkommen_17.pca <- prcomp(TS_Einkommen_17, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)


#PCA Bildung 2017
TS_Bildung_17.pca <- prcomp(TS_Bildung_17, center = TRUE, scale. = TRUE, retx=TRUE, rank. =1 ) 
```

### Eigenwerte der Komponenten

```r
par(mfrow=c(1, 3))
plot(TS_Arbeitswelt_17.pca, main = "Arbeitswelt (Eigenvektoren)", ylim=c(0,2))
plot(TS_Einkommen_17.pca, main = "Einkommen (Eigenverktoren)", ylim=c(0,2))
plot(TS_Bildung_17.pca, main = "Bildung (Eigenvektoren)", ylim=c(0,2))
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
Varianz17_tab <- cbind("F_A" = "Faktor1", "Var_A" = round(TS_Arbeitswelt_17.pca$sdev^2, digits = 3), "Var_E" = round(TS_Einkommen_17.pca$sdev^2, digits = 3), "Var_B" = round(TS_Bildung_17.pca$sdev^2, digits = 3))

Varianz17_tab <- cbind(as.data.frame(Varianz17_tab))

Varianz17_tab$F_A <- c("Faktor 1", "Faktor 2", "Faktor 3")


colnames(Varianz17_tab) <- c("Faktoren", "Varianz Arbeitswelt", "Varianz Einkommen", "Varianz Bildung")

kable(Varianz17_tab, caption = "Varianz der Faktoren (Eigenverktoren) für 2017")
```



Table: (\#tab:unnamed-chunk-11)Varianz der Faktoren (Eigenverktoren) für 2017

|Faktoren |Varianz Arbeitswelt |Varianz Einkommen |Varianz Bildung |
|:--------|:-------------------|:-----------------|:---------------|
|Faktor 1 |1.419               |1.869             |1.373           |
|Faktor 2 |0.974               |0.643             |1.083           |
|Faktor 3 |0.607               |0.488             |0.544           |


### Faktorladungen

```r
# Componentoverview 2017
GISD_Komponents_17 <- cbind("Teildimension"="Arbeitswelt","Faktorladung"=round((TS_Arbeitswelt_17.pca$rotation*sqrt(abs(TS_Arbeitswelt_17.pca$sdev^2))), digits = 3),"Component"=round(TS_Arbeitswelt_17.pca$rotation, digits = 3))
# cbind erstellt Spaltenvektoren mit den Infos aus Teildimension, den (rotierten) Faktorladungen und den Components

GISD_Komponents_17 <- rbind(GISD_Komponents_17,cbind("Teildimension"="Einkommen","Faktorladung"=round((TS_Einkommen_17.pca$rotation*sqrt(abs(TS_Einkommen_17.pca$sdev^2))), digits = 3),"Component"=round(TS_Einkommen_17.pca$rotation, digits = 3)))
# rbind erstellt Zeilenvektoren, diese werden hier in die bereits vorhandenen Spaltenvektoren eingebunden

GISD_Komponents_17 <- rbind(GISD_Komponents_17,cbind("Teildimension"="Bildung","Faktorladung"=round((TS_Bildung_17.pca$rotation*sqrt(abs(TS_Bildung_17.pca$sdev^2))), digits = 3),"Component"=round(TS_Bildung_17.pca$rotation, digits = 3)))
# auch für die Teildimension Bildung werden Zeilenvektoren eingebunden

GISD_Komponents_17 <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents_17)),as.data.frame(GISD_Komponents_17))
# als letztes wird die Matrix in einen Dataframe übersetzt

rownames(GISD_Komponents_17) <- NULL
# die überflüssigen Zeilennamen werden gestrichen

colnames(GISD_Komponents_17) <- c("Variable","Dimension","Faktorladung","Component")

GISD_Komponents_17$prop_dem <- round(as.numeric(GISD_Komponents_17$Component)^2*100,digits=1)
# eine weitere Spalte Proportion wird eingef|gt mit prozentualen Anteilswerten (eine Nachkommastelle)

GISD_Komponents_17$prop_GISD <- round(as.numeric(GISD_Komponents_17$prop_dem)/3, digits = 1)

colnames(GISD_Komponents_17) <- c("Variable","Dimension","Faktorladung","Coponent", "Anteil Dimension", "Anteil GISD")

GISD_Komponents_17$Variable <- c("Beschäftigtenquote", "Arbeitslosigkeit", "Bruttoverdienst", "Einkommensteuer", "Haushaltseinkommen", "Schuldnerquote", "Beschäftigte mit Abschluss", "Beshäftigte ohne Abschluss", "Schulabgänger ohne Abschluss")

kable(GISD_Komponents_17, caption = "Komponenten und Anteile der Dimensionen für 2017")
```



Table: (\#tab:unnamed-chunk-12)Komponenten und Anteile der Dimensionen für 2017

|Variable                     |Dimension   |Faktorladung |Coponent | Anteil Dimension| Anteil GISD|
|:----------------------------|:-----------|:------------|:--------|----------------:|-----------:|
|Beschäftigtenquote           |Arbeitswelt |0.292        |0.245    |              6.0|         2.0|
|Arbeitslosigkeit             |Arbeitswelt |-0.681       |-0.689   |             47.5|        15.8|
|Bruttoverdienst              |Arbeitswelt |0.531        |0.682    |             46.5|        15.5|
|Einkommensteuer              |Einkommen   |0.771        |0.564    |             31.8|        10.6|
|Haushaltseinkommen           |Einkommen   |0.488        |0.609    |             37.1|        12.4|
|Schuldnerquote               |Einkommen   |-0.39        |-0.558   |             31.1|        10.4|
|Beschäftigte mit Abschluss   |Bildung     |0.034        |0.029    |              0.1|         0.0|
|Beshäftigte ohne Abschluss   |Bildung     |0.73         |0.701    |             49.1|        16.4|
|Schulabgänger ohne Abschluss |Bildung     |-0.525       |-0.712   |             50.7|        16.9|

# Generierung des Faktorscores auf Basis 2017

```r
#for(i in listofdeterminants){
#hist_over_year <- ggplot(data = Impdata.imputed) + 
#  geom_histogram(mapping =  aes_string(x = i)) + 
#  facet_wrap(~Jahr)
#print(hist_over_year)
#}
```


```r
TS_Bildung_4items.pca <- prcomp(TS_Bildung_4items, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)

Resultdataset <- Impdata.imputed
Resultdataset$TS_Arbeitswelt_17 <- as.numeric(predict(TS_Arbeitswelt_17.pca, newdata = Resultdataset))
Resultdataset$TS_Einkommen_17 <- as.numeric(predict(TS_Einkommen_17.pca , newdata = Resultdataset))
Resultdataset$TS_Bildung_17 <- as.numeric(predict(TS_Bildung_17.pca, newdata = Resultdataset))
Resultdataset$TS_Arbeitswelt <- as.numeric(predict(TS_Arbeitswelt.pca, newdata = Resultdataset))
Resultdataset$TS_Einkommen <- as.numeric(predict(TS_Einkommen.pca , newdata = Resultdataset))
Resultdataset$TS_Bildung <- as.numeric(predict(TS_Bildung.pca, newdata = Resultdataset))
Resultdataset$TS_Bildung_4items <- as.numeric(predict(TS_Bildung_4items.pca, newdata = Resultdataset))
Resultdataset$TS_Bildung_adj <- as.numeric(predict(TS_Bildung_adj.pca, newdata = Resultdataset))

#summary(Resultdataset %>% select(TS_Arbeitswelt_17, TS_Einkommen_17, TS_Bildung_17))
#descs <- stat.desc(Resultdataset[, -5])
```

## Verteilung der Scores nach Faktorprediction

```r
d_TS_Arbeitswelt_17 <- density(Resultdataset$TS_Arbeitswelt_17)
d_TS_Einkommen_17 <- density(Resultdataset$TS_Einkommen_17)
d_TS_Bildung_17 <- density(Resultdataset$TS_Bildung_17)

par(mfrow=c(2, 2))
plot(d_TS_Arbeitswelt_17, main = "Density Arbeitswelt")
plot(d_TS_Einkommen_17, main = "Density Einkommen")
plot(d_TS_Bildung_17, main = "Density Bildung")
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
# Korrelationen überprüfen
cor_tab <- Resultdataset %>% select(Arbeitslosigkeit,TS_Arbeitswelt_17,TS_Einkommen_17,TS_Bildung_17)  %>% cor( use="pairwise.complete.obs")

cor_tab <- cbind(as.data.frame(cor_tab))

colnames(cor_tab) <- c("Arbeitslosigkeit", "Faktor Arbeitswelt", "Faktor Einkommen", "Faktor Bildung")

rownames(cor_tab) <- c("Arbeitslosigkeit", "Faktor Arbeitswelt", "Faktor Einkommen", "Faktor Bildung")

#kable(cor_tab, caption = "Korrelation von Arbeitslosigkeit und Faktoren")
```



```r
# die Richtung der Skala der Scores ist nach der Generierung willkürlich 
# sie werden nun anhand der Variable Arbeitslosigkeit ausgerichtet,
# d.h. sie werden so gepolt, dass sie positiv mit Arbeitslosigkeit korrelieren, um Deprivation abzubilden:
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung_17,use="pairwise.complete.obs")<0) {
   Resultdataset$TS_Bildung_17 <- Resultdataset$TS_Bildung_17*-1
   }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt_17,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt_17 <- Resultdataset$TS_Arbeitswelt_17*-1
  }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen_17,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen_17 <- Resultdataset$TS_Einkommen_17*-1
}

# Korrelationen erneut überprüfen
cor_tab_pol <- Resultdataset %>% select(Arbeitslosigkeit,TS_Arbeitswelt_17,TS_Einkommen_17,TS_Bildung_17)  %>% cor( use="pairwise.complete.obs")

cor_tab_pol <- cbind(as.data.frame(cor_tab_pol))

colnames(cor_tab_pol) <- c("Arbeitslosigkeit", "Faktor Arbeitswelt", "Faktor Einkommen", "Faktor Bildung")

rownames(cor_tab_pol) <- c("Arbeitslosigkeit", "Faktor Arbeitswelt", "Faktor Einkommen", "Faktor Bildung")

kable(cor_tab_pol, caption = "Korrelation von Arbeitslosigkeit und Faktoren (gepolt)")
```



Table: (\#tab:unnamed-chunk-17)Korrelation von Arbeitslosigkeit und Faktoren (gepolt)

|                   | Arbeitslosigkeit| Faktor Arbeitswelt| Faktor Einkommen| Faktor Bildung|
|:------------------|----------------:|------------------:|----------------:|--------------:|
|Arbeitslosigkeit   |        1.0000000|          0.9425038|        0.7425161|      0.7000867|
|Faktor Arbeitswelt |        0.9425038|          1.0000000|        0.8178952|      0.7146312|
|Faktor Einkommen   |        0.7425161|          0.8178952|        1.0000000|      0.6593492|
|Faktor Bildung     |        0.7000867|          0.7146312|        0.6593492|      1.0000000|


```r
# Normalization
Resultdataset$TS_Arbeitswelt_17 <- (Resultdataset$TS_Arbeitswelt_17 -min(Resultdataset$TS_Arbeitswelt_17 ))/(max(Resultdataset$TS_Arbeitswelt_17 )-min(Resultdataset$TS_Arbeitswelt_17 ))
Resultdataset$TS_Einkommen_17 <- (Resultdataset$TS_Einkommen_17 -min(Resultdataset$TS_Einkommen_17 ))/(max(Resultdataset$TS_Einkommen_17 )-min(Resultdataset$TS_Einkommen_17 ))
Resultdataset$TS_Bildung_17 <- (Resultdataset$TS_Bildung_17 -min(Resultdataset$TS_Bildung_17 ))/(max(Resultdataset$TS_Bildung_17 )-min(Resultdataset$TS_Bildung_17 ))

# GISD
Resultdataset$GISD_Score_17 <- Resultdataset$TS_Arbeitswelt_17+Resultdataset$TS_Einkommen_17+Resultdataset$TS_Bildung_17
Resultdataset$GISD_Score_17 <- (Resultdataset$GISD_Score_17 -min(Resultdataset$GISD_Score_17 ))/(max(Resultdataset$GISD_Score_17 )-min(Resultdataset$GISD_Score_17 ))

#summary(Resultdataset %>% select(TS_Arbeitswelt_17,TS_Einkommen_17,TS_Bildung_17,GISD_Score_17))
#str(Resultdataset %>% select(TS_Arbeitswelt_17,TS_Einkommen_17,TS_Bildung_17,GISD_Score_17))
```

## Verteilung der Scores nach Normalization


```r
d_TS_Arbeitswelt_17_norm <- density(Resultdataset$TS_Arbeitswelt_17)
d_TS_Einkommen_17_norm <- density(Resultdataset$TS_Einkommen_17)
d_TS_Bildung_17_norm <- density(Resultdataset$TS_Bildung_17)
d_GISD_Score_17_norm <- density(Resultdataset$GISD_Score_17)

par(mfrow=c(2, 2))
plot(d_TS_Arbeitswelt_17_norm, main = "Density Arbeitswelt")
plot(d_TS_Einkommen_17_norm, main = "Density Einkommen")
plot(d_TS_Bildung_17_norm, main = "Density Bildung")
plot(d_GISD_Score_17_norm, main = "Density GISD Score 2017")
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
if (cor(Resultdataset$TS_Bildung, Resultdataset$TS_Bildung,use="pairwise.complete.obs")<0) {
   Resultdataset$TS_Bildung <- Resultdataset$TS_Bildung*-1
   }
if (cor(Resultdataset$TS_Arbeitswelt, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1
  }
if (cor(Resultdataset$TS_Einkommen, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1
}

#Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung <- (Resultdataset$TS_Bildung -min(Resultdataset$TS_Bildung ))/(max(Resultdataset$TS_Bildung )-min(Resultdataset$TS_Bildung ))


# GISD
Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung
Resultdataset$GISD_Score <- (Resultdataset$GISD_Score -min(Resultdataset$GISD_Score ))/(max(Resultdataset$GISD_Score )-min(Resultdataset$GISD_Score ))
```


```r
if (cor(Resultdataset$TS_Bildung_4items, Resultdataset$TS_Bildung_4items,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Bildung_4items <- Resultdataset$TS_Bildung_4items*-1
     }
if (cor(Resultdataset$TS_Arbeitswelt, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1
  }
if (cor(Resultdataset$TS_Einkommen, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1
}

#Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung_4items <- (Resultdataset$TS_Bildung_4items -min(Resultdataset$TS_Bildung_4items ))/(max(Resultdataset$TS_Bildung_4items )-min(Resultdataset$TS_Bildung_4items ))


# GISD
Resultdataset$GISD_Score_B4 <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung_4items
Resultdataset$GISD_Score_B4 <- (Resultdataset$GISD_Score_B4 -min(Resultdataset$GISD_Score_B4 ))/(max(Resultdataset$GISD_Score_B4 )-min(Resultdataset$GISD_Score_B4 ))
```


```r
if (cor(Resultdataset$TS_Bildung_adj, Resultdataset$TS_Bildung_adj,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Bildung_adj <- Resultdataset$TS_Bildung_adj*-1
     }
if (cor(Resultdataset$TS_Arbeitswelt, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1
  }
if (cor(Resultdataset$TS_Einkommen, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1
}

#Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung_adj <- (Resultdataset$TS_Bildung_adj -min(Resultdataset$TS_Bildung_adj ))/(max(Resultdataset$TS_Bildung_adj )-min(Resultdataset$TS_Bildung_adj ))


# GISD
Resultdataset$GISD_Score_adj <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung_adj
Resultdataset$GISD_Score_adj <- (Resultdataset$GISD_Score_adj -min(Resultdataset$GISD_Score_adj ))/(max(Resultdataset$GISD_Score_adj )-min(Resultdataset$GISD_Score_adj ))
```



```r
cor_tab_GISDscore <- Resultdataset %>% select(GISD_Score_17, GISD_Score, GISD_Score_adj, GISD_Score_B4)  %>% cor( use="pairwise.complete.obs")

cor_tab_GISDscore <- cbind(as.data.frame(cor_tab_GISDscore))

colnames(cor_tab_GISDscore) <- c("GISD Score 2017", "GISD Score alle Jahre", "GISD Score alle Jahre (adj. Bildung)", "GISD Score alle Jahre (Bildung 4 Items)")

kable(cor_tab_GISDscore, caption = "Korrelation der verschiedenen GISD Scores")
```



Table: (\#tab:unnamed-chunk-19)Korrelation der verschiedenen GISD Scores

|               | GISD Score 2017| GISD Score alle Jahre| GISD Score alle Jahre (adj. Bildung)| GISD Score alle Jahre (Bildung 4 Items)|
|:--------------|---------------:|---------------------:|------------------------------------:|---------------------------------------:|
|GISD_Score_17  |       1.0000000|            -0.9703012|                           -0.9699781|                              -0.8112382|
|GISD_Score     |      -0.9703012|             1.0000000|                            0.9992050|                               0.9198134|
|GISD_Score_adj |      -0.9699781|             0.9992050|                            1.0000000|                               0.9173464|
|GISD_Score_B4  |      -0.8112382|             0.9198134|                            0.9173464|                               1.0000000|

```r
write_rds(Impdata.imputed, paste0("Outfiles/Resultdata_FaktorCheck.rds"))
```
