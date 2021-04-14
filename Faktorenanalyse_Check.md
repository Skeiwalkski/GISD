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
library("tidyverse") # Tidyverse Methods
library(bookdown) 
library(readxl) # Read Excel
library(pastecs) # descriptive stats
library(knitr)
library(ggplot2)

Impdata.imputed <- readRDS("C:/projects_rstudio/GISD/Outfiles/impdata.rds")
```



```r
# Variablenliste für die Faktorenanalyse 
#print(listofdeterminants)
TS_Arbeitswelt <- Impdata.imputed  %>% ungroup() %>% dplyr::select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst) 
TS_Einkommen   <- Impdata.imputed %>% dplyr::select(Einkommensteuer,Haushaltseinkommen,Schuldnerquote) 
# für den Vergleich der Ergebnisse wird zunächst ein Datensatz für die Variablenauswahl der Revision 2019 generiert
TS_Bildung <- Impdata.imputed %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 

# Check dieser Lösung für das 2014er Sample 
#TS_Bildung_r2014 <- Impdata.imputed %>% filter(Jahr<2015) %>%  #dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 

TS_Bildung_4items <- Impdata.imputed %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss, SchulabgaengermitHochschulreife)

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
TS_Bildung_4items.pca <- prcomp(TS_Bildung_4items, center = TRUE, scale. = TRUE, retx=TRUE)
# plot(TS_Bildung_4items.pca)
# TS_Bildung_4items.pca

#tab_Bildung_4items <- cbind("Faktor" = "F", "SD" = TS_Bildung_4items.pca$sdev^2)
#tab_Bildung_4items <- cbind(as.data.frame(tab_Bildung_4items))
#tab_Bildung_4items$Faktor <- c("Faktor 1", "Faktor 2", "Faktor 3")
#colnames(tab_Bildung_4items) <- c("Faktoren für Bildung", "Varianz")
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
|Faktor 1 |1.913               |2.041             |1.408           |
|Faktor 2 |0.722               |0.783             |1.186           |
|Faktor 3 |0.365               |0.177             |0.406           |

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
|Beschäftigtenquote           |Arbeitswelt |0.713        |0.516     |             26.6|         8.9|
|Arbeitslosigkeit             |Arbeitswelt |-0.485       |-0.571    |             32.6|        10.9|
|Bruttoverdienst              |Arbeitswelt |0.386        |0.639     |             40.8|        13.6|
|Einkommensteuer              |Einkommen   |0.925        |0.647     |             41.9|        14.0|
|Haushaltseinkommen           |Einkommen   |0.565        |0.638     |             40.7|        13.6|
|Schuldnerquote               |Einkommen   |-0.175       |-0.417    |             17.4|         5.8|
|Beschäftigte mit Abschluss   |Bildung     |0.418        |0.352     |             12.4|         4.1|
|Beshäftigte ohne Abschluss   |Bildung     |0.585        |0.537     |             28.8|         9.6|
|Schulabgänger ohne Abschluss |Bildung     |-0.489       |-0.767    |             58.8|        19.6|
## Faktorenanalyse gepoolte  
Querschnitte (4 Items Bildung)

### Eigenwerte der Komponenten

```r
par(mfrow=c(1, 3))
plot(TS_Arbeitswelt.pca, main = "Arbeitswelt (Eigenvektoren)", ylim=c(0,2.2))
plot(TS_Einkommen.pca, main = "Einkommen (Eigenverktoren)", ylim=c(0,2.2))
plot(TS_Bildung_4items.pca, main = "Bildung (Eigenvektoren)", ylim=c(0,2.2))
```

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
Varianz_B4_tab <- cbind("F_A" = "Faktor1", "Var_A" = round(TS_Arbeitswelt.pca$sdev^2, digits = 3), "Var_E" = round(TS_Einkommen.pca$sdev^2, digits = 3),  "Var_B" = round(TS_Bildung_4items.pca$sdev^2, digits = 3))

Varianz_B4_tab[4,2] = NA
Varianz_B4_tab[4,3] = NA

Varianz_B4_tab <- cbind(as.data.frame(Varianz_B4_tab))

Varianz_B4_tab$F_A <- c("Faktor 1", "Faktor 2", "Faktor 3", "Faktor 4")

colnames(Varianz_B4_tab) <- c("Faktoren", "Varianz Arbeitswelt", "Varianz Einkommen", "Varianz Bildung")

kable(Varianz_B4_tab, caption = "Varianz der Faktoren (Eigenverktoren)")
```



Table: (\#tab:unnamed-chunk-5)Varianz der Faktoren (Eigenverktoren)

|Faktoren |Varianz Arbeitswelt |Varianz Einkommen |Varianz Bildung |
|:--------|:-------------------|:-----------------|:---------------|
|Faktor 1 |1.913               |2.041             |1.833           |
|Faktor 2 |0.722               |0.783             |1.374           |
|Faktor 3 |0.365               |0.177             |0.409           |
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



Table: (\#tab:unnamed-chunk-6)Faktorladungen und Anteile an den Teilscores sowie am Index

|Variable                         |Dimension   |Faktorladung |Component | Anteil Teilscore| Anteil GISD|
|:--------------------------------|:-----------|:------------|:---------|----------------:|-----------:|
|Beschäftigtenquote               |Arbeitswelt |0.713        |0.516     |             26.6|         8.9|
|Arbeitslosigkeit                 |Arbeitswelt |-0.485       |-0.571    |             32.6|        10.9|
|Bruttoverdienst                  |Arbeitswelt |0.386        |0.639     |             40.8|        13.6|
|Einkommensteuer                  |Einkommen   |0.925        |0.647     |             41.9|        14.0|
|Haushaltseinkommen               |Einkommen   |0.565        |0.638     |             40.7|        13.6|
|Schuldnerquote                   |Einkommen   |-0.175       |-0.417    |             17.4|         5.8|
|Beschäftigte mit Abschluss       |Bildung     |0.881        |0.651     |             42.4|        14.1|
|Beshäftigte ohne Abschluss       |Bildung     |-0.235       |-0.201    |              4.0|         1.3|
|Schulabgänger ohne Abschluss     |Bildung     |-0.223       |-0.349    |             12.2|         4.1|
|Schulabgänger mit Hochschulreife |Bildung     |0.399        |0.644     |             41.5|        13.8|

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

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
Varianz17_tab <- cbind("F_A" = "Faktor1", "Var_A" = round(TS_Arbeitswelt_17.pca$sdev^2, digits = 3), "Var_E" = round(TS_Einkommen_17.pca$sdev^2, digits = 3), "Var_B" = round(TS_Bildung_17.pca$sdev^2, digits = 3))

Varianz17_tab <- cbind(as.data.frame(Varianz17_tab))

Varianz17_tab$F_A <- c("Faktor 1", "Faktor 2", "Faktor 3")


colnames(Varianz17_tab) <- c("Faktoren", "Varianz Arbeitswelt", "Varianz Einkommen", "Varianz Bildung")

kable(Varianz17_tab, caption = "Varianz der Faktoren (Eigenverktoren) für 2017")
```



Table: (\#tab:unnamed-chunk-8)Varianz der Faktoren (Eigenverktoren) für 2017

|Faktoren |Varianz Arbeitswelt |Varianz Einkommen |Varianz Bildung |
|:--------|:-------------------|:-----------------|:---------------|
|Faktor 1 |1.419               |1.869             |1.377           |
|Faktor 2 |0.974               |0.643             |1.046           |
|Faktor 3 |0.607               |0.488             |0.578           |


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



Table: (\#tab:unnamed-chunk-9)Komponenten und Anteile der Dimensionen für 2017

|Variable                     |Dimension   |Faktorladung |Coponent | Anteil Dimension| Anteil GISD|
|:----------------------------|:-----------|:------------|:--------|----------------:|-----------:|
|Beschäftigtenquote           |Arbeitswelt |0.292        |0.245    |              6.0|         2.0|
|Arbeitslosigkeit             |Arbeitswelt |-0.681       |-0.689   |             47.5|        15.8|
|Bruttoverdienst              |Arbeitswelt |0.531        |0.682    |             46.5|        15.5|
|Einkommensteuer              |Einkommen   |0.771        |0.564    |             31.8|        10.6|
|Haushaltseinkommen           |Einkommen   |0.488        |0.609    |             37.1|        12.4|
|Schuldnerquote               |Einkommen   |-0.39        |-0.558   |             31.1|        10.4|
|Beschäftigte mit Abschluss   |Bildung     |-0.125       |-0.107   |              1.1|         0.4|
|Beshäftigte ohne Abschluss   |Bildung     |0.734        |0.717    |             51.4|        17.1|
|Schulabgänger ohne Abschluss |Bildung     |-0.523       |-0.688   |             47.3|        15.8|

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

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


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



Table: (\#tab:unnamed-chunk-14)Korrelation von Arbeitslosigkeit und Faktoren (gepolt)

|                   | Arbeitslosigkeit| Faktor Arbeitswelt| Faktor Einkommen| Faktor Bildung|
|:------------------|----------------:|------------------:|----------------:|--------------:|
|Arbeitslosigkeit   |        1.0000000|          0.9258294|        0.7169293|      0.6967428|
|Faktor Arbeitswelt |        0.9258294|          1.0000000|        0.8467284|      0.6836139|
|Faktor Einkommen   |        0.7169293|          0.8467284|        1.0000000|      0.6065770|
|Faktor Bildung     |        0.6967428|          0.6836139|        0.6065770|      1.0000000|


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

![](Faktorenanalyse_Check_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


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
cor_tab_GISDscore <- Resultdataset %>% select(GISD_Score_17, GISD_Score, GISD_Score_B4)  %>% cor( use="pairwise.complete.obs")

cor_tab_GISDscore <- cbind(as.data.frame(cor_tab_GISDscore))

colnames(cor_tab_GISDscore) <- c("GISD Score 2017", "GISD Score alle Jahre", "GISD Score alle Jahre (Bildung 4 Items)")

rownames(cor_tab_GISDscore) <- c("GISD Score 2017", "GISD Score alle Jahre", "GISD Score alle Jahre (Bildung 4 Items)")

kable(cor_tab_GISDscore, caption = "Korrelation der verschiedenen GISD Scores")
```



Table: (\#tab:unnamed-chunk-16)Korrelation der verschiedenen GISD Scores

|                                        | GISD Score 2017| GISD Score alle Jahre| GISD Score alle Jahre (Bildung 4 Items)|
|:---------------------------------------|---------------:|---------------------:|---------------------------------------:|
|GISD Score 2017                         |       1.0000000|            -0.9589435|                              -0.7460925|
|GISD Score alle Jahre                   |      -0.9589435|             1.0000000|                               0.8931512|
|GISD Score alle Jahre (Bildung 4 Items) |      -0.7460925|             0.8931512|                               1.0000000|
