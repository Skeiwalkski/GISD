---
title: "GISD - German Index of Socio-Economic Deprivation"
author: "Niels Michalski (Überarbeitung der Syntax von Lars Kroll)"
date: "21 April 2020"
output: 
  bookdown::html_document2:
    keep_md: true
    code_folding: hide
    toc: true
    toc_float: false
    toc_depth: 2
    number_sections: false
    fig_caption: true
---



# Intro

Auf diesem Codeblog stelle ich die Generierung des German Index of Socio-Economic Deprivation vor. Dabei handelt es sich um einen Index sozioökonomischer Deprivation auf regionalräumlicher Ebene, der 2016 im Fachgebiet Soziale Determinanten der Gesundheit am RKI entwickelt wurde und seither jährlich aktualisiert wird. Für die Generierung werden Indikatoren der INKAR-Datenbank des BBSR verwendet. Es wird die Revision 2019 vorgestellt, die Daten aus den Jahren 1998 bis 2015 verwendet. 
In einer früheren Revision für 2019 tauchten zum Teil starke Abweichungen zur Revision 2018 auf. Der Grund dafür, war dass bei der Addition der Teildimensionen die Bildungsdimension in umgekehrter Richtung in den GISD einging. Ausschlaggebend für diesen Fehler war eine negative Korrelation der Bildungsdimension mit dem Anteil Arbeitsloser, die im Code der vergangenen Revisionen zu einer Umpolung des Teilscores führt. Der Anteil Arbeitsloser wird als Markerindikator verwendet. Die folgende Darstellung zeigt und diskutiert die Problematik und stellt eine Lösung vor.

# Kommentierte Darstellung der Syntax

Das folgende Kapitel stellt die Syntax zur Generierung der Daten mit R vor. Der Code ist optional darstellbar und enthält detaillierte Kommentare.


## 0. Benötigte Pakete

Der Code nutzt im Wesentlichen die Pakete des Tidyverse. 


```r
library("tidyverse") # Tidyverse Methods
library("bookdown") 
library("readxl") # Read Excel
library("zoo")
library("imputeTS") # Impute Missing Features
library("haven") # write Stata-dta
library("sf") # write Stata-dta

# Create Output directories in working directory if necessary
dir.create("C:/Data/GISD/Outfiles")
dir.create("C:/Data/GISD/Outfiles/2020")
dir.create("C:/Data/GISD/Outfiles/2020/Bund")
dir.create("C:/Data/GISD/Outfiles/2020/Other")
dir.create("C:/Data/GISD/Outfiles/2020/Stata")
```

## I.  Generierung eines ID-Datensatzes

Zunächst muss ein Datensatz generiert werden in dem den kleinsten regionalen Einheiten (Gemeinden) alle
übergeordneten regionalen Einheiten und deren Regionalkennziffern zugeordnet werden. Datenquelle ist die Gebietsstandsreferenz von Destatis Stand 31.12.2015.


```r
Gemeinden_INKAR <- read_excel("Data/Referenz/Referenz_1998_2017.xlsx", sheet = "Gemeinden-GVB", na = "NA") %>% 
  mutate(Kennziffer=as.numeric(gem17),"Kennziffer Gemeindeverband"=vbgem17, fl17=as.numeric(fl17)) %>% filter(!is.na(gem17name))
```

```
## Warning: NAs durch Umwandlung erzeugt

## Warning: NAs durch Umwandlung erzeugt
```

```r
# Pipes: 
# 1. rename von zwei Variablen; " um Leerzeichen zu berücksichtigen; 
# 2. Gemeinden ohne Missing auf der Kennziffervariablen
# Gemeinden_INKAR <- read_excel("Data/Referenz/Referenz_1998_2017.xlsx", sheet = "Gemeinden", na = "NA", skip = 2) %>% 
  # rename(Kennziffer=gem15,"Kennziffer Gemeindeverband"="Gemeindeverband, Stand 31.12.2015") %>% filter(!is.na(Kennziffer))


Gemeindeverbaende_INKAR <- read_excel("Data/Referenz/Referenz_1998_2017.xlsx", sheet = "Gemeindeverbände", na = "NA") %>% filter(!is.na(gvb17name)) %>% 
  select("Kennziffer Gemeindeverband"=gvb17,"Name des Gemeindeverbands"=gvb17name)  
# das ganze nochmal für Gemeindeverbaende  
# Pipes: 
# 1. nur die Variablen gvb15 und Name des Gemeindeverbands ausgewählt 
# 2. Missing herausfiltern

Kreise_INKAR <- read_excel("Data/Referenz/Referenz_1998_2017.xlsx", sheet = "KRS") %>%
 mutate(krs17= as.numeric(krs17)/1000, fl17 = as.numeric(fl17)) %>% filter(!is.na(krs17name))
```

```
## Warning: NAs durch Umwandlung erzeugt

## Warning: NAs durch Umwandlung erzeugt
```

```r
# und für Kreise
# Pipes: 
# 1. neue Variable generieren, die die Kreisvariable auf den Fünfsteller reduzieren
# 2. Missing herausfiltern

# Die drei Datensätze werden nun ausgehend vom Gemeindedatensatz zu einem ID-Datensatz zusammmengefügt
id_dataset <- Gemeinden_INKAR %>% 
              select(Gemeindekennziffer=Kennziffer,"Name der Gemeinde"=gem17name,"Kennziffer Gemeindeverband") %>% 
              mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
              left_join(.,Kreise_INKAR %>% select(Kreiskennziffer=krs17,
                                                  "Name des Kreises"=krs17name,
                                                  "Raumordnungsregion Nr"=ROR11,
                                                  Raumordnungsregion=ROR11name,
                                                  NUTS2,
                                                  "NUTS2 Name"=NUTS2name,
                                                  Bundesland=land) %>% mutate(Kreiskennziffer=floor(Kreiskennziffer/1000)),by="Kreiskennziffer") %>% left_join(.,Gemeindeverbaende_INKAR, by="Kennziffer Gemeindeverband")

# Pipes:  1. (select) Variablenauswahl (gkz, Gemeindename, Gemeindeverband)[wieso hier ``?]
#         2. die Kreiskennziffer wird aus der Gemeindekennziffer generiert; floor rundet nach unten auf ganze Ziffern ab
#         3. leftjoin spielt Kreisdaten über Kreiskennziffer an
#         3.1 select wählt, die anzupielenden Variablen aus, darunter auch NUTS und ROR und Bundesland, dessen Variablenname beim       #             Einlesen zu lang war (...24)
#         3.2 die Kreiskennziffer wurde vor dem leftjoin im Using-Datensatz generiert
#         4. als letztes werden die Gemeindeverbandskennziffern angespielt
```

## II. Erzeugen eines Datensatzes mit Kennziffern als ID unabhängig von der Ebene 

In diesem Code-Abschnitt werden die INKAR-Daten zu den Indikatoren in einem Datensatz zusammengeführt. Die Information für die Indikatoren, die für die Berechnung des GISD verwendet werden, liegt auf unterschiedlichen Ebenen vor. Die Faktorenanalysen sollen auf Gemeindeebene durchgeführt werden, weshalb Information der Kreisebene an die Gemeinden innerhalb der Kreise angespielt wird. Percentile des Indexes können so später für jede regionale Ebene separat berechnet werden. Datenbasis sind die INKAR-Daten der jeweiligen Indikatoren im Excel-Format, die zu jeder Revision aus der INKAR-Datenbank heruntergeladen werden. Tabelle \@ref(tab:indicators) stellt die Indikatoren dar.


```r
# Basis erzeugen: Ausgangspunkt Kreisdaten
# Es werden Indikatoren allen Ebenen angespielt, als erstes die Kreise.
Basedata    <- Kreise_INKAR %>% select(Kennziffer=krs17) %>% mutate(Jahr=2017)
# Datensatz zum Anspielen der Daten generieren
# Ausgangspunkt Kreisdatensatz
# Pipes:  1. nur Kreiskennzifern ausgewählt
#         2. Jahresvariable generiert (2017)

# Liste der Variablen generieren
inputdataset <- list.files("Data/INKAR_1998-2017/") # Variablenliste der Dateinamen im Ordner


# Einlesen der einzelnen Excelfiles zu den Daten (Schleife) 
# for testing file<-inputdataset[1]
for(file in inputdataset){
  myimport <- read_excel(paste0("Data/INKAR_1998-2017/",file), skip = 1, sheet = "Daten", col_types = c("text"))
  names(myimport)[1] <- "Kennziffer"
  myimport[3] <- NULL
  myimport[2] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value)) 
  names(myimport)[3] <- unlist(strsplit(unlist(strsplit(file,"_"))[2],"[.]"))[1]
  Basedata <- full_join(Basedata,myimport,by=c("Kennziffer","Jahr"))
}
```

```
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
```

```r
# Schleife für jedes Excel-File
# 1. Einlesen der Exceldatei; jeweils das Sheet "Daten"; erste Zeile wird geskippt,  die Daten werden als Text eingelesen
# 2. für die erste Spalte wird die Kennziffer importiert; für die zweite und dritte Spalte nichts
# 3. die Daten werde reshaped, um die Jahresinfos im langen Format zu speichern; convert konvertiert das Datenformat automatisch;
# rm.na entfert missing value Zeilen; -"Kennziffer" sorgt dafür, dass die Variable Kennziffer nicht doppelt erzeugt wird
# 4. mutate definiert die Variablentypen 
# 5. von innen nach außen 
# 5.1 das innere strsplit(file, "_") teilt den Filenamen inkl. Dateiendung beim "_"
# 5.2 das innerste unlist generiert einen Vektor mit den Elementen aus dem strsplit
# 5.3 das äußere strsplit das zweite Vektorelement beim ".", sodass nur noch der Variablenname übrig bleibt
# 5.4 das äußere unlist weist auf das erste Vektorelement 
# 5.5 names(import)[3] nimmt dieses Vektorelement als Variablennamen für die dritte Spalte
# 6. jedes file der Schleife wird an Basedata gejoint über Kennziffer und Jahr; full_join übernimmt dabei jede Zeile und Spalte jeder Seite,
# auch wenn die Werte auf einer Seite missing enthalten

rm(inputdataset) 


# Liste der Indikatoren erstellen
listofdeterminants <- names(Basedata)[3:length(Basedata)]

# Regionale Tiefe der Indikatoren 
ind_level <- c("Gemeindeverband","Gemeindeverband","Kreis", "Kreis", "Kreis", "Kreis", "Kreis", "Gemeinde", "Kreis", "Kreis")
level_table <- cbind(listofdeterminants,ind_level)
# Tabelle der Indikatoren mit regionaler Tiefe
ind_col = c("Indikator","Tiefe des Indikators")


# Datensatz für die Gemeindeverbandsebene generieren
Basedata_Gemeindeverbandsebene <- Basedata %>% select(Kennziffer,Jahr,Arbeitslosigkeit,Beschaeftigtenquote,Einkommenssteuer) %>%   
  gather(key,value,3:5) %>% filter(!is.na(value)) %>% spread(key,value) %>% filter(Jahr>=1998) %>% rename("Gemeindeverband"=Kennziffer)
# Pipes:  1. Auswahl der Variablen 
#         2. Reshape der Daten wide nach long      
#         3. Auswahl von Non-Missing 
#         4. Reshape von long nach wide 
#         5. Auswahl der Daten Jahr>=1998
#         6. Umbenennung der Kennziffervariable

# Datensatz für die Kreisebene generieren 
Basedata_Kreisebene <- Basedata %>% select(krs15=Kennziffer,Jahr,listofdeterminants) %>% 
  select(-Arbeitslosigkeit,-Einkommenssteuer,-Beschaeftigtenquote) %>% rename(Kreis=krs15)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(listofdeterminants)` instead of `listofdeterminants` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Pipes:  1. neben der Kennziffer, die einen anderen Namen bekommt wird das Jahr und die Variablenliste ausgewählt
#         2. drei Variablen werden aus der Auswahl ausgeschlossen
#         3. die Kreisvariable wird in Kreis umbenannt, weil im nächsten Schritt Kreisinfos an die Gemeinden angespielt werden

# Join different levels
# Nun werden die Daten bezogen auf die Ebenen gemergt
# Dazu wird erstmal ein Leerdatensatz im Longformat erstellt, der Fälle für alle Gemeinden für jedes Jahr generiert
Workfile <- as.data.frame(expand.grid("Kennziffer"=Gemeinden_INKAR %>% pull(Kennziffer),"Jahr"=seq(min(Basedata$Jahr):max(Basedata$Jahr)) + min(Basedata$Jahr)-1)) %>% mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>% as_tibble() %>%
   left_join(. , Gemeinden_INKAR,by=c("Kennziffer"))  %>%
   select(Gemeindekennziffer=Kennziffer,Kreis=Kreiskennziffer,Gemeindeverband="Kennziffer Gemeindeverband",Jahr,Bevoelkerung=bev17) %>% mutate(Gemeindeverband=as.numeric(Gemeindeverband)) %>% 
  arrange(Gemeindekennziffer,Jahr) %>% # Join Metadata
   left_join(. , Basedata_Kreisebene,by=c("Kreis","Jahr")) %>% # Hier wird über Kreis gematched
   left_join(. , Basedata_Gemeindeverbandsebene,by=c("Gemeindeverband","Jahr")) %>%  # Join Indicators for Level: Gemeindeverband 
   filter(Jahr>=1998)

# als erstes wird ein data.frame erzeugt (Workfile); der alle Gemeindewellen (1998-201x) in den Zeilen stehen hat
# 1. expand.grid erzeugt ein tibble mit allen Kombinationen von Kennziffern und Jahren
#     pull erzeugt einen Vektor für die Variablenwerte von Kennziffer aus dem Datensatz
#     + min(...) wird zu der Sequenz von Jahren aus dem Basedata addiert (1 bis X) damit auch Jahreswerte weitergeben werden
# 2. mutate generiert eine Kreiskennziffer
# 3. as_tibble erzeugt einen tibble, damit left_join genutzt werden kann
# 4. erstes left_join spielt die Gemeindedaten über Kennziffer an, das geht so, weil Gemeinden_INKAR als tibble gespeichert ist
# 5. select, wählt die inhaltlichen Variablen aus, und ändert die Variablennamen; 
# 6. arrange im select sortiert nach Gemeindekennziffer und Jahr
# 7. zweites left_join spielt die Daten der Kreisebene via Kreis und Jahr an
# 8. drittes left_join spielt die Daten der Gemeindeverbandsebene via Gemeindeverband und Jahr an
# Notiz: . in den Befehlen bezieht sich auf den tibble bzw. data.frame der in der Pipe bearbeitet wird

# Stata-Datensatz rausschreiben
write_dta(Workfile, paste0("Outfiles/2020/Stata/workfile.dta"))

# Ende Generierung Basisdatensatz
```


Table: (\#tab:indicators)Liste der Indikatoren

Indikator                         Tiefe des Indikators 
--------------------------------  ---------------------
Arbeitslosigkeit                  Gemeindeverband      
Beschaeftigtenquote               Gemeindeverband      
Bruttoverdienst                   Kreis                
BeschaeftigtemitakadAbschluss     Kreis                
BeschaeftigteohneAbschluss        Kreis                
SchulabgaengermitHochschulreife   Kreis                
SchulabgaengerohneAbschluss       Kreis                
Einkommenssteuer                  Gemeinde             
Haushaltseinkommen                Kreis                
Schuldnerquote                    Kreis                

Es gibt noch einige Probleme bei der Auswahl der Indikatoren, die erst später zum Tragen kommen. Insbesondere der Bildungsindikator Schulabgänger ohne Abschluss macht Probleme, weil der nicht mit dem Anteil Beschäftigter mit akademischem Bildungsabschluss korreliert. 
Eine Betrachtung des alternativen Indikators Schulabgänger mit Hochschulreife zeigt, dass dieser besser mit dem Anteil Beschäftigter mit akademischem Bildungsabschluss korreliert, aber dafür recht stark negativ mit dem ANteil der Beschäftigten ohne Abschluss. Das lässt den Schluss zu, dass in Regionen in denen auch ohne Abschluss eine Perspektive besteht 



## III.Imputation fehlender Werte

Das bisherige Imputationsmodell nutzt Arbeitslosigkeit als Prädiktoren. In den neuen Daten (2016) fehlen für diese Variable 6 Werte.
Der Einfachheit halber werden diese interpoliert.


```r
# Anzahl der Missings über die Indikatoren




# sehr häßliches Coding!!
Workfile_check <- Workfile %>%  filter(Jahr>=1998, Bevoelkerung>0) %>%  group_by(Gemeindeverband) %>% mutate(impu_arblos = na.approx(Arbeitslosigkeit), impu_abi = na.approx(SchulabgaengermitHochschulreife)) %>% select(-Arbeitslosigkeit, -SchulabgaengermitHochschulreife) %>% rename(Arbeitslosigkeit=impu_arblos, SchulabgaengermitHochschulreife=impu_abi)
rm(Workfile)
Workfile <- Workfile_check %>% ungroup(Gemeindeverband)

# Beispiel: Gemeindeverband==5154028

summary(Workfile$Arbeitslosigkeit)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.40   32.30   45.00   56.05   66.50  299.80
```

```r
summary(Workfile$SchulabgaengermitHochschulreife)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   18.12   23.22   23.88   28.95   70.32
```




```r
# Anzahl der Missings über die Indikatoren
summary(Workfile %>% select(listofdeterminants))
```

```
##  Arbeitslosigkeit Beschaeftigtenquote Bruttoverdienst
##  Min.   :  0.40   Min.   :  0.00      Min.   :1420   
##  1st Qu.: 32.30   1st Qu.: 49.73      1st Qu.:1871   
##  Median : 45.00   Median : 53.59      Median :2073   
##  Mean   : 56.05   Mean   : 53.72      Mean   :2100   
##  3rd Qu.: 66.50   3rd Qu.: 57.82      3rd Qu.:2291   
##  Max.   :299.80   Max.   :102.03      Max.   :4367   
##                   NA's   :33168       NA's   :22108  
##  BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss
##  Min.   : 3.33                 Min.   : 3.70             
##  1st Qu.: 6.76                 1st Qu.: 9.17             
##  Median : 8.31                 Median :11.52             
##  Mean   : 8.89                 Mean   :10.59             
##  3rd Qu.:10.32                 3rd Qu.:12.76             
##  Max.   :33.16                 Max.   :21.16             
##  NA's   :154756                NA's   :154756            
##  SchulabgaengermitHochschulreife SchulabgaengerohneAbschluss Einkommenssteuer 
##  Min.   : 0.00                   Min.   : 1.126              Min.   :  -1.33  
##  1st Qu.:18.12                   1st Qu.: 5.653              1st Qu.: 198.89  
##  Median :23.22                   Median : 7.513              Median : 274.20  
##  Mean   :23.88                   Mean   : 7.712              Mean   : 283.57  
##  3rd Qu.:28.95                   3rd Qu.: 9.536              3rd Qu.: 359.90  
##  Max.   :70.32                   Max.   :21.249              Max.   :4432.99  
##                                                              NA's   :33164    
##  Haushaltseinkommen Schuldnerquote 
##  Min.   : 995.4     Min.   : 3.62  
##  1st Qu.:1364.5     1st Qu.: 7.32  
##  Median :1534.6     Median : 8.91  
##  Mean   :1550.6     Mean   : 8.81  
##  3rd Qu.:1723.2     3rd Qu.:10.19  
##  Max.   :3260.9     Max.   :20.99  
##  NA's   :22108      NA's   :66324
```

```r
# sapply(Workfile  %>% select(listofdeterminants) , function(x) sum(is.na(x)))

# Imputation
imputationsliste <- subset(listofdeterminants , 
                           !(listofdeterminants %in%                              c('Arbeitslosigkeit','SchulabgaengermitHochschulreife','SchulabgaengerohneAbschluss')))
# Variablenliste für die Regressionsimputation wird erstellt
# das betrifft alle Variablen, außer die im angebenen Vektor
# letztere sind frei von Missings und können im Imputationsmodell genutzt werden 

Impdata <-  Workfile %>%  dplyr::filter(Jahr>=1998, Bevoelkerung>0) %>% 
  gather(key,value,6:15) %>% mutate(value=ifelse(value<0,NA,value)) %>% spread(key,value)
# Imputationsdatensatz generieren: Jahr>=1998, Bevoelkerung>0 
# gather und spread identifiziern key-Variablen automatisch 
# es geht aber nur darum Werten<0 ein NA zuzordnen

summary(Impdata %>% select(listofdeterminants))
```

```
##  Arbeitslosigkeit Beschaeftigtenquote Bruttoverdienst
##  Min.   :  0.40   Min.   :  0.00      Min.   :1420   
##  1st Qu.: 32.30   1st Qu.: 49.73      1st Qu.:1871   
##  Median : 45.00   Median : 53.59      Median :2073   
##  Mean   : 56.05   Mean   : 53.72      Mean   :2100   
##  3rd Qu.: 66.50   3rd Qu.: 57.82      3rd Qu.:2291   
##  Max.   :299.80   Max.   :102.03      Max.   :4367   
##                   NA's   :33168       NA's   :22108  
##  BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss
##  Min.   : 3.33                 Min.   : 3.70             
##  1st Qu.: 6.76                 1st Qu.: 9.17             
##  Median : 8.31                 Median :11.52             
##  Mean   : 8.89                 Mean   :10.59             
##  3rd Qu.:10.32                 3rd Qu.:12.76             
##  Max.   :33.16                 Max.   :21.16             
##  NA's   :154756                NA's   :154756            
##  SchulabgaengermitHochschulreife SchulabgaengerohneAbschluss Einkommenssteuer
##  Min.   : 0.00                   Min.   : 1.126              Min.   :   0.0  
##  1st Qu.:18.12                   1st Qu.: 5.653              1st Qu.: 198.9  
##  Median :23.22                   Median : 7.513              Median : 274.2  
##  Mean   :23.88                   Mean   : 7.712              Mean   : 283.6  
##  3rd Qu.:28.95                   3rd Qu.: 9.536              3rd Qu.: 359.9  
##  Max.   :70.32                   Max.   :21.249              Max.   :4433.0  
##                                                              NA's   :33167   
##  Haushaltseinkommen Schuldnerquote 
##  Min.   : 995.4     Min.   : 3.62  
##  1st Qu.:1364.5     1st Qu.: 7.32  
##  Median :1534.6     Median : 8.91  
##  Mean   :1550.6     Mean   : 8.81  
##  3rd Qu.:1723.2     3rd Qu.:10.19  
##  Max.   :3260.9     Max.   :20.99  
##  NA's   :22108      NA's   :66324
```

```r
sapply(Impdata  %>% select(listofdeterminants) , function(x) sum(is.na(x)))
```

```
##                Arbeitslosigkeit             Beschaeftigtenquote 
##                               0                           33168 
##                 Bruttoverdienst   BeschaeftigtemitakadAbschluss 
##                           22108                          154756 
##      BeschaeftigteohneAbschluss SchulabgaengermitHochschulreife 
##                          154756                               0 
##     SchulabgaengerohneAbschluss                Einkommenssteuer 
##                               0                           33167 
##              Haushaltseinkommen                  Schuldnerquote 
##                           22108                           66324
```

```r
# Einige Missings basierten auf Gebietsständen ohne Bevölkerung, diese sind entfernt 
# Damit käme auch die Einkommenssteuer als Prädiktor im Imputationsmodell in Frage

# Als erstes wird die Imputationsfunktion erstellt (hier werden noch keine Daten generiert)
# Impute_function (NOT FOR GROUPED DATA!)
my_ts_imputer <- function(data,outcome_name){
  mydata   <- data %>% group_by(Gemeindekennziffer) %>% select(Gemeindekennziffer,Jahr,Arbeitslosigkeit,SchulabgaengerohneAbschluss,SchulabgaengermitHochschulreife,"Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN)+I(Jahr*MEAN) + Arbeitslosigkeit + 
                   SchulabgaengerohneAbschluss ,
                   data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
  }
# Hier wird eine Funktion generiert, die im Datensatz (data) fehlende Daten für ausgewählte Variablen (outcome_name) imputiert
# 1. zunächst werden Mittelwerte für das Outcome (siehe select) jeweils für die Gemeinde generiert, d.h. über alle Wellen aggregiert
# 2. mymodell definiert das Modell (lm); "I()" sichert ab, dass der Operator * erkannt wird und dass ein Spaltenvektor in die Formel eingeht
# 3. zweites mydata: es wird eine Variable Imputed generiert, die sich aus der prediction aus mymodell ergibt
#    während der vorherige Befehl (mymodell) die Koeffizienten generiert, werden nun auf Basis dieses Modells predictions generiert, 
#    und zwar auch für Fälle mit Missing auf den Outcomes
# 4. fehlende Werte in den Outcomes werden durch Werte auf der Variable Imputed ersetzt
# 5. Für einige Fälle erzeugt die prediction unplausible Werte (negative Outcomes), diese werden auf 0 gesetzt
# 6. pull kreiert einen Vektor (hier Variable Outcome), die im nächsten Befehl verwendet wird

# Test Function if necessary
# Impdata %>% mutate(Test=my_ts_imputer(.,"Bruttoverdienst")) %>% select(Gemeindekennziffer,Jahr,Bruttoverdienst,Test) %>% head()

Impdata.imputed <- Impdata %>% mutate(
  Beschaeftigtenquote=my_ts_imputer(.,"Beschaeftigtenquote"),
  Bruttoverdienst=my_ts_imputer(.,"Bruttoverdienst"),
  BeschaeftigtemitakadAbschluss=my_ts_imputer(.,"BeschaeftigtemitakadAbschluss"),
  BeschaeftigteohneAbschluss=my_ts_imputer(.,"BeschaeftigteohneAbschluss"),
  Einkommenssteuer=my_ts_imputer(.,"Einkommenssteuer"),
  Haushaltseinkommen=my_ts_imputer(.,"Haushaltseinkommen"),
  Schuldnerquote=my_ts_imputer(.,"Schuldnerquote")           
  )
# hier wird der Datensatz mit den imputierten Werten generiert. Die Funktion my_ts_imputer wird auf jeden Indikator mit Missings angewendet

# Result of Imputation
summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))
```

```
##  Arbeitslosigkeit Beschaeftigtenquote Bruttoverdienst
##  Min.   :  0.40   Min.   :  0.00      Min.   :1341   
##  1st Qu.: 32.30   1st Qu.: 48.30      1st Qu.:1825   
##  Median : 45.00   Median : 52.43      Median :2032   
##  Mean   : 56.05   Mean   : 52.62      Mean   :2064   
##  3rd Qu.: 66.50   3rd Qu.: 57.01      3rd Qu.:2269   
##  Max.   :299.80   Max.   :102.03      Max.   :4367   
##  BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss
##  Min.   : 1.456                Min.   : 3.703            
##  1st Qu.: 4.113                1st Qu.: 9.401            
##  Median : 5.785                Median :11.653            
##  Mean   : 6.364                Mean   :10.733            
##  3rd Qu.: 7.953                3rd Qu.:12.899            
##  Max.   :33.156                Max.   :21.164            
##  SchulabgaengermitHochschulreife SchulabgaengerohneAbschluss Einkommenssteuer
##  Min.   : 0.00                   Min.   : 1.126              Min.   :   0.0  
##  1st Qu.:18.12                   1st Qu.: 5.653              1st Qu.: 174.0  
##  Median :23.22                   Median : 7.513              Median : 249.7  
##  Mean   :23.88                   Mean   : 7.712              Mean   : 264.1  
##  3rd Qu.:28.95                   3rd Qu.: 9.536              3rd Qu.: 342.1  
##  Max.   :70.32                   Max.   :21.249              Max.   :4433.0  
##  Haushaltseinkommen Schuldnerquote  
##  Min.   : 924.6     Min.   : 3.620  
##  1st Qu.:1315.3     1st Qu.: 7.410  
##  Median :1500.7     Median : 8.975  
##  Mean   :1516.4     Mean   : 8.902  
##  3rd Qu.:1699.2     3rd Qu.:10.370  
##  Max.   :3260.9     Max.   :20.992
```

```r
# Stata-Datensatz rausschreiben
# write_dta(Impdata.imputed, paste0("Outfiles/2019/Stata/impdata.dta"))
```





## IV. Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der Faktorscores

```
##  [1] "Arbeitslosigkeit"                "Beschaeftigtenquote"            
##  [3] "Bruttoverdienst"                 "BeschaeftigtemitakadAbschluss"  
##  [5] "BeschaeftigteohneAbschluss"      "SchulabgaengermitHochschulreife"
##  [7] "SchulabgaengerohneAbschluss"     "Einkommenssteuer"               
##  [9] "Haushaltseinkommen"              "Schuldnerquote"
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3831207 0.8495146 0.6044022
## 
## Rotation (n x k) = (3 x 3):
##                            PC1         PC2        PC3
## Beschaeftigtenquote  0.5155339  0.79166714  0.3278535
## Arbeitslosigkeit    -0.5713446  0.60273879 -0.5570200
## Bruttoverdienst      0.6385845 -0.09984534 -0.7630470
```

```
## [1] 1.3831207 0.8495146 0.6044022
```

![](GISD_Generate_2017_revision2020_files/figure-html/Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der Faktorscores-1.png)<!-- -->

```
## Standard deviations (1, .., p=3):
## [1] 1.3831207 0.8495146 0.6044022
## 
## Rotation (n x k) = (3 x 3):
##                            PC1         PC2        PC3
## Beschaeftigtenquote  0.5155339  0.79166714  0.3278535
## Arbeitslosigkeit    -0.5713446  0.60273879 -0.5570200
## Bruttoverdienst      0.6385845 -0.09984534 -0.7630470
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3831207 0.8495146 0.6044022
## 
## Rotation (n x k) = (3 x 1):
##                            PC1
## Beschaeftigtenquote  0.5155339
## Arbeitslosigkeit    -0.5713446
## Bruttoverdienst      0.6385845
```

```
## Standard deviations (1, .., p=3):
## [1] 1.4285145 0.8846057 0.4204986
## 
## Rotation (n x k) = (3 x 1):
##                           PC1
## Einkommenssteuer    0.6473353
## Haushaltseinkommen  0.6381538
## Schuldnerquote     -0.4167934
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3413074 0.8984677 0.6274155
## 
## Rotation (n x k) = (3 x 1):
##                                        PC1
## BeschaeftigtemitakadAbschluss    0.6446616
## SchulabgaengermitHochschulreife  0.6202977
## SchulabgaengerohneAbschluss     -0.4468134
```

```
##                                        PC1
## BeschaeftigtemitakadAbschluss    0.6446616
## SchulabgaengermitHochschulreife  0.6202977
## SchulabgaengerohneAbschluss     -0.4468134
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3413074 0.8984677 0.6274155
## 
## Rotation (n x k) = (3 x 1):
##                                        PC1
## BeschaeftigtemitakadAbschluss    0.6446616
## SchulabgaengermitHochschulreife  0.6202977
## SchulabgaengerohneAbschluss     -0.4468134
```

```
##  TS_Arbeitswelt      TS_Einkommen        TS_Bildung      
##  Min.   :-5.88039   Min.   :-4.35658   Min.   :-4.36196  
##  1st Qu.:-0.86787   1st Qu.:-1.00237   1st Qu.:-1.02830  
##  Median : 0.04317   Median :-0.07423   Median :-0.08433  
##  Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.00000  
##  3rd Qu.: 0.93872   3rd Qu.: 0.89350   3rd Qu.: 0.88470  
##  Max.   : 5.52912   Max.   :21.37798   Max.   : 8.47658
```

```
##                  Arbeitslosigkeit TS_Arbeitswelt TS_Einkommen TS_Bildung
## Arbeitslosigkeit        1.0000000     -0.7902386   -0.7231056 -0.2091344
## TS_Arbeitswelt         -0.7902386      1.0000000    0.8665142  0.5551099
## TS_Einkommen           -0.7231056      0.8665142    1.0000000  0.5396766
## TS_Bildung             -0.2091344      0.5551099    0.5396766  1.0000000
```

```
##                  Arbeitslosigkeit TS_Arbeitswelt TS_Einkommen TS_Bildung
## Arbeitslosigkeit        1.0000000      0.7902386    0.7231056  0.2091344
## TS_Arbeitswelt          0.7902386      1.0000000    0.8665142  0.5551099
## TS_Einkommen            0.7231056      0.8665142    1.0000000  0.5396766
## TS_Bildung              0.2091344      0.5551099    0.5396766  1.0000000
```

```
##                          Variable   Dimension            Anteil
## 1             Beschaeftigtenquote Arbeitswelt 0.265775202968329
## 2                Arbeitslosigkeit Arbeitswelt 0.326434677135791
## 3                 Bruttoverdienst Arbeitswelt  0.40779011989588
## 4                Einkommenssteuer   Einkommen 0.419042999516428
## 5              Haushaltseinkommen   Einkommen 0.407240284638186
## 6                  Schuldnerquote   Einkommen 0.173716715845386
## 7   BeschaeftigtemitakadAbschluss     Bildung 0.415588602820405
## 8 SchulabgaengermitHochschulreife     Bildung 0.384769224672262
## 9     SchulabgaengerohneAbschluss     Bildung 0.199642172507333
##                Score GISD Proportion
## 1  0.515533900891425 GISD       26.6
## 2 -0.571344622041541 GISD       32.6
## 3  0.638584465748956 GISD       40.8
## 4  0.647335306866872 GISD       41.9
## 5  0.638153809546089 GISD       40.7
## 6 -0.416793373082378 GISD       17.4
## 7  0.644661618851631 GISD       41.6
## 8  0.620297690365087 GISD       38.5
## 9 -0.446813353098733 GISD       20.0
```

```
##  TS_Arbeitswelt    TS_Einkommen      TS_Bildung       GISD_Score    
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.4023   1st Qu.:0.7960   1st Qu.:0.5913   1st Qu.:0.6448  
##  Median :0.4808   Median :0.8336   Median :0.6668   Median :0.7165  
##  Mean   :0.4846   Mean   :0.8307   Mean   :0.6602   Mean   :0.7122  
##  3rd Qu.:0.5607   3rd Qu.:0.8697   3rd Qu.:0.7403   3rd Qu.:0.7847  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000
```

```
## tibble [221,080 x 4] (S3: tbl_df/tbl/data.frame)
##  $ TS_Arbeitswelt: num [1:221080] 0.707 0.68 0.638 0.587 0.58 ...
##  $ TS_Einkommen  : num [1:221080] 0.968 0.961 0.95 0.928 0.927 ...
##  $ TS_Bildung    : num [1:221080] 0.692 0.659 0.626 0.652 0.665 ...
##  $ GISD_Score    : num [1:221080] 0.868 0.841 0.807 0.788 0.79 ...
```

## V.  Datenexport - Erstellung der Datensätze 



## VI.  Datensätze für PLZ generieren



# Allgemeine SOP für die Revision (nach Lars Kroll)
1. Neue Daten und Gebietsstände aus der INKAR-Datenbank herunterladen. Variablennamen und Formate überprüfen.
2. Postleitzahlen in GISD_generate_postcodes.R anhand der Gebietsstandsdatei überprüfen.
3. GISD_Generate.R ausführen

# Anknüpfungungspunkte für eine grundsätzliche Überarbeitung der GISD-Generierung 

Es gibt gute Gründe dafür am Konzept Bildung, Einkommen und Arbeitsweltindikatoren im GISD zu vereinen, auch wenn die Korrelation der Teildimensionen mit Einzelindikatoren der anderen Teildimensionen nur gering korrelieren. 
Es gibt andererseits Möglichkeiten den GISD weiter zu verbessern. Einzelne Schwachstellen sollen hier kurz erwähnt werden.

1. Missing Data 
* Hoher Anteil an Missing Data in den frühen Wellen
* Umgang mit Missing Data kann verbessert werden
2. Faktorenanalyse
* Bisher wird die Faktorenanalyse per pcf-Verfahren durchgeführt. Hier wäre zu prüfen, ob Common Factor-Verfahren oder konfirmatorische Faktorenanalyse zu einer Verbesserung führen könnten.
3.Indikatorenauswahl 
* Die Struktur der Faktorladungen der Bildungsindikatoren ist nicht robust gegenüber Datenschwankungen. Der erste Faktor bildet die intendierte Kompomente ab. Es gibt einen zweiten Faktor mit Eigenwert über 1. Die Gewichte der Faktorladungen der Indikatoren BeschaeftigteohneAbschluss und Schulgaengerohneabschluss variieren sehr stark zwischen den Revisionen 2018 und 2019. Hier könnte man über eine andere Auswahl von Indikatoren nachdenken. Die bisherigen Indikatoren BeschaeftigteohneAbschluss und BeschaeftigtemitHochschulabschluss dieser Teildimension weisen die höchsten Anteile an MissingData auf (75%). Zudem wurde bisher noch nicht berücksichtigt, dass die  zwischenzeitliche Verkürzung der Schulzeit für das Abitur (G8 Reform) und die spätere Rücknahme dieser Reform in einigen Bundesländern im Untersuchungszeitraum zu statistischen Artefakten in den Schulabgängerquoten führt.
4. Methodologische Grundlagen
* Diskussion der dem Messmodell zugrunde liegende Kausalmechanismen 
Dimensionen sozioökonomischer Deprivation auf räumlicher Ebene: Einkommen, Arbeitswelt, Bildung
- Einkommen: (HH-Einkommen, Steueraufkommen, Schuldnerquote)
- Kaufkraft berücksichtigen, Vermögen berücksichtigen
- betrifft Handlungsspielräume der Kommunen, Proxy für Wirtschaftskraft der Kommunen


