---
title: "GISD - German Index of Socio-Economic Deprivation"
author: "Niels Michalski auf Basis der Syntax von Lars Kroll"
date: "04 04 2020"
output: 
  html_document:
    keep_md: true
    code_folding: hide
    toc: true
    toc_float: false
    toc_depth: 2
---



# Intro
...

## Fehleranalyse
In der Revision 2019 der GISD-Daten tauchten zum Teil starke Abweichungen zur Revision 2018 auf.Bei der Addition der Teildimensionen ging die Bildungsdimension in umgekehrter Richtung in den GISD ein. 
Ausschlaggebend ist eine negative Korrelation der Bildungsdimension mit dem Anteil Arbeitsloser, die im Code der vergangenen Revisionen zu einer Umpolung des Teilscores fÃ¼hrt. Der Anteil Arbeitsloser wird als Markerindikator verwendet. 



# Beschreibung der Syntax
## 0. BenÃ¶tigte Pakete


```r
library("tidyverse") # Tidyverse Methods
library("readxl") # Read Excel
library("imputeTS") # Impute Missing Features
library("haven") # write Stata-dta
library("sf") # write Stata-dta

# Create Output directories in working directory if necessary
dir.create("Outfiles")
dir.create("Outfiles/2019")
dir.create("Outfiles/2019/Bund")
dir.create("Outfiles/2019/Other")
dir.create("Outfiles/2019/Stata")
```

## I.  Generierung eines ID-Datensatzes

Generierung eines Datensatzes in dem den kleinsten regionalen Einheiten (Gemeinden) alle
Ã¼bergeordneten regionalen Einheiten und deren Regionalkennziffern zugeordnet werden. 
Datenquelle ist die Gebietsstandsreferenz von Destatis.


```r
Gemeinden_INKAR <- read_excel("Data/Referenz/Referenz_1998_2015.xlsx", sheet = "Gemeinden", na = "NA", skip = 2) %>% 
  rename(Kennziffer=gem15,"Kennziffer Gemeindeverband"="Gemeindeverband, Stand 31.12.2015") %>% filter(!is.na(Kennziffer))
# Pipes: 
# 1. rename von zwei Variablen; " um Leerzeichen zu berÃ¼cksichtigen; 
# 2. Gemeinden ohne Missing auf der Kennziffervariablen

Gemeindeverbaende_INKAR <- read_excel("Data/Referenz/Referenz_1998_2015.xlsx", sheet = "GVB 2015", na = "NA", skip = 2) %>% 
  select("Kennziffer Gemeindeverband"=gvb15,"Name des Gemeindeverbands") %>% filter(!is.na("Kennziffer Gemeindeverband")) 
# das ganze nochmal fÃ¼r Gemeindeverbaende  
# Pipes: 
# 1. nur die Variablen gvb15 und Name des Gemeindeverbands ausgewÃ¤hlt 
# 2. Missing herausfiltern

Kreise_INKAR <- read_excel("Data/Referenz/Referenz_1998_2015.xlsx", sheet = "Kreise", skip = 2) %>%
 mutate(Kennziffer = as.numeric(krs15)/1000) %>% filter(!is.na(Kennziffer))
# und fÃ¼r Kreise
# Pipes: 
# 1. neue Variable generieren, die die Kreisvariable auf den FÃ¼nfsteller reduzieren
# 2. Missing herausfiltern


# Die drei DatensÃ¤tze werden nun ausgehend vom Gemeindedatensatz zu einem ID-Datensatz zusammmengefÃ¼gt
id_dataset <- Gemeinden_INKAR %>% 
              select(Gemeindekennziffer=Kennziffer,"Name der Gemeinde"=`Gemeindename 2015`,"Kennziffer Gemeindeverband") %>% 
              mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
              left_join(.,Kreise_INKAR %>% select(Kreiskennziffer=krs15,
                                                  "Name des Kreises"=krs15name,
                                                  "Raumordnungsregion Nr"=ROR11,
                                                  Raumordnungsregion=ROR11name,
                                                  NUTS2,
                                                  "NUTS2 Name"=NUTS2name,
                                                  Bundesland=...24) %>% mutate(Kreiskennziffer=floor(Kreiskennziffer/1000)),by="Kreiskennziffer") %>% left_join(.,Gemeindeverbaende_INKAR, by="Kennziffer Gemeindeverband")

# Pipes:  1. (select) Variablenauswahl (gkz, Gemeindename, Gemeindeverband)[wieso hier ``?]
#         2. die Kreiskennziffer wird aus der Gemeindekennziffer generiert; floor rundet nach unten auf ganze Ziffern ab
#         3. leftjoin spielt Kreisdaten Ã¼ber Kreiskennziffer an
#         3.1 select wÃ¤hlt, die anzupielenden Variablen aus, darunter auch NUTS und ROR und Bundesland, dessen Variablenname beim       #             Einlesen zu lang war (...24)
#         3.2 die Kreiskennziffer wurde vor dem leftjoin im Using-Datensatz generiert
#         4. als letztes werden die Gemeindeverbandskennziffern angespielt
```


## II. Erzeugen eines Datensatzes mit Kennziffern als ID unabhÃ¤ngig von der Ebene 

In diesem Abschnitt werden die INKAR-Daten zu den Indikatoren in einem Datensatz zusammengefÃ¼hrt. Die Information fÃ¼r die Indikatoren, die fÃ¼r die Berechnung des GISD verwendet werden, liegt auf unterschiedlichen Ebenen vor. Die Faktorenanalyse soll auf Gemeindeebene durchgefÃ¼hrt werden, weshalb Information der Kreisebene an die Gemeinden innerhalb der Kreise angespielt werden. Percentile aus dem Index kÃ¶nen spÃ¤ter fÃ¼r jede regionale Ebene separat berechnet werden. Datenbasis sind die INKAR-Daten der jeweiligen Indikatoren im Excel-Format


```r
# Basis erzeugen: Ausgangspunkt Kreisdaten
# Es werden Indikatoren allen Ebenen angespielt, als erstes die Kreise.
Basedata    <- Kreise_INKAR %>% select(Kennziffer) %>% mutate(Jahr=2015)
# Datensatz zum Anspielen der Daten generieren
# Ausgangspunkt Kreisdatensatz
# Pipes:  1. nur Kreiskennzifern ausgewÃ¤hlt
#         2. Jahresvariable generiert (2015)

# Liste der Variablen generieren
inputdataset <- list.files("Data/INKAR_1998_2015") # Variablenliste der Dateinamen im Ordner


# Einlesen der einzelnen Excelfiles zu den Daten (Schleife) 
# for testing file<-inputdataset[1]
for(file in inputdataset){
  myimport <- read_excel(paste0("Data/INKAR_1998_2015/",file), skip = 1, sheet = "Daten", col_types = c("text"))
  names(myimport)[1] <- "Kennziffer"
  myimport[3] <- NULL
  myimport[2] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value)) 
  names(myimport)[3] <- unlist(strsplit(unlist(strsplit(file,"_"))[2],"[.]"))[1]
  Basedata <- full_join(Basedata,myimport,by=c("Kennziffer","Jahr"))
}
# Schleife fÃ¼r jedes Excel-File
# 1. Einlesen der Exceldatei; jeweils das Sheet "Daten"; erste Zeile wird geskippt,  die Daten werden als Text eingelesen
# 2. fÃ¼r die erste Spalte wird die Kennziffer importiert; fÃ¼r die zweite und dritte Spalte nichts
# 3. die Daten werde reshaped, um die Jahresinfos im langen Format zu speichern; convert konvertiert das Datenformat automatisch;
# rm.na entfert missing value Zeilen; -"Kennziffer" sorgt dafÃ¼r, dass die Variable Kennziffer nicht doppelt erzeugt wird
# 4. mutate definiert die Variablentypen 
# 5. von innen nach auÃen 
# 5.1 das innere strsplit(file, "_") teilt den Filenamen inkl. Dateiendung beim "_"
# 5.2 das innerste unlist generiert einen Vektor mit den Elementen aus dem strsplit
# 5.3 das Ã¤uÃere strsplit das zweite Vektorelement beim ".", sodass nur noch der Variablenname Ã¼brig bleibt
# 5.4 das Ã¤uÃere unlist weist auf das erste Vektorelement 
# 5.5 names(import)[3] nimmt dieses Vektorelement als Variablennamen fÃ¼r die dritte Spalte
# 6. jedes file der Schleife wird an Basedata gejoint Ã¼ber Kennziffer und Jahr; full_join Ã¼bernimmt dabei jede Zeile und Spalte jeder Seite,
# auch wenn die Werte auf einer Seite missing enthalten

rm(inputdataset) 


# Liste der Indikatoren erstellen
listofdeterminants <- names(Basedata)[3:length(Basedata)]

# Regionale Tiefe der Indikatoren 
ind_level <- c("Gemeindeverband","Gemeindeverband","Kreis", "Kreis", "Kreis", "Kreis", "Kreis", "Gemeinde", "Kreis", "Kreis")
level_table <- cbind(listofdeterminants,ind_level)
# Tabelle der Indikatoren mit regionaler Tiefe
ind_col = c("Indikator","Tiefe des Indikators")
knitr::kable(level_table, col.names = ind_col)
```



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

```r
# Datensatz fÃ¼r die Gemeindeverbandsebene generieren
Basedata_Gemeindeverbandsebene <- Basedata %>% dplyr::select(Kennziffer,Jahr,Arbeitslosigkeit,Beschaeftigtenquote,Einkommenssteuer) %>%   
  gather(key,value,3:5) %>% filter(!is.na(value)) %>% spread(key,value) %>% filter(Jahr>=1998) %>% rename("Gemeindeverband"=Kennziffer)
# Pipes:  1. Auswahl der Variablen 
#         2. Reshape der Daten wide nach long      
#         3. Auswahl von Non-Missing 
#         4. Reshape von long nach wide 
#         5. Auswahl der Daten Jahr>=1998
#         6. Umbenennung der Kennziffervariable

# Datensatz fÃ¼r die Kreisebene generieren 
Basedata_Kreisebene <- Basedata %>% select(krs15=Kennziffer,Jahr,listofdeterminants) %>% 
  select(-Arbeitslosigkeit,-Einkommenssteuer,-Beschaeftigtenquote) %>% rename(Kreis=krs15)
# Pipes:  1. neben der Kennziffer, die einen anderen Namen bekommt wird das Jahr und die Variablenliste ausgewÃ¤hlt
#         2. drei Variablen werden aus der Auswahl ausgeschlossen
#         3. die Kreisvariable wird in Kreis umbenannt, weil im nÃ¤chsten Schritt Kreisinfos an die Gemeinden angespielt werden

# Join different levels
# Nun werden die Daten bezogen auf die Ebenen gemergt
Workfile <- as.data.frame(expand.grid("Kennziffer"=Gemeinden_INKAR %>% pull(Kennziffer),"Jahr"=seq(min(Basedata$Jahr):max(Basedata$Jahr)) + min(Basedata$Jahr)-1)) %>%
   mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>% as_tibble() %>%
   left_join(. , Gemeinden_INKAR,by=c("Kennziffer"))  %>%
   select(Gemeindekennziffer=Kennziffer,Kreis=Kreiskennziffer,Gemeindeverband="Kennziffer Gemeindeverband",Jahr,Bevoelkerung=`BevÃ¶lkerung 31.12.2015`) %>% 
      arrange(Gemeindekennziffer,Jahr) %>% # Join Metadata
   left_join(. , Basedata_Kreisebene,by=c("Kreis","Jahr")) %>% # Hier wird Ã¼ber Kreis gematched
   left_join(. , Basedata_Gemeindeverbandsebene,by=c("Gemeindeverband","Jahr")) %>%  # Join Indicators for Level: Gemeindeverband 
   filter(Jahr>=1998)

# als erstes wird ein data.frame erzeugt (Workfile); der alle Gemeindewellen (1998-201x) in den Zeilen stehen hat
# 1. expand.grid erzeugt ein tibble mit allen Kombinationen von Kennziffern und Jahren
#     pull erzeugt einen Vektor fÃ¼r die Variablenwerte von Kennziffer aus dem Datensatz
#     + min(...) wird zu der Sequenz von Jahren aus dem Basedata addiert (1 bis X) damit auch Jahreswerte weitergeben werden
# 2. mutate generiert eine Kreiskennziffer
# 3. as.tibble erzeugt einen tibble, damit left_join genutzt werden kann
# 4. erstes left_join spielt die Gemeindedaten Ã¼ber Kennziffer an, das geht so, weil Gemeinden_INKAR als tibble gespeichert ist
# 5. select, wÃ¤hlt die inhaltlichen Variablen aus, und Ã¤ndert die Variablennamen; 
# 6. arrange im select sortiert nach Gemeindekennziffer und Jahr
# 7. zweites left_join spielt die Daten der Kreisebene via Kreis und Jahr an
# 8. drittes left_join spielt die Daten der Gemeindeverbandsebene via Gemeindeverband und Jahr an
# Notiz: . in den Befehlen bezieht sich auf den tibble bzw. data.frame der in der Pipe bearbeitet wird

# Stata-Datensatz rausschreiben
write_dta(Workfile, paste0("Outfiles/2019/Stata/workfile.dta"))

# Ende Generierung Basisdatensatz
```



## III.Imputation fehlender Werte



```r
# Anzahl der Missings Ã¼ber die Indikatoren
summary(Workfile %>% select(listofdeterminants))
```

```
##  Arbeitslosigkeit Beschaeftigtenquote Bruttoverdienst
##  Min.   :  0.00   Min.   :  0.00      Min.   :1419   
##  1st Qu.: 33.61   1st Qu.: 49.29      1st Qu.:1847   
##  Median : 46.54   Median : 52.82      Median :2023   
##  Mean   : 57.87   Mean   : 52.84      Mean   :2053   
##  3rd Qu.: 69.20   3rd Qu.: 56.56      3rd Qu.:2229   
##  Max.   :500.00   Max.   :102.03      Max.   :4288   
##                   NA's   :34596       NA's   :22332  
##  BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss
##  Min.   : 3.33                 Min.   : 3.44             
##  1st Qu.: 6.42                 1st Qu.: 9.46             
##  Median : 7.92                 Median :11.99             
##  Mean   : 8.51                 Mean   :10.93             
##  3rd Qu.: 9.99                 3rd Qu.:13.32             
##  Max.   :31.85                 Max.   :20.51             
##  NA's   :156324                NA's   :156324            
##  SchulabgaengermitHochschulreife SchulabgaengerohneAbschluss
##  Min.   : 0.00                   Min.   : 1.126             
##  1st Qu.:17.51                   1st Qu.: 5.773             
##  Median :22.50                   Median : 7.679             
##  Mean   :23.16                   Mean   : 7.849             
##  3rd Qu.:27.87                   3rd Qu.: 9.714             
##  Max.   :70.32                   Max.   :21.249             
##                                                             
##  Einkommenssteuer   Haushaltseinkommen Schuldnerquote 
##  Min.   :  -1.326   Min.   : 995       Min.   : 3.00  
##  1st Qu.: 186.993   1st Qu.:1344       1st Qu.: 7.00  
##  Median : 253.012   Median :1498       Median : 8.00  
##  Mean   : 257.310   Mean   :1515       Mean   : 8.28  
##  3rd Qu.: 329.460   3rd Qu.:1678       3rd Qu.:10.00  
##  Max.   :1163.686   Max.   :3260       Max.   :20.00  
##  NA's   :1316       NA's   :22332      NA's   :66996
```

```r
sapply(Workfile  %>% select(listofdeterminants) , function(x) sum(is.na(x)))
```

```
##                Arbeitslosigkeit             Beschaeftigtenquote 
##                               0                           34596 
##                 Bruttoverdienst   BeschaeftigtemitakadAbschluss 
##                           22332                          156324 
##      BeschaeftigteohneAbschluss SchulabgaengermitHochschulreife 
##                          156324                               0 
##     SchulabgaengerohneAbschluss                Einkommenssteuer 
##                               0                            1316 
##              Haushaltseinkommen                  Schuldnerquote 
##                           22332                           66996
```

```r
# Imputation
imputationsliste <- subset(listofdeterminants , 
                           !(listofdeterminants %in%                              c('Arbeitslosigkeit','SchulabgaengermitHochschulreife','SchulabgaengerohneAbschluss')))
# Variablenliste fÃ¼r die Regressionsimputation wird erstellt
# das betrifft alle Variablen, auÃer die im angebenen Vektor
# letztere sind frei von Missings und kÃ¶nnen im Imputationsmodell genutzt werden 

Impdata <-  Workfile %>%  dplyr::filter(Jahr>=1998, Bevoelkerung>0) %>% 
  gather(key,value,6:15) %>% mutate(value=ifelse(value<0,NA,value)) %>% spread(key,value)
# Imputationsdatensatz generieren: Jahr>=1998, Bevoelkerung>0 
# gather und spread identifiziern key-Variablen automatisch 
# es geht aber nur darum Werten<0 ein NA zuzordnen

summary(Impdata %>% select(listofdeterminants))
```

```
##  Arbeitslosigkeit   Beschaeftigtenquote Bruttoverdienst
##  Min.   :  0.3747   Min.   :  0.00      Min.   :1419   
##  1st Qu.: 33.8561   1st Qu.: 49.29      1st Qu.:1846   
##  Median : 46.7480   Median : 52.82      Median :2021   
##  Mean   : 58.2550   Mean   : 52.84      Mean   :2052   
##  3rd Qu.: 69.4340   3rd Qu.: 56.57      3rd Qu.:2229   
##  Max.   :299.8205   Max.   :102.03      Max.   :4288   
##                     NA's   :33276       NA's   :22184  
##  BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss
##  Min.   : 3.33                 Min.   : 3.44             
##  1st Qu.: 6.42                 1st Qu.: 9.38             
##  Median : 7.92                 Median :11.99             
##  Mean   : 8.51                 Mean   :10.92             
##  3rd Qu.:10.01                 3rd Qu.:13.32             
##  Max.   :31.85                 Max.   :20.51             
##  NA's   :155288                NA's   :155288            
##  SchulabgaengermitHochschulreife SchulabgaengerohneAbschluss
##  Min.   : 0.00                   Min.   : 1.126             
##  1st Qu.:17.53                   1st Qu.: 5.776             
##  Median :22.53                   Median : 7.681             
##  Mean   :23.19                   Mean   : 7.853             
##  3rd Qu.:27.89                   3rd Qu.: 9.715             
##  Max.   :70.32                   Max.   :21.249             
##                                                             
##  Einkommenssteuer Haushaltseinkommen Schuldnerquote 
##  Min.   :   0.0   Min.   : 995       Min.   : 3.00  
##  1st Qu.: 187.0   1st Qu.:1343       1st Qu.: 7.00  
##  Median : 253.0   Median :1498       Median : 8.00  
##  Mean   : 257.3   Mean   :1514       Mean   : 8.28  
##  3rd Qu.: 329.5   3rd Qu.:1677       3rd Qu.:10.00  
##  Max.   :1163.7   Max.   :3260       Max.   :20.00  
##  NA's   :5        NA's   :22184      NA's   :66552
```

```r
sapply(Impdata  %>% select(listofdeterminants) , function(x) sum(is.na(x)))
```

```
##                Arbeitslosigkeit             Beschaeftigtenquote 
##                               0                           33276 
##                 Bruttoverdienst   BeschaeftigtemitakadAbschluss 
##                           22184                          155288 
##      BeschaeftigteohneAbschluss SchulabgaengermitHochschulreife 
##                          155288                               0 
##     SchulabgaengerohneAbschluss                Einkommenssteuer 
##                               0                               5 
##              Haushaltseinkommen                  Schuldnerquote 
##                           22184                           66552
```

```r
# Einige Missings basierten auf GebietsstÃ¤nden ohne BevÃ¶lkerung, diese sind entfernt 
# Damit kÃ¤me auch die Einkommenssteuer als PrÃ¤diktor im Imputationsmodell in Frage

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
# Hier wird eine Funktion generiert, die im Datensatz (data) fehlende Daten fÃ¼r ausgewÃ¤hlte Variablen (outcome_name) imputiert
# 1. zunÃ¤chst werden Mittelwerte fÃ¼r das Outcome (siehe select) jeweils fÃ¼r die Gemeinde generiert, d.h. Ã¼ber alle Wellen aggregiert
# 2. mymodell definiert das Modell (lm); "I()" sichert ab, dass der Operator * erkannt wird und dass ein Spaltenvektor in die Formel eingeht
# 3. zweites mydata: es wird eine Variable Imputed generiert, die sich aus der prediction aus mymodell ergibt
#    wÃ¤hrend der vorherige Befehl (mymodell) die Koeffizienten generiert, werden nun auf Basis dieses Modells predictions generiert, 
#    und zwar auch fÃ¼r FÃ¤lle mit Missing auf den Outcomes
# 4. fehlende Werte in den Outcomes werden durch Werte auf der Variable Imputed ersetzt
# 5. FÃ¼r einige FÃ¤lle erzeugt die prediction unplausible Werte (negative Outcomes), diese werden auf 0 gesetzt
# 6. pull kreiert einen Vektor (hier Variable Outcome), die im nÃ¤chsten Befehl verwendet wird

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
##  Arbeitslosigkeit   Beschaeftigtenquote Bruttoverdienst
##  Min.   :  0.3747   Min.   :  0.00      Min.   :1369   
##  1st Qu.: 33.8561   1st Qu.: 47.96      1st Qu.:1803   
##  Median : 46.7480   Median : 51.75      Median :1992   
##  Mean   : 58.2550   Mean   : 51.82      Mean   :2020   
##  3rd Qu.: 69.4340   3rd Qu.: 55.74      3rd Qu.:2206   
##  Max.   :299.8205   Max.   :102.03      Max.   :4288   
##  BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss
##  Min.   : 1.338                Min.   : 3.44             
##  1st Qu.: 3.830                1st Qu.:10.06             
##  Median : 5.379                Median :13.32             
##  Mean   : 5.923                Mean   :12.23             
##  3rd Qu.: 7.391                3rd Qu.:15.02             
##  Max.   :31.846                Max.   :23.92             
##  SchulabgaengermitHochschulreife SchulabgaengerohneAbschluss
##  Min.   : 0.00                   Min.   : 1.126             
##  1st Qu.:17.53                   1st Qu.: 5.776             
##  Median :22.53                   Median : 7.681             
##  Mean   :23.19                   Mean   : 7.853             
##  3rd Qu.:27.89                   3rd Qu.: 9.715             
##  Max.   :70.32                   Max.   :21.249             
##  Einkommenssteuer Haushaltseinkommen Schuldnerquote  
##  Min.   :   0.0   Min.   : 929.8     Min.   : 3.000  
##  1st Qu.: 187.0   1st Qu.:1297.0     1st Qu.: 7.000  
##  Median : 253.0   Median :1464.0     Median : 8.602  
##  Mean   : 257.3   Mean   :1480.4     Mean   : 8.473  
##  3rd Qu.: 329.5   3rd Qu.:1648.0     3rd Qu.:10.000  
##  Max.   :1163.7   Max.   :3260.0     Max.   :20.579
```

```r
# Stata-Datensatz rausschreiben
# write_dta(Impdata.imputed, paste0("Outfiles/2019/Stata/impdata.dta"))
```




## IV. Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der Faktorscores

```r
# Variablenliste fÃ¼r die Faktorenanalyse 
print(listofdeterminants)
```

```
##  [1] "Arbeitslosigkeit"                "Beschaeftigtenquote"            
##  [3] "Bruttoverdienst"                 "BeschaeftigtemitakadAbschluss"  
##  [5] "BeschaeftigteohneAbschluss"      "SchulabgaengermitHochschulreife"
##  [7] "SchulabgaengerohneAbschluss"     "Einkommenssteuer"               
##  [9] "Haushaltseinkommen"              "Schuldnerquote"
```

```r
TS_Arbeitswelt <- Impdata.imputed %>% dplyr::select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst) 
TS_Einkommen   <- Impdata.imputed %>% dplyr::select(Einkommenssteuer,Haushaltseinkommen,Schuldnerquote) 
# fÃ¼r den Vergleich der Ergebnisse wird zunÃ¤chst ein Datensatz fÃ¼r die ursprÃ¼ngliche Variablenauswahl der Revision 2019 generiert
TS_Bildung_old  <- Impdata.imputed %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 
# dann die aktuelle InterimslÃ¶sung
TS_Bildung <- Impdata.imputed %>% dplyr::select(BeschaeftigtemitakadAbschluss,SchulabgaengermitHochschulreife,SchulabgaengerohneAbschluss) 
# hier wurde die Variable BeschaeftigteohneAbschluss durch SchulabgaengermitHochschulreife ersetzt


# Faktorenanalyse basierend auf Hauptkomponentenanalyse fÃ¼r jede der drei Subscalen
# Arbeitswelt: zunÃ¤chst Analyse der FaktorlÃ¶sung
TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE)
	# Option retx erzeugt rotierte LÃ¶sung
head(TS_Arbeitswelt.pca$sdev)
```

```
## [1] 1.3561452 0.8791822 0.6228232
```

```r
# nur die erste Komponente mit Eigenwert Ã¼ber 1
	# (prcomp gibt standardmÃ¤Ãig Sdev statt Varianz aus)
plot(TS_Arbeitswelt.pca)
```

![](GISD_Generate_2015_revision2020_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
	# screeplot - bei nur drei Variablen wird ein Balkendiagramm angezeigt
TS_Arbeitswelt.pca
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3561452 0.8791822 0.6228232
## 
## Rotation (n x k) = (3 x 3):
##                            PC1        PC2        PC3
## Beschaeftigtenquote  0.4827883  0.8433030  0.2361260
## Arbeitslosigkeit    -0.5931929  0.5132653 -0.6202265
## Bruttoverdienst      0.6442342 -0.1593698 -0.7480398
```

```r
# die Faktorladungen der drei Hauptkomponenten fÃ¼r Arbeitswelt 
# die Ladungen der ersten Komponente enstprechen der Erwartung

TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)
# die Option rank erlaubt die BeschrÃ¤nkung der ANzahl an Komponenten (Faktoren)
TS_Arbeitswelt.pca
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3561452 0.8791822 0.6228232
## 
## Rotation (n x k) = (3 x 1):
##                            PC1
## Beschaeftigtenquote  0.4827883
## Arbeitslosigkeit    -0.5931929
## Bruttoverdienst      0.6442342
```

```r
# Hauptkomponentenanalyse fÃ¼r die Einkommensdimension
TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE) 
TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Einkommen.pca
```

```
## Standard deviations (1, .., p=3):
## [1] 1.4411216 0.8543562 0.4395953
## 
## Rotation (n x k) = (3 x 1):
##                           PC1
## Einkommenssteuer   -0.6368996
## Haushaltseinkommen -0.6268504
## Schuldnerquote      0.4487957
```

```r
# Hauptkomponentenanalyse fÃ¼r die Bildungsdimension
TS_Bildung_old.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE) 
# fÃ¼r die Bildung deutet die Analyse eher auf zwei Komponenten hin
# die Faktorladung fÃ¼r SchulabgaengerohneAbschluss ist auf dem ersten Faktor schwach, 
# die Faktorladung fÃ¼r BeschaeftigtemitakadAbschluss auf dem zweiten
# es wird die Komponente ausgewÃ¤hlt, bei der Beschaeftigte mit akad Abschluss positiv korreliert und 
# BeschaeftigteohneAbschluss und SchulabgaengerohneAbschluss negativ
# regionale Deprivation als Merkmal geringer Anteile von Akademikern bei gleichzeitigen hohen Anteilen 
# von Beschaeftigten ohne Abschluss und Schulabgaengern ohne Abschluss
TS_Bildung_old.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Bildung_old.pca
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3378311 0.9049775 0.6254788
## 
## Rotation (n x k) = (3 x 1):
##                                        PC1
## BeschaeftigtemitakadAbschluss    0.6478416
## SchulabgaengermitHochschulreife  0.6230148
## SchulabgaengerohneAbschluss     -0.4383535
```

```r
TS_Bildung_old.pca$rotation
```

```
##                                        PC1
## BeschaeftigtemitakadAbschluss    0.6478416
## SchulabgaengermitHochschulreife  0.6230148
## SchulabgaengerohneAbschluss     -0.4383535
```

```r
# InterimslÃ¶sung Bildungskomponente mit BeschaeftigtemitakadAbschluss,SchulabgaengermitHochschulreife,SchulabgaengerohneAbschluss
TS_Bildung.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Bildung.pca
```

```
## Standard deviations (1, .., p=3):
## [1] 1.3378311 0.9049775 0.6254788
## 
## Rotation (n x k) = (3 x 1):
##                                        PC1
## BeschaeftigtemitakadAbschluss    0.6478416
## SchulabgaengermitHochschulreife  0.6230148
## SchulabgaengerohneAbschluss     -0.4383535
```

```r
# Es wurde auÃerdem eine Komponentenanalyse mit allen vier Bildungsindikatoren durchgefÃ¼hrt. 
# Aber auch hier bestand das Problem, der inkonsistenten Korrelationen zwischen den Teildimensionen.

# Nun wird die Generierung der Faktorscores vorbereitet.

# Componentoverview
GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Anteil"=TS_Arbeitswelt.pca$rotation^2,"Score"=TS_Arbeitswelt.pca$rotation) 
# cbind erstellt Spaltenvektoren mit den Infos aus Teildimension, den (rotierten) Faktorladungen und den Components; 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Anteil"=TS_Einkommen.pca$rotation^2,"Score"=TS_Einkommen.pca$rotation)) 
# rbind erstellt Zeilenvektoren, diese werden hier in die bereits vorhandenen Spaltenvektoren eingebunden
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung","Anteil"=TS_Bildung.pca$rotation^2,"Score"=TS_Bildung.pca$rotation)) 
# auch fÃ¼r die Teildimension Bildung werden Zeilenvektoren eingebunden
GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))
# als letztes wird die Matrix in einen Dataframe Ã¼bersetzt

rownames(GISD_Komponents) <- NULL
# die Ã¼berflÃ¼ssigen Zeilennamen werden gestrichen
colnames(GISD_Komponents) <- c("Variable","Dimension","Anteil","Score")
# aussagekrÃ¤ftige Spaltennamen vergeben
GISD_Komponents$GISD <- "GISD"
# eine weitere Spalte wird eingefÃ¼gt mit dem String "GISD" in jeder Zeile
GISD_Komponents$Proportion <- round(as.numeric(as.character(GISD_Komponents$Anteil))*100,digits=1)
# eine weitere Spalte Proportion wird eingefÃ¼gt mit prozentualen Anteilswerten (eine Nachkommastelle)

# Hier findet die Prediction der Scores statt
Resultdataset <- Impdata.imputed
Resultdataset$TS_Arbeitswelt <- as.numeric(predict(TS_Arbeitswelt.pca, newdata = Impdata.imputed))
Resultdataset$TS_Einkommen <- as.numeric(predict(TS_Einkommen.pca , newdata = Impdata.imputed))
Resultdataset$TS_Bildung <- as.numeric(predict(TS_Bildung.pca, newdata = Impdata.imputed))

summary(Resultdataset %>% dplyr::select(TS_Arbeitswelt, TS_Einkommen, TS_Bildung))
```

```
##  TS_Arbeitswelt      TS_Einkommen        TS_Bildung     
##  Min.   :-5.75213   Min.   :-7.37246   Min.   :-4.2283  
##  1st Qu.:-0.82308   1st Qu.:-0.92273   1st Qu.:-1.0177  
##  Median : 0.05911   Median : 0.04721   Median :-0.1026  
##  Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000  
##  3rd Qu.: 0.91194   3rd Qu.: 0.98654   3rd Qu.: 0.8509  
##  Max.   : 5.91914   Max.   : 4.34194   Max.   : 8.5131
```

```r
# Korrelationen Ã¼berprÃ¼fen
Resultdataset %>% dplyr::select(Arbeitslosigkeit,TS_Arbeitswelt,TS_Einkommen,TS_Bildung)  %>% cor( use="pairwise.complete.obs")  
```

```
##                  Arbeitslosigkeit TS_Arbeitswelt TS_Einkommen TS_Bildung
## Arbeitslosigkeit        1.0000000     -0.8044557    0.7507066 -0.1570911
## TS_Arbeitswelt         -0.8044557      1.0000000   -0.8729769  0.4945504
## TS_Einkommen            0.7507066     -0.8729769    1.0000000 -0.4848497
## TS_Bildung             -0.1570911      0.4945504   -0.4848497  1.0000000
```

```r
# die Richtung der Skala der Scores ist nach der Generierung willkÃ¼rlich 
# sie werden nun anhand der Variable Arbeitslosigkeit ausgerichtet,
# d.h. sie werden so gepolt, dass sie positiv mit Arbeitslosigkeit korrelieren, um Deprivation abzubilden:

if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung,use="pairwise.complete.obs")<0) {
   Resultdataset$TS_Bildung <- Resultdataset$TS_Bildung*-1
   }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1
  }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1
}

# Korrelationen erneut Ã¼berprÃ¼fen
Resultdataset %>% dplyr::select(Arbeitslosigkeit,TS_Arbeitswelt,TS_Einkommen,TS_Bildung) %>% cor( use="pairwise.complete.obs")
```

```
##                  Arbeitslosigkeit TS_Arbeitswelt TS_Einkommen TS_Bildung
## Arbeitslosigkeit        1.0000000      0.8044557    0.7507066  0.1570911
## TS_Arbeitswelt          0.8044557      1.0000000    0.8729769  0.4945504
## TS_Einkommen            0.7507066      0.8729769    1.0000000  0.4848497
## TS_Bildung              0.1570911      0.4945504    0.4848497  1.0000000
```

```r
# nun sind alle Korrelationen positiv
# wenngleich die Korrelation der Bildungsdimension mit Arbeitslosigkeit sehr gering ist
# inhaltlich ist das nicht unplausibel (hÃ¶here Abiturquoten in strukturschwachen Regionen)

GISD_Komponents
```

```
##                          Variable   Dimension            Anteil
## 1             Beschaeftigtenquote Arbeitswelt 0.233084541233513
## 2                Arbeitslosigkeit Arbeitswelt 0.351877801591055
## 3                 Bruttoverdienst Arbeitswelt 0.415037657175432
## 4                Einkommenssteuer   Einkommen 0.405641062139618
## 5              Haushaltseinkommen   Einkommen 0.392941396618166
## 6                  Schuldnerquote   Einkommen 0.201417541242217
## 7   BeschaeftigtemitakadAbschluss     Bildung 0.419698751922249
## 8 SchulabgaengermitHochschulreife     Bildung  0.38814743706575
## 9     SchulabgaengerohneAbschluss     Bildung 0.192153811012001
##                Score GISD Proportion
## 1  0.482788298567305 GISD       23.3
## 2 -0.593192887340244 GISD       35.2
## 3  0.644234163309764 GISD       41.5
## 4 -0.636899569900638 GISD       40.6
## 5  -0.62685037817502 GISD       39.3
## 6  0.448795656443127 GISD       20.1
## 7  0.647841610212133 GISD       42.0
## 8  0.623014796827291 GISD       38.8
## 9 -0.438353522869386 GISD       19.2
```

```r
# Tabelle der Komponenten mit den Anteilen ausgeben und gespeichert
save(GISD_Komponents, file="Outfiles/2019/GISD_Komponents.RData")

# Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung <- (Resultdataset$TS_Bildung -min(Resultdataset$TS_Bildung ))/(max(Resultdataset$TS_Bildung )-min(Resultdataset$TS_Bildung ))


# GISD
Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung
Resultdataset$GISD_Score <- (Resultdataset$GISD_Score -min(Resultdataset$GISD_Score ))/(max(Resultdataset$GISD_Score )-min(Resultdataset$GISD_Score ))

# Result
summary(Resultdataset %>% select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))
```

```
##  TS_Arbeitswelt    TS_Einkommen      TS_Bildung       GISD_Score    
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.4290   1st Qu.:0.5506   1st Qu.:0.6014   1st Qu.:0.5789  
##  Median :0.5021   Median :0.6334   Median :0.6762   Median :0.6626  
##  Mean   :0.5072   Mean   :0.6294   Mean   :0.6681   Mean   :0.6573  
##  3rd Qu.:0.5777   3rd Qu.:0.7136   3rd Qu.:0.7480   3rd Qu.:0.7421  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000
```

```r
str(Resultdataset %>% select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	199656 obs. of  4 variables:
##  $ TS_Arbeitswelt: num  0.705 0.678 0.643 0.593 0.584 ...
##  $ TS_Einkommen  : num  0.868 0.872 0.858 0.853 0.849 ...
##  $ TS_Bildung    : num  0.694 0.659 0.626 0.652 0.663 ...
##  $ GISD_Score    : num  0.843 0.82 0.787 0.775 0.775 ...
```

```r
# Teilscores und GISD-Score in Datensatz speichern
Resultdataset <- Resultdataset %>% select(Gemeindekennziffer,Jahr,Bevoelkerung,contains("TS_"),contains("GISD_Score"))
```

## V.  Datenexport - Erstellung der DatensÃ¤tze 

```r
# Merge IDs to Resultdataset
RawResult <- left_join(Resultdataset,id_dataset,by="Gemeindekennziffer")


# Export by level using for loop
exportlist<- NULL
exportlist$Kennziffern <- c("Gemeindekennziffer","Kreiskennziffer","Kennziffer Gemeindeverband","Raumordnungsregion Nr","NUTS2")
exportlist$Namen <- c("Name der Gemeinde","Name des Kreises","Name des Gemeindeverbands","Raumordnungsregion","NUTS2 Name")
exportlist$Label <- c("Gemeinde","Kreis","Gemeindeverband","Raumordnungsregion","NUTS2")
# mykennziffer <-"Gemeindekennziffer" # for testing


# Es folgt eine sehr lange Schleife
# fÃ¼r alle Regionalkennziffern (siehe Vektor) werden DatensÃ¤tze generiert und in Ordnern abgelegt
for(mykennziffer in exportlist$Kennziffern) {
  myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
  mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung
  outputdata <- RawResult 
  
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% dplyr::select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
  
  # Aggregation
  outputdata.agg <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    dplyr::select(Group,Jahr,"Bevoelkerung",GISD_Score) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevoelkerung), 
              Bevoelkerung = sum(Bevoelkerung))
  # hier werden die bevoelkerungsgewichteten Mittelwerte Ã¼ber die regionalen Einheiten gebildet
  # Achtung: Referenzrahmen fÃ¼r den BevÃ¶lkerungsstand ist das Referenzjahr. Die Varianz der BevÃ¶lkerung Ã¼ber die Jahre wird nicht berÃ¼cksichtigt.
  
  # Daten bereinigen
  names(outputdata.agg)[1] <- mykennziffer
  outputdata.agg <- merge(outputdata.agg,mergedataset,by=mykennziffer) %>%  
    dplyr::select(mykennziffer,myname,Jahr,Bundesland,"Bevoelkerung",GISD_Score) %>%
    group_by(Jahr) %>% as.tibble()
  
  # Rekodierung
  outputdata.agg <- outputdata.agg %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                       GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                       GISD_5 = findInterval(GISD_5, c(1:5)),
                                       GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                       GISD_10 = findInterval(GISD_10, c(1:10)),
                                       GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata.agg %>% select(contains("GISD")))
  # hier wird der GISD-Score neu normalisiert und die Quintile gebildet
  
  
  
  # Ausgabe Bund
  dir.create("Outfiles/2019/Bund", showWarnings=F)  
  dir.create( paste0("Outfiles/2019/Bund/",mylabel), showWarnings=F)  
  mydata <- outputdata.agg %>% ungroup() %>% dplyr::select(-Bundesland)
  write.csv(mydata, paste0("Outfiles/2019/Bund/",mylabel,"/",mylabel,".csv"))
  
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Outfiles/2019/Bund/",mylabel,"/",mylabel,"_long.dta"))
  
  # Ausgabe Bundeslandspezifisch ohne Stadtstaaten und nur fÃ¼r Ebenen Kreis und Gemeindeverband
  if (mylabel %in% c("Gemeindeverband","Kreis")) {
  outputdata.agg <- outputdata.agg %>% ungroup() %>% filter(!(Bundesland %in% c("Bremen","Hamburg","Berlin"))) %>% dplyr::select(-GISD_k,-GISD_5,-GISD_10) %>% group_by(Jahr,Bundesland) 
  
  # Rekodierung Bundesland
  outputdata.agg <- outputdata.agg %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                               GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                               GISD_5 = findInterval(GISD_5, c(1:5)),
                                               GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                               GISD_10 = findInterval(GISD_10, c(1:10)),
                                               GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata)
  
  # Ausgabe BundelÃ¤nder
  ListeBula <- unique(outputdata$Bundesland)
  dir.create("Outfiles/2019/Bundesland")  
  for(myland in ListeBula) {
  dir.create( paste0("Outfiles/2019/Bundesland/",myland), showWarnings=F)  
    dir.create( paste0("Outfiles/2019/Bundesland/",myland,"/",mylabel), showWarnings=F)  
    mydata <- outputdata %>% filter(Bundesland==myland) %>% ungroup() %>% dplyr::select(-Bundesland)
    write.csv(mydata, paste0("Outfiles/2019/Bundesland/",myland,"/",mylabel,"/",mylabel,".csv"))
    
    mydata <- outputdata %>% filter(Bundesland==myland)
    names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
    names(mydata) <- gsub("\\?","oe",names(mydata))
    names(mydata) <- gsub("\\?","ae",names(mydata))
    names(mydata) <- gsub("\\?","ue",names(mydata))
    names(mydata) <- gsub("\\?","ss",names(mydata))
    write_dta(mydata, paste0("Outfiles/2019/Bundesland/",myland,"/",mylabel,"/",mylabel,".dta"))
  }
  }  
}
```

```
## [1] "Level: Name der Gemeinde Label: Gemeinde"
```

```
## Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
## This warning is displayed once per session.
```

```
## [1] "Level: Name des Kreises Label: Kreis"
```

```
## Warning in dir.create("Outfiles/2019/Bundesland"):
## 'Outfiles\2019\Bundesland' existiert bereits
```

```
## [1] "Level: Name des Gemeindeverbands Label: Gemeindeverband"
```

```
## Warning in dir.create("Outfiles/2019/Bundesland"):
## 'Outfiles\2019\Bundesland' existiert bereits
```

```
## [1] "Level: Raumordnungsregion Label: Raumordnungsregion"
## [1] "Level: NUTS2 Name Label: NUTS2"
```


## VI.  DatensÃ¤tze fÃ¼r PLZ generieren



# Allgemeine SOP fÃ¼r die Revision (nach Lars Kroll)
1. Neue Daten und GebietsstÃ¤nde aus der INKAR-Datenbank herunterladen. Variablennamen und Formate Ã¼berprÃ¼fen.
2. Postleitzahlen in GISD_generate_postcodes.R anhand der Gebietsstandsdatei Ã¼berprÃ¼fen.
3. GISD_Generate.R ausfÃ¼hren

# AnknÃ¼pfungungspunkte fÃ¼r eine grundsÃ¤tzliche Ãberarbeitung der GISD-Generierung 

Es gibt gute GrÃ¼nde dafÃ¼r am Konzept Bildung, Einkommen und Arbeitsweltindikatoren im GISD zu vereinen, auch wenn die Korrelation der Teildimensionen mit Einzelindikatoren der anderen Teildimensionen nur gering korrelieren. 
Es gibt andererseits MÃ¶glichkeiten den GISD weiter zu verbessern. Einzelne Schwachstellen sollen hier kurz erwÃ¤hnt werden.

1. Missing Data 
* Hoher Anteil an Missing Data in den frÃ¼hen Wellen
* Umgang mit Missing Data kann verbessert werden
2. Faktorenanalyse
* Bisher wird die Faktorenanalyse per pcf-Verfahren durchgefÃ¼hrt. Hier wÃ¤re zu prÃ¼fen, ob Common Factor-Verfahren oder konfirmatorische Faktorenanalyse zu einer Verbesserung fÃ¼hren kÃ¶nnten.
3.Indikatorenauswahl 
* Die Struktur der Faktorladungen der Bildungsindikatoren ist nicht robust gegenÃ¼ber Datenschwankungen. Der erste Faktor bildet die intendierte Kompomente ab. Es gibt einen zweiten Faktor mit Eigenwert Ã¼ber 1. Die Gewichte der Faktorladungen der Indikatoren BeschaeftigteohneAbschluss und Schulgaengerohneabschluss variieren sehr stark zwischen den Revisionen 2018 und 2019. Hier kÃ¶nnte man Ã¼ber eine andere Auswahl von Indikatoren nachdenken. Die bisherigen Indikatoren BeschaeftigteohneAbschluss und BeschaeftigtemitHochschulabschluss dieser Teildimension weisen die hÃ¶chsten Anteile an MissingData auf (75%). Zudem wurde bisher noch nicht berÃ¼cksichtigt, dass die  zwischenzeitliche VerkÃ¼rzung der Schulzeit fÃ¼r das Abitur (G8 Reform) und die spÃ¤tere RÃ¼cknahme dieser Reform in einigen BundeslÃ¤ndern im Untersuchungszeitraum zu statistischen Artefakten in den SchulabgÃ¤ngerquoten fÃ¼hrt.
4. Methodologische Grundlagen
* Diskussion der dem Messmodell zugrunde liegende Kausalmechanismen 
Dimensionen sozioÃ¶konomischer Deprivation auf rÃ¤umlicher Ebene: Einkommen, Arbeitswelt, Bildung
- Einkommen: (HH-Einkommen, Steueraufkommen, Schuldnerquote)
- Kaufkraft berÃ¼cksichtigen, VermÃ¶gen berÃ¼cksichtigen
- betrifft HandlungsspielrÃ¤ume der Kommunen, Proxy fÃ¼r Wirtschaftskraft der Kommunen


