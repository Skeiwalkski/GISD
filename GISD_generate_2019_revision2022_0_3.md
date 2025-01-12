---
title: "GISD - German Index of Socio-Economic Deprivation_Revision 2022"
author: "Marvin Reis"
date: "22 4 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## 0. Benötigte Pakete  

Der Code nutzt im Wesentlichen die Pakete des Tidyverse, dazu einige Pakete zur Imputation 

```{r Libraries, message=FALSE, warning=FALSE, include=FALSE}

library("tidyverse") # Tidyverse Methods
library("bookdown") 
library("readxl") # Read Excel
library("zoo")
library("imputeTS") # Impute Missing Features
library("haven") # write Stata-dta
library("sf") # write Stata-dta
library(pastecs) # descriptive stats
```


```{r Path Definitions, message=FALSE, warning=FALSE, include=FALSE}
home <- getwd()
# Define Outfiles Directory 
outfiles_dir <- "S:/OE/FG28/205 Regionale Unterschiede/GISD/"
outfiles_daten <- "C:/projects_rstudio/GISD/Outfiles"
#setwd(outfiles_daten)
# Create Output directories in working directory if necessary
dir.create("Outfiles", showWarnings=T)
dir.create("Outfiles/2022", showWarnings=T)
dir.create("Outfiles/2022/Bund", showWarnings=T)
dir.create("Outfiles/2022/Other", showWarnings=T)
dir.create("Outfiles/2022/Stata", showWarnings=T)
setwd(home)
getwd()
```

## I.  Generierung eines ID-Datensatzes

Zunächst wird ein Datensatz (ID-Datensatz) generiert in dem den kleinsten regionalen Einheiten (Gemeinden) alle übergeordneten regionalen Einheiten und deren Regionalkennziffern zugeordnet werden. Dazu wird die Referenzdatei aus der Dokumentation INKAR-Datenbank verwendet [https://www.inkar.de/]. Es wird dabei geprüft, ob die Referenzdaten Missings auf den Regionalkennziffern oder den Namen der Gebietsstände aufweisen.


```{r ID-Datensatz aus den Referenzdaten generieren , message=FALSE, warning=FALSE}
print_missings = function(data) {
  df = data[-1,]; 
  if(sum(is.na(df))>0){print("Missing observations: "); print(df[!complete.cases(df),])}; 
  df}

load_dataset = function(sheet) {
  suppressMessages(
    read_excel("Data/Referenz/Referenz_1998_2019.xls", sheet = sheet, na = "NA")
  )
}

Gemeinden_INKAR <- load_dataset("Gemeinden-GVB") %>% 
  na.omit() %>%
  mutate(Kennziffer=as.numeric(gem19),GVBKennziffer=gvb19, fl19=as.numeric(fl19))

Gemeindeverbaende_INKAR <- load_dataset("Gemeindeverbände") %>% 
  na.omit() %>% 
  select(GVBKennziffer=gvb19,"Name des Gemeindeverbands"=gvb19name)

Kreise_INKAR <- load_dataset("KRS") %>%
  mutate(krs19= as.numeric(krs19), fl19 = as.numeric(fl19))


# Die drei Datensätze werden nun ausgehend vom Gemeindedatensatz zu einem ID-Datensatz zusammmengefügt
id_dataset <- Gemeinden_INKAR %>% 
              select(Gemeindekennziffer=Kennziffer,"Name der Gemeinde"=gem19name,GVBKennziffer, "Bevölkerung") %>% 
              mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
              left_join(.,Kreise_INKAR %>% select("Kreiskennziffer"=krs19,
                                                  "Name des Kreises"=krs19name,
                                                  "Raumordnungsregion Nr"=ROR11,
                                                  "Raumordnungsregion"=ROR11name,
                                                  NUTS2,
                                                  "NUTS2 Name"=NUTS2name,
                                                  "Bundesland"=Bundesland),by="Kreiskennziffer")%>%
              left_join(.,Gemeindeverbaende_INKAR, by="GVBKennziffer")

write_rds(id_dataset, paste0("Outfiles/2022/ID_dataset_2022"))
```

## II. Erzeugen eines Datensatzes mit Kennziffern als ID unabhängig von der Ebene 

In diesem Kapitel werden die Daten der Indikatoren eingelesen und in einem Datensatz zusammengeführt. Die Werte der Indikatoren, die für die Berechnung des GISD verwendet werden, liegen entweder auf Ebende der Gemeinde und Gemeindeverbände oder auf Ebene der Stadt- und Landkreise vor. Die Faktorenanalyse zur Bestimmung der Gewichte soll später auf der Gemeindeebene durchgeführt werden. Deshalb werden Informationen der höheren Ebenen jeweils an alle Gemeinden dieser Kreise angespielt wird. Quelle sind die INKAR-Daten der jeweiligen Indikatoren im Excel-Format, die für jede Revision aus der INKAR-Datenbank heruntergeladen werden. Tabelle \@ref(tab:indicators) stellt die Indikatoren dar.
Für einige Indikatoren, die nur in absoluten Zahlen vorliegen werden in diesem Kapitel Quoten bzw. Anteile berechnet: Arbeitslose pro 1000 Einwohner im Erwerbsalter, Anteil der SV Beschäftigte ohne Abschluss sowie der Anteil der SV Beschäftigten mit akad. Abschluss an allen SV Beschäftigten. 
Gemeinden ohne Bevölkerung werden ausgeschlossen.


```{r Indikatoren einlesen und mit dem ID-Datensatz Zusammeführen, message=FALSE}

# Basisdaten mit allen Kennziffern und Bevölkerung: Kreisebene
Basedata <- Kreise_INKAR %>% select(Kennziffer=krs19) %>% mutate(Jahr=2019)

# Liste der Variablen generieren
inputdataset <- list.files("Data/INKAR_1998_2019/")

inputdataset <- inputdataset[-c(13,14)];
inputdataset

# Einlesen der einzelnen Excelfiles zu den Daten 
for(file in inputdataset){
  suppressMessages(myimport <- read_excel(paste0("Data/INKAR_1998_2019/",file), skip = 1, sheet = "Daten"))
  names(myimport)[1] <- "Kennziffer"
  myimport[2:3] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value))
  names(myimport)[3] <- strsplit(strsplit(file,"_")[[1]][2],"[.]")[[1]][1]
  Basedata <- full_join(Basedata, myimport, by=c("Kennziffer", "Jahr"))
}

rm(inputdataset)


# Liste der Indikatoren erstellen
listofdeterminants <- names(Basedata)[3:length(Basedata)]

# Regionale Tiefe der Indikatoren 
ind_level <- c("Gemeindeverband","Gemeindeverband","Kreis", "Gemeindeverband", "Kreis", "Kreis", "Kreis", "Kreis", "Kreis", "Gemeindeverband", "Kreis", "Kreis")
level_table <- cbind(listofdeterminants,ind_level)
# Tabelle der Indikatoren mit regionaler Tiefe
ind_col = c("Indikator","Tiefe des Indikators")




# Basisdaten mit allen Kennziffern mit Bevölkerung (2019): Gemeindverbandsebene
Basedata_Gemeindeverbandsebene <- Basedata %>% select(Kennziffer,Jahr,Arbeitslosigkeit,Beschaeftigtenquote,ErwerbsfaehigeBevoelkerung,Einkommensteuer) %>%   
  gather(key,value,3:5) %>% filter(!is.na(value)) %>% spread(key,value) %>% filter(Jahr>=1998) %>% rename("Gemeindeverband"=Kennziffer)

# Weiterer Datensatz der Kreisebene, um Daten die auf Gemeindeverbandsebene fehlen mit Kreisinformationen aufzufüllen (vor allem 1998-2000)
Basedata_Kreisebene <- Basedata %>% select(krs15=Kennziffer,Jahr,listofdeterminants) %>% 
  select(-Arbeitslosigkeit,-Einkommensteuer,-Beschaeftigtenquote, -ErwerbsfaehigeBevoelkerung)%>% filter(Jahr>=1998) %>% rename(Kreis=krs15)


# Daten der verschiedenen Ebenen werden zusammengespielt
Workfile <- as.data.frame(expand.grid("Kennziffer"=Gemeinden_INKAR %>% pull(Kennziffer),"Jahr"=seq(min(Basedata$Jahr):max(Basedata$Jahr)) + min(Basedata$Jahr)-1)) %>% mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>% as_tibble() %>%
   left_join(. , Gemeinden_INKAR,by=c("Kennziffer")) %>%
   select(Gemeindekennziffer="Kennziffer",Kreis="Kreiskennziffer",Gemeindeverband=GVBKennziffer,Jahr,Bevoelkerung="Bevölkerung") %>% mutate(Gemeindeverband=as.numeric(Gemeindeverband), Bevoelkerung=as.numeric(Bevoelkerung)) %>% 
  arrange("Gemeindeverband","Jahr") %>% # Join Metadata
   left_join(. , Basedata_Kreisebene,by=c("Kreis","Jahr")) %>%
   left_join(. , Basedata_Gemeindeverbandsebene,by=c("Gemeindeverband","Jahr")) %>%
   filter(Jahr>=1998)

rm(myimport)


##Anspielen der NUTS-2 Indikatoren
NUTS2 <- id_dataset %>% select(NUTS2) %>% mutate(Jahr = 2019) %>% distinct()

inputdataset <- list.files("Data/INKAR_1998_2019/NUTS2/")

for(file in inputdataset){
  suppressMessages(myimport <- read_excel(paste0("Data/INKAR_1998_2019/NUTS2/",file), skip = 1, sheet = "Daten"))
  names(myimport)[1] <- "NUTS2"
  myimport[2:3] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"NUTS2", convert=T, na.rm = T) %>%
    mutate(Value=as.numeric(Value))
  names(myimport)[3] <- strsplit(strsplit(file,"_")[[1]][2],"[.]")[[1]][1]
  NUTS2 <- full_join(NUTS2, myimport, by=c("NUTS2", "Jahr"))
}

NUTS2 <- NUTS2 %>% left_join(id_dataset, by = "NUTS2") %>% mutate(Gemeindeverband = GVBKennziffer) %>% select(Jahr, BevoelkerungmitakadAbschluss, BevoelkerungohneAbschluss, Gemeindeverband)

Workfile <- Workfile %>% left_join(NUTS2, by = c("Gemeindeverband", "Jahr")) %>% distinct()

rm(myimport)


##Anspielen der Arbeitslosigkeit und Erwerbsbevölkerung auf Kreisebene

inputdataset <- list.files("Data/INKAR_1998_2019/Indikatoren_Kreisbene/")

for(file in inputdataset){
  suppressMessages(myimport <- read_excel(paste0("Data/INKAR_1998_2019/Indikatoren_Kreisbene/",file), skip = 1, sheet = "Daten"))
  names(myimport)[1] <- "Kennziffer"
  myimport[2:3] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value))
  names(myimport)[3] <- strsplit(strsplit(file,"_")[[1]][2],"[.]")[[1]][1]
  myimport <- myimport %>% mutate(Kreis = Kennziffer) %>% select(-Kennziffer)
  Workfile <- full_join(Workfile, myimport, by=c("Kreis", "Jahr"))
}

## Ersetzen fehlender Daten auf Gemeindeverbandsebene durch Daten der Kreisebene (Jahre 1998 bis 2000)

Workfile <- Workfile %>% mutate(ErwerbsfaehigeBevoelkerung = ifelse(Jahr < 2001, ErwerbsfaehigeBevoelkerungKreis, ErwerbsfaehigeBevoelkerung), Arbeitslosigkeit = ifelse(Jahr < 2001, ArbeitslosigkeitKreis, Arbeitslosigkeit), Beschaeftigtenquote = ifelse(Jahr < 2001, BeschaeftigtenquoteKreis, Beschaeftigtenquote)) %>% select(-ErwerbsfaehigeBevoelkerungKreis, -ArbeitslosigkeitKreis, -BeschaeftigtenquoteKreis)

rm(myimport)

##Erstellen eines Datensatzes mit Bevölkerungslosen Gemeinden und Entfernen dieser für die bevorstehende Imputation und die Faktoranalyse
Gemeinden_ohne_Bevoelkerung <- Workfile %>% filter(Bevoelkerung==0)
write_dta(Gemeinden_ohne_Bevoelkerung, paste0("Outfiles/Gemeinden_ohne_Bevoelkerung.dta"))
rm(Gemeinden_ohne_Bevoelkerung)


#Berechnung der Anteile für Beschäftigte ohne Abschluss, Beschäftigte mit akad. Abschluss und Entfernen SV Beschäftigte (total)
Workfile <- Workfile %>% filter(Bevoelkerung>0) %>% mutate(BeschaeftigteohneAbschluss = round(BeschaeftigteohneAbschluss / SVBeschaeftigte * 100, digits = 2), BeschaeftigtemitakadAbschluss = round(BeschaeftigtemitakadAbschluss / SVBeschaeftigte * 100, digits = 2)) %>%
  select(-SVBeschaeftigte)
Basedata <- Basedata %>% select(-SVBeschaeftigte)
Basedata_Kreisebene <- Basedata_Kreisebene %>% select(-SVBeschaeftigte)
level_table <- level_table[-9,]
listofdeterminants <- listofdeterminants[-9]


#Berechnung der Quote für Arbeitslosigkeit und Entfernen Erwerbsfähige Bevölkerung
Workfile <- Workfile %>% filter(Bevoelkerung>0) %>% mutate(Arbeitslosigkeit = ifelse(is.na(ErwerbsfaehigeBevoelkerung),NA,round(Arbeitslosigkeit / ErwerbsfaehigeBevoelkerung * 1000, digits = 2)), Arbeitslosigkeit=ifelse(is.nan(Arbeitslosigkeit),NA,Arbeitslosigkeit),
     Arbeitslosigkeit=ifelse(is.infinite(Arbeitslosigkeit),NA,Arbeitslosigkeit)) %>% select(-ErwerbsfaehigeBevoelkerung)
Basedata <- Basedata %>% select(-ErwerbsfaehigeBevoelkerung)
Basedata_Gemeindeverbandsebene <- Basedata_Gemeindeverbandsebene %>% select(-ErwerbsfaehigeBevoelkerung)
level_table <- level_table[-4,]
listofdeterminants <- listofdeterminants[-4]


##Ausschreiben eines Vorab-Workfiles zur Überprüfung der Arbeitslosenquote
Workfile <- Workfile %>% filter(Bevoelkerung>0)
write_dta(Workfile, paste0("Outfiles/2022/Workfile_vorab.dta"))


# Stata-Datensatz rausschreiben
#write_dta(Workfile, paste0("S:/OE/FG28/205 Regionale Unterschiede/GISD/Plausibilitätschecks/workfile.dta"))


# Ende Generierung Basisdatensatz
```

```{r indicators, echo=FALSE}
knitr::kable(level_table, col.names = ind_col, caption = "Liste der Indikatoren")
```


## III.Imputation fehlender Werte

In diesem Kapitel werden fehlende Werte identifiziert und ggfs. imputiert. Zunächst wird die Anzahl der Missings für alle Indikatoren ausgegeben. 

Das bisherige Imputationsmodell nutzt Arbeitslosigkeit als Prädiktoren. In den Daten für einiger Jahre fehlen für diese Variable Werte.
Der Einfachheit halber werden diese interpoliert. 


- Identifikation von Einzelfällen
- Auschluss der Gemeinde 16063104 aufgrund ungewöhnich hoher Missingfälle
- Interpolation der Idikatoren Arbeitslosigkeit und Schulabgänger ohne Abschluss
- Erneute Ausgabe der Missings


```{r Vereinzelte Missings auf den Imputationsvariablen interpolieren}
# Anzahl der Missings für die Indikatoren
listofdeterminants <- names(Workfile)[6:length(Workfile)]

ind_level <- c("Kreis","Kreis","Kreis", "Kreis", "Kreis", "Kreis", "Kreis", "Gemeindeverband", "Gemeindeverband", "Gemeindeverband", "NUTS2", "NUTS2")
level_table <- cbind(listofdeterminants,ind_level)

missings_table = as.data.frame(expand.grid("Jahr"=1998:max(Workfile$Jahr)))
predictors_list = data.frame(Variable=character(), Missings=double(), stringsAsFactors = FALSE)
for (column in level_table[,1]){
  for (year in 1998:max(Workfile$Jahr)){
    missings_table[year-1997,column] = Workfile %>% filter(Jahr==year, is.na(Workfile[,column])) %>% nrow()
  }
  predictors_list[nrow(predictors_list) + 1,] = c(column, Workfile %>% filter(is.na(Workfile[,column])) %>% nrow())
}
predictors_list = predictors_list %>% mutate(Missings=as.integer(Missings))
predictors_list = predictors_list[order(predictors_list$Missings),]
predictors_list


Missing_on_Imputationsvars <- Workfile %>%  filter(is.na(Arbeitslosigkeit) |is.na(SchulabgaengerohneAbschluss))
Missing_on_Imputationsvars


# Fälle betrachten: Beispiel 13075152 
TimeSeries_for_Missing <- Workfile %>%  filter(Gemeindekennziffer==13075152) %>% arrange(Gemeindekennziffer, Jahr, Bevoelkerung)
TimeSeries_for_Missing

# Fälle betrachten: 16063104
TimeSeries_for_Missing <- Workfile %>%  filter(Gemeindekennziffer==16063104) %>% arrange(Gemeindekennziffer, Jahr, Bevoelkerung)
TimeSeries_for_Missing


#Aufgrund der variierenden Verfübarkeit von Daten und der Unklarheit über die Indikatoren auf Gemeindeebene, werden die Gemeinden vorerst ausgeschlossen.
Workfile <- Workfile %>% filter(Gemeindekennziffer!=16063104)
TimeSeries <- Workfile %>%  filter(Gemeindekennziffer==16063104) %>% select(Gemeindekennziffer, Jahr, listofdeterminants) %>% arrange(Gemeindekennziffer, Jahr)
TimeSeries

#2012
Problemfall_2012 <- Workfile %>% filter(Jahr == 2011 | Jahr == 2012 | Jahr == 2013) %>% filter(Gemeindekennziffer == 1054088) %>% select(Jahr, BeschaeftigteohneAbschluss, BeschaeftigtemitakadAbschluss)
Problemfall_2012

# Interpolation der fehlenden Werte über die Zeitreihe (Mittelwert: Vorjahr, Nachjahr)
Workfile <- Workfile %>%  group_by(Gemeindeverband) %>% 
  mutate(impu_arblos = ifelse(Bevoelkerung > 0, na_interpolation(Arbeitslosigkeit), Arbeitslosigkeit),
         impu_oA = ifelse(Bevoelkerung > 0, na_interpolation(SchulabgaengerohneAbschluss), SchulabgaengerohneAbschluss),
         impu_BeschoA = ifelse(Bevoelkerung > 0, na_interpolation(BeschaeftigteohneAbschluss), BeschaeftigteohneAbschluss),
         impu_BeschakadA = ifelse(Bevoelkerung > 0, na_interpolation(BeschaeftigtemitakadAbschluss), BeschaeftigtemitakadAbschluss))  %>% 
  mutate(Arbeitslosigkeit = impu_arblos,
         SchulabgaengerohneAbschluss = impu_oA,
         BeschaeftigteohneAbschluss = ifelse(Jahr == 2012, impu_BeschoA, BeschaeftigteohneAbschluss),
         BeschaeftigtemitakadAbschluss = ifelse(Jahr == 2012, impu_BeschakadA, BeschaeftigtemitakadAbschluss)) %>%
  select(-impu_arblos,
         -impu_oA,
         -impu_BeschoA,
         -impu_BeschakadA) %>%
  ungroup()


Problemfall_2012 <- Workfile %>% filter(Gemeindekennziffer == 1054088) %>% select(Jahr, BeschaeftigteohneAbschluss, BeschaeftigtemitakadAbschluss)
Problemfall_2012


# Check der Interpolation
TimeSeries <- Workfile %>%  filter(Gemeindekennziffer==13075152) %>% arrange(Gemeindekennziffer, Jahr, Bevoelkerung)
TimeSeries



#Erneut Anzahl der Missings für die Indikatoren
missings_table = as.data.frame(expand.grid("Jahr"=1998:max(Basedata$Jahr)))
predictors_list = data.frame(Variable=character(), Missings=double(), stringsAsFactors = FALSE)
for (column in level_table[,1]){
  for (year in 1998:max(Basedata$Jahr)){
    missings_table[year-1997,column] = Workfile %>% filter(Jahr==year, is.na(Workfile[,column])) %>% nrow()
  }
  predictors_list[nrow(predictors_list) + 1,] = c(column, Workfile %>% filter(is.na(Workfile[,column])) %>% nrow())
}
predictors_list = predictors_list %>% mutate(Missings=as.integer(Missings))
predictors_list = predictors_list[order(predictors_list$Missings),]
predictors_list
```


- Imputation der Missings für alle Indikatoren (Regression auf Basis der Zeitreihe (quadratisch))
- In den Revisionen 2018-2022v02 wurde die Zeitreihenimputation auch durch zwei Indikatoren als Prädiktoren informiert. Darauf wurde ab der Revision 2022v03 verzichtet, weil die imputierten Werte mit den als Prädiktoren verwendeten Indikatoren korrelieren würden (der Hinweis kam von Inken Siems (Uni Trier)) 

- Es werden außerdem Imputationen für weiteren Indikatoren vorgenommen, die für Sensitivitätsanalysen verwendet werden:
- Imputation der Missings für Bildungsindikatoren auf NUTS-2-Ebene (für Sensitivitätsanalysen) durch Mittelwerte der Bundesländer
- Imputation für Schulabgänger mit Hochschulabschluss (für Sensitivitätsanalysen) (Regression auf Basis aller Indikatoren)

```{r}
# Imputation

# Variablenliste für die Regressionsimputation wird erstellt
imputationsliste <- subset(listofdeterminants , !(listofdeterminants %in%
                                                    c('Arbeitslosigkeit','SchulabgaengerohneAbschluss',             'SchulabgaengermitHochschulreife')))


# Imputationsdatensatz generieren
Impdata <-  Workfile %>%  filter(Jahr>=1998) %>% 
  gather(key,value,6:15) %>% mutate(value=ifelse(value<0.00001,NA,value)) %>% spread(key,value)


# Impute_function (NOT FOR GROUPED DATA!)
my_ts_imputer <- function(data,outcome_name){
  mydata   <- data %>% group_by(Gemeindekennziffer) %>%
    select(Gemeindekennziffer,Jahr,"Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  mymodell <- lm(Outcome ~
                  I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                   data = mydata  , na.action="na.exclude")
  print(mymodell)
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
}


# hier wird der Datensatz mit den imputierten Werten generiert
Impdata.imputed <- Impdata %>%
  mutate(Beschaeftigtenquote=my_ts_imputer(.,"Beschaeftigtenquote"),
         Bruttoverdienst=my_ts_imputer(.,"Bruttoverdienst"),
         BeschaeftigtemitakadAbschluss=my_ts_imputer(.,"BeschaeftigtemitakadAbschluss"),
         BeschaeftigteohneAbschluss=my_ts_imputer(.,"BeschaeftigteohneAbschluss"),
         Einkommensteuer=my_ts_imputer(.,"Einkommensteuer"),
         Haushaltseinkommen=my_ts_imputer(.,"Haushaltseinkommen"),
         Schuldnerquote=my_ts_imputer(.,"Schuldnerquote"),
         BevoelkerungohneAbschluss=my_ts_imputer(.,"BevoelkerungohneAbschluss"),
         BevoelkerungmitakadAbschluss=my_ts_imputer(.,"BevoelkerungmitakadAbschluss"))
# hier wird der Datensatz mit den imputierten Werten generiert

# Result of Imputation
summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))


########Imputation für Bevölkerung ohne Abschluss und Bevölkerung mit akademischen Abschluss (NUTS-2-Ebene) für Sensitivitätsanalysen
##Bestehende NAs in den NUTS2-Daten werden werden durch Mittelwerte der Bundesländer ersetzt
Impdata.imputed <- Impdata.imputed %>% mutate(BundeslandKZ = round(Kreis / 1000))

#Imputationsfunktion (Bildungs NUTS-2)
my_ts_imputer_NUTS2 <- function(data,outcome_name){
  mydata   <- data %>% group_by(BundeslandKZ) %>%
    select(BundeslandKZ,Jahr,"Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  mymodell <- lm(Outcome ~
                  I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                   data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
}


#Imputation (Bildungs NUTS-2)
Impdata.imputed <- Impdata.imputed %>%
  mutate(BevoelkerungohneAbschluss=my_ts_imputer_NUTS2(.,"BevoelkerungohneAbschluss"),
         BevoelkerungmitakadAbschluss=my_ts_imputer_NUTS2(.,"BevoelkerungmitakadAbschluss"))

summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))

Impdata.imputed <- Impdata.imputed %>% select(-BundeslandKZ)


########Imputation für Schulabgänger mit Hochschulreife für Sensitivitätsanalysen
# Die Schulabgänger mit Hochschulreife werden separat imputiert und durch alle Kovariaten informiert

#Imputationsfunktion
my_ts_imputer <- function(data,outcome_name){
  mydata   <- data %>%
    select(Gemeindekennziffer,Jahr,Arbeitslosigkeit,SchulabgaengerohneAbschluss,Beschaeftigtenquote,Bruttoverdienst,BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,Einkommensteuer,Haushaltseinkommen,"Outcome"=paste(outcome_name)) %>% mutate(MEAN=mean(Outcome , na.rm=T))
  mymodell <- lm(Outcome ~
                  I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Arbeitslosigkeit + 
                   SchulabgaengerohneAbschluss + Beschaeftigtenquote + Bruttoverdienst + BeschaeftigtemitakadAbschluss + BeschaeftigteohneAbschluss + Einkommensteuer + Haushaltseinkommen ,
                   data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
}

#Imputation
Impdata.imputed <- Impdata.imputed %>% mutate(
  SchulabgaengermitHochschulreife=my_ts_imputer(.,"SchulabgaengermitHochschulreife")
  )

summary(Impdata.imputed$SchulabgaengermitHochschulreife)



#Ausgabe der fehlenden Werte
missings_table = as.data.frame(expand.grid("Jahr"=1998:max(Basedata$Jahr)))
predictors_list = data.frame(Variable=character(), Missings=double(), stringsAsFactors = FALSE)
for (column in level_table[,1]){
  for (year in 1998:max(Basedata$Jahr)){
    missings_table[year-1997,column] = Impdata.imputed %>% filter(Jahr==year, is.na(Impdata.imputed[,column])) %>% nrow()
  }
  predictors_list[nrow(predictors_list) + 1,] = c(column, Impdata.imputed %>% filter(is.na(Impdata.imputed[,column])) %>% nrow())
}
predictors_list = predictors_list %>% mutate(Missings=as.integer(Missings))
predictors_list = predictors_list[order(predictors_list$Missings),]
predictors_list

#Missing betrachten
Missing_on_Imputationsvars <- Impdata.imputed %>%  filter(is.na(Arbeitslosigkeit))
Missing_on_Imputationsvars

Problemfall_2012 <- Impdata.imputed %>% filter(Gemeindekennziffer == 1054088) %>% select(Jahr, BeschaeftigteohneAbschluss, BeschaeftigtemitakadAbschluss)
Problemfall_2012

# Fälle betrachten: 16063104
TimeSeries_for_Missing <- Impdata.imputed %>%  filter(Gemeindekennziffer==16071103) %>% arrange(Gemeindekennziffer, Jahr, Bevoelkerung)
TimeSeries_for_Missing

#Aufgrund der variierenden Verfübarkeit von Daten und der Unklarheit über die Indikatoren auf Gemeindeebene, werden die Gemeinden vorerst ausgeschlossen.
Impdata.imputed <- Impdata.imputed %>% filter(Gemeindekennziffer!=16071103)
TimeSeries <- Impdata.imputed %>%  filter(Gemeindekennziffer==16071103) %>% select(Gemeindekennziffer, Jahr, listofdeterminants) %>% arrange(Gemeindekennziffer, Jahr)
TimeSeries

write_rds(Impdata.imputed, paste0("Outfiles/2022/Impdata_check_voradj.rds"))
```


- Erstellen von Hilfsvariablen zur Identifizierung von bildungspolitischen Reformen (G8)
- Adjustierung der Schulabgänger Indikatoren durch Reformeffekten (Koeffizient des Effekts wird vom Indikator abgezogen)
- Check des Adjustments durch Histogramm

```{r Erstellen Variabe zu G8 und Sensibilisierung des Datensatzes}
# Die Bildungsvariablen Anteil der Schulabgänger mit Hochschulreife und Anteil der Schulabgänger ohne Abschluss werden als Quote relativ zur Gesamtanzahl der Schulabgänger berechnet. Durch die G8-Schulreformen und ihre Zurücknahme ergeben sich deshalb in den Bundesländern zu verschiedenen Zeitpunkten Verzerrungen. 
# Diese Verzerrung wird über eine Regressionsmodell herausgerechnet.

# Generierung der Variablen zur Identifikation der Reformen (G8), Rückker zu G9 (SN_KA) und abweichender Anerkennung von Abschlüssen für GymnasiastInnen (THvor2004).
Impdata.imputed <- Impdata.imputed %>%
  mutate(G8 = case_when(Kreis < 2000 & Jahr >= 2016 & Jahr <= 2028 ~ 1,
                                Kreis > 1999 & Kreis < 3000 & Jahr >= 2010 ~ 1,
                                Kreis > 2999 & Kreis < 4000 & Jahr >= 2011 & Jahr <= 2024 ~ 1,
                                Kreis > 3999 & Kreis < 5000 & Jahr >= 2012 ~ 1,
                                Kreis > 4999 & Kreis < 6000 & Jahr >= 2013 & Jahr <= 2028 ~ 1,
                                Kreis > 5999 & Kreis < 7000 & Jahr >= 2013 & Jahr <= 2022 ~ 1,
                                Kreis > 7999 & Kreis < 9000 & Jahr >= 2012 ~ 1,
                                Kreis > 8999 & Kreis < 10000 & Jahr >= 2011 & Jahr <= 2026 ~ 1,
                                Kreis > 9999 & Kreis < 11000 & Jahr >= 2009 ~ 1,
                                Kreis > 10999 & Kreis < 12000 & Jahr >= 2012 ~ 1,
                                Kreis > 11999 & Kreis < 13000 & Jahr >= 2012 ~ 1,
                                Kreis > 12999 & Kreis < 14000 & Jahr >= 2008 ~ 1,
                                Kreis > 13999 & Kreis < 15000 ~ 1,
                                Kreis > 14999 & Kreis < 16000 & Jahr <= 2001 & Jahr >= 2007 ~ 1,
                                Kreis > 15999 ~ 1),
         THvor2004 = ifelse(Jahr < 2004 & Kreis > 15999, 1, 0))
Impdata.imputed$G8[is.na(Impdata.imputed$G8)] = 0
Impdata.imputed$THvor2004[is.na(Impdata.imputed$THvor2004)] = 0

Impdata.imputed <- Impdata.imputed %>%
  mutate(G8_jahr = case_when(Kreis < 2000 & Jahr == 2016 ~ 1,
                                Kreis > 1999 & Kreis < 3000 & Jahr == 2010 ~ 1,
                                Kreis > 2999 & Kreis < 4000 & Jahr == 2011 ~ 1,
                                Kreis > 3999 & Kreis < 5000 & Jahr == 2012 ~ 1,
                                Kreis > 4999 & Kreis < 6000 & Jahr == 2013 ~ 1,
                                Kreis > 5999 & Kreis < 7000 & Jahr == 2013 ~ 1,
                                Kreis > 7999 & Kreis < 9000 & Jahr == 2012 ~ 1,
                                Kreis > 8999 & Kreis < 10000 & Jahr == 2011 ~ 1,
                                Kreis > 9999 & Kreis < 11000 & Jahr == 2009 ~ 1,
                                Kreis > 10999 & Kreis < 12000 & Jahr == 2012 ~ 1,
                                Kreis > 11999 & Kreis < 13000 & Jahr == 2012 ~ 1,
                                Kreis > 12999 & Kreis < 14000 & Jahr == 2008 ~ 1,
                                Kreis > 14999 & Kreis < 16000 & Jahr == 2007 ~ 1),
         SN_KA = ifelse(Jahr == 2001 & Kreis > 14999 & Kreis < 16000, 1, 0))
Impdata.imputed$G8_jahr[is.na(Impdata.imputed$G8_jahr)] = 0
Impdata.imputed$SN_KA[is.na(Impdata.imputed$SN_KA)] = 0



# Anpassung: Ersetzen der Werte in den von Verzerrungen betroffenen Fälle durch um Reformeffekte bereinigte Quoten.
adj_G8_jahr <- function(data,outcome_name){
  mydata   <- data %>%
    group_by(Gemeindekennziffer) %>% 
    select(Gemeindekennziffer, Jahr, G8_jahr, SN_KA, THvor2004, "Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  
    mymodell2 <- lm(Outcome ~
                  I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + G8_jahr + SN_KA + THvor2004, data = mydata  , na.action="na.exclude")
  print(mymodell2)
    mydata %>% mutate(coef = summary(mymodell2)$coefficients[4,1], coef_SH = summary(mymodell2)$coefficients[5,1], coef_TH = summary(mymodell2)$coefficients[6,1]) %>%
      mutate(Outcome = ifelse(G8_jahr == 1, Outcome - coef, Outcome), Outcome = ifelse(SN_KA == 1, Outcome - coef_SH, Outcome), Outcome = ifelse(THvor2004 == 1, Outcome - coef_TH, Outcome)) %>%
      pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>% 
  mutate(SchulabgaengermitHochschulreife_adj = adj_G8_jahr(.,"SchulabgaengermitHochschulreife"),
         SchulabgaengerohneAbschluss_adj = adj_G8_jahr(.,"SchulabgaengerohneAbschluss"))


#Adjustmentprüfung
hist_over_SchulabgaengerohneAbschluss <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"G8_jahr"]==0,], aes_string(x = "SchulabgaengerohneAbschluss"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"G8_jahr"]==1,], aes_string(x = "SchulabgaengerohneAbschluss"), fill ='darkred')

hist_over_SchulabgaengerohneAbschluss_adj <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"G8_jahr"]==0,], aes_string(x = "SchulabgaengerohneAbschluss_adj"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"G8_jahr"]==1,], aes_string(x = "SchulabgaengerohneAbschluss_adj"), fill ='darkred')

hist_over_SchulabgaengerohneAbschluss
hist_over_SchulabgaengerohneAbschluss_adj
```


- Erstellen von Hilfsvariablen um Änderung der Messung von SV Beschäftigten am Wohnort festzustellen
- Adjustment der SV Beschäftigte Indikatoren anhand von Änderungseffekten (Koeffizient des Effekts vor 2013 wird vom Indikator abgezogen)
- Check des Adjustments durch Histogramm

```{r Messänderung}

Impdata.imputed <- Impdata.imputed %>% mutate(Messaenderung_Besch = ifelse(Jahr < 2013, 1, 0))

hist_over_vor_messanpassung <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Jahr"]<2013,], aes_string(x = "BeschaeftigteohneAbschluss"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Jahr"]>=2013,], aes_string(x = "BeschaeftigteohneAbschluss"), fill ='darkred')  


Messaenderung <- function(data,outcome_name){
  mydata   <- data %>%
    select(Gemeindekennziffer, Jahr, Messaenderung_Besch, "Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  
  mymodell_Messaenderung <- lm(Outcome ~
                  I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Messaenderung_Besch, data = mydata  , na.action="na.exclude")
  
  print(mymodell_Messaenderung) 
  summary(mymodell_Messaenderung)$coefficients[4,1]
    mydata <- mydata %>% 
      mutate(coef = summary(mymodell_Messaenderung)$coefficients[4,1]) %>%
      mutate(Outcome = ifelse(Messaenderung_Besch == 1, Outcome - coef, Outcome)) %>%
      pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>% 
  mutate(BeschaeftigteohneAbschluss_adj = Messaenderung(.,"BeschaeftigteohneAbschluss"),
         BeschaeftigtemitakadAbschluss_adj = Messaenderung(.,"BeschaeftigtemitakadAbschluss"))

#Adjustmentprüfung
hist_over_nach_messanpassung <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Jahr"]<2013,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Jahr"]>=2013,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkred')

hist_over_vor_messanpassung
hist_over_nach_messanpassung

```


- Erstellen von Hilfsvariablen zur Identifikation von Ost und West Kreisen
- Adjustment des Beschäftigte ohne Abschluss Indikatoren anhand von Ost-West-Unterschieden (Koeffizient des Effekts wird vom Indikator abgezogen)
- Check des Adjustments durch Histogramm

```{r Sensibilisierung auf Ost-west-Unterschiede}

Impdata.imputed <- Impdata.imputed %>% mutate(OW = ifelse(Kreis < 11000, 0, 1))

# mydata   <- Impdata.imputed %>%
#    select(Gemeindekennziffer, Jahr, OW, BeschaeftigteohneAbschluss_adj) %>% 
#    mutate(Jahr_Dummy = as.factor(Jahr)) %>% ungroup()
#  
#    mymodell_ow <- lm(BeschaeftigteohneAbschluss_adj ~ Jahr_Dummy + relevel(Jahr_Dummy, ref = "2012") * OW, data = #mydata, na.action="na.exclude")
#
#    summary(mymodell_ow)

#Adjustmentprüfung
hist_over_westost <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]<11000,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]>=11000,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkred') 


OW <- function(data,outcome_name){
  mydata   <- data %>%
    select(Gemeindekennziffer, Jahr, OW, "Outcome"=paste(outcome_name)) %>% 
    mutate(Jahr_Dummy = as.factor(Jahr)) %>% ungroup()
  
    mymodell_ow <- lm(Outcome ~ Jahr_Dummy + relevel(Jahr_Dummy, ref = "2012") * OW, data = mydata, na.action="na.exclude")
    
  print(mymodell_ow)
  summary(mymodell_ow)$coefficients[23,1]
    mydata %>% mutate(coef = summary(mymodell_ow)$coefficients[23,1]) %>%
      mutate(Outcome = ifelse(OW == 1, Outcome - coef, Outcome)) %>%
      pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>% 
  mutate(BeschaeftigteohneAbschluss_adj = OW(.,"BeschaeftigteohneAbschluss_adj"))


hist_over_westost_adj <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]<11000,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]>=11000,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkred')

hist_over_westost
hist_over_westost_adj

```


- Anpassung von Einkommensteuer, Haushaltsankommen und Bruttoverdienst an Verbraucherpreisindex
- Logarithmierung von Einkommensteuer, Haushaltseinkommen, Bruttoverdienst

```{r Logarithmierung und Anpassung durch Verbraucherpreisindex}

Verbraucherpreisindex <- data.frame(Jahr = c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
                                    VBindex =  c(78.3, 78.8 , 79.9, 81.5, 82.6, 83.5, 84.9, 86.2, 87.6, 89.6, 91.9, 92.2, 93.2, 95.2, 97.1, 98.5, 99.5, 100, 100.5, 102, 103.8, 105.3, 105.8))

Impdata.imputed <- Impdata.imputed %>% left_join(Verbraucherpreisindex, by = "Jahr")

Impdata.imputed <- Impdata.imputed %>% mutate(Einkommensteuer = Einkommensteuer / VBindex * 100, Haushaltseinkommen = Haushaltseinkommen / VBindex * 100, Bruttoverdienst = Bruttoverdienst / VBindex * 100)


Impdata.imputed <- Impdata.imputed %>% mutate(Einkommensteuer_ln = ifelse(Einkommensteuer==0, 0.75, log(Einkommensteuer)))

Impdata.imputed <- Impdata.imputed %>% mutate(Haushaltseinkommen_ln = log(Haushaltseinkommen))

Impdata.imputed <- Impdata.imputed %>% mutate(Bruttoverdienst_ln = log(Bruttoverdienst))

```


- Ausschreiben des aktuellen Datensatzes als STATA- und RDS-File

```{r}
# Stata-Datensatz rausschreiben
stata_data <- Impdata.imputed %>% 
  mutate(SchulabgmitAbi_adj = SchulabgaengermitHochschulreife_adj, BeschaeftigteohneAbschlussadj = BeschaeftigteohneAbschluss_adj, BeschaeftigtemitakadAbschlussadj = BeschaeftigtemitakadAbschluss_adj) %>% 
  select(-SchulabgaengermitHochschulreife_adj,
         -BeschaeftigteohneAbschluss_adj,
         -BeschaeftigtemitakadAbschluss_adj)
write_dta(stata_data, paste0("Outfiles/2022/Stata/Impdata_check_2019.dta"))
rm(stata_data)

# RDS-Datensatz rausschreiben
write_rds(Impdata.imputed, paste0("Outfiles/2022/Impdata_check.rds"))

```


- weitere Histrogramme zur Überprüfung der Adjustments

```{r Histograms for List of Determinants, eval=FALSE, message=TRUE, include=FALSE, results="HIDE"}

library(ggplot2)
for(i in listofdeterminants){
hist_over_year <- ggplot(data = Impdata.imputed) + 
  geom_histogram(mapping =  aes_string(x = i)) + 
  facet_wrap(~Jahr)
print(hist_over_year)
}

hist_over_all <- ggplot(data = Impdata.imputed) + geom_histogram(mapping = aes_string(x = "BeschaeftigteohneAbschluss")) 
print(hist_over_all)


hist_over_westost <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]<11000,], aes_string(x = "BeschaeftigteohneAbschluss"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]>=11000,], aes_string(x = "BeschaeftigteohneAbschluss"), fill ='darkred')  
print(hist_over_westost)

hist_over_year_all <- ggplot(data = Impdata.imputed) + geom_histogram(mapping = aes_string(x = "BeschaeftigteohneAbschluss")) + facet_wrap(~Jahr)
print(hist_over_year_all)

hist_over_westost_adj <- ggplot() +
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]<11000,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkblue')  + 
  geom_histogram(data = Impdata.imputed[Impdata.imputed[,"Kreis"]>=11000,], aes_string(x = "BeschaeftigteohneAbschluss_adj"), fill ='darkred')
hist_over_westost_adj

# Es gibt eine bimodale Verteilung bei den Beschäftigten ohne Abschluss, die in Ost- und Westdeutschland jedoch unimodal ist
```



## IV. Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der Faktorscores

- Herunterbrechen der Indikatoren in die einzelnen Teildimensonen

```{r Tibbles für die Teilscores generieren, echo=FALSE}
TS_Arbeitswelt_adj <- Impdata.imputed  %>% ungroup() %>% filter(Jahr > 1999) %>% select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst_ln)

TS_Einkommen_adj   <- Impdata.imputed %>% ungroup %>% filter(Jahr > 1999) %>% select(Einkommensteuer_ln,Haushaltseinkommen_ln,Schuldnerquote) 

TS_Bildung_adj <- Impdata.imputed %>% ungroup %>% filter(Jahr > 1999) %>% select(BeschaeftigtemitakadAbschluss_adj,BeschaeftigteohneAbschluss_adj,SchulabgaengerohneAbschluss_adj) 
```


# Faktorenanalyse basierend auf Hauptkomponentenanalyse für jede der drei Subscalen

- PCA für jede Dimension

```{r PCA für die Teilscores, echo=FALSE}
# PCA für die Arbeitsweltdimension
TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center = TRUE, scale. = TRUE, retx=TRUE)
plot(TS_Arbeitswelt_adj.pca)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)
TS_Arbeitswelt_adj.pca


# PCA für die Einkommensdimension
TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center = TRUE, scale. = TRUE, retx=TRUE) 
plot(TS_Einkommen_adj.pca)
TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Einkommen_adj.pca


# PCA für die Bildungsdimension
TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center = TRUE, scale. = TRUE, retx=TRUE) 
plot(TS_Bildung_adj.pca)
TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. =1 ) 
TS_Bildung_adj.pca
```


# Nun wird die Generierung der Faktorscores vorbereitet.

- Erstellen einer Tablle zu den einzelnen Faktorladungen (wie Faktoranalyse-RMD)
- Prediction des GISD-Scores
- Polen der Dimensionen
- Normalisierung des Scores für jedes Jahr
- Ausschreiben des aktuellen Datensatzes

```{r Generierung der Faktorscores, echo=FALSE}
# Componentoverview
GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Anteil"=TS_Arbeitswelt_adj.pca$rotation^2,"Score"=TS_Arbeitswelt_adj.pca$rotation) 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Anteil"=TS_Einkommen_adj.pca$rotation^2,"Score"=TS_Einkommen_adj.pca$rotation)) 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung (adj.)","Anteil"=TS_Bildung_adj.pca$rotation^2,"Score"=TS_Bildung_adj.pca$rotation)) 
GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))

rownames(GISD_Komponents) <- NULL
colnames(GISD_Komponents) <- c("Variable","Dimension","Anteil","Score")
GISD_Komponents$GISD <- "GISD"
GISD_Komponents$Proportion <- round(as.numeric(as.character(GISD_Komponents$Anteil))*100,digits=1)


# Hier findet die Prediction der Scores statt
Resultdataset <- Impdata.imputed
Resultdataset$TS_Arbeitswelt_adj <- as.numeric(predict(TS_Arbeitswelt_adj.pca, newdata = Impdata.imputed))
Resultdataset$TS_Einkommen_adj <- as.numeric(predict(TS_Einkommen_adj.pca , newdata = Impdata.imputed))
Resultdataset$TS_Bildung_adj <- as.numeric(predict(TS_Bildung_adj.pca , newdata = Impdata.imputed))

summary(Resultdataset %>% select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj))
descs <- stat.desc(Resultdataset[, -5])


# Korrelationen überprüfen
Resultdataset %>% select(Arbeitslosigkeit,TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj)  %>% cor( use="pairwise.complete.obs")

#sie werden so gepolt, dass sie positiv mit Arbeitslosigkeit korrelieren, um Deprivation abzubilden
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung_adj,use="pairwise.complete.obs")<0) {
   Resultdataset$TS_Bildung_adj <- Resultdataset$TS_Bildung_adj*-1
   }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt_adj,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt_adj <- Resultdataset$TS_Arbeitswelt_adj*-1
  }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen_adj,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen_adj <- Resultdataset$TS_Einkommen_adj*-1
}

# Korrelationen erneut überprüfen
Resultdataset %>% select(Arbeitslosigkeit,TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj) %>% cor( use="pairwise.complete.obs")


GISD_Komponents

# Tabelle der Komponenten mit den Anteilen ausgeben und gespeichert
save(GISD_Komponents, file="Outfiles/2022/GISD_Komponents.RData")



# Normalization
Resultdataset$TS_Arbeitswelt_adj <- (Resultdataset$TS_Arbeitswelt_adj -min(Resultdataset$TS_Arbeitswelt_adj ))/(max(Resultdataset$TS_Arbeitswelt_adj )-min(Resultdataset$TS_Arbeitswelt_adj ))
Resultdataset$TS_Einkommen_adj <- (Resultdataset$TS_Einkommen_adj -min(Resultdataset$TS_Einkommen_adj ))/(max(Resultdataset$TS_Einkommen_adj )-min(Resultdataset$TS_Einkommen_adj ))
Resultdataset$TS_Bildung_adj <- (Resultdataset$TS_Bildung_adj -min(Resultdataset$TS_Bildung_adj ))/(max(Resultdataset$TS_Bildung_adj )-min(Resultdataset$TS_Bildung_adj ))


# GISD
Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt_adj+Resultdataset$TS_Einkommen_adj+Resultdataset$TS_Bildung_adj
for (i in 1998:2019) {
  Resultdataset <- Resultdataset %>% group_by(Jahr) %>% mutate(GISD_Score = ifelse(Jahr == i,(GISD_Score -min(GISD_Score))/(max(GISD_Score)-min(GISD_Score)), GISD_Score)) %>% ungroup()
}

Resultdataset <- Resultdataset %>% mutate(GISD_Score = round(GISD_Score , digits = 5))

# Result
summary(Resultdataset %>% select(TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj,GISD_Score))
str(Resultdataset %>% select(TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj,GISD_Score))


# Teilscores und GISD-Score in Datensatz speichern
Resultdataset <- Resultdataset %>% select(Gemeindekennziffer,Jahr,Bevoelkerung,contains("TS_"),contains("GISD_Score"))

write_rds(Resultdataset, paste0("Outfiles/2022/Resultdataset.rds"))
write_dta(Resultdataset, paste0("Outfiles/2022/Stata/Resultdataset.dta"))
```

## V.  Datenexport - Erstellung der Datensätze 

- Ausschreibeen des GISD-Scores in den Outfiles-Ordner (für Bund und Bundesland; jedweils als .CSV oder STATA-File)

```{r echo=FALSE}
# Merge IDs to Resultdataset
RawResult <- left_join(Resultdataset,id_dataset,by="Gemeindekennziffer")



exportlist<- NULL
exportlist$Kennziffern <- c("Gemeindekennziffer","Kreiskennziffer","GVBKennziffer","Raumordnungsregion Nr","NUTS2")
exportlist$Namen <- c("Name der Gemeinde","Name des Kreises","Name des Gemeindeverbands","Raumordnungsregion","NUTS2 Name")
exportlist$Label <- c("Gemeinde","Kreis","Gemeindeverband","Raumordnungsregion","NUTS2")


# exportlist$Kennziffern <- c("Gemeindekennziffer") # for testing
  

# Es folgt eine sehr lange Schleife
# für alle Regionalkennziffern (siehe Vektor) werden Datensätze generiert und in Ordnern abgelegt
for(mykennziffer in exportlist$Kennziffern) {
  myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
  mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung
  outputdata <- RawResult 
  
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
  
  # Aggregation
  outputdata.agg <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    select(Group,Jahr,"Bevoelkerung",GISD_Score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevoelkerung), 
              TS_Bildung_adj = weighted.mean(TS_Bildung_adj, Bevoelkerung), 
              TS_Einkommen_adj = weighted.mean(TS_Einkommen_adj, Bevoelkerung),
              TS_Arbeitswelt_adj = weighted.mean(TS_Arbeitswelt_adj, Bevoelkerung),
              Bevoelkerung = sum(Bevoelkerung))
  
  
  
   #hier werden die bevoelkerungsgewichteten Mittelwerte über die regionalen Einheiten gebildet
   #Achtung: Referenzrahmen für den Bevölkerungsstand ist das Referenzjahr. Die Varianz der Bevölkerung über die Jahre wird nicht berücksichtigt.
  
  # Daten bereinigen
  names(outputdata.agg)[1] <- mykennziffer
  outputdata.agg <- merge(outputdata.agg,mergedataset,by=mykennziffer) %>%  
    select(mykennziffer,myname,Jahr,Bundesland,"Bevoelkerung",GISD_Score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
    group_by(Jahr) %>% as_tibble()
  
  # Rekodierung
  # hier wird der GISD-Score neu normalisiert und die Quintile gebildet
  for (i in 1998:2019) {
  outputdata.agg <- outputdata.agg %>% group_by(Jahr) %>% mutate(GISD_Score = ifelse(Jahr == i,(GISD_Score -min(GISD_Score))/(max(GISD_Score)-min(GISD_Score)), GISD_Score))
}
  outputdata.agg <- outputdata.agg %>% group_by(Jahr) %>% mutate(GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                        GISD_5 = findInterval(GISD_5, c(1:5)),
                                        GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                        GISD_10 = findInterval(GISD_10, c(1:10)),
                                        GISD_k = findInterval(GISD_5, c(1,2,5))) %>% ungroup()
                                       
  summary(outputdata.agg %>% select(contains("GISD")))
  
  outputdata.agg <- outputdata.agg %>% mutate(GISD_Score = round(GISD_Score, digits = 5))
  
  # Aktuelles Referenzmodell 


  # Ausgabe Bund
  dir.create("Outfiles/", showWarnings=F)
  dir.create("Outfiles/2022/", showWarnings=F)
  dir.create("Outfiles/2022/Bund/", showWarnings=F)  
  dir.create(paste0("Outfiles/2022/Bund/",mylabel), showWarnings=F)
  mydata <- outputdata.agg %>% ungroup() %>% select(mykennziffer, GISD_Score, GISD_5, GISD_10, GISD_k, myname, Jahr)
  write.csv(mydata, paste0("Outfiles/2022/Bund/",mylabel,"/",mylabel,".csv"))
  
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Outfiles/2022/Bund/",mylabel,"/",mylabel,"_long.dta"))
  
  
  
  
  
  # Ausgabe Bundeslandspezifisch ohne Stadtstaaten und nur für Ebenen Kreis und Gemeindeverband
  if (mylabel %in% c("GVBKennziffer","Kreis")) {
    
    # Datensatzerstellung
  outputdata <- RawResult 
  
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% dplyr::select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
    
    
      # Aggregation
  outputdata.bula <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    dplyr::select(Group,Jahr,"Bevoelkerung",GISD_Score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevoelkerung), 
              TS_Bildung_adj = weighted.mean(TS_Bildung_adj, Bevoelkerung), 
              TS_Einkommen_adj = weighted.mean(TS_Einkommen_adj, Bevoelkerung),
              TS_Arbeitswelt_adj = weighted.mean(TS_Arbeitswelt_adj, Bevoelkerung),
              Bevoelkerung = sum(Bevoelkerung))
  
  
  # Daten bereinigen
  names(outputdata.bula)[1] <- mykennziffer
  outputdata.bula <- merge(outputdata.bula,mergedataset,by=mykennziffer) %>%  
    select(mykennziffer,myname,Jahr,Bundesland,"Bevoelkerung",GISD_Score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
    group_by(Jahr) %>% as_tibble()
    
  outputdata.bula <- outputdata.bula %>% ungroup() %>% filter(!(Bundesland %in% c("Bremen","Hamburg","Berlin"))) %>% group_by(Jahr,Bundesland)
  
  
  # Rekodierung Bundesland
  for (i in 1998:2019) {
  outputdata.bula <- outputdata.bula %>% group_by(Jahr, Bundesland) %>% mutate(GISD_Score = ifelse(Jahr == i,(GISD_Score -min(GISD_Score))/(max(GISD_Score)-min(GISD_Score)), GISD_Score)) %>% ungroup()
}
                         
  summary(outputdata.bula %>% select(contains("GISD")))
  
  outputdata.bula <- outputdata.bula %>% mutate(GISD_Score = round(GISD_Score, digits = 5))
  
  # Ausgabe Bundesländer
  ListeBula <- unique(outputdata.bula$Bundesland)
  dir.create("Outfiles/2022/Bundesland", showWarnings=F)  
  for(myland in ListeBula) {
  dir.create( paste0("Outfiles/2022/Bundesland/",myland), showWarnings=F)  
    dir.create( paste0("Outfiles/2022/Bundesland/",myland,"/",mylabel), showWarnings=F)  
    mydata.bula <- outputdata.bula %>% filter(Bundesland==myland) %>% ungroup() %>% select(GISD_Score, mykennziffer, myname, Jahr)
    write.csv(mydata.bula, paste0("Outfiles/2022/Bundesland/",myland,"/",mylabel,"/",mylabel,".csv"))
    
    names(mydata.bula) <- gsub("\\.","_",make.names(names(mydata.bula)))
    names(mydata.bula) <- gsub("\\?","oe",names(mydata.bula))
    names(mydata.bula) <- gsub("\\?","ae",names(mydata.bula))
    names(mydata.bula) <- gsub("\\?","ue",names(mydata.bula))
    names(mydata.bula) <- gsub("\\?","ss",names(mydata.bula))
    write_dta(mydata.bula, paste0("Outfiles/2022/Bundesland/",myland,"/",mylabel,"/",mylabel,".dta"))
  }
  }  
}

```


## VI.  Datensätze für PLZ generieren

- Ausschreiben des GISD-Scores nach Postleitzahl

```{r eval=FALSE, include=FALSE}
# Output Postcode Data
load("Data/SHP/GEM_Zipcode_Intersections_2015.RData") # AGS/Postcode-Intersections-Dataset in sf format


for (mykennziffer in c("PLZ2","PLZ3","PLZ4","PLZ5")) {
  myname <-  paste0(mykennziffer)
  mylabel<-  paste0(mykennziffer)
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung
  outputdata <- Resultdataset 
  outputdata <- outputdata %>% select(AGS=Gemeindekennziffer,Jahr,GISD_Score)
  outputdata <- left_join(as.data.frame(PLZ.df) %>% ungroup() %>% mutate(AGS=as.numeric(as.character(AGS))),
                          outputdata,by=c("AGS"), all.x = TRUE)
  outputdata <- outputdata %>% filter(!is.na(mykennziffer) & !is.na(EW_Area) & !is.na(Jahr) & EW_Area>0)
  mycol <- which(mykennziffer %in% names(outputdata))
  outputdata <- outputdata %>% group_by(Jahr,AGS) 
  outputdata <- outputdata %>% mutate(GISD_Score = weighted.mean(GISD_Score,EW_Area))
  names(outputdata)[names(outputdata)=="Jahr"]<- "JAHR" # Seltsames Problem Name "Jahr"
  outputdata <- outputdata %>% group_by_at(vars("JAHR",mykennziffer)) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score,EW_Area), Bevölkerung = sum(EW_Area)) %>%
    group_by(JAHR)
  
  outputdata <- outputdata %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                       GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                       GISD_5 = findInterval(GISD_5, c(1:5)),
                                       GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                       GISD_10 = findInterval(GISD_10, c(1:10)),
                                       GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata)            
  head(outputdata)
  ListeJahre <- unique(outputdata$JAHR)
  dir.create( paste0("Outfiles"), showWarnings=F)
  dir.create( paste0("Outfiles/2022"), showWarnings=F) 
  dir.create( paste0("Outfiles/2022/Bund/"), showWarnings=F) 
  dir.create( paste0("Outfiles/2022/Bund/",mylabel), showWarnings=F) 
  mydata <- outputdata %>% ungroup() 
  write.csv2(mydata, paste0("Outfiles/2022/Bund/",mylabel,"/",mylabel,".csv"))
  mydata <- outputdata %>% ungroup() 
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Outfiles/2022/Bund/",mylabel,"/",mylabel,"_long.dta"))
  }
```
