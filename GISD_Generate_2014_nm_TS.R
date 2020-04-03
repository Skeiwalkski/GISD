# GISD - German Index of Socio-Economic Deprivation
# Author: Lars Eric Kroll, Robert Koch Institut, Berlin
# Citation: Kroll LE, Schumann M, Hoebel J et al. (2017) Regional health differences - developing a socioeconomic deprivation 
#           index for Germany. Journal of Health Monitoring 2(2):98-114. DOI 10.17886/RKI-GBE-2017-048ISSN 2511-2708

# Revision: 2018.v3 
# Replication: 2018.v1 by Niels Michalski
# Date: 2018-10-08  



# SOP for Revision
# 1. Obtain new Data and Reference Files from INKAR (manually) -> check format
# 2. Change Year in GISD_generate_postcodes.R according to INKAR Regional Date and execute
# 3. Execute GISD_Generate.R (there should be no edits required)



# Librarys
require("tidyverse") # Tidyverse Methods
require("readxl") # Read Excel
require("imputeTS") # Impute Missing Features
# install.packages("imputeTS")
require("haven") # write Stata-dta
require("sf") # write Stata-dta
# install.packages("sf")

# Create Output directories in working directory if necessary
dir.create("Revisions")
dir.create("Revisions/2018")
dir.create("Revisions/2018/Bund")
dir.create("Revisions/2018/Other")
dir.create("Outfiles")
dir.create("Outfiles/2018")



# Import data: Gebietsstand 2014 // Bevölkerung 2014
Gemeinden_INKAR <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "Gemeinden", na = "NA", skip = 1) %>% 
  rename(Kennziffer="Gemeindekennziffer",kkz="Kreiskennziffer") %>% filter(!is.na(Kennziffer))

Gemeindeverbaende_INKAR <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "GVB", na = "NA", skip = 1) %>% 
  select("Kennziffer Gemeindeverband","Name Gemeindeverband") %>% filter(!is.na("Kennziffer Gemeindeverband")) 

Kreise_INKAR <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "Kreise", skip = 1) %>%
 mutate(Kennziffer = as.numeric(Kreiskennziffer)/1000) %>% filter(!is.na(Kennziffer))

# ID-Dataset
id_dataset <- Gemeinden_INKAR %>% 
              select(Gemeindekennziffer=Kennziffer,"Name der Gemeinde","Kennziffer Gemeindeverband") %>% 
              mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
              left_join(.,Kreise_INKAR %>% select(Kreiskennziffer,
                                                  "Name des Kreises",
                                                  "Raumordnungsregion Nr"=Raumordnungsregion,
                                                  Raumordnungsregion=...18,
                                                  NUTS2="NUTS2-Region",
                                                  "NUTS2 Name"=...26,
                                                  Bundesland=...6) %>% 
                          mutate(Kreiskennziffer=floor(Kreiskennziffer/1000)),
                        by="Kreiskennziffer") %>%
              left_join(.,Gemeindeverbaende_INKAR, by="Kennziffer Gemeindeverband")



# Create Basedataset
# all levels of input will be added, Kreise is just a starting point.
Basedata    <- Kreise_INKAR %>% select(Kennziffer) %>% mutate(Jahr=2014)
# Datensatz zum Anspielen der Daten generieren
# Ausgangspunkt Kreisdatensatz
# Pipes:  1. nur Kreiskennzifern ausgewählt
#         2. Jahresvariable generiert (2014)

# Load INKAR datasets
inputdataset <- list.files("INKAR_1998_2014")
# es wird eine Variablenliste auf Grundlage des Inhalts des Ordners "INKAR_1998_2014" generiert


# for testing file<-inputdataset[1]
for(file in inputdataset){
  myimport <- read_excel(paste0("INKAR_1998_2014/",file), skip = 1, sheet = "Daten", col_types = c("text"))
  names(myimport)[1] <- "Kennziffer"
  myimport[3] <- NULL
  myimport[2] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value)) 
  names(myimport)[3] <- unlist(strsplit(unlist(strsplit(file,"_"))[2],"[.]"))[1]
  Basedata <- full_join(Basedata,myimport,by=c("Kennziffer","Jahr"))
}

# Schleife für jedes Excel-File
# 1. Einlesen der Exceldatei; jeweils Sheet Daten; erste Zeile wird geskippt,  die Daten werden zunächst als Text eingelesen
# 2. für die erste Spalte wird die Kennziffer importiert; für die zweite und dritte Spalte nichts
# 3. die Daten werde reshaped, um die Jahresinfos im langen Format zu speichern; convert konvertiert das Datenformat automatisch;
# rm.na entfert missing value Zeilen; -"Kennziffer" sorgt dafür, dass die Variable Kennziffer nicht doppelt verwendet wird
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
# Missings werden nun entfernt

names(Basedata)
# Variablennamen von Basedata ausgeben lassen



# Manual separation of data and levels
listofdeterminants <- names(Basedata)[3:length(Basedata)]
# Liste der Variablen erstellen von 3. Variable bis Ende der Variablenliste

# Datensatz für die Gemeindeverbandsebene generieren
Basedata_Gemeindeverbandsebene <- Basedata %>% dplyr::select(Kennziffer,Jahr,Arbeitslosigkeit,Beschaeftigtenquote,Einkommenssteuer) %>%   
  gather(key,value,3:5) %>% filter(!is.na(value)) %>% spread(key,value) %>% filter(Jahr>=1998) %>% rename("Gemeindeverband"=Kennziffer)
# Pipes:  1. Auswahl der Variablen 
#         2. Reshape der Daten         
#         3. Auswahl von Non-Missing 
#         4. Reshape von long nach wide 
#         5. Auswahl der Daten Jahr>=1998
#         6. Umbenennung der Kennziffervariable

# Datensatz für die Kreisebene generieren 
Basedata_Kreisebene <- Basedata %>% select(Kennziffer,Jahr,listofdeterminants) %>% 
  select(-Arbeitslosigkeit,-Einkommenssteuer,-Beschaeftigtenquote) %>% rename(Kreis=Kennziffer)
# Pipes:  1. neben der Kennziffer, die einen anderen Namen bekommt wird das Jahr und die Variablenliste ausgewählt
#         2. drei Variablen werden aus der Auswahl ausgeschlossen
#         3. die Kreisvariable wird erneut umbenannt (warum?)


  
# Join different levels
# Nun werden die Daten bezogen auf die Ebenen gemergt
Workfile <- as.data.frame(expand.grid("Kennziffer"=Gemeinden_INKAR %>% pull(Kennziffer),"Jahr"=seq(min(Basedata$Jahr):max(Basedata$Jahr)) + min(Basedata$Jahr)-1)) %>%
   mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>% as.tibble() %>%
   left_join(. , Gemeinden_INKAR,by=c("Kennziffer")) %>%
   select(Gemeindekennziffer=Kennziffer,Kreis=Kreiskennziffer,Gemeindeverband="Kennziffer Gemeindeverband",Jahr,Bevoelkerung=Bevölkerung) %>% 
      arrange(Gemeindekennziffer,Jahr) %>% # Join Metadata
   left_join(. , Basedata_Kreisebene,by=c("Kreis","Jahr")) %>% # Join Indicators for Level: Kreis
   left_join(. , Basedata_Gemeindeverbandsebene,by=c("Gemeindeverband","Jahr")) %>%  # Join Indicators for Level: Gemeindeverband 
   filter(Jahr>=1998)
names(Workfile)
# als erstes wird ein data.frame erzeugt (Workfile); der alle Gemeindewellen (1998-201x) in den Zeilen stehen hat
# 1. expand.grid erzeugt ein tibble mit allen Kombinationen von Kennziffern und Jahren
#     pull erzeugt einen Vektor für die Variablenwerte von Kennziffer aus dem Datensatz
#     + min(...) wird zu der Sequenz von Jahren aus dem Basedata addiert (1 bis X) damit auch Jahreswerte weitergeben werden
# 2. mutate generiert eine Kreiskennziffer
# 3. as.tibble erzeugt einen tibble, damit left_join genutzt werden kann
# 4. erstes left_join spielt die Gemeindedaten über Kennziffer an, das geht so, weil Gemeinden_INKAR als tibble gespeichert ist
# 5. select, wählt die inhaltlichen Variablen aus, und ändert die Variablennamen; 
# 6. arrange im select sortiert nach Gemeindekennziffer und Jahr
# 7. zweites left_join spielt die Daten der Kreisebene via Kreis und Jahr an
# 8. drittes left_join spielt die Daten der Gemeindeverbandsebene via Gemeindeverband und Jahr an
# Notiz: . in den Befehlen bezieht sich auf den tibble/data.frame der in der Pipe bearbeitet wird

# Impute Missing Values
summary(Workfile %>% select(listofdeterminants))

# Imputation
imputationsliste <- subset(listofdeterminants , 
                           !(listofdeterminants %in% 
                               c('Arbeitslosigkeit','SchulabgaengermitHochschulreife','SchulabgaengerohneAbschluss')))
# Variablenliste für die Regressionsimputation wird erstellt
# das betrifft alle Variablen, außer die im angebenen Vektor
# letztere sind frei von Missings und werden vom Imputationsmodell genutzt

Impdata <-  Workfile %>%  dplyr::filter(Jahr>=1998, Bevoelkerung>0) %>% 
  gather(key,value,6:15) %>% mutate(value=ifelse(value<0,NA,value)) %>% spread(key,value)
# Imputationsdatensatz generieren: Jahr>=1998, Bevoelkerung>0 
# gather und spread identifiziern key-Variablen automatisch 
# es geht aber nur darum Werten<0 ein NA zuzordnen

summary(Impdata %>% select(listofdeterminants))
names(Impdata)
# nun ist der Datensatz generiert, für den imputiert werden soll


# Impute_function (NOT FOR GROUPED DATA!)
# imputiert werden nur die Daten auf Gemeindeebene?
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


# Test Funtion if necessary
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
# . steht jeweils für den data.frame Impdata


# Result of Imputation
summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))
# 

# Variablenliste für die Faktorenanalyse 
print(listofdeterminants)
TS_Arbeitswelt <- Impdata.imputed %>% dplyr::select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst) 
TS_Einkommen   <- Impdata.imputed %>% dplyr::select(Einkommenssteuer,Haushaltseinkommen,Schuldnerquote) 
TS_Bildung     <- Impdata.imputed %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 


# Faktorenanalyse basierend auf Hauptkomponentenanalyse für jede der drei Subscalen
# Arbeitswelt: zunächst Analyse der Faktorlösung
TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE)
# Option retx erzeugt rotierte Lösung
head(TS_Arbeitswelt.pca$sdev)
# nur die erste Komponente mit Eigenwert über 1
# (prcomp gibt standardmäßig Sdev statt Varianz aus)
plot(TS_Arbeitswelt.pca)
# screeplot - bei nur drei Variablen wird ein Balkendiagramm angezeigt
TS_Arbeitswelt.pca
# die Faktorladungen der drei Hauptkomponenten für Arbeitswelt 
# die Ladungen der ersten Komponente enstprechen der Erwartung

# die Option rank erlaubt die Beschränkung der ANzahl an Komponenten (Faktoren)
TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)
TS_Arbeitswelt.pca

# die Hauptkomponenten werden auch für die beiden anderen Teilindikatoren durchgeführt
TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE) 
TS_Einkommen.pca
TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Einkommen.pca
TS_Bildung.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE) 
TS_Bildung.pca
# für die Bildung deutet die Analyse eher auf zwei Komponenten hin
# die Faktorladung für SchulabgaengerohneAbschluss ist auf dem ersten Faktor schwach, 
# die Faktorladung für BeschaeftigtemitakadAbschluss auf dem zweiten
# es wird die Komponente ausgewählt, bei der Beschaeftigte mit akad Abschluss positiv korreliert und 
# BeschaeftigteohneAbschluss und SchulabgaengerohneAbschluss negativ
# regionale Deprivation als Merkmal geringer Anteile von Akademikern bei gleichzeitigen hohen Anteilen 
# von Beschaeftigten ohne Abschluss und Schulabgaengern ohne Abschluss
TS_Bildung.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Bildung.pca
TS_Bildung.pca$rotation


# Componentoverview
GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Anteil"=TS_Arbeitswelt.pca$rotation^2,"Score"=TS_Arbeitswelt.pca$rotation) 
# cbind erstellt Spaltenvektoren mit den Infos aus Teildimension, den (rotierten) Faktorladungen und den Components; 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Anteil"=TS_Einkommen.pca$rotation^2,"Score"=TS_Einkommen.pca$rotation)) 
# rbind erstellt Zeilenvektoren, diese werden hier in die bereits vorhandenen Spaltenvektoren eingebunden
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung","Anteil"=TS_Bildung.pca$rotation^2,"Score"=TS_Bildung.pca$rotation)) 
# auch für die Teildimension Bildung werden Zeilenvektoren eingebunden
GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))
# als letztes wird die Matrix in einen Dataframe übersetzt

rownames(GISD_Komponents) <- NULL
# die überflüssigen Zeilennamen werden gestrichen
colnames(GISD_Komponents) <- c("Variable","Dimension","Anteil","Score")
# aussagekräftige Spaltennamen vergeben
GISD_Komponents$GISD <- "GISD"
# eine weitere Spalte wird eingefügt mit dem String "GISD" in jeder Zeile
GISD_Komponents$Proportion <- round(as.numeric(as.character(GISD_Komponents$Anteil))*100,digits=1)
# eine weitere Spalte Proportion wird eingefügt mit prozentualen Anteilswerten (eine Nachkommastelle)

# Prediction and normalization
Resultdataset <- Impdata.imputed
Resultdataset$TS_Arbeitswelt <- as.numeric(predict(TS_Arbeitswelt.pca, newdata = Impdata.imputed))
Resultdataset$TS_Einkommen <- as.numeric(predict(TS_Einkommen.pca , newdata = Impdata.imputed))
Resultdataset$TS_Bildung <- as.numeric(predict(TS_Bildung.pca, newdata = Impdata.imputed))

summary(Resultdataset %>% dplyr::select(TS_Arbeitswelt, TS_Einkommen, TS_Bildung))

# Korrelationen überprüfen
Resultdataset %>% dplyr::select(Arbeitslosigkeit,TS_Arbeitswelt,TS_Einkommen,TS_Bildung)  %>% cor( use="pairwise.complete.obs")  



# Correction for Direction of generatet Factors (correlation w. Income should be negative)
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung,use="pairwise.complete.obs")<0) {
   Resultdataset$TS_Bildung <- Resultdataset$TS_Bildung*-1
   }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1
  }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1
}

# gecheckt bisher nur bis hier - NM


Resultdataset %>% dplyr::select(Arbeitslosigkeit,TS_Arbeitswelt,TS_Einkommen,TS_Bildung) %>% cor( use="pairwise.complete.obs")
GISD_Komponents
save(GISD_Komponents, file="Outfiles/2018/GISD_Komponents.RData")


# Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung <- (Resultdataset$TS_Bildung -min(Resultdataset$TS_Bildung ))/(max(Resultdataset$TS_Bildung )-min(Resultdataset$TS_Bildung ))

# GISD
Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung
Resultdataset$GISD_Score <- (Resultdataset$GISD_Score -min(Resultdataset$GISD_Score ))/(max(Resultdataset$GISD_Score )-min(Resultdataset$GISD_Score ))

# Result
summary(Resultdataset %>% select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))
str(Resultdataset %>% select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))

Resultdataset <- Resultdataset %>% select(Gemeindekennziffer,Jahr,Bevoelkerung,contains("TS_"),contains("GISD_Score"))

# Merge IDs to Resultdataset
RawResult <- left_join(Resultdataset,id_dataset,by="Gemeindekennziffer")


# Export by level using for loop
exportlist<- NULL
exportlist$Kennziffern <- c("Gemeindekennziffer","Kreiskennziffer","Kennziffer Gemeindeverband","Raumordnungsregion Nr","NUTS2")
exportlist$Namen <- c("Name der Gemeinde","Name des Kreises","Name Gemeindeverband","Raumordnungsregion","NUTS2 Name")
exportlist$Label <- c("Gemeinde","Kreis","Gemeindeverband","Raumordnungsregion","NUTS2")
# mykennziffer <-"Kreiskennziffer" 
# for testing


for(mykennziffer in exportlist$Kennziffern) {
  myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
  mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
  print(paste("Level:",myname,"Label:",mylabel))
  

  
  # Datensatzerstellung
  outputdata <- RawResult 
  
  # Datensatz zum Mergen der jeweiligen Kennziffer (Gemeinde, Kreis etc.) erstellen
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% dplyr::select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
  
  # Aggregation
  outputdata.agg <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    dplyr::select(Group,Jahr,Bevoelkerung, GISD_Score, TS_Bildung, TS_Einkommen, TS_Arbeitswelt) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevoelkerung), 
              TS_Bildung = weighted.mean(TS_Bildung, Bevoelkerung), 
              TS_Einkommen = weighted.mean(TS_Einkommen, Bevoelkerung),
              TS_Arbeitswelt = weighted.mean(TS_Arbeitswelt, Bevoelkerung),
              Bevoelkerung = sum(Bevoelkerung))

  
  # Daten bereinigen
  names(outputdata.agg)[1] <- mykennziffer
  outputdata.agg <- merge(outputdata.agg,mergedataset,by=mykennziffer) %>%  
    dplyr::select(mykennziffer,myname,Jahr,Bundesland,"Bevoelkerung",GISD_Score, TS_Bildung, TS_Einkommen, TS_Arbeitswelt) %>%
    group_by(Jahr) %>% as.tibble()
  

#  sapply(outputdata.agg  %>% select("GISD_Score", mykennziffer) , function(x) sum(is.na(x)))
  
  # Rekodierung
  outputdata.agg <- outputdata.agg %>%  mutate(GISD_Score_2 = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                       GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                       GISD_5 = findInterval(GISD_5, c(1:5)),
                                       GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                       GISD_10 = findInterval(GISD_10, c(1:10)),
                                       GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata.agg %>% select(contains("GISD")))
  
  # Ausgabe Bund
  dir.create("Revisions/2018/Bund", showWarnings=F)  
  dir.create( paste0("Revisions/2018/Bund/",mylabel), showWarnings=F)  
  mydata <- outputdata.agg %>% ungroup() %>% dplyr::select(-Bundesland)
  write.csv(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,".csv"))
  
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,"_long.dta"), version = 15)
  
  # Ausgabe Bundeslandspezifisch ohne Stadtstaaten und nur für Ebenen Kreis und Gemeindeverband
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
  
  # Ausgabe Bundel?nder
  ListeBula <- unique(outputdata$Bundesland)
  dir.create("Revisions/2018/Bundesland", showWarnings=F)  
  for(myland in ListeBula) {
  print(paste("Bundesland: ",myland))
  dir.create( paste0("Revisions/2018/Bundesland/",myland), showWarnings=F)  
    dir.create( paste0("Revisions/2018/Bundesland/",myland,"/",mylabel), showWarnings=F)  
    mydata <- outputdata %>% filter(Bundesland==myland) %>% ungroup() %>% dplyr::select(-Bundesland)
    write.csv(mydata, paste0("Revisions/2018/Bundesland/",myland,"/",mylabel,"/",mylabel,".csv"))
    
    mydata <- outputdata %>% filter(Bundesland==myland)
    names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
    names(mydata) <- gsub("\\?","oe",names(mydata))
    names(mydata) <- gsub("\\?","ae",names(mydata))
    names(mydata) <- gsub("\\?","ue",names(mydata))
    names(mydata) <- gsub("\\?","ss",names(mydata))
    write_dta(mydata, paste0("Revisions/2018/Bundesland/",myland,"/",mylabel,"/",mylabel,".dta"))
  }
  }  
}


# Output Postcode Data
load("PLZ/GEM_Zipcode_Intersections_2015.RData") # AGS/Postcode-Intersections-Dataset in sf format


for (mykennziffer in c("PLZ2","PLZ3","PLZ4","PLZ5")) {
  myname <-  paste0(mykennziffer)
  mylabel<-  paste0(mykennziffer)
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung # weighted.mean fehlt wg. Fehler Evaluation error: 'x' and 'w' must have the same length
  outputdata <- Resultdataset 
  outputdata <- outputdata %>% dplyr::select(AGS=Gemeindekennziffer,Jahr,GISD_Score,TS_Bildung, TS_Einkommen, TS_Arbeitswelt)

  outputdata <- left_join(as.data.frame(PLZ.df) %>% ungroup() %>% mutate(AGS=as.numeric(as.character(AGS))),
                          outputdata,by=c("AGS"), all.x = TRUE)
                          # as.numeric(as.character vermutlich um führende Nullen zu löschen
  
  outputdata <- outputdata %>% filter(!is.na(mykennziffer) & !is.na(EW_Area) & !is.na(Jahr) & EW_Area>0)
  
  mycol <- which(mykennziffer %in% names(outputdata))
  
  outputdata <- outputdata %>% group_by(Jahr,AGS) 
  outputdata <- outputdata %>% mutate(GISD_Score = weighted.mean(GISD_Score,EW_Area), 
                                      TS_Bildung = weighted.mean(TS_Bildung, EW_Area),
                                      TS_Einkommen = weighted.mean(TS_Einkommen, EW_Area),
                                      TS_Arbeitswelt = weighted.mean(TS_Arbeitswelt, EW_Area))
  
  names(outputdata)[names(outputdata)=="Jahr"]<- "JAHR" # Seltsames Problem Name "Jahr"
  
  outputdata <- outputdata %>% group_by_at(vars("JAHR",mykennziffer)) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score,EW_Area),
              TS_Bildung = weighted.mean(TS_Bildung, EW_Area),
              TS_Einkommen = weighted.mean(TS_Einkommen, EW_Area),
              TS_Arbeitswelt = weighted.mean(TS_Arbeitswelt, EW_Area),              
              Bevoelkerung = sum(EW_Area)) %>%
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
  dir.create( paste0("Revisions/2018/Bund/",mylabel), showWarnings=F)  
  mydata <- outputdata %>% ungroup() 
  write.csv2(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,".csv"))
  mydata <- outputdata %>% ungroup() 
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,"_long.dta"))
  }



