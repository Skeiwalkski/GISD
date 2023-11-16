# Hier wird die Population des Vorjahres in den Gebietsstand des Jahres danach konvertiert
# Beispiel: Wir haben Population für 1989 Berlin Ost und Berlin West.
#           1990 fusionieren sie zu Gesamt-Berlin. Am Ende des Prozesses
#           haben wir die Population für Gesamt-Berlin für das Jahr 1989

# Umsteigeschlüssel herunterladen wenn noch nicht vorhanden
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html)
if (!file.exists("Data/Referenz/ref-gemeinden-2010-2020.xlsx")) {
url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/ref-gemeinden-2010-2020.xlsx?__blob=publicationFile&v=5"
download.file(url, destfile = "Data/Referenz/ref-gemeinden-2010-2020.xlsx", mode = "wb")
rm(url)
}

# Gemeinden-Gemeindeverbands-Schlüssel herunterladen wenn noch nicht vorhanden
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/downloads/download-referenzen.html)
if (!file.exists("Data/Referenz/gemeinden-gemeindeverbaende-2019.csv")) {
  url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/gemeinden/gemeinden-gemeindeverbaende/gemeinden-gemeindeverbaende-2019.csv?__blob=publicationFile&v=5"
  download.file(url, destfile = "Data/Referenz/gemeinden-gemeindeverbaende-2019.csv", mode = "wb")
  rm(url)
}

# Kreise-Kreisregionen-Schlüssel
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/downloads/archiv/download-referenzen.html)
if (!file.exists("Data/Referenz/kreis-kreisregionen-2018.xlsx")) {
  url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/kreise/Kreise_Kreisregionen/kreis-kreisregionen-2018.xlsx?__blob=publicationFile&v=3"
  download.file(url, destfile = "Data/Referenz/kreis-kreisregionen-2018.xlsx", mode = "wb")
  rm(url)
}

# Umsteigeschlüssel Kreise
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html)
if (!file.exists("Data/Referenz/ref-kreise-umrech-2019-1990-2018.xlsx")) {
  url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/ref-kreise-umrech-2019-1990-2018.xlsx?__blob=publicationFile&v=3"
  download.file(url, destfile = "Data/Referenz/ref-kreise-umrech-2019-1990-2018.xlsx", mode = "wb")
  rm(url)
}

## Gemeinden
# Referenzen einlesen
gem18 <- read_excel("Data/Referenz/Erzeugte Referenzen/gemeinden-gemeinderegionen-2018.xls") # Quelle: Niels Michalski

gem19 <- read_excel("Data/Referenz/ref-gemeinden-2010-2020.xlsx",
                    sheet = "2018-2019") %>%
  mutate(gkz18 = `Gemeinden\r\n 31.12.2018`)

# Vorjahr und mit aktuellem Jahr mergen
merged_data <- full_join(gem19, gem18, by = "gkz18")

# Daten auf Gebietsstand von aktuellem Jahr collapsen
merged_data <- merged_data %>%
  mutate(gem19 = `Gemeinden\r\n 31.12.2019`,
         bev18 = `Bevölkerung am 31.12.2018 in 100`,
         bevum18 = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
         bevraw19 = bev18 * bevum18)

aggregated_data <- merged_data %>%
  group_by(gem19) %>%
  summarise(bev19 = sum(bevraw19, na.rm = TRUE))

# Durch summarise sind die anderen Variablen verloren gegangen
merged_data <- left_join(aggregated_data, merged_data, by = "gem19")

# Redundante Duplikate entfernen
merged_data <- merged_data %>%
  distinct(gem19, .keep_all = TRUE)

duplicates <- merged_data %>%
  group_by(gem19) %>%
  filter(n() > 1) %>%
  ungroup() 
duplicates
rm(duplicates)

# Aufräumen
gem19 <- merged_data %>%
  rename(gem19name = `Gemeindename 2019`,
         fl19 = `Fläche am 31.12.2018 in km²`) %>%
  select(gem19, bev19, gem19name, ZentralörtlicheEinstufungjan,
         D, ZentralörlticheEinstufung4er, F, ZentralörtlicheEinstufungdiffe,
         H, fl19, Bundesland,	L, WestOst, N, RaumtypnachLage, P,
         StadtGemeindetyp, R)

rm(aggregated_data, merged_data)

# Als Excel speichern
write_xlsx(gem19, "Data/Referenz/Erzeugte Referenzen/gemeinden-gemeinderegionen-2019.xlsx")


## Gemeinden-GVB
# Referenz einlesen
gvb19 <- read_delim("Data/Referenz/gemeinden-gemeindeverbaende-2019.csv",
                    locale = locale(encoding = "latin1"), delim = ";") %>%
  mutate(gem19 = `Gemeinden (2019) Kennziffer`,
         gvb19 = `Gemeindeverbände (2019) Kennziffer`,
         gvb19name = `Gemeindeverbände (2019) Name`)

# Gemeinden zu Gemeindeverbänden zuordnen
merged_data <- full_join(gvb19, gem19, by = "gem19")

# Aufräumen
merged_data <- merged_data %>%
  select(gem19, gvb19, gvb19name, bev19, gem19name, fl19,
         L, N, P, StadtGemeindetyp, R) %>% 
  rename(Bundesland = L, WestOst = N)

# Als Excel speichern
write_xlsx(merged_data, "Data/Referenz/Erzeugte Referenzen/gemeinden-gvb-2019.xlsx")

## Gemeindeverbände
# Daten auf Gebietsstand vom aktuellen Jahr collapsen
merged_data <- merged_data %>%
  select(c(-gem19, -gem19name))

aggregated_data <- merged_data %>%
  group_by(gvb19) %>%
  summarise(bev19 = sum(bev19, na.rm = TRUE),
            fl19 = sum(fl19, na.rm = TRUE))

# Durch summarise sind die anderen Variablen verloren gegangen
merged_data <- merged_data %>%
  select(c(-bev19, -fl19))

gvb19 <- full_join(aggregated_data, merged_data, by = "gvb19")

# Duplikate entfernen
gvb19 <- gvb19 %>%
  distinct(gvb19, .keep_all = TRUE)

duplicates <- gvb19 %>%
  group_by(gvb19) %>%
  filter(n() > 1) %>%
  ungroup() 
duplicates
rm(duplicates)

# Als Excel speichern
write_xlsx(gvb19, "Data/Referenz/Erzeugte Referenzen/gemeindeverbände-2019.xlsx")

rm(gem18, gem19, gvb19, aggregated_data, merged_data)


## Kreise
# Kreise-Kreisregionen-Schlüssel einlesen
kreise <- read_excel("Data/Referenz/kreis-kreisregionen-2018.xlsx", range = "A3:AI404") %>%
  rename(kkz18 = krs18)

# Umsteigeschlüssel einlesen
umsteig <- read_excel("Data/Referenz/ref-kreise-umrech-2019-1990-2018.xlsx", sheet = "2018-2019") %>%
  rename(kkz18 = `Kreise\r\n 31.12.2018`)

# Zusammenlegen
merged_data <- full_join(umsteig, kreise, by = "kkz18") %>%
  filter(!is.na(kkz18))

# Neue Variablen generieren
merged_data <- merged_data %>%
  mutate(krs19 = `Kreise\r\n 31.12.2019`,
         bev18 = `Bevölkerung am 31.12.2018 in 1000`,
         bevum18 = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
         bevraw19 = bev18 * bevum18) 

# Collapsen
aggregated_data <- merged_data %>%
  group_by(krs19) %>%
  summarise(bev19 = sum(bevraw19, na.rm = TRUE))

# Durch summarise sind die anderen Variablen verloren gegangen
aggregated_data <- aggregated_data %>% 
  left_join(merged_data, by = "krs19")

# Duplikate entfernen
aggregated_data <- aggregated_data %>%
  distinct(krs19, .keep_all = TRUE)

duplicates <- aggregated_data %>%
  group_by(krs19) %>%
  filter(n() > 1) %>%
  ungroup() 
duplicates
rm(duplicates)
  
# Aufräumen
aggregated_data <- aggregated_data %>% 
  rename(krs19name = `Kreisname 2019`,
         fl19 = fl18,
         kslk = kslk...6,
         G = kslk...7,
         kreg19 = kreg18,
         kreg19name = kreg18name,
         st_kreg = st_kreg...10,
         K = st_kreg...11,
         kslk_kreg = kslk_kreg...12,
         M = kslk_kreg...13,
         ktyp4 = ktyp4...14,
         O = ktyp4...15,
         slraum = slraum...16,
         Q = slraum...17,
         S = ...19,
         rtyp3 = rtyp3...22,
         W = rtyp3...23,
         land = land...32,
         Bundesland = land...33,
         WO = WO...34,
         AI = WO...35) %>% 
  select(krs19, bev19, krs19name, fl19, ksitz, kslk, G, kreg19, kreg19name,
         st_kreg, K, kslk_kreg, M, ktyp4, O, slraum, Q, raumt2010lage_kreis,
         S, ROR11, ROR11name, rtyp3, W, amr16, amr16name, RBZ, RBZname, NUTS2,
         NUTS2name, metropolen_IKM_2015_a, metropolen_IKM_2015_b, land,
         Bundesland, WO, AI)

# Als Excel speichern
write_xlsx(aggregated_data, "Data/Referenz/Erzeugte Referenzen/kreis-kreisregionen-2019")

rm(kreise, umsteig, merged_data, aggregated_data)