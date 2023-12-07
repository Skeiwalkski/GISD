# Hier wird die Population des Vorjahres in den Gebietsstand des Jahres danach konvertiert

# Beispiel: Wir haben Population für 1989 Berlin Ost und Berlin West.
#           1990 fusionieren sie zu Gesamt-Berlin. Am Ende des Prozesses
#           haben wir die Population für Gesamt-Berlin für das Jahr 1989
#           (und Berlin Ost und West dann nicht mehr vorhanden in 1989)

#           Langweiler Wald war 2018 eigenständig und wird auf 2019
#           auf die Dörfer A B C aufgeteilt. Am Ende des Prozesses
#           Haben wir die Population von A B und C inkl. Langweiler Wald-Anteil
#           für 2018 (Langweiler Wald dann nicht mehr vorhanden in 2018).

# WARNUNG: BBSR ist nicht konsistent mit Bevölkerungszahlen.
#          Manchmal in 100 (z.B. Flensburg 906), manchmal in 1000 (Flensburg 90,6)
#          Wenn auf das nächste Jahr geupdated wird unbedingt sicherstellen dass
#          alles wieder zusammenpasst.

### REFERENZEN VOM BBSR BESORGEN WENN NOCH NICHT VORHANDEN
## Umsteigeschlüssel Gemeinden
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html)
if (!file.exists("Data/Referenz/ref-gemeinden-2010-2020.xlsx")) {
url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/ref-gemeinden-2010-2020.xlsx?__blob=publicationFile&v=5"
download.file(url, destfile = "Data/Referenz/ref-gemeinden-2010-2020.xlsx", mode = "wb")
rm(url)
}

## Gemeinden-Gemeindeverbands-Schlüssel
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/downloads/download-referenzen.html)
if (!file.exists("Data/Referenz/gemeinden-gemeindeverbaende-2020.csv")) {
  url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/gemeinden/gemeinden-gemeindeverbaende/gemeinden-gemeindeverbaende-2020.csv?__blob=publicationFile&v=3"
  download.file(url, destfile = "Data/Referenz/gemeinden-gemeindeverbaende-2020.csv", mode = "wb")
  rm(url)
}

## Kreise-Kreisregionen-Schlüssel
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/downloads/archiv/download-referenzen.html)
if (!file.exists("Data/Referenz/kreis-kreisregionen-2018.xlsx")) {
  url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/kreise/Kreise_Kreisregionen/kreis-kreisregionen-2018.xlsx?__blob=publicationFile&v=3"
  download.file(url, destfile = "Data/Referenz/kreis-kreisregionen-2018.xlsx", mode = "wb")
  rm(url)
}

# Umsteigeschlüssel Kreise
# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umsteigeschluessel.html)
if (!file.exists("Data/Referenz/ref-kreise-umrech-2020-1990-2019.xlsx")) {
  url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/ref-kreise-umrech-2020-1990-2019.xlsx?__blob=publicationFile&v=3"
  download.file(url, destfile = "Data/Referenz/ref-kreise-umrech-2020-1990-2019.xlsx", mode = "wb")
  rm(url)
}

# Gemeinden-GVB-Schlüssel mit 8-stelliger GVB-Kennziffer
# (Quelle: https://www.inkar.de/)
if (!file.exists("Data/Referenz/INKAR/Referenz_Gemeinden_Kreise_NUTS_2020.xlsx")) {
  url <- "https://www.inkar.de/documents/Referenz%20Gemeinden,%20Kreise,%20NUTS.xlsx"
  download.file(url, destfile = "Data/Referenz/INKAR/Referenz_Gemeinden_Kreise_NUTS_2020.xlsx", mode = "wb")
  rm(url)
}



### MASTER REFERENZ ERSTELLEN
## Gemeinden
# Referenzen einlesen
gem.alt <- read_excel("Data/Referenz/Erzeugte Referenzen/gemeinden-gemeinderegionen-2019.xlsx") %>% # Quelle: Niels Michalski
  rename(gkz_alt = gem19) 

gem.neu <- read_excel("Data/Referenz/ref-gemeinden-2010-2020.xlsx",
                    sheet = "2019-2020") %>%
  mutate(gkz_alt = `Gemeinden\r\n 31.12.2019`) #gkz = Gemeindekennziffer

# Vorjahr und mit aktuellem Jahr mergen
merged_data <- full_join(gem.neu, gem.alt, by = "gkz_alt") %>%
  mutate(gkz_neu = `Gemeinden\r\n 31.12.2020`,
         bev_alt = `Bevölkerung am 31.12.2019 in 100`, #Bevölkerung in 100
         bpu = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
         bev_neu_raw = bev_alt * bpu) #Proportionale Bevölkerung

# Rohe proportionale Bevölkerung aufsummieren
aggregated_data <- merged_data %>%
  group_by(gkz_neu) %>%
  summarise(Bevoelkerung = sum(bev_neu_raw, na.rm = TRUE))

# Verlorene Variablen zurückholen
merged_data <- left_join(aggregated_data, merged_data, by = "gkz_neu")

# Duplikate entfernen
duplicates <- merged_data %>%
  group_by(gkz_neu) %>%
  filter(n() > 1) %>%
  ungroup() 
duplicates
rm(duplicates)

merged_data <- merged_data %>%
  distinct(gkz_neu, .keep_all = TRUE)

# Aufräumen
gem.neu <- merged_data %>%
  rename(gemeindename = `Gemeindename 2020`,
         flaeche = `Fläche am 31.12.2019 in km²`,
         gkz = gkz_neu) %>%
  select(gkz, Bevoelkerung, gemeindename, ZentralörtlicheEinstufungjan,
         D, ZentralörlticheEinstufung4er, F, ZentralörtlicheEinstufungdiffe,
         H, flaeche, Bundesland,	L, WestOst, N, RaumtypnachLage, P,
         StadtGemeindetyp, R)

rm(aggregated_data, merged_data)

# Als Excel speichern
write_xlsx(gem.neu, "Data/Referenz/Erzeugte Referenzen/gemeinden-gemeinderegionen-2020.xlsx")

## Gemeinden-GVB
# Referenz einlesen
gemgvb <- read_delim("Data/Referenz/gemeinden-gemeindeverbaende-2020.csv",
                     locale = locale(encoding = "latin1"), delim = ";") %>%
  mutate(gkz = as.integer(`Gemeinden (2020) Kennziffer`),
         gvb_kz_kurz = as.integer(`Gemeindeverbände (2020) Kennziffer`),
         gvbname = `Gemeindeverbände (2020) Name`)

# 8-stellige GVB-Kennziffern hinzufügen
gvb_inkar <- read_excel("Data/Referenz/INKAR/Referenz_Gemeinden_Kreise_NUTS_2020.xlsx", skip = 1) %>% 
  rename(gkz = 'Gemeinden Kennziffer',
         gvb_kz_lang = '...7') %>%
  mutate(gvb_kz_lang = case_when( #Fehler auf BBSR-Seite (2020) - GVB-KZ für Berlin fehlt und wird händisch nachgetragen
    `Gemeinden Name` == "Berlin, Stadt" ~ 110000000,
    TRUE ~ gvb_kz_lang)) %>% 
  select(gkz, gvb_kz_lang)

# Gemeinden zu Gemeindeverbänden zuordnen
merged_data <- full_join(gemgvb, gem.neu, by = "gkz") %>%
  left_join(., gvb_inkar, by = "gkz")

# Aufräumen
merged_data <- merged_data %>%
  select(gkz, gvb_kz_lang, gvb_kz_kurz, gvbname, Bevoelkerung, gemeindename, flaeche,
         L, N, P, StadtGemeindetyp, R) %>% 
  rename(Bundesland = L, WestOst = N)

gemgvb <- merged_data

# Als Excel speichern
write_xlsx(gemgvb, "Data/Referenz/Erzeugte Referenzen/gemeinden-gvb-2020.xlsx")

## Gemeindeverbände
# Gemeindepopulation auf Gemeindeverbände aufsummieren
merged_data <- merged_data %>%
  select(c(-gkz, -gemeindename))

aggregated_data <- merged_data %>%
  group_by(gvb_kz_lang) %>%
  summarise(Bevoelkerung = sum(Bevoelkerung, na.rm = TRUE),
            flaeche = sum(flaeche, na.rm = TRUE))

merged_data <- merged_data %>%
  select(c(-Bevoelkerung, -flaeche))

# Verlorene Variablen zurückholen
aggregated_data <- full_join(aggregated_data, merged_data, by = "gvb_kz_lang")

# Duplikate entfernen
duplicates <- aggregated_data %>%
  group_by(gvb_kz_lang) %>%
  filter(n() > 1) %>%
  ungroup() 
duplicates
rm(duplicates)

gvb <- aggregated_data %>%
  distinct(gvb_kz_lang, .keep_all = TRUE)

# Als Excel speichern
write_xlsx(gvb, "Data/Referenz/Erzeugte Referenzen/gemeindeverbände-2020.xlsx")

rm(aggregated_data, merged_data)


## Kreise
# Kreise-Kreisregionen-Schlüssel einlesen
kreise <- read_excel("Data/Referenz/kreis-kreisregionen-2018.xlsx", range = "A3:AI404") %>%
  rename(kkz_alt = krs18)

# Umsteigeschlüssel einlesen
k.umsteig <- read_excel("Data/Referenz/ref-kreise-umrech-2020-1990-2019.xlsx", sheet = "2019-2020") %>%
  rename(kkz_alt = `Kreise\r\n 31.12.2019`)

# Zusammenlegen
merged_data <- full_join(k.umsteig, kreise, by = "kkz_alt") %>%
  filter(!is.na(kkz_alt))

# Neue Variablen generieren
merged_data <- merged_data %>%
  mutate(kkz_neu = `Kreise\r\n 31.12.2020`,
         bev_alt = (`Bevölkerung am 31.12.2019 in 1000` * 10), # ACHTUNG: Bevölkerung in 1000! (Alles bisher in 100) daher mal 10
         bpu = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
         bev_neu_raw = bev_alt * bpu) #Proportionale Bevölkerung

# Rohe proportionale Bevölkerung aufsummieren
aggregated_data <- merged_data %>%
  group_by(kkz_neu) %>%
  summarise(Bevoelkerung = sum(bev_neu_raw, na.rm = TRUE))

# Durch summarise sind die anderen Variablen verloren gegangen
aggregated_data <- aggregated_data %>% 
  left_join(merged_data, by = "kkz_neu")

# Aufräumen
kreise <- aggregated_data %>% 
  rename(kkz = kkz_neu,
         kreisname = `Kreisname 2019`,
         flaeche = fl18,
         kslk = kslk...6,
         G = kslk...7,
         kreisregion = kreg18,
         kreisregionname = kreg18name,
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
  select(kkz, Bevoelkerung, kreisname, flaeche, ksitz, kslk, G, kreisregion, kreisregionname,
         st_kreg, K, kslk_kreg, M, ktyp4, O, slraum, Q, raumt2010lage_kreis,
         S, ROR11, ROR11name, rtyp3, W, amr16, amr16name, RBZ, RBZname, NUTS2,
         NUTS2name, metropolen_IKM_2015_a, metropolen_IKM_2015_b, land,
         Bundesland, WO, AI) %>%
  mutate(NUTS2 = case_when(
    NUTS2name == "Chemnitz" ~ "DED4",  # Das BBSR hat verpasst, die NUTS2 Codes
    NUTS2name == "Leipzig" ~ "DED5",   # für Leipzig und Chemnitz zu updaten
    TRUE ~ NUTS2)) %>% 
  mutate(kkz=floor(kkz/1000)) #Kreiskennziffer auf 4 Stellen bringen


# Als Excel speichern
write_xlsx(kreise, "Data/Referenz/Erzeugte Referenzen/kreis-kreisregionen-2020.xlsx")

# Master-Referenz speichern
sheets <- list("Gemeindeverbände" = gvb,
               "Gemeinden" = gem.neu,
               "Gemeinden-GVB" = gemgvb,
               "KRS" = kreise)

write_xlsx(sheets, "Data/Referenz/Erzeugte Referenzen/Referenz_1998_2020.xlsx")

rm(gem.alt, gem.neu, gemgvb, gvb, aggregated_data, merged_data, kreise, k.umsteig, sheets)