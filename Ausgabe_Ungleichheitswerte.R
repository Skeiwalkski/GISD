
##an Niels: Komplettes Script ausführen, Tabelle in Console am Ende gibt die Werte aus

library(haven)
library(readxl)
library(dplyr)
library(tidyr)

###Daten Einlesen

dat_2022 <- read.csv("Outfiles/2022/Bund/Gemeinde/Gemeinde.csv") %>% mutate(Kreis = floor(Gemeindekennziffer/1000))


Lebenserwartung_Frauen <- read_excel("Data/Lebenserwartung/Lebenserwartung_female_13_17.xlsx", skip = 1, sheet = "Daten")
names(Lebenserwartung_Frauen)[1] <- "Kreis"
Lebenserwartung_Frauen[2:3] <- NULL
Lebenserwartung_Frauen <- Lebenserwartung_Frauen %>% gather(key = "Jahr", value = "Value" , -"Kreis", convert=T, na.rm = T) %>% mutate(Kreis = as.numeric(Kreis))

Lebenserwartung_Frauen <- Lebenserwartung_Frauen %>% filter(Jahr > 2014) %>% group_by(Kreis) %>% mutate(Lebenserwartung_Frauen_2016 = mean(Value)) %>% ungroup() %>% filter(Jahr == 2016) %>% select(Kreis,Lebenserwartung_Frauen_2016)


Lebenserwartung_Männer <- read_excel("Data/Lebenserwartung/Lebenserwartung_male_13_17.xlsx", skip = 1, sheet = "Daten")
names(Lebenserwartung_Männer)[1] <- "Kreis"
Lebenserwartung_Männer[2:3] <- NULL
Lebenserwartung_Männer <- Lebenserwartung_Männer %>% gather(key = "Jahr", value = "Value" , -"Kreis", convert=T, na.rm = T) %>% mutate(Kreis = as.numeric(Kreis))

Lebenserwartung_Männer <- Lebenserwartung_Männer %>% filter(Jahr > 2014) %>% group_by(Kreis) %>% mutate(Lebenserwartung_Männer_2016 = mean(Value)) %>% ungroup() %>% filter(Jahr == 2016) %>% select(Kreis,Lebenserwartung_Männer_2016)


dat_Lebenserwartung <- Lebenserwartung_Frauen %>% left_join(Lebenserwartung_Männer, by = "Kreis")


dat_Leberw_2022 <- dat_2022 %>% filter(Jahr == 2016) %>% left_join(dat_Lebenserwartung, by = "Kreis") %>% distinct(Kreis, .keep_all = TRUE) %>% unique()


###Modelle erstellen

mod_Frauen <- lm(Lebenserwartung_Frauen_2016 ~ GISD_Score, data = dat_Leberw_2022)
summary(mod_Frauen)

mod_Männer <- lm(Lebenserwartung_Männer_2016 ~ GISD_Score, data = dat_Leberw_2022)
summary(mod_Männer)


#Frauen
Frauen_high <- predict(mod_Frauen, data.frame(GISD_Score = 1))
Frauen_high

Frauen_low <- predict(mod_Frauen, data.frame(GISD_Score = 0))
Frauen_low

SDI_Frauen <- Frauen_low - Frauen_high
SDI_Frauen

Rsqu_Frauen <- summary(mod_Frauen)$r.squared
Rsqu_Frauen

#Männer
Männer_high <- predict(mod_Männer, data.frame(GISD_Score = 1))
Männer_high

Männer_low <- predict(mod_Männer, data.frame(GISD_Score = 0))
Männer_low

SDI_Männer <- Männer_low - Männer_high
SDI_Männer

Rsqu_Männer <- summary(mod_Männer)$r.squared
Rsqu_Männer


###Tabelle
tab <- matrix(c(SDI_Frauen, SDI_Männer, Rsqu_Frauen, Rsqu_Männer), nrow = 2, ncol = 2, byrow = T)

colnames(tab) <- c("Frauen", "Männer")
rownames(tab) <- c("SDI", "R-squared")

tab

