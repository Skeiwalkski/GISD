
mydata   <- Impdata %>% group_by(Gemeindekennziffer) %>% select(Gemeindekennziffer,Jahr,Arbeitslosigkeit,
                                                             SchulabgaengerohneAbschluss,SchulabgaengermitHochschulreife, "Beschaeftigtenquote") %>% 
                                                             mutate(MEAN=mean(Beschaeftigtenquote , na.rm=T)) %>% ungroup()

mymodell <- lm(Beschaeftigtenquote ~ I(Jahr*Jahr*MEAN)+I(Jahr*MEAN) + Arbeitslosigkeit + SchulabgaengerohneAbschluss , mydata, na.action="na.exclude")


mydata   <- Impdata %>% group_by(Gemeindekennziffer) %>% select(Gemeindekennziffer,Jahr,Arbeitslosigkeit,
                                                                SchulabgaengerohneAbschluss,SchulabgaengermitHochschulreife, "Beschaeftigtenquote") %>% 
                                                            mutate(MEAN=mean(Beschaeftigtenquote , na.rm=T)) %>% ungroup()

