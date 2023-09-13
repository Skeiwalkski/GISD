---
title: "Scatterplots - Lebenserwartung und Sozio-Ökonomische Deprivation"
author: "Marvin Reis"
date: "21 5 2021"
output:
  bookdown::html_document2:
    keep_md: true
    code_folding: hide
    number_sections: false
    fig_caption: true
    theme: cerulean
    highlight: tango
---





```r
Lebenserwartung_dat <- read_excel("Data/Lebenserwartung/Lebenserwartung_17_18_19.xlsx", skip = 1, sheet = "Daten")
names(Lebenserwartung_dat)[1] <- "Kreis"
Lebenserwartung_dat[2:3] <- NULL
Lebenserwartung_dat <- Lebenserwartung_dat %>% gather(key = "Jahr", value = "Value" , -"Kreis", convert=T, na.rm = T) %>% mutate(Kreis = as.numeric(Kreis))

Lebenserwartung_dat <- Lebenserwartung_dat %>% group_by(Kreis) %>% mutate(Lebenserwartung = mean(Value)) %>% ungroup() %>% filter(Jahr == 2018) %>% select(-Jahr, -Value)

GISD_data_Kreis <- read.csv("Outfiles/2022_v03/Bund/GISD_Bund_Kreis.csv") %>% filter(year == 2018)

GISD_data_Kreis <- GISD_data_Kreis %>% rename(Kreis = kreis_id, GISD_Score = gisd_score) %>% select(Kreis, GISD_Score) %>% distinct(Kreis, .keep_all = TRUE) %>% unique()

GISD_Lebenserw_Kreis <- left_join(GISD_data_Kreis, Lebenserwartung_dat, by = "Kreis")
```


```r
#Normalization
GISD_Lebenserw_Kreis$GISD_Score <- (GISD_Lebenserw_Kreis$GISD_Score -min(GISD_Lebenserw_Kreis$GISD_Score ))/(max(GISD_Lebenserw_Kreis$GISD_Score )-min(GISD_Lebenserw_Kreis$GISD_Score ))
```


```r
ggplot(GISD_Lebenserw_Kreis, aes(x = GISD_Score, y = Lebenserwartung)) +
  geom_point(size = 1.5, alpha = 0.5, col = "navy") +
  geom_rug(size = 0.5) + 
  labs(x = "GISD-Score", title = "Lebenserwartung der Landkreise nach dem GISD", subtitle = "auf Basis 2018", y = "Lebenserwartung in Jahren") +
  theme_rki()
```

![](Lebenserwartung_GISD_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
ggplot(GISD_Lebenserw_Kreis, aes(x = GISD_Score, y = Lebenserwartung)) +
  geom_point(size = 1.5, alpha = 0.5, col = "navy") +
  geom_rug(size = 0.5) +
  geom_smooth(method = loess, col = "red", linetype = "dashed", fill = "grey50", alpha = 0.5) +
  labs(x = "GISD-Score", title = "Lebenserwartung der Landkreise nach dem GISD", subtitle =  "auf Basis 2018, mit Regressionslinie (Loess)", y = "Lebenserwartung in Jahren") +
  theme_rki()
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Lebenserwartung_GISD_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
ggplot(GISD_Lebenserw_Kreis, aes(x = GISD_Score, y = Lebenserwartung)) +
  geom_point(size = 1.5, alpha = 0.5, col = "navy") +
  geom_rug(size = 0.5) +
  geom_smooth(method = lm,col = "red", linetype = "dashed", fill = "grey50", alpha = 0.5) +
  labs(x = "GISD-Score", title = "Lebenserwartung der Landkreise nach dem GISD", subtitle = "auf Basis 2018, mit Regressionslinie (Linear)", y = "Lebenserwartung in Jahren") +
  theme_rki()
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Lebenserwartung_GISD_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
ggplot(GISD_Lebenserw_Kreis, aes(x = GISD_Score, y = Lebenserwartung)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_density2d(size = 1, col = "navy", alpha = 0.5) +
  labs(x = "GISD-Score", title = "Lebenserwartung der Landkreise nach dem GISD (Density)", subtitle = "auf Basis 2018", y = "Lebenserwartung in Jahren") +
  theme_rki()
```

![](Lebenserwartung_GISD_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



```r
GISD_dat <- read.csv("Outfiles/2022_v03/Bund/GISD_Bund_Kreis.csv") %>% 
  rename(Kreis = kreis_id, GISD_5 = gisd_5, Jahr = year) %>% 
  select(GISD_5, Jahr, Kreis) %>% 
  mutate(GISD_5 = case_when(GISD_5 == 1 ~ 5,
                            GISD_5 == 2 ~ 4,
                            GISD_5 == 3 ~ 3,
                            GISD_5 == 4 ~ 2,
                            GISD_5 == 5 ~ 1))



##Datenaufbereitung der Frauen Daten
Lebenserwartung_female <- read_excel("Data/Lebenserwartung/Lebenserwartung_female_97_17.xlsx", skip = 1, sheet = "Daten")
names(Lebenserwartung_female)[1] <- "Kreis"
Lebenserwartung_female[2:3] <- NULL
Lebenserwartung_female <- Lebenserwartung_female %>% gather(key = "Jahr", value = "Value" , -"Kreis", convert=T, na.rm = T) %>% mutate(Kreis = as.numeric(Kreis))

Lebenserwartung_Kreise <- Lebenserwartung_female %>% select(Kreis)

for (i in 1998:2016) {
Lebenserwartung <- Lebenserwartung_female %>% group_by(Kreis) %>% filter(Jahr == i | Jahr == i+1 | Jahr == i-1) %>% mutate(Lebenserwartung_adj = mean(Value)) %>% ungroup() %>% filter(Jahr == i) %>% select(-Value)

Lebenserwartung <- spread(Lebenserwartung, key = Jahr, value = Lebenserwartung_adj)

Lebenserwartung_Kreise <- Lebenserwartung_Kreise %>% left_join(Lebenserwartung, by = c("Kreis"))
}

Lebenserwartung_female_mean <- gather(Lebenserwartung_Kreise, key = "Jahr", value = "Lebenserwartung_adj", 2:20) %>% mutate(Jahr = as.numeric(Jahr))

Lebenserwartung_female_mean <- Lebenserwartung_female_mean %>% left_join(GISD_dat, by = c("Kreis", "Jahr")) %>% group_by(GISD_5, Jahr) %>% mutate(Lebenserwartung_mean = mean(Lebenserwartung_adj)) %>% ungroup()


##Datenaufbereitung der Männer Daten
Lebenserwartung_male <- read_excel("Data/Lebenserwartung/Lebenserwartung_male_97_17.xlsx", skip = 1, sheet = "Daten")
names(Lebenserwartung_male)[1] <- "Kreis"
Lebenserwartung_male[2:3] <- NULL
Lebenserwartung_male <- Lebenserwartung_male %>% gather(key = "Jahr", value = "Value" , -"Kreis", convert=T, na.rm = T) %>% mutate(Kreis = as.numeric(Kreis))

Lebenserwartung_Kreise <- Lebenserwartung_male %>% select(Kreis)

for (i in 1998:2016) {
Lebenserwartung <- Lebenserwartung_male %>% group_by(Kreis) %>% filter(Jahr == i | Jahr == i+1 | Jahr == i-1) %>% mutate(Lebenserwartung_adj = mean(Value)) %>% ungroup() %>% filter(Jahr == i) %>% select(-Value)

Lebenserwartung <- spread(Lebenserwartung, key = Jahr, value = Lebenserwartung_adj)

Lebenserwartung_Kreise <- Lebenserwartung_Kreise %>% left_join(Lebenserwartung, by = c("Kreis"))
}

Lebenserwartung_male_mean <- gather(Lebenserwartung_Kreise, key = "Jahr", value = "Lebenserwartung_adj", 2:20) %>% mutate(Jahr = as.numeric(Jahr))

Lebenserwartung_male_mean <- Lebenserwartung_male_mean %>% left_join(GISD_dat, by = c("Kreis", "Jahr")) %>% group_by(GISD_5, Jahr) %>% mutate(Lebenserwartung_mean = mean(Lebenserwartung_adj)) %>% ungroup()
```


```r
ggplot(Lebenserwartung_female_mean, aes(x = Jahr, y = Lebenserwartung_mean, col = as.factor(GISD_5))) +
  geom_line(size = 1.25) +
  geom_point() +
  scale_color_rki(labels = c("5 - most deprived", "4", "3", "2", "1 - least deprived"), guide = guide_legend(reverse=TRUE)) +
  scale_x_continuous(limits = c(1998, 2016), breaks = c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016), labels = c("1997/1999", "1999/2001", "2001/2003", "2003/2005", "2005/2007", "2007/2009", "2009/2011", "2011/2013", "2013/2015", "2015/2017"), guide = guide_axis(angle = -22.25)) +
  labs(title = "Life expectancy trend for women by GISD-Quintile", x = "Year", y = "avg. life expectancy in years", col = "GISD-Quintile") +
  theme_rki()
```

![](Lebenserwartung_GISD_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
ggplot(Lebenserwartung_male_mean, aes(x = Jahr, y = Lebenserwartung_mean, col = as.factor(GISD_5))) +
  geom_line(size = 1.25) +
  geom_point() +
  scale_color_rki(labels = c("5 - most deprived", "4", "3", "2", "1 - least deprived"), guide = guide_legend(reverse=TRUE)) +
  scale_x_continuous(limits = c(1998, 2016), breaks = c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016), labels = c("1997/1999", "1999/2001", "2001/2003", "2003/2005", "2005/2007", "2007/2009", "2009/2011", "2011/2013", "2013/2015", "2015/2017"), guide = guide_axis(angle = -22.25)) +
  labs(title = "Life expectancy trend for men by GISD-Quintile", x = "Year", y = "avg. life expectancy in years", col = "GISD-Quintile") +
  theme_rki()
```

![](Lebenserwartung_GISD_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

