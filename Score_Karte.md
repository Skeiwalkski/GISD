---
title: "Karte des GISD Scores"
author: "Marvin Reis"
date: "20 5 2021"
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
GISD_data_Kreis <- read.csv("C:/git_projects/GISD/Outfiles/2021/Bund/Kreis/Kreis.csv") %>% mutate(Kreis = Kreiskennziffer) %>% select(Kreis, GISD_Score, GISD_5, GISD_10, Bundesland) %>% distinct(Kreis, .keep_all = TRUE) %>% unique() %>% lazy_dt()

Kreise_data <- readRDS("C:/git_projects/GISD/Data/SHP/kreise_bkg.rds") %>% lazy_dt() %>% mutate(Kreis = as.numeric(id)) %>% select(-id) %>% left_join(GISD_data_Kreis, by = "Kreis") %>% lazy_dt()

Kreise_data <- as_tibble(Kreise_data)

GISD_data_Gem <- read.csv("C:/git_projects/GISD/Outfiles/2021/Bund/Gemeinde/Gemeinde.csv") %>% select(Gemeindekennziffer, GISD_Score, GISD_5, GISD_10, Bundesland) %>% distinct(Gemeindekennziffer, .keep_all = TRUE) %>% unique() %>% lazy_dt()

Gemeinden_data <- readRDS("C:/git_projects/GISD/Data/SHP/BRD_Gemeinden.rds") %>% lazy_dt() %>% mutate(Gemeindekennziffer = as.numeric(id)) %>% select(-id) %>% left_join(GISD_data_Gem, by = "Gemeindekennziffer") %>% lazy_dt()

Gemeinden_data <- as_tibble(Gemeinden_data)


GISD_data_Lander <- read.csv("C:/git_projects/GISD/Outfiles/2021/Bund/Raumordnungsregion/Raumordnungsregion.csv") %>% mutate(ROR_id = Raumordnungsregion.Nr) %>%  select(ROR_id, GISD_Score, GISD_5, GISD_10, Bundesland) %>% distinct(ROR_id, .keep_all = TRUE) %>% unique() %>% lazy_dt()

Lander_data <- readRDS("C:/git_projects/GISD/Data/SHP/ROR_map.rds") %>% lazy_dt() %>% mutate(ROR_id = as.numeric(id)) %>% select(-id) %>% left_join(GISD_data_Lander, by = "ROR_id") %>% lazy_dt()

Lander_data <- as_tibble(Lander_data)
```

## GISD-Score auf Gemeindeebene

```r
ggplot(Gemeinden_data, aes(long, lat, group = group, fill = GISD_Score)) +
  geom_polygon() +
  scale_fill_gradient(limits = c(0,1)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score")
```

![](Score_Karte_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#which(is.na(Gemeinden_data$GISD_Score))
```


```r
ggplot(Gemeinden_data, aes(long, lat, group = group, fill = GISD_5)) +
  geom_polygon() +
  scale_fill_gradient(breaks = c(1, 2, 3, 4, 5)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Quintile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
ggplot(Gemeinden_data, aes(long, lat, group = group, fill = GISD_10)) +
  geom_polygon() +
  scale_fill_gradient(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Dezile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## GISD-Score auf Kreisebene

```r
ggplot(Kreise_data, aes(long, lat, group = group, fill=GISD_Score)) +
  geom_polygon() +
  scale_fill_gradient(limits = c(0,1)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score")
```

![](Score_Karte_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
ggplot(Kreise_data, aes(long, lat, group = group, fill=GISD_5)) +
  geom_polygon() +
  scale_fill_gradient(breaks = c(1, 2, 3, 4, 5)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Quintile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
ggplot(Kreise_data, aes(long, lat, group = group, fill=GISD_10)) +
  geom_polygon() +
  scale_fill_gradient(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Dezile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## GISD-Score auf Länderebene

```r
ggplot(Lander_data, aes(long, lat, group = group, fill=GISD_Score)) +
  geom_polygon() +
  scale_fill_gradient(limits = c(0,1)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Dezile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
ggplot(Lander_data, aes(long, lat, group = group, fill=GISD_5)) +
  geom_polygon() +
  scale_fill_gradient(breaks = c(1, 2, 3, 4, 5)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Quintile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
ggplot(Lander_data, aes(long, lat, group = group, fill=GISD_10)) +
  geom_polygon() +
  scale_fill_gradient(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  coord_equal() +
  theme_rki_void() +
  labs(fill = "GISD-Score (Dezile)")
```

![](Score_Karte_files/figure-html/unnamed-chunk-10-1.png)<!-- -->