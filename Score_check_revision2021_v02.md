---
title: "Prüfung der GISD-Indikatoren v02"
author: "Marvin Reis"
date: "22 4 2021"
output:
  bookdown::html_document2:
    keep_md: true
    code_folding: hide
    toc: true
    toc_float: true
    toc_depth: 2
    number_sections: false
    fig_caption: true
    theme: cerulean
    highlight: tango  
---



## Info




## Deskription: Wie verteilt sich die Variation der Indikatoren in den GISD-Scores über die Zeit und über die Gemeinden?





![](Score_check_revision2021_v02_files/figure-html/Plots-1.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-2.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-3.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-4.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-5.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-6.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-7.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-8.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-9.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-10.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-11.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-12.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-13.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-14.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-15.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](Score_check_revision2021_v02_files/figure-html/Plots-16.png)<!-- -->

```
## Saving 7 x 5 in image
```

## Erklärung einiger Ausreißer - G8 Reformen


Ausreißer 2008 bis 2017: Einführung von G8 in verschiedenen BL, Beispiele:

- Ausreißer 2003: Einführung von G8 in Bayern reduziert sich der Nenner (alle Schulabgänger) für die Berechnung der Schulabgängeranteile in diesem Jahr, dadurch steigen die QUoten der Abgänger ohne Abschluss https://de.wikipedia.org/wiki/Abitur_in_Bayern_(G8)#%C3%9Cbergangsphase


Ausreißer 2001

- In Sachsen-Anhalt galt bis 2000 das 12-jährige Abitur, ab 2001 wurde G9 bei den Abschlüssen wirksam. Dadurch reduzierte sich für 2001 einmalig der Nenner (alle Schulabgänger) für die Berechnung der Schulabgängeranteile in diesem Jahr. Die Quoten der Abgänger ohne Abschluss fiel und die Abiturientenquote stieg.Die erneute Einführung von G8 2003/4 kam 2007 mit höheren Abschlussquoten zum Tragen. 

Besonderheit Thüringen vor 2004

-  In Thüringen gab es bis 2003 keine Regelung für Abgänger_innen aus der gymnasialen Oberstufe. Diese galten als Schulabgänger ohne Abschluss. Danach wurde mit Versetzung in die 10. Klasse der Haupt- bei Versetzung in die 11. Klasse der Realschulabschluss anerkannt (siehe Eichhorn & Huter 2004) ähnlich wie in den anderen Bundesländern.

"S:/OE/FG28/205 Regionale Unterschiede/GISD/Eichhorn & Huter 2004.pdf"


## Identifikation der Ausreißer

```r
Corr_data$outl <- "Kein Ausreißer"
# Ausreißer: München, Berlin, Pirmasens
Corr_data$outl[Corr_data$Kreis %in% c("9184", "11000", "7317")] <- "Ausreißer"

ggplot(Corr_data, aes(GISD_Score, Jahr)) + geom_tile(aes(color = outl), alpha = 0.5, size =0.5) + 
  theme_rki() + theme(legend.position="bottom") + scale_y_discrete(limits=rev) + 
  scale_x_continuous(position = "top") +
  labs(colour = "Ausreißer", x = "GISD Score", "Ausreißer im GISD-Score")
```

![](Score_check_revision2021_v02_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggsave("Outfiles/Outliers.png")
```

```
## Saving 7 x 5 in image
```

```r
Corr_data$outl <- "Andere Kreise"
Corr_data$outl[Corr_data$Kreis %in% c("9184")] <- "München"

ggplot(Corr_data, aes(GISD_Score, Jahr)) + geom_tile(aes(color = outl), alpha = 0.5, size =0.5) + 
  theme_rki() + theme(legend.position="bottom") + scale_y_discrete(limits=rev) + 
  scale_x_continuous(position = "top") +
  labs(colour = "Ausreißer", x = "GISD Score")
```

![](Score_check_revision2021_v02_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
ggsave("Outfiles/Outliers_München.png")
```

```
## Saving 7 x 5 in image
```

```r
Corr_data$outl <- "Andere Kreise"
Corr_data$outl[Corr_data$Kreis %in% c("11000")] <- "Berlin"

ggplot(Corr_data, aes(GISD_Score, Jahr)) + geom_tile(aes(color = outl), alpha = 0.5, size =0.5) + 
  theme_rki() + theme(legend.position="bottom") + scale_y_discrete(limits=rev) + 
  scale_x_continuous(position = "top") +
  labs(colour = "Ausreißer", x = "GISD Score")
```

![](Score_check_revision2021_v02_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
ggsave("Outfiles/Outliers_Berlin.png")
```

```
## Saving 7 x 5 in image
```

```r
Corr_data$outl <- "Andere Kreise"
Corr_data$outl[Corr_data$Kreis %in% c("7317")] <- "Primasens"

ggplot(Corr_data, aes(GISD_Score, Jahr)) + geom_tile(aes(color = outl), alpha = 0.5, size =0.5) + 
  theme_rki() + theme(legend.position="bottom") + scale_y_discrete(limits=rev) + 
  scale_x_continuous(position = "top") +
  labs(colour = "Ausreißer", x = "GISD Score")
```

![](Score_check_revision2021_v02_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
ggsave("Outfiles/Outliers_Primasens.png")
```

```
## Saving 7 x 5 in image
```

## Verteilung des GISD

```r
Corr_data$ow <- "Ost"
Corr_data$ow[Corr_data$Kreis < 11000] <- "West"

ggplot(Corr_data, aes(GISD_Score, Jahr)) + geom_tile(aes(color = ow), alpha = 0.5, size =0.5) + 
  theme_rki() + theme(legend.position="bottom") + scale_y_discrete(limits=rev) + 
  scale_x_continuous(position = "top") +
  labs(colour = "Ost und west", x = "GISD Score", title = "GISD-Score nach ost- und westdeutschen Landkreisen")
```

![](Score_check_revision2021_v02_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggsave("Outfiles/OstWest.png")
```

```
## Saving 7 x 5 in image
```

```r
Corr_data$BL <- "Schleswig-Holstein"
Corr_data$BL[Corr_data$Kreis == 2000] <- "Hamburg"
Corr_data$BL[Corr_data$Kreis > 2999 & Corr_data$Kreis < 4000] <- "Niedersachsen"
Corr_data$BL[Corr_data$Kreis > 3999 & Corr_data$Kreis < 5000] <- "Bremen"
Corr_data$BL[Corr_data$Kreis > 4999 & Corr_data$Kreis < 6000] <- "Nordrhein-Westfalen"
Corr_data$BL[Corr_data$Kreis > 5999 & Corr_data$Kreis < 7000] <- "Hessen"
Corr_data$BL[Corr_data$Kreis > 6999 & Corr_data$Kreis < 8000] <- "Rheinland-Pfalz"
Corr_data$BL[Corr_data$Kreis > 7999 & Corr_data$Kreis < 9000] <- "Baden-Würtemberg"
Corr_data$BL[Corr_data$Kreis > 8999 & Corr_data$Kreis < 10000] <- "Bayern"
Corr_data$BL[Corr_data$Kreis > 9999 & Corr_data$Kreis < 11000] <- "Saarland"
Corr_data$BL[Corr_data$Kreis > 10999 & Corr_data$Kreis < 12000] <- "Berlin"
Corr_data$BL[Corr_data$Kreis > 11999 & Corr_data$Kreis < 13000] <- "Brandenburg"
Corr_data$BL[Corr_data$Kreis > 12999 & Corr_data$Kreis < 14000] <- "Mecklenburg-Vorpommern"
Corr_data$BL[Corr_data$Kreis > 13999 & Corr_data$Kreis < 15000] <- "Sachsen"
Corr_data$BL[Corr_data$Kreis > 14999 & Corr_data$Kreis < 16000] <- "Sachsen-Anhalt"
Corr_data$BL[Corr_data$Kreis > 15999] <- "Thüringen"


ggplot(Corr_data, aes(GISD_Score, Jahr)) + geom_tile(aes(color = BL), alpha = 0.5, size =0.5) + 
  theme_rki() + scale_y_discrete(limits=rev) + 
  scale_x_continuous(position = "top") +
  labs(colour = "Bundesland", x = "GISD Score", title = "GISD-Score nach Bundesland")
```

![](Score_check_revision2021_v02_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
ggsave("Outfiles/Bundesland.png")
```

```
## Saving 7 x 5 in image
```



