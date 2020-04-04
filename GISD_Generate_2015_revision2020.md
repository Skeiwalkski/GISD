Fehlerbehebung in der Revision 2019  
von Niels Michalski

Hintergrund Fehlerbehebung: In der Revision 2019 der GISD-Daten tauchten
zum Teil starke Abweichungen zur Revision 2018 auf.Bei der Addition der
Teildimensionen ging die Bildungsdimension in umgekehrter Richtung in
den GISD ein. Mehrere Faktoren spielten dabei eine Rolle: 1. Die
Struktur der Faktorladungen der Bildungsindikatoren ist nicht robust
gegenüber Datenschwankungen. a) Der erste Faktor bildet die intendierte
Kompomente ab. Es gibt allerdings einen zweiten Faktor mit Eigenwert
über 1. b) Die Gewichte der Faktorladungen der Indikatoren
BeschaeftigteohneAbschluss und Schulgaengerohneabschluss variieren sehr
stark zwischen den Revisionen 2018 und 2019 2. Die Indikatoren
BeschaeftigteohneAbschluss und BeschaeftigtemitHochschulabschluss dieser
Teildimension weisen die höchsten Anteile an MissingData auf (75%). 3.
Ausschlaggebend ist am Ende eine negative Korrelation der
Bildungsdimension mit dem Anteil Arbeitsloser, der zur Umkehrung des
Faktors führt. Der Anteil Arbeitsloser wird als Markerindikator
verwendet.

Eine Möglichkeit: Der Indikator BeschäftigteohneAbschluss wird durch
SchulabgängermitHochschulreife ersetzt.

SOP for Revision (nach Lars Kroll) 1. Obtain new Data and Reference
Files from INKAR (manually) -\> check format 2. Change Year in
GISD\_generate\_postcodes.R according to INKAR Regional Date and execute
3. Execute GISD\_Generate.R (there should be no edits required)

Inhalt
------

1.  Benötigte Pakete

<!-- -->

1.  Generierung eines ID-Datensatzes
2.  Erzeugen eines Datensatzes mit Kennziffern als ID unabhÃ¤ngig von
    Ebene III.Imputation fehlender Werte
3.  Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der
    Faktorscores
4.  Datenexport - Erstellung der DatensÃ¤tze

R Markdown
----------

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see
<a href="http://rmarkdown.rstudio.com" class="uri">http://rmarkdown.rstudio.com</a>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

Including Plots
---------------

You can also embed plots, for example:

![](GISD_Generate_2015_revision2020_files/figure-markdown_github/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
