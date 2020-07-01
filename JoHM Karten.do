*-------------------------------------------------------------------------------*
* Pfade RKI Office
* Shapefiles Ordner
* global shape "S:\OE\FG28\205 Regionale Unterschiede\Referenzdaten\Kartendaten"
* Working Directory
* cd "S:\OE\FG28\200 Soziale Ungleichheit\Covid 19\Artikel JoHM\Dofiles"
* Outfiles Ordner
* global outfiles "S:\OE\FG28\200 Soziale Ungleichheit\Covid 19\Artikel JoHM\Abbildungen und Tabellen"
*-------------------------------------------------------------------------------*

*-------------------------------------------------------------------------------*
* Pfade Home
* Shapefiles Ordner
global shape "C:\Kartendaten"
* Working Directory
cd "C:\projects_RStudio\GISD"
* Outfiles Ordner
global outfiles "C:\projects_RStudio\GISD\Outfiles"
*-------------------------------------------------------------------------------*


import excel  using "$shape\Abb_2__Deutschlandkarten_Kreise.xls", clear first
gen jahr=2014
rename kkz KRkennziffer
save "$outfiles\Abb_2__Deutschlandkarten_Kreise.dta", replace

import delimited using "C:\projects_RStudio\GISD\Outfiles\2020b\Bund\Kreis\Kreis.csv", clear 
* Karten
rename kreiskennziffer KRkennziffer
sort KRkennziffer

merge m:1 KRkennziffer using "$outfiles\Abb_2__Deutschlandkarten_Kreise.dta"
drop _merge
tab gisd_5 GISD_5


* Kreisgebietsreform Göttingen: Zusammenlegung von LK Göttingen und LK Osterode am Harz
recode KRkennziffer 3159=3152	// KKZ Göttingens mit alter KKZ überschreiben
* Die bislang verwendeten Kartendaten trennen beide Landkreise noch. 
* Damit die Information des zusammengefassten Landkreises Göttingen auf der Karte auch für Osterode angezeigt werden, muss ein weiterer Fall kreiert werden. 
* Osterode im Harz
list if KRkennziffer==3152 // Zeile von Göttingen identifizieren
expand 2 in 26 // Zeile von Göttingen duplizieren
recode KRkennziffer 3152=3156 in 26 // Kreiskennziffer in einem der Zeilen durch KKZ Osterodes ersetzen
merge m:1 KRkennziffer using "$shape\BRD\2012\BRD_KRS.dta"
drop _merge

		  	
* Karte GISD_5	
spmap gisd_5 if jahr==2017 using "$shape\BRD\2012\BRD_KRS_Koordinaten.dta" , legenda(on)   ///
  id(KRkennziffer ) fcolor(rkicmyk5 rkicmyk4 rkicmyk3 rkicmyk2 rkicmyk1) ///
  clm(unique) clbreaks(1 2 3 4 5) ///
  ndlabel(keine Daten)	  ///
  subtitle("") legorder(lohi)   ///
  polygon(  osize(thin) data("$shape\BRD\2012\BRD_KRS_Koordinaten.dta") legshow(2 3 4 5))  ///
  osize(thin .. )  legstyle(2)   graphregion(margin(zero) style(none))  ///
  legend(size(small)  keygap(minuscule) symysize(medium) ///
          symxsize(small) ring(1) row(1) pos(6) rowgap(tiny) colgap(small)) legjunction(" {&ge} ") 	///
		  saving(1, replace)
		  
gr export "$outfiles\GISD5_2017.png" , width(450) replace	


* Karte GISD_5	
spmap GISD_5 if jahr==2014 using "$shape\BRD\2012\BRD_KRS_Koordinaten.dta" , legenda(on)   ///
  id(KRkennziffer ) fcolor(rkicmyk5 rkicmyk4 rkicmyk3 rkicmyk2 rkicmyk1) ///
  clm(unique) clbreaks(1 2 3 4 5) ///
  ndlabel(keine Daten)	  ///
  subtitle("") legorder(lohi)   ///
  polygon(  osize(thin) data("$shape\BRD\2012\BRD_KRS_Koordinaten.dta") legshow(2 3 4 5))  ///
  osize(thin .. )  legstyle(2)   graphregion(margin(zero) style(none))  ///
  legend(size(small)  keygap(minuscule) symysize(medium) ///
          symxsize(small) ring(1) row(1) pos(6) rowgap(tiny) colgap(small)) legjunction(" {&ge} ") 	///
		  saving(2, replace)
		  
gr export "$outfiles\GISD5_2014.png" , width(450) replace	 
graph combine 1.gph 2.gph, saving(GISD5_2017vs2014, replace)
graph export GISD5_2017vs2014.png, replace

	

* Karte Inzidenz

spmap incidence_esp13 if jahr==2017 using "$shape\BRD\2012\BRD_KRS_Koordinaten.dta" , legenda(on)   ///
  id(KRkennziffer ) fcolor(rkicmyk5 rkicmyk4 rkicmyk3 rkicmyk2 rkicmyk1) ///
  clm(custom) clbreaks(0 125 250 500 1000 15000) ///
  ndlabel(keine Daten)	  ///
  subtitle("") legorder(lohi)   ///
  polygon(  osize(thin) data("$shape\BRD\2012\BRD_KRS_Koordinaten.dta") legshow(2 3 4 5))  ///
  osize(thin .. )  legstyle(2)   graphregion(margin(zero) style(none))  ///
  legend(size(small)  keygap(minuscule) symysize(medium) ///
          symxsize(small) ring(1) row(1) pos(6) rowgap(tiny) colgap(small)) legjunction(" {&ge} ") 		
gr export "$outfiles\incidence_esp13.png" , width(450) replace	


