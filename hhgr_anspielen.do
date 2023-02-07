use "S:\Projekte\Abt2_Daten_ID0059\Projekte\BartigS\Daten\CoMobu2_v4.dta", clear

* Bisherige HH-Variable (nur RKI-SOEP-Teilnehme im HH) umbenennen
ren hhgr hhgr_rkisoep2

* Variable für HH-Größe (gesamter HH) anspielen (Lieferung SOEP 04.01.23 von Kaminsky/SOEP an Hoebel/RKI nach Absprache mit M.M. Grabka)
merge m:m hid syear using "S:\Projekte\Abt2_Daten_ID0059\Projekte\HoebelJ\Daten\hhgr2021.dta"
drop if _merge==2
drop _merge
ren hhgr hhsize
lab var hhsize "Aktuelle Haushaltsgröße"

* Für Hauhsalte mit einer hid, die es laut M.M. Grabka nicht (mehr) gibt und deswegen ein Missing haben, HH-Daten auf Basis der pid zuspielen (Lieferung SOEP 05.01.23 von Kaminsky/SOEP an Hoebel/RKI nach Absprache mit M.M. Grabka)
merge m:m pid syear using "S:\Projekte\Abt2_Daten_ID0059\Projekte\HoebelJ\Daten\hhgr2021_rest.dta"
replace hhsize = hhgr if hhsize == .
drop _merge
drop hhgr

* Netto-Äquivalenzeinkommen berechnen (Wurzel-Variante)
gen hhinc = .
replace hhinc = hlc0005_v2 if hlc0005_v2 >0
gen nae = hhinc/sqrt(hhsize)
tab nae, m

