capture clear
cd "S:\OE\FG28\205 Regionale Unterschiede\Publikationen\Regionaler_SES_Index"
// ZIEL REG-SES

// Daten
use if inlist(Jahr,1998,2003,2008,2012) using Daten/2012/reg_ses_daten.dta  , clear
* merge 1:1 Kennziffer Jahr using  "Daten/reg_ses_daten_CORESES.dta"

* Ausgangsliste
quietly: ds  	alo		 beschaeftigte_hq beschaeft_mHSA		///
	bruttolohn		hheink	///
		schulabg_oA	schuldner	steuereinnahmen beschaeftigte
local indexkomponenten = r(varlist)
d `indexkomponenten'


* z-Standardisierung
foreach var of varlist `indexkomponenten' {
		quietly: sum `var' [iw=Bev]
		gen Z_`var' = (`var'  - r(mean) ) /r(sd) 
	
}
quietly: ds Z_*
local indexkomponenten = r(varlist)

* Indexbildung
* Faktor: Einkommen der Haushalte
factor Z_hheink Z_steuereinnahmen Z_schuldner if Jahr == 2012  [aw=Bev] , factors(1) pcf
rotate 
predict TS_GISD_EINK 

* Faktor: Bildung
/* factor Z_beschaeft_mHSA schulabg_oA  if Jahr == 2012  [aw=Bev] , factors(1)
rotate 
predict TS_GISD_BILD
*/
gen TS_GISD_BILD = 2*Z_beschaeft_mHSA - Z_schulabg_oA  

* Faktor: Beschäftigung
factor alo bruttolohn  beschaeftigte if Jahr == 2012  [aw=Bev] , factors(1) pcf
rotate 
predict TS_GISD_BESCH

foreach Teilscore of varlist TS_GISD_* {
sum `Teilscore' , meanonly
replace `Teilscore' = 7/(((`Teilscore'-r(min))/(r(max)-r(min)))*6+1)
sum `Teilscore'
}
gen GISD = TS_GISD_EINK+TS_GISD_BILD+TS_GISD_BESCH
corr alo GISD TS_* [aw=Bev]
reg regSES alo  [aw=Bev]
reg regSES GISD [aw=Bev]
reg regSES TS_GISD* [aw=Bev]

// Quantile bilden
gen GISD10 = .
gen GISD5  = .

lab var GISD "German Index of Socio-economic Deprivation"
lab var GISD10 "German Index of Socio-economic Deprivation (Deciles)"
lab var GISD5 "German Index of Socio-economic Deprivation (Quintiles)"

foreach Jahr of numlist 1998 2003 2008 2012 {
	quietly {
		xtile GISD10`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(10)
		replace GISD10 = GISD10`Jahr'  if `Jahr' == Jahr 
		xtile GISD5`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(5)
		replace GISD5 = GISD5`Jahr'  if `Jahr' == Jahr 
		drop GISD5`Jahr' GISD10`Jahr'
		}
	}

gen GISD_k:nmh = 1 if GISD5==1
replace GISD_k = 2 if inlist(GISD5,2,3,4)
replace GISD_k = 3 if GISD5==5
lab var GISD_k "German Index of Socio-economic Deprivation (Gruppen)"
lab def nmh 1 "niedrig" 2 "mittel" 3 "hoch" , modify

compress
save Daten/GISD_full.dta , replace
gen n = 1 

* Plausi-check
table Jahr [iw=Bev] , c(mean GISD sum n mean alo mean hheink) format(%12.1f)
table Jahr GISD5 [iw=Bev] , c(mean GISD ) format(%12,1f) col row
table Jahr GISD5 [iw=Bev] , c(mean alo ) format(%2,1f) col row
table Jahr GISD5 [iw=Bev] , c(mean hheink ) format(%12.0f) col row

// Export Gemeindeverbandsebene	
use Daten/GISD_full.dta , clear
keep Kennziffer Jahr GISD* TS_*
reshape wide  GISD GISD10 GISD5 GISD_k TS_* , i(Kennziffer) j(Jahr) 
foreach var of varlist GISD* {
	foreach jahr of numlist 1998 2003 2008 2012 {
		local neuername = subinstr("`var'","`jahr'","_`jahr'",.)
		capture ren `var' `neuername'
		}
  }
order Kennziffer 
compress

save  Daten/GISD_GKZ2012.dta, replace
export excel using "Daten/GISD_GKZ2012.xlsx", sheet("GISD_KGS_31122012") firstrow(variables) nolabel replace

// Export Kreise
use Daten/GISD_full.dta , clear
gen n = 1
collapse (mean) GISD  (sum) n  [iw=Bev] , by(KREIS Jahr)
replace GISD = round(GISD,0.01)
sum GISD  if Jahr == 2012
replace GISD = round(((GISD - r(min))/(r(max)-r(min)))*100,.01) 
	
ren n Bev
lab var GISD "German Index of Socio-economic Deprivation (Score 0-100)"
gen GISD5 = .
lab var GISD5 "German Index of Socio-economic Deprivation (Quintiles)"
gen GISD10 = .
lab var GISD10 "German Index of Socio-economic Deprivation (Deciles)"



foreach Jahr of numlist 1998 2003 2008 2012 {
	xtile GISD10`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(10)
	replace GISD10 = GISD10`Jahr'  if `Jahr' == Jahr 
		
	xtile GISD5`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(5)
	replace GISD5 = GISD5`Jahr'  if `Jahr' == Jahr 
	
	
	drop GISD5`Jahr' GISD10`Jahr'
	}

gen GISD_k:nmh = 1 if GISD5==1
replace GISD_k = 2 if inlist(GISD5,2,3,4)
replace GISD_k = 3 if GISD5==5
lab var GISD_k "German Index of Socio-economic Deprivation (Gruppen)"
lab def nmh 1 "niedrig" 2 "mittel" 3 "hoch" , modify	
	
drop Bev
order KREIS Jahr
ren KREIS Kreis2012
lab var	Kreis2012 "Kreiskennziffer am 31.12.2012"

reshape wide  GISD GISD10 GISD5 GISD_k , i(Kreis2012) j(Jahr) 
foreach var of varlist GISD* {
	foreach jahr of numlist 1998 2003 2008 2012 {
		local neuername = subinstr("`var'","`jahr'","_`jahr'",.)
		capture ren `var' `neuername'
		}
  }

foreach Jahr of numlist 1998 2003 2008 2012 {  
	lab var GISD_`Jahr' 		"German Index of Socio-economic Deprivation (Score 0-100)" 
	lab var GISD10_`Jahr' 	"German Index of Socio-economic Deprivation (Deciles)"
	lab var GISD5_`Jahr' 	"German Index of Socio-economic Deprivation (Quintiles)"
	lab var GISD_k_`Jahr' 	"German Index of Socio-economic Deprivation (Quintiles)"
	lab val GISD_k_`Jahr' nmh
	}
sort Kreis2012
compress  
save  Daten/GISD_KREIS2012.dta, replace
export excel using "Daten/GISD_KREIS2012.xlsx", sheet("GISD_KGS_31122012") firstrow(variables) nolabel replace

// Export Raumordnungsregionen ROR2012
use Daten/GISD_full.dta , clear
gen n = 1
collapse (mean) GISD  (sum) n  [iw=Bev] , by(ROR2012 Jahr)
replace GISD = round(GISD,0.01)
sum GISD  if Jahr == 2012
replace GISD = round(((GISD - r(min))/(r(max)-r(min)))*100,.01) 
	
ren n Bev
lab var GISD "German Index of Socio-economic Deprivation (Score 0-100)"
gen GISD5 = .
lab var GISD5 "German Index of Socio-economic Deprivation (Quintiles)"
gen GISD10 = .
lab var GISD10 "German Index of Socio-economic Deprivation (Deciles)"



foreach Jahr of numlist 1998 2003 2008 2012 {
	xtile GISD10`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(10)
	replace GISD10 = GISD10`Jahr'  if `Jahr' == Jahr 
		
	xtile GISD5`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(5)
	replace GISD5 = GISD5`Jahr'  if `Jahr' == Jahr 
	
	
	drop GISD5`Jahr' GISD10`Jahr'
	}

gen GISD_k:nmh = 1 if GISD5==1
replace GISD_k = 2 if inlist(GISD5,2,3,4)
replace GISD_k = 3 if GISD5==5
lab var GISD_k "German Index of Socio-economic Deprivation (Gruppen)"
lab def nmh 1 "niedrig" 2 "mittel" 3 "hoch" , modify	
	
drop Bev
order ROR2012 Jahr
lab var	ROR "Raumordnungsregion am 31.12.2012"

reshape wide  GISD GISD10 GISD5 GISD_k , i(ROR2012) j(Jahr) 
foreach var of varlist GISD* {
	foreach jahr of numlist 1998 2003 2008 2012 {
		local neuername = subinstr("`var'","`jahr'","_`jahr'",.)
		capture ren `var' `neuername'
		}
  }

foreach Jahr of numlist 1998 2003 2008 2012 {  
	lab var GISD_`Jahr' 		"German Index of Socio-economic Deprivation (Score 0-100)" 
	lab var GISD10_`Jahr' 	"German Index of Socio-economic Deprivation (Deciles)"
	lab var GISD5_`Jahr' 	"German Index of Socio-economic Deprivation (Quintiles)"
	lab var GISD_k_`Jahr' 	"German Index of Socio-economic Deprivation (Quintiles)"
	lab val GISD_k_`Jahr' nmh
	}
sort ROR2012
compress  
save  Daten/GISD_ROR2012.dta, replace
export excel using "Daten/GISD_ROR2012.xlsx", sheet("GISD_KGS_31122012") firstrow(variables) nolabel replace


// Export NUTS-2
use Daten/GISD_full.dta , clear
gen n = 1
replace NUTS2 = "DE40" if inlist(NUTS2,"DE41","DE42") 
replace NUTS2 = "DED4" if inlist(NUTS2,"DED1") // Chemnitz, neue Nummer, nicht 2
replace NUTS2 = "DED5" if inlist(NUTS2,"DED3") // Leipzig, neue Nummer, nicht 2
collapse (mean) GISD  (sum) n  [iw=Bev] , by(NUTS2 Jahr)
replace GISD = round(GISD,0.01)
sum GISD  if Jahr == 2012
replace GISD = round(((GISD - r(min))/(r(max)-r(min)))*100,.01) 
	
ren n Bev
lab var GISD "German Index of Socio-economic Deprivation (Score 0-100)"
gen GISD5 = .
lab var GISD5 "German Index of Socio-economic Deprivation (Quintiles)"
gen GISD10 = .
lab var GISD10 "German Index of Socio-economic Deprivation (Deciles)"



foreach Jahr of numlist 1998 2003 2008 2012 {
	xtile GISD10`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(10)
	replace GISD10 = GISD10`Jahr'  if `Jahr' == Jahr 
		
	xtile GISD5`Jahr' = GISD [aw=Bev] if `Jahr' == Jahr , nq(5)
	replace GISD5 = GISD5`Jahr'  if `Jahr' == Jahr 
	
	
	drop GISD5`Jahr' GISD10`Jahr'
	}

gen GISD_k:nmh = 1 if GISD5==1
replace GISD_k = 2 if inlist(GISD5,2,3,4)
replace GISD_k = 3 if GISD5==5
lab var GISD_k "German Index of Socio-economic Deprivation (Gruppen)"
lab def nmh 1 "niedrig" 2 "mittel" 3 "hoch" , modify	
	
order NUTS2 Jahr
ren NUTS2 NUTS22012
lab var	NUTS22012 "NUTS2 am 31.12.2012"

reshape wide  GISD GISD10 GISD5 GISD_k Bev , i(NUTS22012) j(Jahr) 
foreach var of varlist GISD* Bev* {
	foreach jahr of numlist 1998 2003 2008 2012 {
		local neuername = subinstr("`var'","`jahr'","_`jahr'",.)
		capture ren `var' `neuername'
		}
  }

foreach Jahr of numlist 1998 2003 2008 2012 {  
	lab var Bev_`Jahr' 		"Bevölkerung am 31.12.`Jahr'"
	lab var GISD_`Jahr' 		"German Index of Socio-economic Deprivation (Score 0-100)" 
	lab var GISD10_`Jahr' 	"German Index of Socio-economic Deprivation (Deciles)"
	lab var GISD5_`Jahr' 	"German Index of Socio-economic Deprivation (Quintiles)"
	lab var GISD_k_`Jahr' 	"German Index of Socio-economic Deprivation (Quintiles)"
	lab val GISD_k_`Jahr' nmh
	}
sort NUTS22012
compress  
save  Daten/GISD_NUTS22012.dta, replace
export excel using "Daten/GISD_NUTS2_2012.xlsx", sheet("GISD_KGS_31122012") firstrow(variables) nolabel replace

exit
