use "C:\projects_rstudio\GISD\Outfiles\2022\Stata\Impdata_check_2019.dta", clear

/* Kreisebene zum ausprobieren
gen Einkommensteuer_hat=.
levelsof Kreis, local(levels)
 foreach l of local levels {
	reg Einkommensteuer c.Jahr##c.Jahr if Kreis==`l'
	predict xb if Kreis==`l' 
	replace Einkommensteuer_hat=xb if Kreis==`l' 
	drop xb
							}

*/
	
* Beispiel mit Einkommensteuer (besser log(Einkommensteuer))
gen Einkommensteuer_hat=.
levelsof Gemeindekennziffer, local(levels)
 foreach l of local levels {
	reg Einkommensteuer c.Jahr##c.Jahr if Gemeindekennziffer==`l'
	predict xb if Gemeindekennziffer==`l' 
	replace Einkommensteuer_hat=xb if Gemeindekennziffer==`l' 
	drop xb
							}

