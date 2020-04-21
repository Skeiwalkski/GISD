use "P:\Work\51_Thema_-_Regionale_Ungleichheiten\GISD\GISD Replication Niels\Outfiles\2019\Stata\workfile.dta", clear





fac 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss, pcf 
scree // drei Faktoren 
rot, pro blanks(.25)


*nur west
fac 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis<10000, pcf 
scree // eher 2 oder 5 Faktoren 
rot, pro blanks(.25)

*nur Ost
fac 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis>=10000, pcf 
scree // 2 bis 4 Faktoren
rot, pro blanks(.25)

*nur west 2015
fac 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis<10000 & Jahr==2015, pcf 
scree // unsichere 3
rot, pro blanks(.25)

*nur ost 2015
fac 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis>=10000 & Jahr==2015, pcf 
scree // unsichere 2 oder 4
rot, pro blanks(.25)

fac 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis>=10000 & Jahr==2015, fac(3) pcf 
scree // unsichere 2 oder 4
rot, pro blanks(.25)



corr 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss ///
		if Jahr==2015 

		
corr 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss ///
		if Jahr==2015 & Kreis<10000 

corr 	Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst ///
		Einkommenssteuer Haushaltseinkommen Schuldnerquote  ///
		BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss ///
		if Jahr==2015 & Kreis>=10000 

			
		
bysort Jahr: fac Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst , pcf fac(1)
bysort Jahr: fac Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst if Kreis<10000, pcf fac(1)
bysort Jahr: fac Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst if Kreis>=10000 , pcf fac(1)
* die Faktorstruktur ändert sich nicht über die Zeit und unterscheidet sich nicht zwischen Ost und West
* Eindimensionalität kann aber vor allem für Westdeutschland angenommen werden
* für Gesamtdeutschland sind die Gewichte für die Beschaeftigtenquote für die Jahre ab 2010 sehr gering
* für Ostdeutschland hat der Bruttoverdienst ein geringeres Gewicht

bysort Jahr: fac Einkommenssteuer Haushaltseinkommen Schuldnerquote , pcf fac(1)
bysort Jahr: fac Einkommenssteuer Haushaltseinkommen Schuldnerquote if Kreis<10000, pcf fac(1)
bysort Jahr: fac Einkommenssteuer Haushaltseinkommen Schuldnerquote if Kreis>=10000 , pcf fac(1)
* für die Teildimension Einkommen ist die Faktorstruktur konsisten


* Bildung
bysort Jahr: fac BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss , pcf fac(1)
bysort Jahr: fac BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis<10000, pcf fac(1)
bysort Jahr: fac BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis>=10000 , pcf fac(1)
* Faktorstruktur ändert sich über die Zeit, wenn man Osten und Westen getrennt berücksichtigt
* für eine Analyse innerhalb der beiden Regionen ist die Teildimension ungeeignet
* sie spiegelt vor allem Unterschiede zwischen Ost und West wider


sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst), method(mlmv) latent(TS_Arbeitswelt)
sem, stand
sem (TS_Einkommen -> Einkommenssteuer Haushaltseinkommen Schuldnerquote ), method(mlmv) iterate(60)  latent(TS_Einkommen)
sem, stand
sem (TS_Bildung -> BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss), method(mlmv) iterate(60) latent(TS_Bildung)
*konvergiert nicht
sem, stand
corr BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss
* Korrelation mit SchulgaengerohneAbschluss zu gering

sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst) (TS_Einkommen -> Einkommenssteuer Haushaltseinkommen Schuldnerquote ), method(mlmv) latent(TS_Arbeitswelt TS_Einkommen)
sem, stand

sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst) ///
	(TS_Einkommen -> Einkommenssteuer Haushaltseinkommen Schuldnerquote )	///
	, method(mlmv) latent(TS_Arbeitswelt TS_Einkommen)						///
	cov(e.Arbeitslosigkeit*e.Schuldnerquote)
sem, stand

* Multiple Group Analysis für die Teildimensionen

* ARBEITSWELT
sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst) if Jahr>2000 & Kreis<10000,  latent(TS_Arbeitswelt) group(Jahr) means(TS_Arbeitswelt@0) ginvariant(none)
*für West ist das Faktormodell konsistent über die Wellen
sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst) if Jahr>2000 & Kreis<10000,  latent(TS_Arbeitswelt) group(Jahr) means(TS_Arbeitswelt@0) ginvariant(mcoef)
*man kann sogar von metrischer Invarianz ausgehen, Modellfit gut
sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst) if Jahr>2004 & Jahr<2010 & Kreis<10000,  latent(TS_Arbeitswelt) group(Jahr) iterate(60) 
* 2010 tanzt aus der Reihe 
sem (TS_Arbeitswelt -> Beschaeftigtenquote Arbeitslosigkeit Bruttoverdienst) if Jahr>2001 & Jahr!=2003 & Jahr!=2010 & Kreis<10000,  latent(TS_Arbeitswelt) group(Jahr) 
* 2001, 2003 und 2010 sorgen für Nonconvergence
estat gof, stats(all) // schlechter fit
* man kann davon ausgehen, dass die Skala über die Jahre nicht skalar-invariant ist: Man kann die latenten Mittelwerte aus messtheoretischer Perspektive nicht miteinander vergleichen
estat mindices


* EINKOMMEN 
* Beschränkung der Samples wegen Kompletten Missings in einigen Jahren
sem (TS_Einkommen -> Einkommenssteuer Haushaltseinkommen Schuldnerquote ) if Jahr>=2004 & Jahr!=2009 & Jahr!=2010,  latent(TS_Einkommen) group(Jahr) means(TS_Einkommen@0) ginvariant(none)
sem (TS_Einkommen -> Einkommenssteuer Haushaltseinkommen Schuldnerquote ) if Jahr>=2004 & Jahr!=2005 & Jahr!=2009 & Jahr!=2010,  latent(TS_Einkommen) group(Jahr) means(TS_Einkommen@0) ginvariant(mcoef)
*man kann sogar von metrischer Invarianz ausgehen, Modellfit gut, diesmal für das gesamte Sample
sem (TS_Einkommen -> Einkommenssteuer Haushaltseinkommen Schuldnerquote ) if Jahr>=2004 & Jahr<=2011 & Jahr!=2005 & Jahr!=2008 & Jahr!=2009 & Jahr!=2010,  latent(TS_Einkommen) group(Jahr)
*konvergiert für einige Jahre nicht 
* man kann davon ausgehen, dass die Skala über die Jahre nicht skalar-invariant ist: Man kann die latenten Mittelwerte aus messtheoretischer Perspektive nicht miteinander vergleichen


sem (TS_Bildung -> BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss ), latent(TS_Bildung)
sem (TS_Bildung -> BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss ) if Jahr>=2014,  latent(TS_Bildung) group(Jahr) means(TS_Bildung@0) ginvariant(none)



cor BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss










use "P:\Work\51_Thema_-_Regionale_Ungleichheiten\GISD\GISD Replication Niels\impdata_imp.dta" , clear


bysort Jahr: fac BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss , pcf

bysort Jahr: fac BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis<10000, pcf fac(1)
bysort Jahr: fac BeschaeftigtemitakadAbschluss BeschaeftigteohneAbschluss SchulabgaengerohneAbschluss if Kreis>=10000 , pcf fac(1)
* gleiches Ergebnis wie für die nichtimputierten Daten
