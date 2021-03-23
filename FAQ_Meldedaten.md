FAQ Meldedaten 
================
AG Regio
30 06 2020

Update: 22 03 2021

## Wie wird mit unplausiblen Altersangaben umgegangen?
  * Die Eintragungen über 100 sind zum großen Teil auf Fehler beim Eintragen zurückzuführen.
  * Für die ECDC-Übermittlung kodiert Abteilung 3 alles über 110 Jahre als missing.
  	
## Worauf bezieht sich das Meldedatum?
  * "Bei der Darstellung der Neuinfektionen pro Tag wird das Meldedatum verwendet, also das Datum, an dem das Gesundheitsamt Kenntnis von dem Fall erlangt und ihn als solchen elektronisch erfasst." Eingefügt aus <https://www.esri.de/de-de/landingpages/corona-impact-2020/faq> 
	* Mit jedem täglichen Update werden auch Fälle mit Meldedatum hinzugefügt, die vor dem tagesaktuellen Datum liegen. 
	* Es macht Sinn, die Daten für die Analysen auf einen Zeitraum einzugrenzen, der ein paar Tage (Neuinfektionen) bzw. 3 bis 4 Wochen (Sterbefälle) vor dem Datum des Datenstandes liegt.
	* Replikationen sind ggfs. gewährleistet, da Abteilung 3 Datenstände der Meldedaten für alle vergangenen Meldetage vorhält. 

		
## Wie sollte mit dem Meldeverzug insbesondere am Wochenende umgegangen werden?
  * Für die meisten Fragestellungen reicht es auch die Fälle nach Meldewoche zu berichten. 
	Mit dem Abschluss einer Meldewoche sollten die Fluktuationen über die Wochentage ausgeglichen sein
	
## Wo ist der Unterschied zwischen den Dashboard-Zahlen und dem was man als Datensatz herunterladen kann?
  * Die Daten des RKI-Dashboards unterliegen besonderer Aufmerksamkeit durch die Presse und informierte Bürger. 
  * Es kommen deshalb einzelne Löschungen problematischer Fälle vor.
	* Die Dashbaord-Daten liefern deshalb nicht für alle Auswertungen exakt die gleichen Zahlen (aber sehr nahe dran), wie die Survstat-Daten (und die Dashboard Grafiken).
	
	
## Wie sinnvoll ist es den Erkrankungsbeginn als Variable zu verwenden?
  * Das Meldedatum bildet den Verlauf der Pandemie mit ausreichend großer Exaktheit ab.
  * Der Erkrankungsbeginn ist zwar für symptomatische Fälle exakter, für asymptomatische Fälle aber umso unsicherer, denn symptomfreie Fälle würden als nicht-erkrankt, nur infiziert gelten .
  
## Was heißt erkrankt?
  * Infos zur Klinik vorhanden
  * Mindestens ein Symptom


## Teilweise liegt das Meldedatum vor Erkrankungsbeginn. Wie kann das sein? Wird der Erkrankungsbeginn nachgemeldet?
  * Man geht hier von Fehlern bei den Eintragungen aus.
	* Vorgehen RKI-Dashboard: "Bei Fällen ohne Angaben zum Erkrankungsbeginn (Erkrankungsbeginn unbekannt bzw. Fälle ohne Symptome) wird ersatzweise das Meldedatum verwendet. Die abnehmende Fallzahl über die letzten Tage kann durch den Melde- und Übermittlungsverzug bedingt sein." 
	  * Eingefügt aus <https://npgeo-de.maps.arcgis.com/apps/opsdashboard/index.html#/2694322fc7894bf5886647f652f093ca> 
  * "Es werden nur Fälle veröffentlicht, bei denen eine labordiagnostische Bestätigung unabhängig vom klinischen Bild vorliegt. Die Daten werden am RKI einmal täglich jeweils um 00:00 Uhr prozessiert. Die Daten stehen dann in den frühen Morgenstunden aktualisiert im Layer zur Verfügung."
	  * Eingefügt aus <https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6> 
	* Bei der Darstellung der Neuinfektionen pro Tag wird das Meldedatum verwendet, also das Datum, an dem das GA Kenntnis von dem Fall erlangt und ihn als solchen elektronisch erfasst.
		* Eingefügt aus <https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6> 



## +++ Neue und noch nicht beantwortete Fragen +++

### Beispiele, wie wir unplausible Daten ausgeschlossen haben und mit nichtvorhandener Information umgegangen sind:
  * CoMoBu - bundesweite Seroprävalenzschätzung:
  * Ausgeschlossen Fälle in vorläufigen Analysen mit Daten bis einschließlich 4. Oktober:
    * 4 Fälle mit Meldewoche <5/2020
    * 31 Fälle mit Alter ≥ 110
    *	1339 Fälle mit Alter = Missing (0,2 %)
    
  * Auswertungen mit dem GISD für den JoHM-Beitrag im Sommer. ausgeschlossen wurden:
    * Alter ≥ 106, Alter Missing 
    *	Geschlecht unbekannt oder divers 
    
  Gibt es noch weitere Dinge zu beachten?

### Wir schauen uns auch regelmäßig die Hospitalisierungen an. Bis jetzt haben wir nur die bestätigten Fälle (ja) genutzt. Die weiteren Kategorien sind -nicht ermittelbar- und -nicht erhoben-. Wie stark werden die wahren Hospitalisierungsraten ungefähr unterschätzt?

### Ist es sinnvoll Case-Fatality-Rates berechnen?

### In der Sero-Auswerungsgruppe fiel die Bemerkung, dass sich die Falldefinition über die Zeit geändert hat. Schlägt das quantitativ durch, so dass man es in den Auswertungen berücksichtigen müsste.
  




