
* ============================================================================= *
* 							REGRESSIONSANALYSE		 							*
* 							Fabian Kalleitner		 							*
* 							16.06.2026      		 							*
* ============================================================================= *

* ----------------------------------------------------------------------------- *
*						Beschreibung des Zwecks		    			    		*
* ----------------------------------------------------------------------------- *

/*
In diesem File wird eine einfache Regressionsanalyse durchgeführt. Ziel ist
es die Frage zu beantworten ob Personen in Österreich dazu neigen die
Vollzeitarbeit von Frauen bei Präsenz eines Kleinkindes im Haushalt stärker
abzulehnen als bei Männern und ob dieser Unterschied durch die Präsenz eines
Kindes im Haushalt der Befragten moderiert wird.

Auf Theorien basierende Thesen zu den Wirkmechanismen:
1) Traditionelle Wertvorstellungen unter Teilen der Bevölkerung führt dazu, dass
Vollzeitarbeit bei Präsenz eines Kleinkindes im Haushalt bei Frauen eher
abgelehnt wird als bei Männern.
2) Präsenz eines Kindes steigert die Spezialisierungstendenzen und verfestigt
traditionelle Wertvorstellungen aufgrund der ansonsten entstehenden kognitiven
Dissonanz.

Davon abgeleitete Hypothesen:
H1: Personen lehnen die Vollzeitarbeit von Frauen bei Präsenz eines Kleinkindes
im Haushalt stärker ab als bei Männern.

H2: Der Gendereffekt in der Ablehnung von Vollzeitarbeit bei der Präsenz von
Kleinkindern im Haushalt ist stärker ausgeprägt bei Personen die selbst ein Kind
im Haushalt haben als bei Personen ohne Kinder im Haushalt.

Datenstruktur: ESS Round 3 beinhaltet randomisierte Fragen die entweder nach der
Beurteilung von Frauen oder Männer fragt, dadurch kann der kausale Effekt des
Geschlechts der fiktiven Person berechnet werden. Für die Moderationsanalyse
sind Kontrollvariablen essentiell die auf mögliche Confounder von Moderator
(Kind im Haushalt) und abhängige Variable kontrollieren.

Infos über die Datenstruktur finden sie in der Data Documentation online zu
ihrem jeweiligen Datensatz. Vor allem Codebook und Survey Questionnaire
(Fragebogen) sind essentiell.
*/

* ----------------------------------------------------------------------------- *
*						Beginn des do-Files		    			    			*
* ----------------------------------------------------------------------------- *

version 17			// Version von Stata für die das Do-File geschrieben wurde
clear all			// löscht Speicher
set more off		// Do-File läuft ohne Unterbrechungen ab

// Verzeichnisstruktur
// Code speichert die Pfade als Variablen (globals) damit das File einfacher transformierbar ist 

global 	maindir		"C:/Users/ra85miz/LRZ Sync+Share/Lehre/SE Arbeitsmarktsoziologie/2026_S/Exercises/Working with R and Stata" // HIER ENTSPRECHEND IHREM HAUPTFOLDER ANPASSEN

global 	data_in		"`maindir'Data/raw"
global 	data_out	"`maindir'Data/processed"
global 	figures		"`maindir'Output/fig"
global 	tables		"`maindir'Output/tab"

cd "$maindir" // Stammpfad = Projektpfad

// Lade die Daten

use "$data_in/ESS3e03_7.dta", clear // öffnet Datenfile Stata-Beispielfile verfügbar unter: https://doi.org/10.21338/ess3e03_7

* ----------------------------------------------------------------------------- *
*			          Deskriptive Analysen & Rekodieren	    		    		*
* ----------------------------------------------------------------------------- *

*******
*** ZIELSAMPLE AUSWÄHLEN
*******

// In dieser Studie werden nur Umfragedaten aus Österreich verwendet:

tab cntry // Häufigkeitsauszählung (tabulate)
tab cntry, nol  // Werte (statt Labels) werden gezeigt
				// in diesem Fall sind den Labels keine Zahlen hinterlegt
				// es kann deshalb direkt mit den Labels gefiltert werden
keep if cntry == "AT" // behalte nur Fälle (Befragte) aus Österreich (AT)

*******
*** VORBEREITENDE ANALYSEN
*******

// ABHÄNGIGE VARIABLE: Dafür/dagegen wenn Mütter/Väter mit unter 3-jährigem Kind
// Vollzeit erwerbstätig ist

tab aftjbyc 	// Häufigkeitsauszählung (tabulate) -> 
				// ist die Varaible sehr schief verteilt, gibt es Ausreißer?

tab aftjbyc, m		// Inspektion der fehlenden Werte (missing) - diese können
					// hier vernachlässigt werden


// ZENTRALE UNABHÄNGIGE VARIABLEN
tab icsbfm	// Indikator ob Frage aftjbyc sich auf Männer oder Frauen bezieht

// WEITERE UNABHÄNGIGE VARIABLEN: Geschlecht, Alter, Bildung, Stadt/Land, Kinder
codebook gndr age edulvla domicil bthcld	// Wertebereiche, Labels und fehlende
											// Werte auf einen Blick

// Kreuztabellen zwischen abhängiger und unabhängigen Variablen
// um die Rekodierung zu unterstützen

tab aftjbyc edulvla
tab aftjbyc edulvla, nof col 			 //No Frequencies Befehl gibt col Spaltenprozente
//man darf not compl. nicht interpretieren da ja erkannt dass zu wenige Fälle
// kein sehr lineares Bild upper secondary ist problem
//entscheiden welche Gruppen zusammengefasst werden
								   	//row cell
tab aftjbyc domicil
tab aftjbyc domicil, nof col
mean aftjbyc, over(domicil) //entscheiden welche Gruppen zusammengefasst werden

*******
*** REKODIEREN DER ABHÄNGIGEN VARIABLE(N)
*******

// Bereits in die richtige Richtung codiert -> desto höher desto progressivere
// Einstellung, aber umcodiert um bei 0 zu beginnen

recode aftjbyc (1=4 "sehr dagegen") (2=3 "dagegen") ///
	(3=2 "unentschieden") (4=1 "dafür") ///
	(5=0 "sehr dafür"), gen(trad_family)
label variable trad_family "Ablehnung von Vollzeitarbeit"

*******
*** REKODIEREN DER UNABHÄNGIGEN VARIABLEN
*******

* (0) Treatment (Gender der zu beurteilenden fiktiven Person)
recode icsbfm (2=0 "Mann") (1=1 "Frau"), gen(treatment)
label variable treatment "Geschlecht der beurteilten Person"

tab icsbfm treatment

* (1) Geschlecht
recode gndr (1=0 "Mann") (2=1 "Frau"), gen(geschlecht)
label variable geschlecht "Geschlecht der befragten Person"

tab gndr geschlecht

* (2) Alter
gen alter=trunc(age)		// Kommastellen werden "abgeschnitten" (truncated)

// "/" bedeutet bis

recode alter (min/29=0 "15-29 Jahre") (30/44=1 "30-44 Jahre")  ///
(45/59=2 "45-59 Jahre") (60/99=3 ">=60 Jahre"), gen(alter_gr)
label variable alter_gr "Alter"

tab alter alter_gr 	// Überprüfen der rekodierten Variable durch eine
					// Kreuztabelle

* (3) Bildung
recode edulvla (1/2=0 "<= Sekundarstufe I") (3=1 "Sekundarstufe II") ///
(4/5=2 "post-sek., tertiär"), gen(bildung)
label variable bildung "Höchster Bildungsabschluss"

tab edulvla bildung

* (4) Stadt/Land
recode domicil (1=0 "Gr. Stadt") (2/3=1 "Vorstadt, kleine Stadt") ///
(4/5=2 "Land"), gen(urban)
label variable urban "Wohnortgröße"

tab domicil urban

* (5) Kinder
recode chldhm (1=1 "Kind im Haushalt") (2=0 "Kein Kind im Haushalt"), gen(child)
label variable child "Kind <18 im Haushalt"

tab chldhm child

*******
*** ERSTE DESKRIPTIVE ANALYSEN
*******

tab trad_family		// Verteilung der abhängigen Variablen (in den Appendix)
					// Wird fast immer angegeben als Hintergrundinformation
tab1 geschlecht alter_gr bildung urban child	// Verteilung der unabh. Variablen
							// (in den Methodenteil oder den Anhang)



tab2 trad_family geschlecht alter_gr bildung urban child, firstonly miss //kreuztabelle zwischen 1. und allen anderen (firstonly)
tab2 trad_family geschlecht alter_gr bildung urban child, firstonly nof col
			// Kreuztabellen zwischen abhängiger und unabhängigen Variablen
			// (auf Anomalien überprüfen auch hinsichtlich von missings)


*******
*** Analytisches Sample erstellen
*******

sum idno  //2405 Individuen haben Fragen des ESS in Österreich beantwortet

* Einschränkung der Personengruppe (hier nicht notwendig) Bsp:
//drop if mnactic == 6 // Schließe Personen aus der Analyse aus die in Rente sind
//sum idno  //1949 Individuen haben Fragen des ESS in Österreich beantwortet und sind nicht in Rente#

// Listwise deletion: Exkludiere alle Fälle mit fehlenden Werten in Modellvariablen
// Achtung: vorher UNBEDINGT genau nachsehen wie fehlende Werte codiert sind
// Variablen icsbfm, geschlecht, alter_gr, urban haben keine fehlenden Werte

mark insample if !missing(trad_family, bildung, child)	// markiert vollständige Fälle
keep if insample										// behält nur vollständige Fälle
drop insample											// Hilfsvariable wieder löschen

sum idno //2295 Individuen haben Fragen des ESS in Österreich beantwortet und 
//keine fehlenden Werte in den Modellvariablen (Achten Sie darauf dass sie 
//genügend Fälle - statistische Power - haben um Ihre Analysen durchführen zu können)

*******
*** DESKRIPTIVER ZUSAMMENHANG ZWISCHEN UNABHÄNGIGER UND ABHÄNGIGER VARIABLE
*******

*Einfache Tabelle mit Frequencies
tab trad_family treatment, nof col

tab trad_family treatment [aweight = pspwght], nof col // ACHTUNG Gewichtung beachten insbesondere bei deskriptiven Resultaten

// Balkendiagramm: Kategorien nebeneinander, Farbe nach Geschlecht der beurteilten Person
// Zuerst: X-Achsen-Labels mit Zeilenumbrüchen (char(10)) neu definieren
label define trad_family ///
    0 "sehr dafür"        ///
    1 "dafür"                            ///
    2 "unentschieden" ///
    3 "dagegen"                         ///
    4 "sehr dagegen", modify

graph bar (percent) [pweight = pspwght], over(treatment) over(trad_family) asyvars ///
    title("Zustimmung/Ablehnung von Vollzeitarbeit nach Geschlecht der beurteilten Person") ///
    ytitle("Prozent (%)") ///
    legend(order(1 "Mann" 2 "Frau") title("Beurteilte Person")) ///
	xsize(8.5)
graph export "$figures/balkendiagramm_treatment.png", replace

// Vollzeitarbeit von Frauen bei Präsenz eines Kleinkindes Haushalt wird im 
// Schnitt viel stärker abgelehnt als bei Männern.


*******
*** LINEARE REGRESSION (OLS)
*******


// Modell 1: Nur Treatment (bivariat)
regress trad_family i.treatment // * Im Idealfall benutzen Sie hier auch die 
//Gewichtungsvariablen regress trad_family i.treatment [pweight = pspwght] 
//hier der Einfachheit in allen Regressionen ohne Gewichtung
est store m1 // Speichere Resultate

// Modell 2: + Kontrollvariablen
regress trad_family i.treatment i.child i.geschlecht i.alter_gr i.bildung i.urban
		// i. vor der Variable bezeichnet eine Kategoriale/Dummy-Variable
		// 1. Kategorie ist automatisch die Referenzkategorie
est store m2

// Modell 3: + Interaktionseffekt Treatment x Kind im Haushalt (H2)
regress trad_family i.treatment##i.child i.geschlecht i.alter_gr i.bildung i.urban
		// ## erzeugt automatisch Haupteffekte UND Interaktionsterm
est store m3

*******
*** REGRESSIONSTABELLE EXPORTIEREN (etable)
*******

etable, estimates(m1 m2 m3) ///
    showstars ///
    showstarsnote ///
	mstat(N) mstat(r2) ///
    column(estimates)
	
// Der bereits in der Deskription gefundenen Unterschied ist auch statistische
// signifikant. Ebenso zeigt sich ein statistisch signifikanter Moderations-
// effekt des Treatments je nachdem ob im Haushalt der befragten Person ein Kind
// wohnt. 
	
* Add a top border to the model statistics (for looks only)
collect style cell cell_type[column-header], border(bottom)

collect style cell result[N], border(top)

collect export "$tables/regressionen1.docx", replace // exportiert die Tabelle in eine Word datei

*******
*** INTERAKTIONSEFFEKT: AME des Treatments nach Kindstatus (H2)
*******

//Zur einfacheren Interpretation von Interaktionseffekten sollten hier immer
//Average Marginal Effecs berechnet werden: Zeigt wie stark ist der Treatment-
//Effekt je nach Zustand der Moderatorvariable. 

est restore m3 //berechne Marginale Effekte aufgrund von Modell 3
// Average Marginal Effect (AME) des Treatments getrennt nach Kindstatus
margins child, dydx(treatment)
		// dydx(treatment) = durchschn. marginaler Effekt von treatment
		// getrennt nach den Kategorien von child (0=kein Kind, 1=Kind vorhanden)

// Grafische Darstellung der AMEs mit Konfidenzintervallen
marginsplot, ///
    recast(scatter) ///
    title("AME des Treatments nach Kind im Haushalt (H2)") ///
    ytitle("Marginaler Effekt von Treatment" "(Frau vs. Mann)") ///
    xtitle("") ///
    xlabel(0 "Kein Kind" 1 "Kind vorhanden") ///
    yline(0, lpattern(dash) lcolor(gray)) ///
	yscale(range(0 .)) ///
	ylabel(0(.25)1.5) ///
    xscale(range(-0.5 1.5)) ///
    legend(off) 
graph export "$figures/ame_treatment_nach_kind.png", replace

*******
*** VERGLEICH ZWISCHEN MODELLEN FÜR VERSCHIEDENE GRUPPEN (AUCH: LÄNDER)
*******
//Beispiel: Frage Unterscheidet sich der Kind Effekt nach Geschlecht der befragten Person?
//Hier eher besser einen Interaktionseffekt zu schätzen aber manchmal will man 
//alle unabhängigen Faktoren unabhängig von einem möglichen Moderator schätzen
//dann macht ein Split-Sample approach sinn.

//Gleiche Kontrollvariablen, aber getrennte Schätzung nach Geschlecht
regress trad_family i.treatment i.child i.alter_gr i.bildung i.urban if geschlecht==0
est store men		// store estimates für Männer

regress trad_family i.treatment i.child i.alter_gr i.bildung i.urban if geschlecht==1
est store women		// store estimates für Frauen


suest men women // Kombiniere die Regressionsmodelle
test [men_mean = women_mean] // Test ob ALLE Koeffizienten sich signifikant zwischen den Gruppen unterscheiden
test [men_mean]1.child = [women_mean]1.child // Test ob der Koffizient von Kinder im Haushalt sich signifikant zwischen den Gruppen unterscheidet --> Nein

*******
*** LOGISTISCHE REGRESSION für binäre (0-1) abhängige Variable insebsondere bei sehr ungleich verteilten Gruppen
*******

// abhängige Variable wird binär rekodiert in (sehr) dagegen vs. alle andere
gen trad_family_bin = .
replace trad_family_bin = 1 if inlist(aftjbyc, 1, 2)
replace trad_family_bin = 0 if inlist(aftjbyc, 3, 4, 5)

label variable trad_family_bin "Gegen Vollzeit Arbeit"
label define trad_family_bin 0 "nicht dagegen" 1 "dagegen"
label values trad_family_bin trad_family_bin

logit trad_family_bin i.treatment i.child i.geschlecht i.alter_gr i.bildung i.urban //beta coefficients
est store logit_b

margins, dydx(*) post // Average marginal effects based on logit regression
estimates store AME

logit trad_family_bin i.treatment i.child i.geschlecht i.alter_gr i.bildung i.urban, or  //odds ratios 
est store logit_or 

reg trad_family_bin i.treatment i.child i.geschlecht i.alter_gr i.bildung i.urban //linear propability model
estimates store lpm

collect clear

//Resultate zeigen beinahe identische Koeffizienten im Vergleich Marginale 
//Effekte einer logistischen Regression und Koeffizienten eines Linear 
//Propability Models

etable, estimates(logit_b logit_or AME lpm) ///
    showstars ///
    showstarsnote ///
	mstat(N) mstat(sudo_r2=e(r2_p)) mstat(r2) ///
    column(estimates)
	
* Add a top border to the model statistics
collect style cell cell_type[column-header], border(bottom)

collect style cell result[N], border(top)

collect export "$tables/regressionen2.docx", replace

	