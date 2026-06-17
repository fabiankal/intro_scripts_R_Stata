# ============================================================================= #
#                           REGRESSIONSANALYSE                                  #
#                           Fabian Kalleitner                                   #
#                           16.06.2026                                          #
# ============================================================================= #

# ----------------------------------------------------------------------------- #
#                         Beschreibung des Zwecks                               #
# ----------------------------------------------------------------------------- #

# In diesem File wird eine einfache Regressionsanalyse durchgeführt. Ziel ist
# es die Frage zu beantworten ob Personen in Österreich dazu neigen die
# Vollzeitarbeit von Frauen bei Präsenz eines Kleinkindes im Haushalt stärker
# abzulehnen als bei Männern und ob dieser Unterschied durch die Präsenz eines
# Kindes im Haushalt der Befragten moderiert wird.
#
# Davon abgeleitete Hypothesen:
# H1: Personen lehnen die Vollzeitarbeit von Frauen bei Präsenz eines Kleinkindes
#     im Haushalt stärker ab als bei Männern.
# H2: Der Gendereffekt ist stärker ausgeprägt bei Personen die selbst ein Kind
#     im Haushalt haben als bei Personen ohne Kinder im Haushalt.
#
# Datenstruktur: ESS Round 3 beinhaltet randomisierte Fragen die entweder nach
# der Beurteilung von Frauen oder Männer fragt, dadurch kann der kausale Effekt
# des Geschlechts der fiktiven Person berechnet werden.

# ----------------------------------------------------------------------------- #
#                           Pakete laden                                        #
# ----------------------------------------------------------------------------- #

# PACKAGES MÜSSEN EINMALIG INSTALLIERT WERDEN ->
# Zur Installation einfach in den folgenden Zeilen den Kommentarbefehl entfernen
# (die #) und den Code ausführen.

# install.packages(c("haven", "tidyverse", "marginaleffects", "modelsummary",
#                    "srvyr", "labelled", "margins"))

library(haven)            # Stata .dta Dateien einlesen
library(tidyverse)        # Datenmanipulation & Grafik (dplyr, ggplot2, ...)
library(marginaleffects)  # Average Marginal Effects (AME)
library(modelsummary)     # Regressionstabellen exportieren
library(srvyr)            # Gewichtete deskriptive Analysen (survey design)
library(labelled)         # Variablen- und Wertelabels aus Stata-Dateien

# ----------------------------------------------------------------------------- #
#                           Beginn des Skripts                                  #
# ----------------------------------------------------------------------------- #

# Verzeichnisstruktur
# Aufpassen! Pfade müssen dem eigenen System angepasst werden

maindir  <- "C:/Users/ra85miz/LRZ Sync+Share/Lehre/SE Arbeitsmarktsoziologie/2026_S/Exercises/Working with R and Stata" # HIER ENTSPRECHEND IHREM HAUPTFOLDER ANPASSEN

# file.path() fügt Ordner und Dateinamen mit dem richtigen Trennzeichen zusammen
data_in  <- file.path(maindir, "Data/raw")
data_out <- file.path(maindir, "Data/processed")
figures  <- file.path(maindir, "Output/fig")
tables   <- file.path(maindir, "Output/tab")

# Lade die Daten
# read_dta() liest Stata-.dta-Dateien ein und behält Variablen- und Wertelabels

df_raw <- read_dta(file.path(data_in, "ESS3e03_7.dta"))
# Datenfile verfügbar unter: https://doi.org/10.21338/ess3e03_7

# ----------------------------------------------------------------------------- #
#                   Deskriptive Analysen & Rekodieren                           #
# ----------------------------------------------------------------------------- #

# -----------------------------------------------------------------------
# ZIELSAMPLE AUSWÄHLEN
# -----------------------------------------------------------------------

# count() zählt Beobachtungen pro Kategorie (entspricht: tab in Stata)
count(df_raw, cntry)

# In dieser Studie werden nur Umfragedaten aus Österreich verwendet
# filter() behält nur Zeilen die die Bedingung erfüllen (entspricht: keep if in Stata)
df <- df_raw |>
  filter(cntry == "AT")

# -----------------------------------------------------------------------
# VORBEREITENDE ANALYSEN
# -----------------------------------------------------------------------

# ABHÄNGIGE VARIABLE: Dafür/dagegen wenn Mütter/Väter mit unter 3-jährigem
# Kind Vollzeit erwerbstätig ist
# -> ist die Variable sehr schief verteilt, gibt es Ausreißer?
count(df, aftjbyc)

# Inspektion der fehlenden Werte (missing)
# summarise() berechnet zusammenfassende Statistiken für den gesamten Datensatz
# sum() summiert Werte, is.na() gibt TRUE zurück wenn ein Wert fehlt (NA)
df |> summarise(missing = sum(is.na(aftjbyc)))

# ZENTRALE UNABHÄNGIGE VARIABLEN
# Indikator ob Frage aftjbyc sich auf Männer oder Frauen bezieht
count(df, icsbfm)

# WEITERE UNABHÄNGIGE VARIABLEN: Geschlecht, Alter, Bildung, Stadt/Land, Kinder
# Wertebereiche, Labels und fehlende Werte auf einen Blick
# select() wählt bestimmte Spalten aus dem Datensatz aus
# summary() gibt Minimum, Maximum, Mittelwert und fehlende Werte aus
df |>
  select(gndr, age, edulvla, domicil, bthcld) |>
  summary()

# Kreuztabellen zwischen abhängiger und unabhängigen Variablen
# um die Rekodierung zu unterstützen
# pivot_wider() dreht das Ergebnis von lang nach breit (eine Spalte pro Kategorie)
df |>
  count(aftjbyc, edulvla) |>
  pivot_wider(names_from = edulvla, values_from = n)

# Spaltenprozente (entspricht: tab aftjbyc edulvla, nof col)
# group_by() gruppiert den Datensatz – nachfolgende Operationen laufen innerhalb
# der Gruppen ab (hier: Prozente werden innerhalb jeder edulvla-Gruppe berechnet)
# mutate() erstellt neue Spalten oder verändert bestehende
df |>
  count(aftjbyc, edulvla) |>
  group_by(edulvla) |>
  mutate(pct = n / sum(n) * 100) |>
  select(-n) |>           # -n entfernt die Spalte n
  pivot_wider(names_from = edulvla, values_from = pct)

# Mittelwert der abhängigen Variable nach Wohnortgröße
# (entspricht: mean aftjbyc, over(domicil))
# mean() berechnet den Mittelwert, na.rm = TRUE ignoriert fehlende Werte
df |>
  group_by(domicil) |>
  summarise(mean_aftjbyc = mean(as.numeric(aftjbyc), na.rm = TRUE))

# -----------------------------------------------------------------------
# REKODIEREN DER ABHÄNGIGEN VARIABLE(N)
# -----------------------------------------------------------------------

# Bereits in die richtige Richtung codiert -> desto höher desto progressivere
# Einstellung, aber umcodiert um bei 0 zu beginnen.
# as.numeric() löst zunächst die Stata-Labels auf (1-5), dann recode.
# case_when() ist das R-Äquivalent zu recode in Stata:
# Bedingung ~ neuer Wert, von oben nach unten ausgewertet
# factor() wandelt eine Variable in eine Faktorvariable um und vergibt Labels

df <- df |>
  mutate(
    trad_family = case_when(
      as.numeric(aftjbyc) == 1 ~ 4,  # sehr dagegen
      as.numeric(aftjbyc) == 2 ~ 3,  # dagegen
      as.numeric(aftjbyc) == 3 ~ 2,  # unentschieden
      as.numeric(aftjbyc) == 4 ~ 1,  # dafür
      as.numeric(aftjbyc) == 5 ~ 0   # sehr dafür
    ),
    trad_family = factor(trad_family,
                         levels = 0:4,
                         labels = c("sehr dafür", "dafür", "unentschieden",
                                    "dagegen", "sehr dagegen"))
  )

# -----------------------------------------------------------------------
# REKODIEREN DER UNABHÄNGIGEN VARIABLEN
# -----------------------------------------------------------------------

# mutate() innerhalb von |> verändert den bestehenden Datensatz
# %in% prüft ob ein Wert in einem Vektor enthalten ist (z.B. 1:2 = c(1, 2))
# trunc() schneidet Nachkommastellen ab (entspricht: trunc() in Stata)

df <- df |>
  mutate(
    # (0) Treatment: Geschlecht der zu beurteilenden fiktiven Person
    treatment = factor(case_when(
      as.numeric(icsbfm) == 2 ~ 0,  # Mann
      as.numeric(icsbfm) == 1 ~ 1   # Frau
    ), levels = c(0, 1), labels = c("Mann", "Frau")),

    # (1) Geschlecht der befragten Person
    geschlecht = factor(case_when(
      as.numeric(gndr) == 1 ~ 0,  # Mann
      as.numeric(gndr) == 2 ~ 1   # Frau
    ), levels = c(0, 1), labels = c("Mann", "Frau")),

    # (2) Altersgruppen ("/" bedeutet bis)
    alter    = trunc(as.numeric(age)),  # Kommastellen abschneiden (truncated)
    alter_gr = case_when(
      alter <= 29               ~ "15-29 Jahre",
      alter >= 30 & alter <= 44 ~ "30-44 Jahre",
      alter >= 45 & alter <= 59 ~ "45-59 Jahre",
      alter >= 60               ~ ">=60 Jahre"
    ),
    alter_gr = factor(alter_gr,
                      levels = c("15-29 Jahre", "30-44 Jahre",
                                 "45-59 Jahre", ">=60 Jahre")),

    # (3) Bildung
    bildung = case_when(
      as.numeric(edulvla) %in% 1:2 ~ 0,  # <= Sekundarstufe I
      as.numeric(edulvla) == 3     ~ 1,  # Sekundarstufe II
      as.numeric(edulvla) %in% 4:5 ~ 2   # post-sek., tertiär
    ),
    bildung = factor(bildung,
                     levels = 0:2,
                     labels = c("<= Sekundarstufe I", "Sekundarstufe II",
                                "post-sek., tertiär")),

    # (4) Stadt/Land
    urban = case_when(
      as.numeric(domicil) == 1     ~ 0,  # Gr. Stadt
      as.numeric(domicil) %in% 2:3 ~ 1,  # Vorstadt, kleine Stadt
      as.numeric(domicil) %in% 4:5 ~ 2   # Land
    ),
    urban = factor(urban,
                   levels = 0:2,
                   labels = c("Gr. Stadt", "Vorstadt, kleine Stadt", "Land")),

    # (5) Kinder
    child = factor(case_when(
      as.numeric(chldhm) == 1 ~ 1,  # Kind im Haushalt
      as.numeric(chldhm) == 2 ~ 0   # Kein Kind im Haushalt
    ), levels = c(0, 1), labels = c("Kein Kind im Haushalt", "Kind im Haushalt"))
  )

# Variablenlabels vergeben (entspricht: label variable x "..." in Stata)
# attr() setzt Metadaten (Attribute) einer Variable; "label" wird von
# modelsummary automatisch als Spaltenbeschriftung in Tabellen verwendet
attr(df$trad_family, "label") <- "Ablehnung von Vollzeitarbeit"
attr(df$treatment,   "label") <- "Geschlecht der beurteilten Person"
attr(df$geschlecht,  "label") <- "Geschlecht der befragten Person"
attr(df$alter_gr,    "label") <- "Alter"
attr(df$bildung,     "label") <- "Höchster Bildungsabschluss"
attr(df$urban,       "label") <- "Wohnortgröße"
attr(df$child,       "label") <- "Kind <18 im Haushalt"

# Überprüfen der Rekodierungen durch Kreuztabellen
# print(n = 20) zeigt bis zu 20 Zeilen an (Standard sind 10)
count(df, icsbfm, treatment)
count(df, gndr, geschlecht)
count(df, alter, alter_gr) |> print(n = 20)
count(df, edulvla, bildung)
count(df, domicil, urban)
count(df, chldhm, child)

# -----------------------------------------------------------------------
# ERSTE DESKRIPTIVE ANALYSEN
# -----------------------------------------------------------------------

# Verteilung der abhängigen Variable (in den Appendix)
# Wird fast immer angegeben als Hintergrundinformation
count(df, trad_family)

# Verteilung der unabhängigen Variablen (in den Methodenteil oder Anhang)
# map() wendet eine Funktion auf jede Spalte an und gibt eine Liste zurück
# ~ ist eine Kurzschreibweise für function(x), .x ist das jeweilige Argument
df |>
  select(geschlecht, alter_gr, bildung, urban, child) |>
  map(~ count(data.frame(x = .x), x))

# Kreuztabellen zwischen abhängiger und unabhängigen Variablen
# (auf Anomalien überprüfen auch hinsichtlich von missings)
# for-Schleife: führt den Block für jede Variable in der Liste aus
# .data[[var]] greift dynamisch auf eine Spalte per Name zu
# round() rundet auf die angegebene Anzahl an Dezimalstellen
for (var in c("geschlecht", "alter_gr", "bildung", "urban", "child")) {
  cat("\n--- trad_family x", var, "---\n")  # cat() gibt Text in der Konsole aus
  print(
    df |>
      count(trad_family, .data[[var]]) |>
      group_by(.data[[var]]) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      select(-n) |>
      pivot_wider(names_from = .data[[var]], values_from = pct)
  )
}

# -----------------------------------------------------------------------
# ANALYTISCHES SAMPLE ERSTELLEN
# -----------------------------------------------------------------------

nrow(df) # nrow() gibt die Anzahl der Zeilen (Beobachtungen) aus
         # 2405 Individuen haben Fragen des ESS in Österreich beantwortet

# Einschränkung der Personengruppe (hier nicht notwendig) Bsp:
# df <- df |> filter(mnactic != 6)  # Schließe Rentner aus der Analyse aus
# nrow(df)

# Listwise deletion: Exkludiere alle Fälle mit fehlenden Werten in Modellvariablen
# Achtung: vorher UNBEDINGT genau nachsehen wie fehlende Werte codiert sind
# Variablen icsbfm, geschlecht, alter_gr, urban haben keine fehlenden Werte
# ! kehrt eine logische Bedingung um (TRUE wird FALSE und umgekehrt)

df <- df |>
  filter(!is.na(trad_family), !is.na(bildung), !is.na(child))

nrow(df) # 2295 Individuen: vollständige Fälle in allen Modellvariablen
# Achten Sie darauf dass sie genügend Fälle - statistische Power -
# haben um Ihre Analysen durchführen zu können

# -----------------------------------------------------------------------
# DESKRIPTIVER ZUSAMMENHANG ZWISCHEN UNABHÄNGIGER UND ABHÄNGIGER VARIABLE
# -----------------------------------------------------------------------

# Einfache Tabelle mit Frequencies (entspricht: tab trad_family treatment, nof col)
df |>
  count(trad_family, treatment) |>
  group_by(treatment) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  select(-n) |>
  pivot_wider(names_from = treatment, values_from = pct)

# Gewichtete Tabelle (ACHTUNG: Gewichtung beachten insbesondere bei deskriptiven Resultaten)
# as_survey_design() definiert das Stichprobendesign mit Gewichtungsvariable
# survey_mean() berechnet gewichtete Mittelwerte inkl. Konfidenzintervall (vartype = "ci")
df_svy <- df |>
  as_survey_design(weights = pspwght)

df_svy |>
  group_by(trad_family, treatment) |>
  summarise(pct = survey_mean(vartype = "ci") * 100) |>
  select(-c(pct_low, pct_upp)) |>
  pivot_wider(names_from = treatment, values_from = pct)

# Balkendiagramm: Kategorien nebeneinander, Farbe nach Geschlecht der beurteilten Person
# Vollzeitarbeit von Frauen bei Präsenz eines Kleinkindes wird im Schnitt viel
# stärker abgelehnt als bei Männern.

plotdata <- df |>
  count(trad_family, treatment) |>
  group_by(treatment) |>
  mutate(pct = n / sum(n) * 100)

# ggplot() öffnet eine neue Grafik; aes() definiert welche Variablen auf welche
# Achsen bzw. Eigenschaften (Farbe, Form) gemappt werden
# geom_col() zeichnet Balken; position_dodge() stellt Balken nebeneinander
# scale_fill_manual() weist Farben manuell zu
# labs() setzt Titel, Achsenbeschriftungen und Legendentitel
# theme_minimal() verwendet ein übersichtliches Layout ohne Hintergrundfarbe
# str_wrap() bricht langen Text auf mehrere Zeilen um (Zeilenumbrüche X-Achse)
ggplot(data = plotdata,
       aes(x = trad_family, y = pct, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80)) +
  scale_fill_manual(values = c("Mann" = "#4472C4", "Frau" = "#ED7D31")) +
  labs(
    title = "Zustimmung/Ablehnung von Vollzeitarbeit\nnach Geschlecht der beurteilten Person",
    x     = NULL,
    y     = "Prozent (%)",
    fill  = "Beurteilte Person"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank())  # vertikale Gitterlinien entfernen

# ggsave() speichert die zuletzt erstellte Grafik in eine Datei
ggsave(file.path(figures, "balkendiagramm_treatment_R.png"),
       width = 8.5, height = 5, dpi = 300)

# -----------------------------------------------------------------------
# LINEARE REGRESSION (OLS)
# -----------------------------------------------------------------------

# Für die Regressionen: trad_family als numerisch (0-4)
# factor-Variablen können nicht direkt als abhängige Variable in lm() verwendet werden
# -1 weil as.numeric() bei factor-Variablen mit 1 zu zählen beginnt
df <- df |>
  mutate(trad_family_num = as.numeric(trad_family) - 1)

# Im Idealfall benutzen Sie hier auch die Gewichtungsvariable:
# lm(trad_family_num ~ treatment, data = df, weights = pspwght)
# der Einfachheit halber hier in allen Modellen ohne Gewichtung

# lm() schätzt eine lineare Regression (OLS); Formelschreibweise: y ~ x1 + x2
# factor()-Variablen werden automatisch als Dummy-Variablen behandelt
# die erste Kategorie ist automatisch die Referenzkategorie

# Modell 1: Nur Treatment (bivariat)
m1 <- lm(trad_family_num ~ treatment, data = df)

# Modell 2: + Kontrollvariablen
m2 <- lm(trad_family_num ~ treatment + child + geschlecht + alter_gr + bildung + urban,
         data = df)

# Modell 3: + Interaktionseffekt Treatment x Kind im Haushalt (H2)
# * erzeugt automatisch Haupteffekte UND Interaktionsterm (entspricht: ## in Stata)
m3 <- lm(trad_family_num ~ treatment * child + geschlecht + alter_gr + bildung + urban,
         data = df)

# -----------------------------------------------------------------------
# REGRESSIONSTABELLE EXPORTIEREN (modelsummary)
# -----------------------------------------------------------------------

# Der bereits in der Deskription gefundene Unterschied ist auch statistisch
# signifikant. Ebenso zeigt sich ein statistisch signifikanter Moderations-
# effekt des Treatments je nachdem ob im Haushalt der befragten Person ein Kind
# wohnt.

# modelsummary() erstellt eine formatierte Regressionstabelle
# stars = Signifikanzniveaus für Sterne; gof_map = anzuzeigende Modellstatistiken
# coef_rename = TRUE verwendet die Variablenlabels aus attr(..., "label")
# Ausgabe in der Konsole zur Kontrolle
modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2, "Modell 3" = m3),
  stars       = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = TRUE,
  title       = "Lineare Regression: Ablehnung von Vollzeitarbeit bei Kleinkind im Haushalt"
)

# Export als Word-Dokument (.docx) in den Output/tab Ordner
modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2, "Modell 3" = m3),
  stars       = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = TRUE,
  title       = "Lineare Regression: Ablehnung von Vollzeitarbeit bei Kleinkind im Haushalt",
  output      = file.path(tables, "regressionen1_R.docx")
)

# -----------------------------------------------------------------------
# INTERAKTIONSEFFEKT: AME des Treatments nach Kindstatus (H2)
# -----------------------------------------------------------------------

# Zur einfacheren Interpretation von Interaktionseffekten sollten hier immer
# Average Marginal Effects berechnet werden: Zeigt wie stark ist der Treatment-
# Effekt je nach Zustand der Moderatorvariable.

# avg_slopes() berechnet durchschnittliche marginale Effekte (AME)
# variables = welche Variable, by = getrennt nach welcher Gruppenvariable
# (entspricht: margins child, dydx(treatment) in Stata)
ame_m3 <- avg_slopes(m3,
                     variables = "treatment",
                     by        = "child")
print(ame_m3)

# Grafische Darstellung der AMEs mit Konfidenzintervallen
# geom_hline() zeichnet eine horizontale Linie (hier: Nulllinie als Referenz)
# geom_pointrange() zeichnet Punkt mit vertikalem Konfidenzintervall
# conf.low / conf.high: untere/obere Grenze des 95%-KI aus avg_slopes()
ame_m3 |>
  ggplot(aes(x = child, y = estimate,
             ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(size = 0.8) +
  scale_x_discrete(labels = c("Kein Kind", "Kind vorhanden")) +
  labs(
    title = "AME des Treatments nach Kind im Haushalt (H2)",
    y     = "Marginaler Effekt von Treatment\n(Frau vs. Mann)",
    x     = NULL
  ) +
  theme_minimal(base_size = 14)

ggsave(file.path(figures, "ame_treatment_nach_kind_R.png"),
       width = 6, height = 5, dpi = 300)

# -----------------------------------------------------------------------
# VERGLEICH ZWISCHEN MODELLEN FÜR VERSCHIEDENE GRUPPEN (AUCH: LÄNDER)
# -----------------------------------------------------------------------

# Beispiel: Unterscheidet sich der Kind-Effekt nach Geschlecht der befragten Person?
# Hier eher besser einen Interaktionseffekt zu schätzen aber manchmal will man
# alle unabhängigen Faktoren unabhängig von einem möglichen Moderator schätzen –
# dann macht ein Split-Sample-Ansatz Sinn.

# Gleiche Kontrollvariablen, aber getrennte Schätzung nach Geschlecht
# df |> filter(...) filtert den Datensatz direkt in der lm()-Funktion
m_men   <- lm(trad_family_num ~ treatment + child + alter_gr + bildung + urban,
              data = df |> filter(geschlecht == "Mann"))
m_women <- lm(trad_family_num ~ treatment + child + alter_gr + bildung + urban,
              data = df |> filter(geschlecht == "Frau"))

# Koeffizienten nebeneinander: Unterschiede visuell beurteilen
modelsummary(
  list("Männer" = m_men, "Frauen" = m_women),
  stars   = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "r.squared")
)

# Formaler Test auf Gruppenunterschiede via hypotheses() aus marginaleffects
# suest in Stata schätzt Modelle getrennt, stapelt dann die Koeffizientenvektoren
# und berechnet eine Sandwich-Kovarianzmatrix. Bei vollständig unabhängigen
# Stichproben (Männer ≠ Frauen) ist diese Sandwich-Matrix block-diagonal und
# damit äquivalent zum gepoolten Interaktionsmodell mit OLS-Standardfehlern.
# -> gepooltes Modell mit * ist die korrekte R-Entsprechung zu suest.

m_pooled <- lm(trad_family_num ~ (treatment + child + alter_gr + bildung + urban) * geschlecht,
               data = df)

# hypotheses() testet lineare Hypothesen über Modellkoeffizienten
# joint = TRUE führt einen gemeinsamen Chi²-Test aller angegebenen Terme durch
# grep() findet alle Koeffizientennamen die ":geschlecht" enthalten
# (entspricht: test [men_mean = women_mean] in Stata)
interaction_terms <- grep(":geschlecht", names(coef(m_pooled)), value = TRUE)
hypotheses(m_pooled, joint = interaction_terms)

# Test ob der child-Koeffizient sich signifikant zwischen den Gruppen unterscheidet
# Der Interaktionsterm ist direkt die Differenz der Koeffizienten zwischen Gruppen
# (entspricht: test [men_mean]1.child = [women_mean]1.child in Stata)
hypotheses(m_pooled, "`childKind im Haushalt:geschlechtFrau` = 0")

# -----------------------------------------------------------------------
# LOGISTISCHE REGRESSION für binäre (0-1) abhängige Variable
# insbesondere bei sehr ungleich verteilten Gruppen
# -----------------------------------------------------------------------

# Abhängige Variable wird binär rekodiert in (sehr) dagegen vs. alle anderen
df <- df |>
  mutate(
    trad_family_bin = case_when(
      as.numeric(aftjbyc) %in% 1:2 ~ 1,  # dagegen / sehr dagegen
      as.numeric(aftjbyc) %in% 3:5 ~ 0   # alle anderen
    )
  )

# glm() schätzt verallgemeinerte lineare Modelle
# family = binomial(link = "logit") spezifiziert die logistische Regression
# Koeffizienten sind auf der Log-Odds-Skala
logit_b <- glm(trad_family_bin ~ treatment + child + geschlecht + alter_gr + bildung + urban,
               data   = df,
               family = binomial(link = "logit"))

# Average Marginal Effects der logistischen Regression
# (entspricht: margins, dydx(*) post in Stata)
# avg_slopes() aus marginaleffects wäre optimal, ist aber nicht direkt mit
# modelsummary kombinierbar -> margins::margins() liefert identische Ergebnisse
ame_logit <- margins::margins(logit_b)

# Linear Probability Model (LPM) zum Vergleich
# Lineare Regression mit binärer abhängiger Variable; Koeffizienten sind direkt
# als Wahrscheinlichkeitsunterschiede interpretierbar
lpm <- lm(trad_family_bin ~ treatment + child + geschlecht + alter_gr + bildung + urban,
          data = df)

# Resultate zeigen beinahe identische Koeffizienten im Vergleich: Marginale
# Effekte einer logistischen Regression und Koeffizienten eines Linear
# Probability Models
# exponentiate = TRUE rechnet Log-Odds in Odds Ratios um (für Logit OR Spalte)
modelsummary(
  list("Logit (b)"  = logit_b,
       "Logit (OR)" = logit_b,
       "AME"        = ame_logit,
       "LPM"        = lpm),
  exponentiate = c(FALSE, TRUE, FALSE, FALSE),
  stars        = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map      = c("nobs", "r.squared", "r2.nagelkerke")
)

modelsummary(
  list("Logit (b)"  = logit_b,
       "Logit (OR)" = logit_b,
       "AME"        = ame_logit,
       "LPM"        = lpm),
  exponentiate = c(FALSE, TRUE, FALSE, FALSE),
  stars        = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map      = c("nobs", "r.squared", "r2.nagelkerke"),
  output       = file.path(tables, "regressionen2_R.docx")
)
