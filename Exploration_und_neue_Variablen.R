
##Neue Variblen erstellen:

#Summe für SCI Coping Methods erstellen ohne alk Variable:
Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <- 
  Projekt_Praxis$SCI_positiv_sum + Projekt_Praxis$SCI_aktiv_sum + 
  Projekt_Praxis$SCI_support_sum + Projekt_Praxis$SCI_glaube_sum

sort(sapply(Projekt_Praxis[, 112, drop = FALSE], unique))
#Diese Varible hat einen Skalenbereich von 16-64


#Summe Präpartale Ängste:
Projekt_Praxis$präpartal_Ängste_sum <- Projekt_Praxis$präpartal_Ängste_1 + 
  Projekt_Praxis$präpartal_Ängste_2 +  Projekt_Praxis$präpartal_Ängste_3 +  
  Projekt_Praxis$präpartal_Ängste_4 +  Projekt_Praxis$präpartal_Ängste_5 +  
  Projekt_Praxis$präpartal_Ängste_6 +  Projekt_Praxis$präpartal_Ängste_7 + 
  Projekt_Praxis$präpartal_Ängste_8 +  Projekt_Praxis$präpartal_Ängste_9 + 
  Projekt_Praxis$präpartal_Ängste_10 +  Projekt_Praxis$präpartal_Ängste_11 + 
  Projekt_Praxis$präpartal_Ängste_12

sort(sapply(Projekt_Praxis[, 113, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von 12-48


#Summe Postpartales Befinden (Schmerzen werden abgezogen):
Projekt_Praxis$postpartal_3d_Befinden_sum <- Projekt_Praxis$postpartal_3d_Befinden_1 + 
  Projekt_Praxis$postpartal_3d_Gesundheitszustand_1 + Projekt_Praxis$postpartal_3d_Gesundheitszustand_2 + 
  Projekt_Praxis$postpartal_3d_Gesundheitszustand_3 - Projekt_Praxis$postpartal_3d_Schmerzen_1

sort(sapply(Projekt_Praxis[, 114, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von 0-15

#BSF vor der Geburt (Positive Kategorien werden Substrahiert(Engagement, gehobene Stimmung), negative Addiert(Müdigkeit, Ängstliche Depressivität, Ärger, Teilnahmelosigkeit); D.h. eine hohe Summe bedeutet eine negative/unausgeglichene Stimmung):
Projekt_Praxis$BSF_präpartal_sumall <- - Projekt_Praxis$BSF_präpartal_Engagement_sumof2 - 
  Projekt_Praxis$BSF_präpartal_GehobeneStimmung_sumof2 + Projekt_Praxis$BSF_präpartal_Müdigkeit_sumof2 + 
  Projekt_Praxis$BSF_präpartal_Ängstlichkeit_sumof4 + Projekt_Praxis$BSF_präpartal_Ärger_sumof2 + 
  Projekt_Praxis$BSF_präpartal_Teilnahmslosigkeit_sumof2

sort(sapply(Projekt_Praxis[, 115, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von (-10)-46

#BSF nach der Geburt (Positive Kategorien werden Substrahiert(Engagement, gehobene Stimmung), negative Addiert(Müdigkeit, Ängstliche Depressivität, Ärger, Teilnahmelosigkeit); D.h. eine hohe Summe bedeutet eine negative/unausgeglichene Stimmung):
Projekt_Praxis$BSF_postpartal_sumall <- - Projekt_Praxis$BSF_postpartal_3d_Engagement_sumof2_A - 
  Projekt_Praxis$BSF_postpartal_3d_GehobeneStimmung_sumof2_A + Projekt_Praxis$BSF_postpartal_3d_Müdigkeit_sumof2_A + 
  Projekt_Praxis$BSF_postpartal_3d_Ängstlichkeit_sumof4_A + Projekt_Praxis$BSF_postpartal_3d_Ärger_sumof2_A + 
  Projekt_Praxis$BSF_postpartal_3d_Teilnahmslosigkeit_sumof2_A

sort(sapply(Projekt_Praxis[, 116, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von (-10)-46

#Covid Belastungs Summen:

#Präpartal (während der Schwangerschaft):
Projekt_Praxis$covid_belastungss_sum <- Projekt_Praxis$covid_belastungss + 
  Projekt_Praxis$covid_ängstess_ansteckung_kind + Projekt_Praxis$covid_ängstess_ansteckung_sie + 
  Projekt_Praxis$covid_ängstess_zeit_pp_hebammen + Projekt_Praxis$covid_ängstess_trennung_Fam_nach + 
  Projekt_Praxis$covid_ängstess_freizeit + Projekt_Praxis$covid_ängstess_kontakt_freunde
sort(sapply(Projekt_Praxis[, 117, drop = FALSE], unique))
#Skalenbereich von 7-28

#Postpartal (kurz nach der Geburt):
Projekt_Praxis$covid_belastunggeburt_sum <- Projekt_Praxis$covid_belastunggeburt + 
  Projekt_Praxis$covid_ängstegeburt_ansteckung_kind + Projekt_Praxis$covid_ängstegeburt_ansteckung_sie + 
  Projekt_Praxis$covid_ängstegeburt_zeit_nach + Projekt_Praxis$covid_ängstegeburt_trennung_fam_nach + 
  Projekt_Praxis$covid_ängstegeburt_freizeit + Projekt_Praxis$covid_ängstegeburt_kontakt_freunde
sort(sapply(Projekt_Praxis[, 118, drop = FALSE], unique))
#Skalenbereich von 7-28

#Wochenbett: 
Projekt_Praxis$covid_belastungwochenbett_sum <- Projekt_Praxis$covid_belastungwochenbett + 
  Projekt_Praxis$covid_ängstewochenbett_ansteckung_kind + Projekt_Praxis$covid_ängstewochenbett_ansteckung_sie + 
  Projekt_Praxis$covid_ängstewochenbett_unterstützung + Projekt_Praxis$covid_ängstewochenbett_trennung_fam_nach + 
  Projekt_Praxis$covid_ängstewochenbett_freizeit + Projekt_Praxis$covid_ängstewochenbett_kontakt_freunde
sort(sapply(Projekt_Praxis[, 119, drop = FALSE], unique))
#Skalenbereich von 7-28

#2 Monate:
Projekt_Praxis$covid_belastung2m_sum <- Projekt_Praxis$covid_belastung2m + 
  Projekt_Praxis$covid_ängste2m_ansteckung_kind + Projekt_Praxis$covid_ängste2m_ansteckung_sie + 
  Projekt_Praxis$covid_ängste2m_unterstützung + Projekt_Praxis$covid_ängste2m_trennung_fam_nach + 
  Projekt_Praxis$covid_ängste2m_freizeit + Projekt_Praxis$covid_ängste2m_kontakt_freunde
sort(sapply(Projekt_Praxis[, 120, drop = FALSE], unique))
#Skalenbereich von 7-28


#2-6 Monate:
Projekt_Praxis$covid_belastung2bis6m_sum <- Projekt_Praxis$covid_belastung2bis6m + 
  Projekt_Praxis$covidängste2bis6m_ansteckung_kind + Projekt_Praxis$covidängste2bis6m_ansteckung_sie + 
  Projekt_Praxis$covidängste2bis6m_unterstützung + Projekt_Praxis$covidängste2bis6m_trennung_fam_nach + 
  Projekt_Praxis$covidängste2bis6m_freizeit + Projekt_Praxis$covidängste2bis6m_kontakt_freunde
sort(sapply(Projekt_Praxis[, 121, drop = FALSE], unique))
#Skalenbereich von 7-28

#6 Monate:
Projekt_Praxis$covid_belastung6m_sum <- Projekt_Praxis$covid_belastung6m + 
  Projekt_Praxis$covidängste6m_ansteckung_kind + Projekt_Praxis$covidängste6m_ansteckung_sie + 
  Projekt_Praxis$covidängste6m_unterstützung + Projekt_Praxis$covidängste6m_trennung_fam_nach + 
  Projekt_Praxis$covidängste6m_freizeit + Projekt_Praxis$covidängste6m_kontakt_freunde
sort(sapply(Projekt_Praxis[, 122, drop = FALSE], unique))
#Skalenbereich von 7-28

#Korrektion falsch benannter Variablen:
names(Projekt_Praxis)[names(Projekt_Praxis) == "EDPS_in_Kategorien_postpartal_1m"] <- "EPDS_in_Kategorien_postpartal_1m"
names(Projekt_Praxis)[names(Projekt_Praxis) == "EDPS_in_Kategorien_postpartal_2m"] <- "EPDS_in_Kategorien_postpartal_2m"

#Teildatensätze nocheinmal neu erzeugen, damit die neuen Variablen enthalten sind:
daten_pro <- Projekt_Praxis[grepl('Pat', Projekt_Praxis$Patientennummer),]
daten_retro <- Projekt_Praxis[grepl('Retro', Projekt_Praxis$Patientennummer),]

write.csv(daten_pro,"daten_pro.csv", row.names = TRUE)
write.csv(daten_retro,"daten_retro.csv", row.names = TRUE)

##Daten Exploration: interesannte Variablen plotten

library("ggplot2")

##interessante Variblen Fragestellung 1 (Besteht ein Zusammenhang zwischen dem Auftreten einer postpartalen Depression und den gemessenen Belastungen durch die COVID-19-Pandemie?):
#Die interessanten Variablen hier sind die Covid Belastung zu allen Zeitpunkten und EPDS zu allen Zeitpunkten.

#Covid Belastung(ich visualisiere hier die von mir oben erstellten Summen):

#Präpartal:
hist_covid_präpartal <- ggplot(data = Projekt_Praxis, mapping = aes(x = covid_belastungss_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(0, 28) +
  ylim(0, 35) +
  labs(title = "Histogramm: Covid Belastung Präpartal", 
       x = "Covid Belastung (Summe aus mehreren Variablen)", y = "Anzahl")
hist_covid_präpartal

#Postpartal (kurz nach der Geburt):
hist_covid_postpartal <- ggplot(data = Projekt_Praxis, mapping = aes(x = covid_belastunggeburt_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(0, 28) +
  ylim(0, 35) +
  labs(title = "Histogramm: Covid Belastung Postpartal", 
       x = "Covid Belastung (Summe aus mehreren Variablen)", y = "Anzahl")
hist_covid_postpartal

#Wochenbett: 
hist_covid_wochenbett <- ggplot(data = Projekt_Praxis, mapping = aes(x = covid_belastungwochenbett_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(0, 28) +
  ylim(0, 35) +
  labs(title = "Histogramm: Covid Belastung Wochenbett", 
       x = "Covid Belastung (Summe aus mehreren Variablen)", y = "Anzahl")
hist_covid_wochenbett

#2 Monate:
hist_covid_2m <- ggplot(data = Projekt_Praxis, mapping = aes(x = covid_belastung2m_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(0, 28) +
  ylim(0, 35) +
  labs(title = "Histogramm: Covid Belastung 2 Monate", 
       x = "Covid Belastung (Summe aus mehreren Variablen)", y = "Anzahl")
hist_covid_2m

#2-6 Monate:
hist_covid_2bis6m <- ggplot(data = Projekt_Praxis, mapping = aes(x = covid_belastung2bis6m_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(0, 28) +
  ylim(0, 35) +
  labs(title = "Histogramm: Covid Belastung 2-6 Monate", 
       x = "Covid Belastung (Summe aus mehreren Variablen)", y = "Anzahl")
hist_covid_2bis6m

#6 Monate:
hist_covid_6m <- ggplot(data = Projekt_Praxis, mapping = aes(x = covid_belastung6m_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(0, 28) +
  ylim(0, 35) +
  labs(title = "6 Monate", 
       x = "Covid Belastung (Summe aus mehreren Variablen)", y = "Anzahl")
hist_covid_6m

#EPDS:

#Präpartal
barplot_EPDS_präpartal <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_sum_präpartal)) + 
  geom_bar() +
  xlim(-0.5, 25) +
  ylim(0, 15) +
  labs(title = "Häufigkeiten: EPDS Score Präpartal", 
       x = "EPDS Score", y = "Anzahl")
barplot_EPDS_präpartal

barplot_EPDS_präpartal_kategorien <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_in_Kategorien_präpartal)) + 
  geom_bar() +
  xlim(0.5, 3.5) +
  ylim(0, 85) +
  labs(title = "Häufigkeiten: EPDS Kategorien Präpartal", 
       x = "EPDS Kategorie", y = "Anzahl")
barplot_EPDS_präpartal_kategorien

#Postpartal (1 Monat nach der Geburt):
barplot_EPDS_1m <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_sum_postpartal_1m)) + 
  geom_bar() +
  xlim(-0.5, 25) +
  ylim(0, 15) +
  labs(title = "Häufigkeiten: EPDS Score Postpartal", 
       x = "EPDS Score", y = "Anzahl")
barplot_EPDS_1m

barplot_EPDS_1m_kategorien <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_in_Kategorien_postpartal_1m)) + 
  geom_bar() +
  xlim(0.5, 3.5) +
  ylim(0, 85) +
  labs(title = "Häufigkeiten: EPDS Kategorien Postpartal", 
       x = "EPDS Kategorie", y = "Anzahl")
barplot_EPDS_1m_kategorien

#Postpartal (2 Monat nach der Geburt):
barplot_EPDS_2m <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_sum_postpartal_2m)) + 
  geom_bar() +
  xlim(-0.5, 25) +
  ylim(0, 15) +
  labs(title = "Häufigkeiten: EPDS Score nach 2 Monaten", 
       x = "EPDS Score", y = "Anzahl")
barplot_EPDS_2m

barplot_EPDS_2m_kategorien <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_in_Kategorien_postpartal_2m)) + 
  geom_bar() +
  xlim(0.5, 3.5) +
  ylim(0, 85) +
  labs(title = "Häufigkeiten: EPDS Kategorien nach 2 Monaten", 
       x = "EPDS Kategorie", y = "Anzahl")
barplot_EPDS_2m_kategorien

#Postpartal (6 Monat nach der Geburt):
barplot_EPDS_6m <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_sum_postpartal_6m)) + 
  geom_bar() +
  xlim(-0.5, 25) +
  ylim(0, 15) +
  labs(title = "Häufigkeiten: EPDS Score nach 6 Monaten", 
       x = "EPDS Score", y = "Anzahl")
barplot_EPDS_6m

barplot_EPDS_6m_kategorien <- ggplot(data = Projekt_Praxis, mapping = aes(x = EPDS_in_Kategorien_postpartal_6m)) + 
  geom_bar() +
  xlim(0.5, 3.5) +
  ylim(0, 85) +
  labs(title = "Häufigkeiten: EPDS Kategorien nach 6 Monaten", 
       x = "EPDS Kategorie", y = "Anzahl")
barplot_EPDS_6m_kategorien

##interessante Variblen Fragestellung 2 (Stellt der SCI-Fragebogen ein suffizientes Instrument dar, um Schwangere vor der Geburt auf das Auftreten von postnatalen Wochenbettdepressionen zu screenen?):
#Die interessanten Variablen hier sind die SCI Variablen (Stressbelastung und Coping Methoden) und EPDS zu allen Zeitpunkten (EPDS wurde oben schon einmal Visualisiert).

#SCI:
#Stressbelastung:
hist_SCI_Stressbelastung <- ggplot(data = Projekt_Praxis, mapping = aes(x = Aktuelle_Stressbelastung_sum)) + 
  geom_histogram(binwidth = 4) +
  labs(title = "Histogramm: SCI - Aktuelle Stressbelastung", 
       x = "Aktuelle Stressbelastung (Summe)", y = "Anzahl")
hist_SCI_Stressbelastung

#Coping Methoden:
hist_SCI_Coping <- ggplot(data = Projekt_Praxis, mapping = aes(x = Adaptive_Stressbewältigung_sum_new)) + 
  geom_histogram(binwidth = 4) +
  labs(title = "Histogramm: SCI - Stressbewältigungs-Summe", 
       x = "Stressbewältigungs-Summe", y = "Anzahl")
hist_SCI_Coping

#SCI-Profile:
barplot_SCI_Profile <- ggplot(data = Projekt_Praxis, mapping = aes(x = SCI_Profile)) + 
  geom_bar() +
  ylim(0, 35) +
  labs(title = "Häufigkeiten - SCI-Profile", 
       x = "SCI-Profil", y = "Anzahl")
barplot_SCI_Profile

###interessante Variblen Fragestellung 3 (Besteht ein Zusammenhang zwischen dem Befinden und der Stimmung der Patientinnen vor und nach der Geburt (zum Beispiel Ängste vor der Geburt, Befinden nach der Geburt, Stimmungslage) und postnatalen depressiven Symptomen?):
#Die interessanten Variablen hier sind die Variablen zu Präpartale Angst, Befinden nach der Geburt, BSF vor und nach der Geburt und EPDS zu allen Zeitpunkten (EPDS wurde oben schon einmal Visualisiert).

#Präpartale Ängste:
hist_präpartal_Ängste <- ggplot(data = Projekt_Praxis, mapping = aes(x = präpartal_Ängste_sum)) + 
  geom_histogram(binwidth = 2) +
  xlim(11.5, 48) +
  ylim(0, 15) +
  labs(title = "Histogramm: Präpartale Ängste Summe", 
       x = "Präpartale Ängste Summe", y = "Anzahl")
hist_präpartal_Ängste

#Befinden nach der Geburt:
hist_postpartal_Befinden <- ggplot(data = Projekt_Praxis, mapping = aes(x = postpartal_3d_Befinden_sum)) + 
  geom_histogram(binwidth = 1) +
  xlim(-0.5, 15.5) +
  ylim(0, 20) +
  labs(title = "Histogramm: Befinden nach der Geburt Summe", 
       x = "Befinden nach der Geburt Summe", y = "Anzahl")
hist_postpartal_Befinden

#BSF vor der Geburt:
hist_BSF_präpartal <- ggplot(data = Projekt_Praxis, mapping = aes(x = BSF_präpartal_sumall)) + 
  geom_histogram(binwidth = 2) +
  xlim(-11.5, 30) +
  ylim(0, 15) +
  labs(title = "Histogramm: BSF präpartal Summe", 
       x = "BSF präpartal Summe", y = "Anzahl")
hist_BSF_präpartal

#BSF nach der Geburt:
hist_BSF_postpartal <- ggplot(data = Projekt_Praxis, mapping = aes(x = BSF_postpartal_sumall)) + 
  geom_histogram(binwidth = 2) +
  xlim(-11.5, 30) +
  ylim(0, 15) +
  labs(title = "Histogramm: BSF postpartal Summe", 
       x = "BSF postpartal Summe", y = "Anzahl")
hist_BSF_postpartal
