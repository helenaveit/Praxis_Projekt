
##Neue Variblen erstellen:

#Summe für SCI Coping Methods erstellen ohne alk Variable:
Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <- 
  Projekt_Praxis$SCI_positiv_sum + Projekt_Praxis$SCI_aktiv_sum + Projekt_Praxis$SCI_support_sum + Projekt_Praxis$SCI_glaube_sum

sort(sapply(Projekt_Praxis[, 112, drop = FALSE], unique))
#Diese Varible hat einen Skalenbereich von 16-64


#Summe Präpartale Ängste:
Projekt_Praxis$präpartal_Ängste_sum <- Projekt_Praxis$präpartal_Ängste_1 + Projekt_Praxis$präpartal_Ängste_2 +  Projekt_Praxis$präpartal_Ängste_3 +  Projekt_Praxis$präpartal_Ängste_4 +  Projekt_Praxis$präpartal_Ängste_5 +  Projekt_Praxis$präpartal_Ängste_6 +  Projekt_Praxis$präpartal_Ängste_7 + Projekt_Praxis$präpartal_Ängste_8 +  Projekt_Praxis$präpartal_Ängste_9 + Projekt_Praxis$präpartal_Ängste_10 +  Projekt_Praxis$präpartal_Ängste_11 + Projekt_Praxis$präpartal_Ängste_12

sort(sapply(Projekt_Praxis[, 113, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von 12-48


#Summe Postpartales Befinden (Schmerzen werden abgezogen):
Projekt_Praxis$postpartal_3d_Befinden_sum <- Projekt_Praxis$postpartal_3d_Befinden_1 + Projekt_Praxis$postpartal_3d_Gesundheitszustand_1 + Projekt_Praxis$postpartal_3d_Gesundheitszustand_2 + Projekt_Praxis$postpartal_3d_Gesundheitszustand_3 - Projekt_Praxis$postpartal_3d_Schmerzen_1

sort(sapply(Projekt_Praxis[, 114, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von 0-15


#Covid Belastungs Summen:

#Präpartal (während der Schwangerschaft):
Projekt_Praxis$covid_belastungss_sum <- Projekt_Praxis$covid_belastungss + 
  Projekt_Praxis$covid_ängstess_ansteckung_kind + Projekt_Praxis$covid_ängstess_ansteckung_sie + 
  Projekt_Praxis$covid_ängstess_zeit_pp_hebammen + Projekt_Praxis$covid_ängstess_trennung_Fam_nach + 
  Projekt_Praxis$covid_ängstess_freizeit + Projekt_Praxis$covid_ängstess_kontakt_freunde
sort(sapply(Projekt_Praxis[, 115, drop = FALSE], unique))
#Skalenbereich von 7-28

#Postpartal (kurz nach der Geburt):
Projekt_Praxis$covid_belastunggeburt_sum <- Projekt_Praxis$covid_belastunggeburt + 
  Projekt_Praxis$covid_ängstegeburt_ansteckung_kind + Projekt_Praxis$covid_ängstegeburt_ansteckung_sie + 
  Projekt_Praxis$covid_ängstegeburt_zeit_nach + Projekt_Praxis$covid_ängstegeburt_trennung_fam_nach + 
  Projekt_Praxis$covid_ängstegeburt_freizeit + Projekt_Praxis$covid_ängstegeburt_kontakt_freunde
sort(sapply(Projekt_Praxis[, 116, drop = FALSE], unique))
#Skalenbereich von 7-28

#Wochenbett: 
Projekt_Praxis$covid_belastungwochenbett_sum <- Projekt_Praxis$covid_belastungwochenbett + 
  Projekt_Praxis$covid_ängstewochenbett_ansteckung_kind + Projekt_Praxis$covid_ängstewochenbett_ansteckung_sie + 
  Projekt_Praxis$covid_ängstewochenbett_unterstützung + Projekt_Praxis$covid_ängstewochenbett_trennung_fam_nach + 
  Projekt_Praxis$covid_ängstewochenbett_freizeit + Projekt_Praxis$covid_ängstewochenbett_kontakt_freunde
sort(sapply(Projekt_Praxis[, 117, drop = FALSE], unique))
#Skalenbereich von 7-28

#2 Monate:
Projekt_Praxis$covid_belastung2m_sum <- Projekt_Praxis$covid_belastung2m + 
  Projekt_Praxis$covid_ängste2m_ansteckung_kind + Projekt_Praxis$covid_ängste2m_ansteckung_sie + 
  Projekt_Praxis$covid_ängste2m_unterstützung + Projekt_Praxis$covid_ängste2m_trennung_fam_nach + 
  Projekt_Praxis$covid_ängste2m_freizeit + Projekt_Praxis$covid_ängste2m_kontakt_freunde
sort(sapply(Projekt_Praxis[, 118, drop = FALSE], unique))
#Skalenbereich von 7-28


#2-4 Monate:
Projekt_Praxis$covid_belastung2bis6m_sum <- Projekt_Praxis$covid_belastung2bis6m + 
  Projekt_Praxis$covidängste2bis6m_ansteckung_kind + Projekt_Praxis$covidängste2bis6m_ansteckung_sie + 
  Projekt_Praxis$covidängste2bis6m_unterstützung + Projekt_Praxis$covidängste2bis6m_trennung_fam_nach + 
  Projekt_Praxis$covidängste2bis6m_freizeit + Projekt_Praxis$covidängste2bis6m_kontakt_freunde
sort(sapply(Projekt_Praxis[, 119, drop = FALSE], unique))
#Skalenbereich von 7-28

#6 Monate:
Projekt_Praxis$covid_belastung6m_sum <- Projekt_Praxis$covid_belastung6m + 
  Projekt_Praxis$covidängste6m_ansteckung_kind + Projekt_Praxis$covidängste6m_ansteckung_sie + 
  Projekt_Praxis$covidängste6m_unterstützung + Projekt_Praxis$covidängste6m_trennung_fam_nach + 
  Projekt_Praxis$covidängste6m_freizeit + Projekt_Praxis$covidängste6m_kontakt_freunde
sort(sapply(Projekt_Praxis[, 120, drop = FALSE], unique))
#Skalenbereich von 7-28

#Teildatensätze nocheinmal neu erzeugen, damit die neuen Variablen enthalten sind:
daten_pro <- Projekt_Praxis[grepl('Pat', Projekt_Praxis$Patientennummer),]
daten_retro <- Projekt_Praxis[grepl('Retro', Projekt_Praxis$Patientennummer),]

write.csv(daten_pro,"daten_pro.csv", row.names = TRUE)
write.csv(daten_retro,"daten_retro.csv", row.names = TRUE)

##Daten Exploration: interesannte Variablen plotten

library("ggplot2")

#interessante Variblen Fragestellung 1 (Besteht ein Zusammenhang zwischen dem Auftreten einer postpartalen Depression und den gemessenen Belastungen durch die COVID-19-Pandemie?):

#

