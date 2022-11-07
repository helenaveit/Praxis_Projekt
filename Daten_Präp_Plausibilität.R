#Datensatz laden:
library(readr)
Projekt_Praxis <- read_sav("Projekt Praxis.sav")

#Daten aufbereiten:
is.data.frame(Projekt_Praxis)
#Datensatz ist als dataframe gespeichert

#Plausibilitäts-Check:
#Im folgenden checke ich alle Variablen auf die auftretenden Ausprägungen:
sapply(sapply(Projekt_Praxis, unique), sort, na.last=TRUE)

#Dann verschaffe ich mich einen Überblick über die Verteilung der Daten:
summary(Projekt_Praxis)
ls.str(Projekt_Praxis)

#Im folgenden schaue ich, wieviel 'NA' Werte in den verschiedenen Variablen auftreten:
colSums(is.na(Projekt_Praxis))
#Bei den Retrospektiv Befragten fehlen die Werte für alle Variablem im 
# Zusammenhang mit 'Präpartalen Ängsten','Postpartale Gesundheitszustand', '
# 'EPDS präpartal' sowie 'EDPS 1m'
#Bei den restlichen Daten wird davon ausgegeangen, dass keine systematisches
# fehlen von Daten auftritt.

#Jetzt teile ich den Datensatz auf in zwei Unterkategorien: 
# Prospektiv Befragte und Retrospektiv Befragte

daten_pro <- Projekt_Praxis[grepl('Pat', Projekt_Praxis$Patientennummer),]
daten_retro <- Projekt_Praxis[grepl('Retro', Projekt_Praxis$Patientennummer),]

#Jetzt exportiere ich die neu erstellten Datensätze:
write.csv(daten_pro,"daten_pro.csv", row.names = TRUE)
write.csv(daten_retro,"daten_retro.csv", row.names = TRUE)