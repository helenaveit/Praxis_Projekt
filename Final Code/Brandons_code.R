# Extra Frage
## 1. Covid und Stressbelastung
### falsche Daten korrigieren
Projekt_Praxis$covid_belastung6m[Projekt_Praxis$covid_belastung6m == 2.3] <- NA

Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum <= 30] <- "20-30"
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum >30 & Projekt_Praxis$Aktuelle_Stressbelastung_sum <= 40] <- "31-40"
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum >40 & Projekt_Praxis$Aktuelle_Stressbelastung_sum <= 50] <- "41-50"
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum >50] <- "51-80"

#Präpartal

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungss)), aes(x = Belastungkat)) +
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastungss), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 109

p1 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungss)))+
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastungss), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung",
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "Präpartal\n(N = 109)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#Geburt:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastunggeburt)), aes(x = Belastungkat)) +
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastunggeburt), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 109

p2 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastunggeburt)))+
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastunggeburt), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung",
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "Geburt\n(N = 109)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#Wochenbett:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungwochenbett)), aes(x = Belastungkat)) +
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastungwochenbett), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 108

p3 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungwochenbett)))+
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastungwochenbett), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung",
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "Wochenbett\n(N = 108)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#2 Monate Pospartal:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2m)), aes(x = Belastungkat)) +
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastung2m), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 109

p4 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2m)))+
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastung2m), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung",
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "2 Monate\n(N = 109)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#2 bis 6 Monate Pospartal:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2bis6m)), aes(x = Belastungkat)) +
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastung2bis6m), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 98

p5 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2bis6m)))+
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastung2bis6m), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung",
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "2 bis 6 Monate\n(N = 98)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#2 bis 6 Monate Pospartal:

Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m)
Projekt_Praxis$covid_belastung6m <- as.numeric(Projekt_Praxis$covid_belastung6m)
Projekt_Praxis$EPDS_sum_postpartal_6m <- as.numeric(Projekt_Praxis$EPDS_sum_postpartal_6m)

Projekt_Praxis_Plausibel <- Projekt_Praxis[Projekt_Praxis$covid_belastung6m != "2.3", ]

Projekt_Praxis_Plausibel$covid_belastung6m <- as.factor(Projekt_Praxis_Plausibel$covid_belastung6m)

#N count:
ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung6m)), aes(x = Belastungkat)) +
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastung6m), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 97

p6 <- ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung6m)))+
  geom_bar(mapping=aes(x = Belastungkat, fill = covid_belastung6m), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung",
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "6 Monate\n(N = 97)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

p <- ggarrange(p1, p2 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p3 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p4 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p5 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p6 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
               ncol = 6, nrow = 1, common.legend = TRUE, legend = "right", widths = c(1.25, 1, 1, 1, 1, 1))

annotate_figure(p, left = textGrob("Anteil Patientinnen", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Stressbelastungs Score", gp = gpar(cex = 1)))

# 2. Covid und Bewaeltigung
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >25 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 35] <- "25-35"
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >35 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 45] <- "36-45"
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >45 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 65] <- "46-65"

#Präpartal

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungss)), aes(x =Copingkat)) +
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastungss), position = position_stack(), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 109

p1 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungss)))+
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastungss), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung", breaks=c('4', '3', '2', '1'),
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "Präpartal\n(N = 109)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#Geburt:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastunggeburt)), aes(x =Copingkat)) +
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastunggeburt), position = position_stack(), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 109

p2 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastunggeburt)))+
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastunggeburt), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung", breaks=c('4', '3', '2', '1'),
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "Geburt\n(N = 109)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#Wochenbett:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungwochenbett)), aes(x =Copingkat)) +
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastungwochenbett), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 108

p3 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungwochenbett)))+
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastungwochenbett), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung", breaks=c('4', '3', '2', '1'),
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "Wochenbett\n(N = 108)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#2 Monate postpartal:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2m)), aes(x =Copingkat)) +
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastung2m), position = position_stack(), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 109

p4 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2m)))+
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastung2m), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung", breaks=c('4', '3', '2', '1'),
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "2 Monate\n(N = 109)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#2 bis 6 Monate postpartal:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2bis6m)), aes(x =Copingkat)) +
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastung2bis6m), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 98

p5 <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2bis6m)))+
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastung2bis6m), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung", breaks=c('4', '3', '2', '1'),
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "2 bis 6 Monate\n(N = 98)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

#6 Monate postpartal:

Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m)
Projekt_Praxis$covid_belastung6m <- as.numeric(Projekt_Praxis$covid_belastung6m)
Projekt_Praxis$EPDS_sum_postpartal_6m <- as.numeric(Projekt_Praxis$EPDS_sum_postpartal_6m)

Projekt_Praxis_Plausibel <- Projekt_Praxis[Projekt_Praxis$covid_belastung6m != "2.3", ]

Projekt_Praxis_Plausibel$covid_belastung6m <- as.factor(Projekt_Praxis_Plausibel$covid_belastung6m)

#N count:
ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung6m)), aes(x =Copingkat)) +
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastung6m), position = position_stack(reverse = T), show.legend = T) +
  scale_color_manual(na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 97

p6 <- ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung6m)))+
  geom_bar(mapping=aes(x = Copingkat, fill = covid_belastung6m), position = position_fill(), show.legend = T)+
  scale_fill_brewer(palette = "BuPu", name = "Covid Belastung", breaks=c('4', '3', '2', '1'),
                    labels=c("1" = "sehr wenig", "2" = "wenig", "3" = "stark", "4" = "sehr stark"))+
  labs(title = "6 Monate\n(N = 97)", x = NULL, y = NULL)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13), axis.text.x = element_text(angle = 90))

p <- ggarrange(p1, p2 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p3 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p4 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p5 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p6 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
               ncol = 6, nrow = 1, common.legend = TRUE, legend = "right", widths = c(1.2, 1, 1, 1, 1, 1))

annotate_figure(p, left = textGrob("Anteil Patientinnen", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Stressbewältigungs Score", gp = gpar(cex = 1)))

## 3. Einzelnen Kategorien
Projekt_Praxis$ansteckung_kind <- rowMeans(Projekt_Praxis[, c(52, 64, 72, 79, 86, 94)], na.rm = T)
Projekt_Praxis$ansteckung_sie <- rowMeans(Projekt_Praxis[, c(53, 65, 73, 80, 87, 95)], na.rm = T)
Projekt_Praxis$trennung_Fam <- rowMeans(Projekt_Praxis[, c(57, 68, 75, 82, 89, 97)], na.rm = T)
Projekt_Praxis$kontakt_freunde <- rowMeans(Projekt_Praxis[, c(62, 70, 77, 84, 91, 99)], na.rm = T)
Projekt_Praxis$freizeit <- rowMeans(Projekt_Praxis[, c(61, 69, 76, 83, 90, 98)], na.rm = T)

###belastung:

#Ansteckung Kind:

cor(Projekt_Praxis$Aktuelle_Stressbelastung_sum, Projekt_Praxis$ansteckung_kind, use = "complete.obs", method = "spearman")
#0.12 -> schwacher monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(ansteckung_kind)), aes(x = Belastungkat)) +
  geom_bar(aes(group = ansteckung_kind, color = ansteckung_kind), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p1 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Aktuelle_Stressbelastung_sum, y = ansteckung_kind), alpha = 0.4)+
  labs(x = NULL, y = NULL, title = "Ansteckung Kind\n(N = 114)") +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Ansteckung Sie:

cor(Projekt_Praxis$Aktuelle_Stressbelastung_sum, Projekt_Praxis$ansteckung_sie, use = "complete.obs", method = "spearman")
#0.21 -> schwacher monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(ansteckung_sie)), aes(x = Belastungkat)) +
  geom_bar(aes(group = ansteckung_sie, color = ansteckung_sie), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p2 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Aktuelle_Stressbelastung_sum, y = ansteckung_sie), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  labs(x = NULL, y = NULL, title = "Ansteckung Sie\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Trennung von Familie:

cor(Projekt_Praxis$Aktuelle_Stressbelastung_sum, Projekt_Praxis$trennung_Fam, use = "complete.obs", method = "spearman")
#0.35 -> mittlerer monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(trennung_Fam)), aes(x = Belastungkat)) +
  geom_bar(aes(group = trennung_Fam, color = trennung_Fam), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p3 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Aktuelle_Stressbelastung_sum, y = trennung_Fam), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  labs(x = NULL, y = NULL, title = "Trennung von Familie\n(N = 144)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Kontakt zu Freunden:

cor(Projekt_Praxis$Aktuelle_Stressbelastung_sum, Projekt_Praxis$kontakt_freunde, use = "complete.obs", method = "spearman")
#0.34 -> mittlerer monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(kontakt_freunde)), aes(x = Belastungkat)) +
  geom_bar(aes(group = kontakt_freunde, color = kontakt_freunde), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p4 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Aktuelle_Stressbelastung_sum, y = kontakt_freunde), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  labs(x = NULL, y = NULL, title = "Fehlender Kontakt\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Freizeit Einschränkung:

cor(Projekt_Praxis$Aktuelle_Stressbelastung_sum, Projekt_Praxis$freizeit, use = "complete.obs", method = "spearman")
#0.21 -> mittlerer monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(freizeit)), aes(x = Belastungkat)) +
  geom_bar(aes(group = freizeit, color = freizeit), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p5 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Aktuelle_Stressbelastung_sum, y = freizeit), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  labs(x = NULL, y = NULL, title = "Freizeit Einschränkung\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

annotate_figure(ggarrange(p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                          p2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                          p3 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                          p4,p5 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), align = "h", widths = c(1.2, 1, 1, 1.2, 1, 1)),
                left = textGrob("Durchschnittliche Covidbelastung ", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
bottom = textGrob("Stressbelastungs Score", gp = gpar(cex = 1.3)))

###bewaeltigung:

#Ansteckung Kind:

cor(Projekt_Praxis$Adaptive_Stressbewältigung_sum_new, Projekt_Praxis$ansteckung_kind, use = "complete.obs", method = "spearman")
#-0.00 -> fast kein monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(ansteckung_kind)), aes(x = Copingkat)) +
  geom_bar(aes(group = ansteckung_kind, color = ansteckung_kind), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p1 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Adaptive_Stressbewältigung_sum_new, y = ansteckung_kind), alpha = 0.4)+
  labs(x = NULL, y = NULL, title = "Ansteckung Kind\n(N = 114)") +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_continuous(breaks = c(16, 30, 40, 50, 64), limits = c(16, 64)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Ansteckung Sie:

cor(Projekt_Praxis$Adaptive_Stressbewältigung_sum_new, Projekt_Praxis$ansteckung_sie, use = "complete.obs", method = "spearman")
#-0.06 -> fast kein monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(ansteckung_sie)), aes(x = Copingkat)) +
  geom_bar(aes(group = ansteckung_sie, color = ansteckung_sie), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p2 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Adaptive_Stressbewältigung_sum_new, y = ansteckung_sie), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_continuous(breaks = c(16, 30, 40, 50, 64), limits = c(16, 64)) +
  labs(x = NULL, y = NULL, title = "Ansteckung Sie\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Trennung von Familie:

cor(Projekt_Praxis$Adaptive_Stressbewältigung_sum_new, Projekt_Praxis$trennung_Fam, use = "complete.obs", method = "spearman")
#0.11 -> schwacher monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(trennung_Fam)), aes(x = Copingkat)) +
  geom_bar(aes(group = trennung_Fam, color = trennung_Fam), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p3 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Adaptive_Stressbewältigung_sum_new, y = trennung_Fam), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_continuous(breaks = c(16, 30, 40, 50, 64), limits = c(16, 64)) +
  labs(x = NULL, y = NULL, title = "Trennung Familien\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Trennung von Familie:

cor(Projekt_Praxis$Adaptive_Stressbewältigung_sum_new, Projekt_Praxis$kontakt_freunde, use = "complete.obs", method = "spearman")
#0.10 -> schwacher monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(kontakt_freunde)), aes(x = Copingkat)) +
  geom_bar(aes(group = kontakt_freunde, color = kontakt_freunde), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p4 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Adaptive_Stressbewältigung_sum_new, y = kontakt_freunde), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_continuous(breaks = c(16, 30, 40, 50, 64), limits = c(16, 64)) +
  labs(x = NULL, y = NULL, title = "Fehlender Kontakt\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

#Trennung von Familie:

cor(Projekt_Praxis$Adaptive_Stressbewältigung_sum_new, Projekt_Praxis$freizeit, use = "complete.obs", method = "spearman")
#0.10 -> schwacher monotoner Zusammenhang

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(freizeit)), aes(x = Copingkat)) +
  geom_bar(aes(group = freizeit, color = freizeit), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p5 <- ggplot(Projekt_Praxis)+
  geom_point(aes(x = Adaptive_Stressbewältigung_sum_new, y = freizeit), alpha = 0.4)+
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_continuous(breaks = c(16, 30, 40, 50, 64), limits = c(16, 64)) +
  labs(x = NULL, y = NULL, title = "Freizeit Einschränkung\n(N = 114)")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 15))

annotate_figure(ggarrange(p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                          p2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                          p3 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                          p4,p5 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), align = "h", widths = c(1.2, 1, 1, 1.2, 1, 1)),
                left = textGrob("Durchschnittliche Covidbelastung ", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Stressbewältigungs Score", gp = gpar(cex = 1.3)))



## SCI und EPDS

#Stressbelastung und EPDS:
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum <= 30] <- "20-30"
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum >30 & Projekt_Praxis$Aktuelle_Stressbelastung_sum <= 40] <- "31-40"
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum >40 & Projekt_Praxis$Aktuelle_Stressbelastung_sum <= 50] <- "41-50"
Projekt_Praxis$Belastungkat[Projekt_Praxis$Aktuelle_Stressbelastung_sum >50] <- "51-80"

#präpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_präpartal)), aes(x = EPDS_in_Kategorien_präpartal))+
  geom_bar(aes(group = Belastungkat, color = Belastungkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 94

p17 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_präpartal)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_präpartal, fill = Belastungkat), position = position_fill())+
  scale_fill_brewer(palette = "YlOrBr", name = "Belastung Score")+
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  labs(y = NULL, x = NULL, title = "Präpartal\n(N = 94)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

#1 Monat postpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)), aes(x = EPDS_in_Kategorien_postpartal_1m))+
  geom_bar(aes(group = Belastungkat, color = Belastungkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 84

p18 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_postpartal_1m, fill = Belastungkat), position = position_fill())+
  scale_fill_brewer(palette = "YlOrBr", name = "Belastung Score") +
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  labs(y = NULL, x = NULL, title = "1 Monat postpartal\n(N = 84)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

#2 Monate postpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)), aes(x = EPDS_in_Kategorien_postpartal_2m))+
  geom_bar(aes(group = Belastungkat, color = Belastungkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 110

p19 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_postpartal_2m, fill = Belastungkat), position = position_fill())+
  scale_fill_brewer(palette = "YlOrBr", name = "Belastung Score")+
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  labs(y = NULL, x = NULL, title = "2 Monate postpartal\n(N = 110)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

#6 Monate postpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_6m)), aes(x = EPDS_in_Kategorien_postpartal_6m))+
  geom_bar(aes(group = Belastungkat, color = Belastungkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 99

p20 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_6m)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_postpartal_6m, fill = Belastungkat), position = position_fill())+
  scale_fill_brewer(palette = "YlOrBr", name = "Belastung Score")+
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  labs(y = NULL, x = NULL, title = "6 Monate postpartal\n(N = 99)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

p <- ggarrange(p17  + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()),
               p18 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p19, p20 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               nrow = 2, ncol = 2, common.legend = T, legend = "right", widths = c(1.15, 1, 1.15, 1))
annotate_figure(p, left = textGrob("Anteil der Patientinnen", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("EPDS Kategorie", gp = gpar(cex = 1.1)))

##Stressbewältigung und EPDS:
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >25 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 35] <- "25-35"
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >35 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 45] <- "36-45"
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >45 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 55] <- "46-55"
Projekt_Praxis$Copingkat[Projekt_Praxis$Adaptive_Stressbewältigung_sum_new >55 & Projekt_Praxis$Adaptive_Stressbewältigung_sum_new <= 65] <- "56-65"

#präpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_präpartal)), aes(x = EPDS_in_Kategorien_präpartal))+
  geom_bar(aes(group = Copingkat, color = Copingkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 94

p21 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_präpartal)))+
  geom_bar(mapping = aes(x = EPDS_in_Kategorien_präpartal, fill = Copingkat), position = position_fill(reverse = TRUE))+
  scale_fill_brewer(palette = "YlOrBr", name = "Bewältigung Score", direction=-1, breaks=c('56-65', '46-55', '36-45', '25-35'))+
  labs(y = NULL, x = NULL, title = "Präpartal\n(N = 94)") +
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))
  
#1 Monat postpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)), aes(x = EPDS_in_Kategorien_postpartal_1m))+
  geom_bar(aes(group = Copingkat, color = Copingkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 84

p22 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_postpartal_1m, fill = Copingkat), position = position_fill(reverse = TRUE))+
  scale_fill_brewer(palette = "YlOrBr", name = "Bewältigung Score", direction=-1, breaks=c('56-65', '46-55', '36-45', '25-35'))+
  labs(y = NULL, x = NULL, title = "1 Monat postpartal\n(N = 84)") +
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

#2 Monate postpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)), aes(x = EPDS_in_Kategorien_postpartal_2m))+
  geom_bar(aes(group = Copingkat, color = Copingkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 110

p23 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_postpartal_2m, fill = Copingkat), position = position_fill(reverse = TRUE))+
  scale_fill_brewer(palette = "YlOrBr", name = "Bewältigung Score", direction=-1, breaks=c('56-65', '46-55', '36-45', '25-35'))+
  labs(y = NULL, x = NULL, title = "2 Monate postpartal\n(N = 110)") +
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

#6 Monate postpartal:

#N count:
ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_6m)), aes(x = EPDS_in_Kategorien_postpartal_6m))+
  geom_bar(aes(group = Copingkat, color = Copingkat), position = position_stack())+
  geom_text(stat='count', aes(label=..count..), vjust= -1)
#N = 99

p24 <- ggplot(subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_6m)))+
  geom_bar(mapping=aes(x = EPDS_in_Kategorien_postpartal_6m, fill = Copingkat), position = position_fill(reverse = TRUE))+
  scale_fill_brewer(palette = "YlOrBr", name = "Bewältigung Score", direction=-1, breaks=c('56-65', '46-55', '36-45', '25-35'))+
  labs(y = NULL, x = NULL, title = "6 Monate postpartal\n(N = 99)") +
  scale_x_discrete(labels=c(c('3' = '13-30', '2' = '10-12', '1' = '0-9'))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13))

p <- ggarrange(p21  + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()),
               p22 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p23, p24 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               nrow = 2, ncol = 2, common.legend = T, legend = "right", widths = c(1.15, 1, 1.15, 1))
annotate_figure(p, left = textGrob("Anteil der Patientinnen", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("EPDS Kategorie", gp = gpar(cex = 1.1)))


#Frage 1
# Hatte eine Frau zu einem Zeitpunkt eine hohe wahrscheinlichkeit für eine postpartale Depression?
# (Variable ist 0, falls die Frau zu allen postpartalen EPDS Erhebungszeitpunkten 
# (EPDS_in_Kategorien_postpartal_1m, EPDS_in_Kategorien_postpartal_2m, EPDS_in_Kategorien_postpartal_6m)
# nie die Kategorie 3 hatte, 1, falls sie mind. 1 mal in die Kategorie 3 gefallen ist.)
v1 <- Projekt_Praxis$epds_kat1m == 3
v1[is.na(v1)] <- FALSE
v2 <- Projekt_Praxis$epds_kat2m == 3
v2[is.na(v2)] <- FALSE
v3 <- Projekt_Praxis$epds_kat6m == 3
v3[is.na(v3)] <- FALSE
r <- v1+v2+v3
#vector über Frauen, die mind. einmal hohe Wahrscheinlichkeit hatten depressiv zu sein:
depr_01 <- as.numeric(r > 0)
depr_01[depr_01 == 0] <- "nie hohe W.keit"
depr_01[depr_01 == 1] <- "mind. 1 mal hohe W.keit"
Projekt_Praxis$EPDS_postpartal_alleZP <- factor(depr_01, levels = c("nie hohe W.keit", "mind. 1 mal hohe W.keit"))

##EPDS und Covid Einzelfaktoren:

#Ansteckung Kind:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(ansteckung_kind)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = ansteckung_kind, color = ansteckung_kind), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p1 <- ggplot(Projekt_Praxis) +
  geom_boxplot(aes(x = EPDS_postpartal_alleZP, y = ansteckung_kind, fill = EPDS_postpartal_alleZP), varwidth = T)+
  labs(x = NULL, y = NULL, title = "Ansteckung Kind\n(N = 114)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 12.5)) +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Ansteckung Sie:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(ansteckung_sie)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = ansteckung_sie, color = ansteckung_sie), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p2 <- ggplot(Projekt_Praxis) +
  geom_boxplot(aes(x = EPDS_postpartal_alleZP, y = ansteckung_sie, fill = EPDS_postpartal_alleZP), varwidth = T)+
  labs(x = NULL, y = NULL, title = "Ansteckung Sie\n(N = 114)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 12.5)) +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Trennung von der Familie:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(trennung_Fam)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = trennung_Fam, color = trennung_Fam), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p3 <- ggplot(Projekt_Praxis)+
  geom_boxplot(aes(x = EPDS_postpartal_alleZP, y = trennung_Fam, fill = EPDS_postpartal_alleZP), varwidth = T)+
  labs(x = NULL, y = NULL, title = "Trennung von Familie\n(N = 114)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 12.5)) +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Fehlender Konakt zu Freunden:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(kontakt_freunde)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = kontakt_freunde, color = kontakt_freunde), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p4 <- ggplot(Projekt_Praxis)+
  geom_boxplot(aes(x = EPDS_postpartal_alleZP, y = kontakt_freunde, fill = EPDS_postpartal_alleZP), varwidth = T)+
  labs(x = NULL, y = NULL, title = "Fehlender Kontakt\n(N = 114)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 12.5)) +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Freizeit Einschränkung:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(freizeit)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = freizeit, color = freizeit), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 114

p5 <- ggplot(Projekt_Praxis)+
  geom_boxplot(aes(x = EPDS_postpartal_alleZP, y = freizeit, fill = EPDS_postpartal_alleZP), varwidth = T)+
  labs(x = NULL, y = NULL, title = "Freizeit Einschränkung\n(N = 114)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 12.5)) +
  scale_y_continuous(breaks = 1:4, labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

p <- ggarrange(p1,p2 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p3 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p4 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               p5 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
               nrow = 1, legend = "none", align = "h")
annotate_figure(p, left = textGrob("Durchschnittliche Covidbelastung ", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Wahrscheinlichkeit für Depression nach EPDS", gp = gpar(cex = 1.3)))
