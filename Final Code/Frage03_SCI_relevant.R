
Projekt_Praxis$SCI_Profile <- as.factor(Projekt_Praxis$SCI_Profile)

#1m

EPDS_SCI_boxplot_1m <- ggplot(data = Projekt_Praxis, aes(x = SCI_Profile, y = EPDS_sum_postpartal_1m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.1,alpha = 0.5, aes(color = EPDS_in_Kategorien_postpartal_1m)) +
  scale_x_discrete(labels=c("1" = "Gruppe A", "2" = "Gruppe B", "3" = "Gruppe C", "4" = "Gruppe D")) +
  labs(title = "1 Monat Postpartal", color = "EPDS Kategorie") +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE,
                              breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#2m

EPDS_SCI_boxplot_2m <- ggplot(data = Projekt_Praxis, aes(x = SCI_Profile, y = EPDS_sum_postpartal_2m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.1,alpha = 0.5, aes(color = EPDS_in_Kategorien_postpartal_2m)) +
  scale_x_discrete(labels=c("1" = "Gruppe A", "2" = "Gruppe B", "3" = "Gruppe C", "4" = "Gruppe D")) +
  labs(title = "2 Monate Postpartal", color = "EPDS Kategorie") +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE,
                     breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#6m

EPDS_SCI_boxplot_6m <- ggplot(data = Projekt_Praxis, aes(x = SCI_Profile, y = EPDS_sum_postpartal_6m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.1,alpha = 0.5, aes(color = EPDS_in_Kategorien_postpartal_6m)) +
  scale_x_discrete(labels=c("1" = "Gruppe A", "2" = "Gruppe B", "3" = "Gruppe C", "4" = "Gruppe D")) +
  labs(title = "6 Monate Postpartal", color = "EPDS Kategorie") +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'),
                     labels=c('13-30', '10-12', '0-9'))

#all in all:
SCI_Gruppen_plot <- ggarrange(EPDS_SCI_boxplot_1m,
          EPDS_SCI_boxplot_2m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          EPDS_SCI_boxplot_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", widths = c(1.15, 1, 1))
require(grid)
annotate_figure(SCI_Gruppen_plot, left = textGrob("EPDS Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("SCI Gruppe", gp = gpar(cex = 1.1)))

#Alle ZP
Projekt_Praxis$SCI_Profile <- as.factor(Projekt_Praxis$SCI_Profile)

SCI_EPDS_Stapel_plot <- ggplot(data = subset(Projekt_Praxis, !is.na(SCI_Profile)), aes(x = SCI_Profile, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "Gruppe A", "2" = "Gruppe B", "3" = "Gruppe C", "4" = "Gruppe D")) +
  ylab("Prozent") +
  xlab("SCI Gruppe") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))


##SCI Stress Kategorien:

#Unsicherheit:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(Stress_sum_1)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = Stress_sum_1, color = Stress_sum_1), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_Unsicherheit_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = Stress_sum_1)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Unsicherheit\"\n(N = 122)") +
  scale_y_continuous(breaks = c(7, 20, 30, 42), limits = c(7, 42)) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Überforderung:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(Stress_sum_2)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = Stress_sum_2, color = Stress_sum_2), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_Ueberforderung_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = Stress_sum_2)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Überforderung\"\n(N = 122)") +
  scale_y_continuous(breaks = c(7, 20, 30, 42), limits = c(7, 42)) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Verlust:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(Stress_sum_3)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = Stress_sum_3, color = Stress_sum_3), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_Verlust_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = Stress_sum_3)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Verlust\"\n(N = 122)") +
  scale_y_continuous(breaks = c(7, 20, 30, 42), limits = c(7, 42)) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"))

SCI_Stressfaktoren_plot <- ggarrange(SCI_Unsicherheit_EPDS_boxplot, SCI_Ueberforderung_EPDS_boxplot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                     SCI_Verlust_EPDS_boxplot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
                                     ncol = 3, nrow = 1, widths = c(1.1, 1, 1))
require(grid)
annotate_figure(SCI_Stressfaktoren_plot, left = textGrob("Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Wahrscheinlichkeit für eine Depression nach EPDS", gp = gpar(cex = 1.1)))

##SCI Stress Coping:

#Positives Denken:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(SCI_positiv_sum)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = SCI_positiv_sum, color = SCI_positiv_sum), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_positiv_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = SCI_positiv_sum)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Positives Denken\"\n(N = 122)") +
  ylim(4, 16) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Aktive Bewältigung:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(SCI_aktiv_sum)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = SCI_aktiv_sum, color = SCI_aktiv_sum), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_aktiv_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = SCI_aktiv_sum)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Aktive Bewältigung\"\n(N = 122)") +
  ylim(4, 16) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Soz. Unterstützung:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(SCI_support_sum)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = SCI_support_sum, color = SCI_support_sum), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_support_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = SCI_support_sum)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Soz. Unterstützung\"\n(N = 122)") +
  ylim(4, 16) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Halt in Religion:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(SCI_glaube_sum)), aes(x = EPDS_postpartal_alleZP)) +
  geom_bar(aes(group = SCI_glaube_sum, color = SCI_glaube_sum), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 122

SCI_glaube_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = SCI_glaube_sum)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Halt in Religion\"\n(N = 122)") +
  ylim(4, 16) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))

#Alkohol- und Zigarettenkonsum:

SCI_alk_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = SCI_alk_sum)) +
  geom_boxplot(varwidth = TRUE, aes(fill = EPDS_postpartal_alleZP), show.legend = FALSE) +
  labs(title = "\"Alkohol- und Zigarettenkonsum\"") +
  ylim(4, 16) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("nie hohe W.keit" = "nie\nhohe W.keit", "mind. 1 mal hohe W.keit" = "mind. 1 mal\nhohe W.keit")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'minestens 1 mal'))



SCI_Coping_plot <- ggarrange(SCI_positiv_EPDS_boxplot, SCI_aktiv_EPDS_boxplot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                             SCI_support_EPDS_boxplot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                             SCI_glaube_EPDS_boxplot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                     ncol = 4, nrow = 1, widths = c(1.15, 1, 1, 1))
require(grid)
annotate_figure(SCI_Coping_plot, left = textGrob("Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Wahrscheinlichkeit für eine Depression nach EPDS", gp = gpar(cex = 1.1)))


