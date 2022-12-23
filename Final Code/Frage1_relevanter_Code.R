#Boxplots zu den jeweiligen Zeirpunkten:

library(ggplot2)

#Präpartal:
Projekt_Praxis$EPDS_in_Kategorien_präpartal <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_präpartal)
Projekt_Praxis$covid_belastungss <- as.numeric(Projekt_Praxis$covid_belastungss)
Projekt_Praxis$EPDS_sum_präpartal <- as.numeric(Projekt_Praxis$EPDS_sum_präpartal)

Projekt_Praxis$covid_belastungss <- as.factor(Projekt_Praxis$covid_belastungss)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_präpartal)), aes(x = covid_belastungss)) +
  geom_bar(aes(color = EPDS_in_Kategorien_präpartal), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 81

covid_EPDS_boxplot_präpartal <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungss)), aes(x = covid_belastungss, y = EPDS_sum_präpartal)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, aes(fill = EPDS_in_Kategorien_präpartal), pch = 21) +
  labs(title = "Präpartal\n(N = 81)", fill = "EPDS Kategorie") +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9')) 


#1m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m)
Projekt_Praxis$covid_belastungwochenbett <- as.numeric(Projekt_Praxis$covid_belastungwochenbett)
Projekt_Praxis$EPDS_sum_postpartal_1m <- as.numeric(Projekt_Praxis$EPDS_sum_postpartal_1m)

Projekt_Praxis$covid_belastungwochenbett <- as.factor(Projekt_Praxis$covid_belastungwochenbett)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)), aes(x = covid_belastungwochenbett)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_1m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 77

covid_EPDS_boxplot_1m <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungwochenbett)), aes(x = covid_belastungwochenbett, y = EPDS_sum_postpartal_1m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.15, height = 0,alpha = 0.5, aes(fill = EPDS_in_Kategorien_postpartal_1m), pch = 21) +
  labs(title = "1 Monat Postpartal\n(N = 77)", fill = "EPDS Kategorie") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#2m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m)
Projekt_Praxis$covid_belastung2m <- as.numeric(Projekt_Praxis$covid_belastung2m)
Projekt_Praxis$EPDS_sum_postpartal_2m <- as.numeric(Projekt_Praxis$EPDS_sum_postpartal_2m)

Projekt_Praxis$covid_belastung2m <- as.factor(Projekt_Praxis$covid_belastung2m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)), aes(x = covid_belastung2m)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_2m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 109

covid_EPDS_boxplot_2m <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2m), !is.na(EPDS_sum_postpartal_2m)), aes(x = covid_belastung2m, y = EPDS_sum_postpartal_2m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.15, height = 0,alpha = 0.5, aes(fill = EPDS_in_Kategorien_postpartal_2m), pch = 21) +
  labs(title = "2 Monate Postpartal\n(N = 109)", fill = "EPDS Kategorie") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#6m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m)
Projekt_Praxis$covid_belastung6m <- as.numeric(Projekt_Praxis$covid_belastung6m)
Projekt_Praxis$EPDS_sum_postpartal_6m <- as.numeric(Projekt_Praxis$EPDS_sum_postpartal_6m)

Projekt_Praxis_Plausibel <- Projekt_Praxis[Projekt_Praxis$covid_belastung6m != "2.3", ]

Projekt_Praxis_Plausibel$covid_belastung6m <- as.factor(Projekt_Praxis_Plausibel$covid_belastung6m)

#N count:
ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(EPDS_in_Kategorien_postpartal_6m)), aes(x = covid_belastung6m)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_6m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 97

covid_EPDS_boxplot_6m <- ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung6m)), aes(x = covid_belastung6m, y = EPDS_sum_postpartal_6m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, aes(fill = EPDS_in_Kategorien_postpartal_6m), pch = 21) +
  labs(title = "6 Monate Postpartal\n(N = 97)", fill = "EPDS Kategorie") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "wenig", "3" = "stark", "4" = "sehr\nstark")) +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#all in all:
library(ggpubr)
all_covid_EPDS_Boxplots <- ggarrange(covid_EPDS_boxplot_präpartal + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()), 
                                     covid_EPDS_boxplot_1m + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          covid_EPDS_boxplot_2m, 
          covid_EPDS_boxplot_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")
require(grid)
annotate_figure(all_covid_EPDS_Boxplots, left = textGrob("EPDS Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Covid Belastung", gp = gpar(cex = 1.1)))


#Barplots allg. Covid belastung:
#Schewangerschaft
Projekt_Praxis$covid_belastungss <- as.numeric(Projekt_Praxis$covid_belastungss)
Projekt_Praxis$covid_belastungss <- as.factor(Projekt_Praxis$covid_belastungss)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = covid_belastungss)) +
  geom_bar(aes(color = EPDS_postpartal_alleZP), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 109

covid_EPDS_Stapel_plot_ss <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastungss)), aes(x = covid_belastungss, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(title = "Schwanger-\nschaft (N = 109)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "", "3" = "", "4" = "sehr\nstark")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))

# Geburt
Projekt_Praxis$covid_belastunggeburt <- as.numeric(Projekt_Praxis$covid_belastunggeburt)
Projekt_Praxis$covid_belastunggeburt <- as.factor(Projekt_Praxis$covid_belastunggeburt)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = covid_belastunggeburt)) +
  geom_bar(aes(color = EPDS_postpartal_alleZP), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 109

covid_EPDS_Stapel_plot_geburt <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastunggeburt)), aes(x = covid_belastunggeburt, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(title = "Geburt\n(N = 109)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "", "3" = "", "4" = "sehr\nstark")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))

#postpartal 1m

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = covid_belastungwochenbett)) +
  geom_bar(aes(color = EPDS_postpartal_alleZP), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 108

covid_EPDS_Stapel_plot_1m <- ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastungwochenbett)), aes(x = covid_belastungwochenbett, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(title = "Wochenbett\n(N = 108)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "", "3" = "", "4" = "sehr\nstark")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))

#2m

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = covid_belastung2m)) +
  geom_bar(aes(color = EPDS_postpartal_alleZP), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 109

covid_EPDS_Stapel_plot_2m <- ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung2m)), aes(x = covid_belastung2m, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(title = "2 Monate\n(N = 109)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "", "3" = "", "4" = "sehr\nstark")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))

#2-6m
Projekt_Praxis$covid_belastung2bis6m <- as.numeric(Projekt_Praxis$covid_belastung2bis6m)
Projekt_Praxis$covid_belastung2bis6m <- as.factor(Projekt_Praxis$covid_belastung2bis6m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = covid_belastung2bis6m)) +
  geom_bar(aes(color = EPDS_postpartal_alleZP), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 98

covid_EPDS_Stapel_plot_2bis6m <- ggplot(data = subset(Projekt_Praxis, !is.na(covid_belastung2bis6m)), aes(x = covid_belastung2bis6m, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(title = "2 bis 6 Monate\n(N = 98)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "", "3" = "", "4" = "sehr\nstark")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))

#6m

#N count:
ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(EPDS_postpartal_alleZP)), aes(x = covid_belastung6m)) +
  geom_bar(aes(color = EPDS_postpartal_alleZP), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 97

covid_EPDS_Stapel_plot_6m <- ggplot(data = subset(Projekt_Praxis_Plausibel, !is.na(covid_belastung6m)), aes(x = covid_belastung6m, fill = EPDS_postpartal_alleZP)) +
  geom_bar(position = position_fill()) +
  labs(title = "6 Monate\n(N = 97)", fill = "hohe Wahrscheinlichkeit\nfür eine postpartale\nDepression aufgetreten") +
  scale_x_discrete(labels=c("1" = "sehr\nwenig", "2" = "", "3" = "", "4" = "sehr\nstark")) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("blue", "red"), labels=c('nie', 'mindestens 1 mal'))

all_covid_EPDS_Boxplots_percernt <- ggarrange(covid_EPDS_Stapel_plot_ss,
                                              covid_EPDS_Stapel_plot_geburt + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                              covid_EPDS_Stapel_plot_1m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                              covid_EPDS_Stapel_plot_2m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
                                              covid_EPDS_Stapel_plot_2bis6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                              covid_EPDS_Stapel_plot_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                     ncol = 6, nrow = 1, common.legend = TRUE, legend = "right", widths = c(1.2, 1, 1, 1, 1, 1))
require(grid)
annotate_figure(all_covid_EPDS_Boxplots_percernt, left = textGrob("Prozent", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("Covid Belastung", gp = gpar(cex = 1)))

