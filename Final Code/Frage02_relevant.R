#BSF vor der Geburt (Positive Kategorien werden Addiert(Engagement, gehobene Stimmung), negative Subtrahiert; 
# D.h. eine niedrige/negative Summe bedeutet eine negative/unausgeglichene Stimmung):
Projekt_Praxis$BSF_präpartal_sumall <-  Projekt_Praxis$BSF_präpartal_Engagement_sumof2 + 
  Projekt_Praxis$BSF_präpartal_GehobeneStimmung_sumof2 - Projekt_Praxis$BSF_präpartal_Müdigkeit_sumof2 - 
  (Projekt_Praxis$BSF_präpartal_Ängstlichkeit_sumof4/2) - Projekt_Praxis$BSF_präpartal_Ärger_sumof2 - 
  Projekt_Praxis$BSF_präpartal_Teilnahmslosigkeit_sumof2

sort(sapply(Projekt_Praxis[, 115, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von (-36)-12

#BSF nach der Geburt (Positive Kategorien werden Addiert(Engagement, gehobene Stimmung), negative Subtrahiert; 
# D.h. eine niedrige/negative Summe bedeutet eine negative/unausgeglichene Stimmung):
Projekt_Praxis$BSF_postpartal_sumall <- Projekt_Praxis$BSF_postpartal_3d_Engagement_sumof2_A + 
  Projekt_Praxis$BSF_postpartal_3d_GehobeneStimmung_sumof2_A - Projekt_Praxis$BSF_postpartal_3d_Müdigkeit_sumof2_A - 
  (Projekt_Praxis$BSF_postpartal_3d_Ängstlichkeit_sumof4_A/2) - Projekt_Praxis$BSF_postpartal_3d_Ärger_sumof2_A - 
  Projekt_Praxis$BSF_postpartal_3d_Teilnahmslosigkeit_sumof2_A

sort(sapply(Projekt_Praxis[, 116, drop = FALSE], unique))
#Diese Variable hat einen Skalenbereich von (-36)-12

#präpartale Ängste in Bins zusammengefasst für einfachere Darstellung in Boxplots:

Projekt_Praxis$präpartal_Ängste_allgemein_Skala <- as.numeric(Projekt_Praxis$präpartal_Ängste_allgemein_Skala)

Ängste_bin_fun <- function(x) {
  if ((x == 1 | x == 2) & !is.na(x)) {
    return("1-2")
  }
  if ((x == 3 | x == 4) & !is.na(x)) {
    return("3-4")
  }
  if ((x == 5 | x == 6) & !is.na(x)) {
    return("5-6")
  }
  if ((x == 7 | x == 8) & !is.na(x)) {
    return("7-8")
  }
  if ((x == 9 | x == 10) & !is.na(x)) {
    return("9-10")
  }
  NA_character_
}

Projekt_Praxis$präpartal_Ängste_allgemein_Bins <- factor(vapply(as.vector(Projekt_Praxis$präpartal_Ängste_allgemein_Skala), FUN = Ängste_bin_fun, FUN.VALUE = character(1)))


library(ggplot2)

#Präpartale Ängste allgemein:
#Boxplotss zu den jeweiligen Zeirpunkten - Präpartale Ängste:

#Präpartal:
Projekt_Praxis$EPDS_in_Kategorien_präpartal <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_präpartal)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_präpartal)), aes(x = präpartal_Ängste_allgemein_Bins)) +
  geom_bar(aes(color = EPDS_in_Kategorien_präpartal), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 94

Prä_Ängste_EPDS_plot_präpartal <- ggplot(data = subset(Projekt_Praxis, !is.na(präpartal_Ängste_allgemein_Bins)), aes(y = EPDS_sum_präpartal, x = präpartal_Ängste_allgemein_Bins)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_präpartal), pch = 21) +
  labs(title = "Präpartal\n(N = 94)", fill = "EPDS Kategorie")  +
  ylab("") +
  xlab("") +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))


#1m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)), aes(x = präpartal_Ängste_allgemein_Bins)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_1m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 84

Prä_Ängste_EPDS_plot_postpartal_1m <- ggplot(data = subset(Projekt_Praxis, !is.na(präpartal_Ängste_allgemein_Bins)), aes(y = EPDS_sum_postpartal_1m, x = präpartal_Ängste_allgemein_Bins)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_postpartal_1m), pch = 21) +
  labs(title = "1 Monat postpartal\n(N = 84)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#2m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)), aes(x = präpartal_Ängste_allgemein_Bins)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_2m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 83

Prä_Ängste_EPDS_plot_postpartal_2m <- ggplot(data = subset(Projekt_Praxis, !is.na(präpartal_Ängste_allgemein_Bins)), aes(y = EPDS_sum_postpartal_2m, x = präpartal_Ängste_allgemein_Bins)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_postpartal_2m), pch = 21) +
  labs(title = "2 Monate postpartal\n(N = 83)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_2m, Projekt_Praxis$präpartal_Ängste_allgemein_Skala, use = "complete.obs", method = "spearman")
#-0.3789408 -> mittlerer monotoner Zusammenhang

#6m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_6m)), aes(x = präpartal_Ängste_allgemein_Bins)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_6m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 72

Prä_Ängste_EPDS_plot_postpartal_6m <- ggplot(data = subset(Projekt_Praxis, !is.na(präpartal_Ängste_allgemein_Bins)), aes(y = EPDS_sum_postpartal_6m, x = präpartal_Ängste_allgemein_Bins)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_postpartal_6m), pch = 21) +
  labs(title = "6 Monate postpartal\n(N = 72)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_6m, Projekt_Praxis$präpartal_Ängste_allgemein_Skala, use = "complete.obs", method = "spearman")
#-0.2932908 -> schwacher monotoner Zusammenhang

library(ggpubr)
#all in all:
Prä_BSF_plot <- ggarrange(Prä_Ängste_EPDS_plot_präpartal + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                          Prä_Ängste_EPDS_plot_postpartal_1m  + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
                          Prä_Ängste_EPDS_plot_postpartal_2m, Prä_Ängste_EPDS_plot_postpartal_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                          ncol = 2, nrow = 2, common.legend = TRUE, legend = "right", widths = c(1.15, 1, 1, 1, 1, 1))
require(grid)
annotate_figure(Prä_BSF_plot, left = textGrob("EPDS Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Präpartale Ängste", gp = gpar(cex = 1.1)))

#Postpartales Befinden allgemein:
#Boxplotss zu den jeweiligen Zeirpunkten - Postpartales Befinden:

Projekt_Praxis$postpartal_3d_Befinden_1 <- as.numeric(Projekt_Praxis$postpartal_3d_Befinden_1)
Projekt_Praxis$postpartal_3d_Befinden_1 <- factor(Projekt_Praxis$postpartal_3d_Befinden_1)

#1m:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_1m)), aes(x = postpartal_3d_Befinden_1)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_1m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 81

Befinden_EPDS_boxplot_1m <- ggplot(data = subset(Projekt_Praxis, !is.na(postpartal_3d_Befinden_1)), aes(x = postpartal_3d_Befinden_1, y = EPDS_sum_postpartal_1m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_postpartal_1m), pch = 21) +
  labs(title = "1 Monat Postpartal\n(N = 81)", fill = "EPDS Kategorie") +
  scale_x_discrete(labels=c("1" = "sehr schlecht","2" = "schlecht", "3" = "gut", "4" = "sehr gut")) +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#2m:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_2m)), aes(x = postpartal_3d_Befinden_1)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_2m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 79

Befinden_EPDS_boxplot_2m <- ggplot(data = subset(Projekt_Praxis, !is.na(postpartal_3d_Befinden_1)), aes(x = postpartal_3d_Befinden_1, y = EPDS_sum_postpartal_2m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_postpartal_2m), pch = 21) +
  labs(title = "2 Monate Postpartal\n(N = 79)", fill = "EPDS Kategorie") +
  scale_x_discrete(labels=c("1" = "sehr schlecht", "2" = "schlecht", "3" = "gut", "4" = "sehr gut")) +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

#6m:

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_in_Kategorien_postpartal_6m)), aes(x = postpartal_3d_Befinden_1)) +
  geom_bar(aes(color = EPDS_in_Kategorien_postpartal_6m), position = position_stack()) +
  scale_color_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 70

Befinden_EPDS_boxplot_6m <- ggplot(data = subset(Projekt_Praxis, !is.na(postpartal_3d_Befinden_1)), aes(x = postpartal_3d_Befinden_1, y = EPDS_sum_postpartal_6m)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) + 
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, aes(fill = EPDS_in_Kategorien_postpartal_6m), pch = 21) +
  labs(title = "6 Monate Postpartal\n(N = 70)", fill = "EPDS Kategorie") +
  scale_x_discrete(labels=c("1" = "sehr schlecht", "2" = "schlecht", "3" = "gut", "4" = "sehr gut")) +
  ylim(0,30) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 13)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))


#all in all:
library(ggpubr)
all_Befinden_EPDS_Boxplots <- ggarrange(Befinden_EPDS_boxplot_1m,
                                     Befinden_EPDS_boxplot_2m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                     Befinden_EPDS_boxplot_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                                     ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", widths = c(1.15, 1, 1, 1, 1, 1))
require(grid)
annotate_figure(all_Befinden_EPDS_Boxplots, left = textGrob("EPDS Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("postpartales Befinden", gp = gpar(cex = 1.1)))



#Scatterplots zu den jeweiligen Zeirpunkten - BSF Präpartal:

#Präpartal:
Projekt_Praxis$EPDS_in_Kategorien_präpartal <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_präpartal)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_sum_präpartal)), aes(x = EPDS_in_Kategorien_präpartal)) +
  geom_bar(aes(group = BSF_präpartal_sumall, color = BSF_präpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 94

Prä_BSF_EPDS_plot_präpartal <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_präpartal, 
                                                                x = BSF_präpartal_sumall, fill = EPDS_in_Kategorien_präpartal)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "Präpartal\n(N = 97)", fill = "EPDS Kategorie")  +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_präpartal, Projekt_Praxis$BSF_präpartal_sumall, use = "complete.obs", method = "spearman")
#-0.5320085 -> mittlerer monotoner Zusammenhang

#1m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_sum_postpartal_1m)), aes(x = EPDS_in_Kategorien_postpartal_1m)) +
  geom_bar(aes(group = BSF_präpartal_sumall, color = BSF_präpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 84

Prä_BSF_EPDS_plot_postpartal_1m <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_postpartal_1m, 
                                                                    x = BSF_präpartal_sumall, fill = EPDS_in_Kategorien_postpartal_1m)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "1 Monat postpartal\n(N = 84)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_1m, Projekt_Praxis$BSF_präpartal_sumall, use = "complete.obs", method = "spearman")
#-0.4225345 -> mittlerer monotoner Zusammenhang

#2m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_sum_postpartal_2m)), aes(x = EPDS_in_Kategorien_postpartal_2m)) +
  geom_bar(aes(group = BSF_präpartal_sumall, color = BSF_präpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 110

Prä_BSF_EPDS_plot_postpartal_2m <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_postpartal_2m, 
                                                                    x = BSF_präpartal_sumall, fill = EPDS_in_Kategorien_postpartal_2m)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "2 Monate postpartal\n(N = 110)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_2m, Projekt_Praxis$BSF_präpartal_sumall, use = "complete.obs", method = "spearman")
#-0.3789408 -> mittlerer monotoner Zusammenhang

#6m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_sum_postpartal_6m)), aes(x = EPDS_in_Kategorien_postpartal_6m)) +
  geom_bar(aes(group = BSF_präpartal_sumall, color = BSF_präpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 99

Prä_BSF_EPDS_plot_postpartal_6m <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_postpartal_6m, 
                                                                    x = BSF_präpartal_sumall, fill = EPDS_in_Kategorien_postpartal_6m)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "6 Monate postpartal\n(N = 99)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_6m, Projekt_Praxis$BSF_präpartal_sumall, use = "complete.obs", method = "spearman")
#-0.2932908 -> schwacher monotoner Zusammenhang

library(ggpubr)
#all in all:
Prä_BSF_plot <- ggarrange(Prä_BSF_EPDS_plot_präpartal + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                          Prä_BSF_EPDS_plot_postpartal_1m+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
          Prä_BSF_EPDS_plot_postpartal_2m, Prä_BSF_EPDS_plot_postpartal_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "right", widths = c(1.1, 1, 1.1, 1))
require(grid)
annotate_figure(Prä_BSF_plot, left = textGrob("EPDS Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("BSF Score", gp = gpar(cex = 1.1)))

#Alle ZP Boxplot:
Prä_BSF_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = BSF_präpartal_sumall)) +
  geom_boxplot(varwidth = TRUE) + 
  geom_jitter(width = 0.5, height = 0, alpha = 0.5) +
  labs(title = "BSF präpartal und EPDS", color = "EPDS Kategorie") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylab("BSF Score") +
  xlab("Wahrscheinlichkeit für eine Depression nach EPDS") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



#Scatterplots zu den jeweiligen Zeirpunkten - BSF Postpartal:

#1m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_1m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(BSF_postpartal_sumall)), aes(x = EPDS_in_Kategorien_postpartal_1m)) +
  geom_bar(aes(group = BSF_postpartal_sumall, color = BSF_postpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 81

Post_BSF_EPDS_plot_postpartal_1m <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_postpartal_1m, 
                                                              x = BSF_postpartal_sumall, fill = EPDS_in_Kategorien_postpartal_1m)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "1 Monat postpartal\n(N = 81)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_1m, Projekt_Praxis$BSF_postpartal_sumall, use = "complete.obs", method = "spearman")
#-0.3843764 -> mittlerer monotoner Zusammenhang

#2m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_2m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(BSF_postpartal_sumall)), aes(x = EPDS_in_Kategorien_postpartal_2m)) +
  geom_bar(aes(group = BSF_postpartal_sumall, color = BSF_postpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 78

Post_BSF_EPDS_plot_postpartal_2m <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_postpartal_2m, 
                                                              x = BSF_postpartal_sumall, fill = EPDS_in_Kategorien_postpartal_2m)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "2 Monate postpartal\n(N = 78)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_2m, Projekt_Praxis$BSF_postpartal_sumall, use = "complete.obs", method = "spearman")
#-0.2777494 -> schwacher monotoner Zusammenhang

#6m:
Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m <- as.factor(Projekt_Praxis$EPDS_in_Kategorien_postpartal_6m)

#N count:
ggplot(data = subset(Projekt_Praxis, !is.na(BSF_postpartal_sumall)), aes(x = EPDS_in_Kategorien_postpartal_6m)) +
  geom_bar(aes(group = BSF_postpartal_sumall, color = BSF_postpartal_sumall), position = position_stack()) +
  scale_color_continuous() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
#N = 70

Post_BSF_EPDS_plot_postpartal_6m <- ggplot(Projekt_Praxis, aes(y = EPDS_sum_postpartal_6m, 
                                                              x = BSF_postpartal_sumall, fill = EPDS_in_Kategorien_postpartal_6m)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.5, pch = 21) +
  labs(title = "6 Monate postpartal\n(N = 70)", fill = "EPDS Kategorie") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = c(-36, -30, -20, -10, 0, 12), limits = c(-36, 12)) +
  ylim(0, 30) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 14)) +
  scale_fill_manual(values=c("dark red", "orange", "blue"), na.translate = FALSE, breaks=c('3', '2', '1'), labels=c('13-30', '10-12', '0-9'))

cor(Projekt_Praxis$EPDS_sum_postpartal_6m, Projekt_Praxis$BSF_postpartal_sumall, use = "complete.obs", method = "spearman")
#-0.2209893 -> schwacher monotoner Zusammenhang

library(ggpubr)
#all in all:
Post_BSF_plot <- ggarrange(Post_BSF_EPDS_plot_postpartal_1m, 
          Post_BSF_EPDS_plot_postpartal_2m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          Post_BSF_EPDS_plot_postpartal_6m + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
          ncol = 3, common.legend = TRUE, legend = "right", widths = c(1.2, 1, 1))
require(grid)
annotate_figure(Post_BSF_plot, left = textGrob("EPDS Score", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("BSF Score", gp = gpar(cex = 1.1)))

#Alle ZP Boxplot:
Post_BSF_EPDS_boxplot <- ggplot(data = subset(Projekt_Praxis, !is.na(EPDS_postpartal_alleZP)), aes(x = EPDS_postpartal_alleZP, y = BSF_postpartal_sumall)) +
  geom_boxplot(varwidth = TRUE) + 
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "BSF postpartal und EPDS", color = "EPDS Kategorie") +
  ylim(-36,12) +
  ylab("BSF Score") +
  xlab("Wahrscheinlichkeit für eine Depression nach EPDS") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

