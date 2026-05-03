rm(list = ls())

# Daten einlesen
daten <- read.csv("C:/Users/ahmed/Downloads/Kuckuckseier.txt",
                  header = TRUE, sep = "", stringsAsFactors = FALSE)

# Daten transformieren
library(tidyr)
daten_cleaned <- pivot_longer(
  daten,
  cols = everything(), 
  names_to = "Wirtsvogelart", 
  values_to = "Länge"
)

# Entfernen der NA-Werte
daten_cleaned <- na.omit(daten_cleaned)

# Reihenfolge der Gruppen festlegen (WP, BP, RK, ZK)
gruppen_reihenfolge <- c("WP", "BP", "RK", "ZK")
daten_cleaned$Wirtsvogelart <- factor(daten_cleaned$Wirtsvogelart, levels = gruppen_reihenfolge) # Reihenfolge als Faktor definieren
daten_cleaned <- daten_cleaned[order(daten_cleaned$Wirtsvogelart), ]

### Deskriptive Analyse

# Deskriptive Statistik berechnen
deskriptiv <- aggregate(Länge ~ Wirtsvogelart, data = daten_cleaned, 
                        FUN = function(x) c(Mittelwert = mean(x), 
                                            SD = sd(x), 
                                            Minimum = min(x), 
                                            Maximum = max(x),
                                            Median = median(x), 
                                            IQR = IQR(x)))
# Ergebnisse schöner darstellen
deskriptiv <- do.call(data.frame, deskriptiv)

# Ergebnisse anzeigen
print(deskriptiv)
#   Wirtsvogelart Länge.Mittelwert  Länge.SD Länge.Minimum Länge.Maximum Länge.Median Länge.IQR
# 1            WP         22.15444 0.9785670         19.65         24.45        21.95      1.10
# 2            BP         23.09333 0.9051177         21.05         24.10        23.35      1.20
# 3            RK         22.59375 0.6896557         21.05         23.85        22.60      0.95
# 4            ZK         21.12667 0.7391759         19.85         22.25        21.05      0.90

# Namen für die Legende
legenden_labels <- c(
  "WP" = "Wiesenpieper (WP)",
  "BP" = "Baumpieper (BP)",
  "RK" = "Rotkehlchen (RK)",
  "ZK" = "Zaunkönig (ZK)"
)

# Boxplot erstellen
library(ggplot2)

ggplot(daten_cleaned, aes(x = Wirtsvogelart, y = Länge, fill = Wirtsvogelart)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 21, outlier.size = 3) + # Ausreißer hervorheben
  scale_fill_manual(
    values = c("WP" = "#6baed6", "BP" = "#74c476", "RK" = "#fc9272", "ZK" = "#fdbe85"),
    labels = legenden_labels # Legendenbeschriftungen hinzufügen
  ) +
  labs(x = "Wirtsvogelart", y = "Länge der Kuckuckseier (mm)", fill = "Wirtsvogelart") + # Beschriftung
  theme_gray(base_size = 14) + # Graues Standardthema mit angepasster Schriftgröße
  theme(
    axis.title = element_text(face = "bold"), # Achsentitel fett
    legend.title = element_text(face = "bold", size = 16), # Legendentitel fett und Schriftgröße vergrößert
    legend.text = element_text(size = 14), # Legendentext größer
    plot.title = element_blank() # Kein Titel für den Graph
  )




### Frage 1: Unterscheiden sich die Kuckuckseier, die in den Nestern der verschiedenen Wirtsvögel
### gefunden wurden, in ihrer Länge?

### Voraussetzungen:

## Unabhängigkeit: Inhaltlich begründen

## Normalverteilung: Besser mit dem QQ-PLot
# Normalverteilung für jede Gruppe prüfen

# Daten laden
data <- read.csv("C:/Users/ahmed/OneDrive/Bureau/Kuckuckseier_cleaned.csv")

# Vogelarten mit vollständigen Namen definieren
gruppen <- unique(data$Gruppe)
gruppen_namen <- c("WP" = "Wiesenpieper (WP)", 
                   "BP" = "Baumpieper (BP)", 
                   "RK" = "Rotkehlchen (RK)", 
                   "ZK" = "Zaunkönig (ZK)")  # Beispiel: Kürzel und vollständige Namen

par(mfrow = c(2, 2), mar = c(2, 2, 1, 1))

# QQ-Plots erstellen
for (gruppe in gruppen) {
  # Subset der Daten für die aktuelle Gruppe
  gruppe_data <- data[data$Gruppe == gruppe, "Länge"]
  
  # QQ-Plot mit vollständigem Namen in der Überschrift
  qqnorm(gruppe_data, 
         main = paste("QQ-Plot:", gruppen_namen[gruppe]), 
         xlab = "Theoretische Quantile", 
         ylab = "Beobachtete Quantile")
  qqline(gruppe_data, col = "red", lwd = 2)  # Linie für Normalverteilung
}





##############################################



## Varinanzhomogenität mit Varianzen/ SD berechnen und dann die vergleichen

##################################
library(dplyr)
summary_stats <- daten_cleaned %>%
  group_by(Wirtsvogelart) %>%
  summarise(
    Varianz = var(Länge, na.rm = TRUE),
    Standardabweichung = sd(Länge, na.rm = TRUE)
  )
##################################

# ANOVA
anova_modell <- aov(Länge ~ Wirtsvogelart, data = daten_cleaned)

# ANOVA-Ergebnisse anzeigen
summary(anova_modell)
# p-Wert: 2.65e-07

# Die Nullhypothese, dass es keinen signifikanten Unterschied gibt, wird verworfen.
# Also es gibt einen signifikanten Unterschied

### Frage 2: Wenn es Unterschiede gibt, ist darüber hinaus von
### Interesse, bei welchen (je zwei) Wirtsvogelarten die Kuckuckseier sich in der Länge
### unterscheiden.

# Daten laden
library(readxl)
kuckuckseier <-read.csv("C:/Users/ahmed/OneDrive/Bureau/Kuckuckseier_cleaned.csv")
colnames(kuckuckseier) <- c("Gruppe", "Länge")

# Funktion zur Durchführung von ANOVA für spezifische Gruppen
perform_anova <- function(data, groups) {
  subset_data <- subset(data, Gruppe %in% groups)
  model <- aov(Länge ~ Gruppe, data = subset_data)
  return(summary(model))
}

# Hypothesen
hypotheses <- list(
  c("WP", "BP", "RK"),
  c("WP", "RK", "ZK"),
  c("WP", "BP", "ZK"),
  c("BP", "RK", "ZK")
)

# Ergebnisse für jede Hypothese berechnen
results <- lapply(hypotheses, function(groups) perform_anova(kuckuckseier, groups))

# Ergebnisse anzeigen
names(results) <- c("WP, BP, RK", "WP, RK, ZK", "WP, BP, ZK", "BP, RK, ZK")
results

# $WP, BP, RK
# p-Wert: 0.00305

# $WP, RK, ZK
# p-Wert: 4.39e-05

# $WP, BP, ZK
# p-Wert: 8.56e-07

# $BP, RK, ZK
# p-Wert: 4.43e-08





### Voraussetzungen:

## Unabhängigkeit der Beobachtungen
## Normalverteilung, bei der ersten Frage
## Varianzhomogenität, bei der ersten Frage




# Funktion zur Durchführung eines Zweistichproben-t-Tests
perform_t_test <- function(data, group1, group2, alpha) {
  subset1 <- data$Länge[data$Gruppe == group1]
  subset2 <- data$Länge[data$Gruppe == group2]
  
  # Durchführung des t-Tests
  test <- t.test(subset1, subset2, var.equal = TRUE) # Annahme gleicher Varianzen
  list(
    Hypothese = paste(group1, "vs", group2),
    p_Wert = test$p.value,
    Signifikanz = ifelse(test$p.value < alpha, "verwerfen", "Nicht verwerfen")
  )
}

# Hypothesenpaare definieren
hypothesen <- list(
  c("WP", "BP"),
  c("RK", "ZK"),
  c("WP", "RK"),
  c("BP", "ZK"),
  c("WP", "ZK"),
  c("BP", "RK")
)

# Ergebnisse berechnen
alpha <- 0.025 # Adjustiertes Alpha
results <- lapply(hypothesen, function(hyp) perform_t_test(kuckuckseier, hyp[1], hyp[2], alpha))

# Ergebnisse anzeigen
results_df <- do.call(rbind, lapply(results, as.data.frame))
print(results_df)

# Hypothese       p_Wert     Signifikanz
# 1  WP vs BP 0.0018         verwerfen
# 2  RK vs ZK 3.47e-06       verwerfen
# 3  WP vs RK 0.1039         Nicht verwerfen
# 4  BP vs ZK 4.59e-07       verwerfen
# 5  WP vs ZK 0.0005         verwerfen
# 6  BP vs RK 0.0933         Nicht verwerfen

# Alle Hypothesenpaare werden verworfen.
# Es werden die nächsten Durchschnittshypothesen betrachtet.


# Datensatz laden
kuckuckseier <- read.csv("C:/Users/ahmed/OneDrive/Bureau/Kuckuckseier_cleaned.csv")
colnames(kuckuckseier) <- c("Gruppe", "Länge")

# Funktion zur Durchführung eines Zweistichproben-t-Tests
perform_t_test <- function(data, group1, group2, alpha) {
  subset1 <- data$Länge[data$Gruppe == group1]
  subset2 <- data$Länge[data$Gruppe == group2]
  
  # Durchführung des t-Tests
  test <- t.test(subset1, subset2, var.equal = TRUE) # Annahme gleicher Varianzen
  result <- list(
    Hypothese = paste(group1, "vs", group2),
    p_Wert = test$p.value,
    Signifikanz = ifelse(test$p.value < alpha, "verwerfen", "Nicht verwerfen")
  )
  return(result)
}

# Hypothesenpaare definieren
hypothesen <- list(
  c("WP", "BP"),
  c("WP", "RK"),
  c("WP", "ZK"),
  c("BP", "RK"),
  c("BP", "ZK"),
  c("RK", "ZK")
)

# Ergebnisse berechnen
alpha <- 0.05 # Lokales Signifikanzniveau
results <- lapply(hypothesen, function(hyp) perform_t_test(kuckuckseier, hyp[1], hyp[2], alpha))

# Ergebnisse in einem DataFrame formatieren
results_df <- do.call(rbind, lapply(results, as.data.frame))

# Ergebnisse anzeigen
print(results_df)

# Hypothese       p_Wert     Signifikanz
# 1  WP vs BP   0.0018         verwerfen
# 2  WP vs RK   0.1039         Nicht verwerfen
# 3  WP vs ZK   0.0005         verwerfen
# 4  BP vs RK   0.0933         Nicht verwerfen
# 5  BP vs ZK   4.59e-07       verwerfen
# 6  RK vs ZK   3.47e-06       verwerfen

# Für alle betrachteten Paare außer bei Wiesenpiepern vs. Rotkehlchen und
# Baumpiepern vs. Rotkehlchen wird H0 verworfen.


# Datensatz laden
kuckuckseier <- read.csv("C:/Users/ahmed/OneDrive/Bureau/Kuckuckseier_cleaned.csv")
colnames(kuckuckseier) <- c("Gruppe", "Länge")

# Funktion zur Durchführung eines Zweistichproben-t-Tests
perform_t_test <- function(data, group1, group2) {
  subset1 <- data$Länge[data$Gruppe == group1]
  subset2 <- data$Länge[data$Gruppe == group2]
  
  # Durchführung des t-Tests
  test <- t.test(subset1, subset2, var.equal = TRUE) # Annahme gleicher Varianzen
  return(test$p.value)
}

# Hypothesenpaare definieren
hypothesen <- list(
  c("WP", "BP"),
  c("WP", "RK"),
  c("WP", "ZK"),
  c("BP", "RK"),
  c("BP", "ZK"),
  c("RK", "ZK")
)

# p-Werte berechnen
p_values <- sapply(hypothesen, function(hyp) perform_t_test(kuckuckseier, hyp[1], hyp[2]))

# Bonferroni-Holm-Verfahren anwenden
adjusted_p_values <- p.adjust(p_values, method = "holm")

# Ergebnisse anzeigen
results <- data.frame(
  Hypothese = sapply(hypothesen, function(hyp) paste(hyp[1], "vs", hyp[2])),
  Original_p_Wert = p_values,
  Adjustierter_p_Wert = adjusted_p_values,
  Signifikanz = ifelse(adjusted_p_values < 0.05, "Signifikant", "Nicht signifikant")
)

print(results)
# Hypothese Original_p_Wert Adjustierter_p_Wert       Signifikanz
# 1  WP vs BP    1.781444e-03        5.344333e-03       Signifikant
# 2  WP vs RK    1.039258e-01        1.865269e-01 Nicht signifikant
# 3  WP vs ZK    4.502217e-04        1.800887e-03       Signifikant
# 4  BP vs RK    9.326347e-02        1.865269e-01 Nicht signifikant
# 5  BP vs ZK    4.594980e-07        2.756988e-06       Signifikant
# 6  RK vs ZK    3.468876e-06        1.734438e-05       Signifikant

# Zum Niveau α = 0.05 zeigen sich bei allen Paaren außer bei Wiesenpiepern vs.
# Rotkehlchen und Baumpiepern vs. Rotkehlchen signifikante Unterschiede.
