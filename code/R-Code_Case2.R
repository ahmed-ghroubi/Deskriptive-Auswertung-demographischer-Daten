# Initialisierung
rm(list = ls())

# Einlesen der Daten (angepasst für das neue Dateiformat)
daten <- read.delim("~/Downloads/Konzentrationsdaten.txt")

# Setzen der fehlenden Werte für id == 14 auf NA
daten[daten$id == 14, ] <- NA

# Für Aufgabe 1: Daten aus dem ersten Durchgang
daten_first_try <- daten[daten$durchgang == 1, ]
daten_first_try_gu <- daten_first_try[daten_first_try$test_typ == "gu", ]
daten_first_try_ug <- daten_first_try[daten_first_try$test_typ == "ug", ]

# Frage 1: Shapiro-Wilk-Test
shapiro.test(daten_first_try_gu$KL)
shapiro.test(daten_first_try_ug$KL)

# Für Aufgabe 2: Differenzen der Werte zwischen erstem und zweitem Durchgang
daten_sec_try <- daten[daten$durchgang == 2, ]
daten_diff_KL <- daten_sec_try$KL - daten_first_try$KL
daten_diff_B <- daten_sec_try$B - daten_first_try$B

# Überprüfung der Normalverteilung
shapiro.test(daten_diff_KL)
shapiro.test(daten_diff_B)

# Einseitige t-Tests
t.test(daten_diff_KL, alternative = "greater")
t.test(daten_diff_B, alternative = "less")

# Für Aufgabe 3: Vergleich zwischen Gruppen
daten <- na.omit(daten)  # Entfernen von NA-Werten
daten_first_group <- daten[daten$gruppe == 1, ]
daten_sec_group <- daten[daten$gruppe == 2, ]

daten_first_group_diff <- daten_first_group[daten_first_group$durchgang == 2, ]$KL - 
  daten_first_group[daten_first_group$durchgang == 1, ]$KL

daten_sec_group_diff <- daten_sec_group[daten_sec_group$durchgang == 2, ]$KL - 
  daten_sec_group[daten_sec_group$durchgang == 1, ]$KL

# Varianzvergleich und t-Test
var.test(daten_first_group_diff, daten_sec_group_diff)
t.test(daten_first_group_diff, daten_sec_group_diff, 
       alternative = "greater", var.equal = TRUE)

# Univariate Analyse der numerischen Variablen
summary_stats <- function(variable) {
  list(
    Mittelwert = mean(variable, na.rm = TRUE),
    Median = median(variable, na.rm = TRUE),
    Standardabweichung = sd(variable, na.rm = TRUE),
    Minimum = min(variable, na.rm = TRUE),
    Maximum = max(variable, na.rm = TRUE)
  )
}

beschreibung <- list(
  Bearbeitungszeit_B = summary_stats(daten$B),
  Richtige_Antworten_AR = summary_stats(daten$AR),
  Falsche_Antworten_AA = summary_stats(daten$AA),
  Verpasste_Antworten_AF = summary_stats(daten$AF),
  Konzentrationsscore_KL = summary_stats(daten$KL)
)

# Ergebnisse anzeigen
beschreibung

# Histogramme erstellen
library(ggplot2)

ggplot(daten, aes(x = B)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 10) +
  labs(x = "Bearbeitungszeit (Sekunden)", y = "Absolute Häufigkeit") +
  theme_gray(base_size = 15) +
  theme(axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 14))

ggplot(daten, aes(x = KL)) +
  geom_histogram(fill = "lightyellow", color = "black", bins = 10) +
  labs(x = "Konzentrationsscore (korrekte Zeichen/Min)", 
       y = "Absolute Häufigkeit") +
  theme_gray(base_size = 15) +
  theme(axis.title = element_text(size = 22, face = "bold"), 
        axis.text = element_text(size = 14))
