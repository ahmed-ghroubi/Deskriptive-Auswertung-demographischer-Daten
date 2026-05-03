rm(list = ls())
# Laden der notwendigen Bibliotheken
library(tidyr)
library(readxl)
library(dplyr)

# Einlesen der Daten
daten <- readxl::read_xlsx("C:\\Users\\aktmu\\OneDrive\\Desktop\\Fallstudien1\\Projekt4\\Medaillen.xlsx")
# Umbenennen der Spalten
colnames(daten)[colnames(daten) == "Sportart"] <- "Sportartgruppen"
### Frage 1:

library(readxl)
library(ggplot2)
library(reshape2)


# Erstellen der Kontingenztabelle
contingency_table <- xtabs(Total ~ Land + Sportartgruppen, data = daten)
print(contingency_table)
#Funktion zum Berechnen der erw Haeufigkeiten
compute_expected_frequencies <- function(data) {
  # Prüfen, ob die Eingabe eine Kontingenztabelle (Matrix oder Tabelle) der Form m*k ist
  if (!is.matrix(data) && !is.table(data)) {
    stop("Die Eingabe muss eine Kontingenztabelle (Matrix oder Tabelle) der Form m*k sein.")
  }
  
  # Berechnen der Zeilen- und Spaltensummen
  row_totals <- rowSums(data)
  col_totals <- colSums(data)
  total <- sum(data)
  
  # Initialisieren einer Matrix für die erwarteten Häufigkeiten
  expected <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  
  # Berechnen der erwarteten Häufigkeiten
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      expected[i, j] <- (row_totals[i] * col_totals[j]) / total
    }
  }
  
  # Setzen von Zeilen- und Spaltennamen
  dimnames(expected) <- dimnames(data)
  
  return(expected)
}
print(compute_expected_frequencies(contingency_table))
# Chi-Quadrat-Test
chi_test <- chisq.test(contingency_table)

# Ergebnisse des Chi-Quadrat-Tests ausgeben
print(chi_test)

# Erwartete Werte ausgeben
print(chi_test$expected)
#Es besteht signfikante Abh zwischen Variablen

#Frage 2
# Filtern der Daten für Sportartgruppe Kampfsport
kampfsport_daten <- daten %>% filter(Sportartgruppen == "Kampfsport")

# Aufteilen der Medaillendaten nach Medaillenfarbe
Kampfsport_kontingenz <- kampfsport_daten %>% 
  select(Land, NrGold, NrSilber, NrBronze) %>% 
  pivot_longer(cols = starts_with("Nr"), 
               names_to = "Medaillenfarbe", 
               values_to = "Anzahl") %>%
  mutate(Medaillenfarbe = recode(Medaillenfarbe, 
                                 NrGold = "Gold", 
                                 NrSilber = "Silber", 
                                 NrBronze = "Bronze"))

# Erstellen der Kontingenztafel
kontingenztafel_Kampfsport <- xtabs(Anzahl ~ Land + Medaillenfarbe, data = Kampfsport_kontingenz)

# Anzeigen der Kontingenztafel
print("Kontingenztafel für die Sportartgruppe Kampfsport")
print(kontingenztafel_Kampfsport)
compute_expected_frequencies(kontingenztafel_Kampfsport)

# Chi-Quadrat-Test zur Untersuchung der Abhängigkeit
fisher_test1 <- fisher.test(kontingenztafel_Kampfsport)
p.adjust(fisher_test1$p.value, method = "holm")




#Filtern der Daten um die Frage  fuer Leichtathletik zu filtern
leichtathletik_daten <- daten %>% filter(Sportartgruppen == "Leichtathletik")

# Aufteilen der Medaillendaten nach Medaillenfarbe
Leichtathletik_kontingenz <- leichtathletik_daten %>% 
  select(Land, NrGold, NrSilber, NrBronze) %>% 
  pivot_longer(cols = starts_with("Nr"), 
               names_to = "Medaillenfarbe", 
               values_to = "Anzahl") %>%
  mutate(Medaillenfarbe = recode(Medaillenfarbe, 
                                 NrGold = "Gold", 
                                 NrSilber = "Silber", 
                                 NrBronze = "Bronze"))

# Erstellen der Kontingenztafel
kontingenztafel_Leichtathletik <- xtabs(Anzahl ~ Land + Medaillenfarbe, data = Leichtathletik_kontingenz)
compute_expected_frequencies(kontingenztafel_Leichtathletik)
# Anzeigen der Kontingenztafel
print("Kontingenztafel für die Sportartgruppe Kampfsport")
print(kontingenztafel_Leichtathletik)

# Chi-Quadrat-Test zur Untersuchung der Abhängigkeit
fisher_test2 <- fisher.test(kontingenztafel_Leichtathletik)
p.adjust(fisher_test2$p.value, method = "holm")



#Filtern der Daten um die Frage  fuer Ballsportart zu filtern
ballsportart_daten <- daten %>% filter(Sportartgruppen == "Ballsportart")

# Aufteilen der Medaillendaten nach Medaillenfarbe
Ballsportart_kontingenz <- ballsportart_daten %>% 
  select(Land, NrGold, NrSilber, NrBronze) %>% 
  pivot_longer(cols = starts_with("Nr"), 
               names_to = "Medaillenfarbe", 
               values_to = "Anzahl") %>%
  mutate(Medaillenfarbe = recode(Medaillenfarbe, 
                                 NrGold = "Gold", 
                                 NrSilber = "Silber", 
                                 NrBronze = "Bronze"))

# Erstellen der Kontingenztafel
kontingenztafel_Ballsportart <- xtabs(Anzahl ~ Land + Medaillenfarbe, data = Ballsportart_kontingenz)
compute_expected_frequencies(kontingenztafel_Ballsportart)
# Anzeigen der Kontingenztafel
print("Kontingenztafel für die Sportartgruppe Kampfsport")
print(kontingenztafel_Ballsportart)

# Chi-Quadrat-Test zur Untersuchung der Abhängigkeit
fisher_test3 <- fisher.test(kontingenztafel_Ballsportart)
p.adjust(fisher_test3$p.value)



daten

#Filtern der Daten um die Frage  fuer Ballsportart zu filtern
schwimmen_daten <- daten %>% filter(Sportartgruppen == "Schwimmen")

# Aufteilen der Medaillendaten nach Medaillenfarbe
Schwimmen_kontingenz <- schwimmen_daten %>% 
  select(Land, NrGold, NrSilber, NrBronze) %>% 
  pivot_longer(cols = starts_with("Nr"), 
               names_to = "Medaillenfarbe", 
               values_to = "Anzahl") %>%
  mutate(Medaillenfarbe = recode(Medaillenfarbe, 
                                 NrGold = "Gold", 
                                 NrSilber = "Silber", 
                                 NrBronze = "Bronze"))

# Erstellen der Kontingenztafel
kontingenztafel_Schwimmen <- xtabs(Anzahl ~ Land + Medaillenfarbe, data = Schwimmen_kontingenz)
compute_expected_frequencies(kontingenztafel_Schwimmen)
# Anzeigen der Kontingenztafel
print("Kontingenztafel für die Sportartgruppe Kampfsport")
print(kontingenztafel_Schwimmen)

# Chi-Quadrat-Test zur Untersuchung der Abhängigkeit
fisher_test4 <- fisher.test(kontingenztafel_Schwimmen)
p.adjust(fisher_test4$p.value, method = "holm")


rm(list = ls())
#Frage 3
# Laden der notwendigen Bibliotheken
library(readxl)
library(dplyr)

# Einlesen der Daten
daten <- readxl::read_xlsx("C:\\Users\\aktmu\\OneDrive\\Desktop\\Fallstudien1\\Projekt4\\Medaillen.xlsx")

# Umbenennen der Spalten
colnames(daten)[colnames(daten) == "Sportart"] <- "Sportartgruppen"





# Funktion zur Erstellung und Analyse der Kontingenztafel mit Fisher-Test
analyze_country_fisher <- function(country_name, data) {
  # Filtern der Daten für das spezifische Land
  country_data <- data %>% filter(Land == country_name)
  
  # Aufteilen der Medaillendaten nach Medaillenfarbe
  country_kontingenz <- country_data %>% 
    select(Sportartgruppen, NrGold, NrSilber, NrBronze) %>% 
    pivot_longer(cols = starts_with("Nr"), 
                 names_to = "Medaillenfarbe", 
                 values_to = "Anzahl") %>%
    mutate(Medaillenfarbe = recode(Medaillenfarbe, 
                                   NrGold = "Gold", 
                                   NrSilber = "Silber", 
                                   NrBronze = "Bronze"),
           # Reihenfolge der Medaillenfarben festlegen
           Medaillenfarbe = factor(Medaillenfarbe, levels = c("Gold", "Silber", "Bronze")))
  
  # Erstellen der Kontingenztafel
  kontingenztafel <- xtabs(Anzahl ~ Sportartgruppen + Medaillenfarbe, data = country_kontingenz)
  
  # Anzeigen der Kontingenztafel
  print(paste("Kontingenztafel für", country_name, ":"))
  print(kontingenztafel)
  
  # Fisher-Exakt-Test zur Untersuchung der Abhängigkeit
  fisher_test <- fisher.test(kontingenztafel)
  
  # Ergebnisse des Fisher-Tests anzeigen
  print(paste("Ergebnisse des Fisher-Exakt-Tests für", country_name, ":"))
  print(fisher_test)
}

# Liste der Länder
countries <- unique(daten$Land)

# Analyse für jedes Land mit Fisher-Test
for (country in countries) {
  analyze_country_fisher(country, daten)
}




#Erstellen der Abbildung 1
# Laden der notwendigen Pakete
library(readxl)

# Excel-Datei laden
data <- read_excel("C:\\Users\\aktmu\\OneDrive\\Desktop\\Fallstudien1\\Projekt4\\Medaillen.xlsx")

# Sportarten und Länder extrahieren
sports <- unique(data$Sportart)
countries <- unique(data$Land)

# Layout der Diagramme definieren
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (sport in sports) {
  # Filter für die aktuelle Sportart
  subset <- data[data$Sportart == sport, ]
  
  # Sicherstellen, dass alle Länder enthalten sind, auch wenn sie keine Medaillen haben
  subset <- merge(data.frame(Land = countries), subset, by = "Land", all.x = TRUE)
  
  # Fehlende Werte durch 0 ersetzen
  subset[is.na(subset)] <- 0
  
  # Werte für die Balkendiagramme vorbereiten
  gold <- subset$NrGold
  silver <- subset$NrSilber
  bronze <- subset$NrBronze
  
  # X-Achse Werte für die Länder
  bar_positions <- barplot(matrix(c(gold, silver, bronze), nrow = 3, byrow = TRUE),
                           beside = TRUE,
                           col = c("gold", "gray", "#CD7F32"),  # Farben der Balken
                           border = "black",                    # Schwarze Umrandung
                           lwd = 1,                             # Dickere Umrandung für Säulen
                           ylim = c(0, 17),
                           ylab = expression(bold("Anzahl Medaillen")),
                           xlab = expression(bold("Land")),
                           main = sport,
                           cex.names = 0.8)                     # Schriftgröße der Ländernamen
  
  # Ländernamen schräg darstellen
  text(x = colMeans(bar_positions), y = par("usr")[3] - 0.5, 
       labels = subset$Land, srt = 45, adj = 1, xpd = TRUE, cex = 1)
  
  # Füge die Legende nur für Leichtathletik hinzu
  if (sport == "Kampfsport") {
    legend("topright",
           legend = c("Gold", "Silber", "Bronze"),
           fill = c("gold", "gray", "#CD7F32"),
           cex = 0.716,
           bty = "o",
           text.font = 1)
  }
}



