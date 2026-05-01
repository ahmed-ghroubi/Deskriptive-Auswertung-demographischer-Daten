daten <- read.csv("C:\\Users\\salim\\Downloads\\Fallstudien 1\\Projekt_1\\census_2022_2002.csv")
daten
daten_2022 <- daten[which(daten$Year == 2022),]
daten_2022 <- as.data.frame(daten_2022)
daten_2022$Subregion

## Frage 1:
'library(dplyr)
# Summary statistics for Life Expectancy (Overall, Male, Female) and Total Fertility Rate
summary_stats <- daten_2022 %>%
  summarise(
    mean_life_expectancy_overall = mean(Life_Expectancy_Overall, na.rm = TRUE),
    median_life_expectancy_overall = median(Life_Expectancy_Overall, na.rm = TRUE),
    mean_life_expectancy_male = mean(Life_Expectancy_Male, na.rm = TRUE),
    median_life_expectancy_male = median(Life_Expectancy_Male, na.rm = TRUE),
    mean_life_expectancy_female = mean(Life_Expectancy_Female, na.rm = TRUE),
    median_life_expectancy_female = median(Life_Expectancy_Female, na.rm = TRUE),
    mean_fertility_rate = mean(Total_Fertility_Rate, na.rm = TRUE),
    median_fertility_rate = median(Total_Fertility_Rate, na.rm = TRUE)
  )
summary_stats'

library(ggplot2)


# Histogramm fuer Lebenserwartung der Maenner in 2022
ggplot(data = daten_2022, aes(x = Life_Expectancy_Male)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "lightblue", 
                 color = "darkblue", 
                 alpha = 0.7) +
  labs(
    x = "Lebenserwartung (in Jahren)",
    y = "Dichte") +
  theme_gray() +  # Verwende theme_gray()
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(t = 10, b = 10)),
    axis.title.x = element_text(face = "bold", size = 18),  # Vergrößere die x-Achsentitel auf Größe 16
    axis.title.y = element_text(face = "bold", size = 18),  # Vergrößere die y-Achsentitel auf Größe 16
    axis.text.x = element_text(size = 14),  # Vergrößere den Text der x-Achsenbeschriftung auf Größe 14
    axis.text.y = element_text(size = 14)   # Vergrößere den Text der y-Achsenbeschriftung auf Größe 14
  )




# Histogramm fuer Lebenserwartung der Frauen in 2022
ggplot(data = daten_2022, aes(x = Life_Expectancy_Female)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "lightpink", 
                 color = "black", 
                 alpha = 0.7) +
  labs(
    x = "Lebenserwartung (in Jahren)",
    y = "Dichte") +
  theme_gray() +  # Verwende theme_gray()
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(t = 10, b = 10)),
    axis.title.x = element_text(face = "bold", size = 18),  # Vergrößere die x-Achsentitel auf Größe 16
    axis.title.y = element_text(face = "bold", size = 18),  # Vergrößere die y-Achsentitel auf Größe 16
    axis.text.x = element_text(size = 14),  # Vergrößere den Text der x-Achsenbeschriftung auf Größe 14
    axis.text.y = element_text(size = 14)   # Vergrößere den Text der y-Achsenbeschriftung auf Größe 14
  )









'# Histogramm fuer Lebenserwartung der Maenner in 2022
hist1 <- ggplot(data = daten_2022, aes(x = Life_Expectancy_Male)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "lightblue", 
                 color = "darkblue", 
                 alpha = 0.7) +
  labs(
    x = "Lebenserwartung in Jahren",
    y = "Dichte") +
  theme_gray() +  # Verwende theme_gray()
  theme(plot.title = element_text(hjust = 0.5, size = 14, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Histogramm fuer Lebenserwartung der Frauen in 2022
hist2 <- ggplot(data = daten_2022, aes(x = Life_Expectancy_Female)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "lightpink", 
                 color = "black", 
                 alpha = 0.7) +
  labs(
    x = "Lebenserwartung in Jahren",
    y = "Dichte") +
  theme_gray() +  # Verwende theme_gray()
  theme(plot.title = element_text(hjust = 0.5, size = 14, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

'


## Frage 2:

# Daten für das Jahr 2022 filtern
daten_2022 <- subset(daten, Year == 2022)

# Fehlende Werte entfernen
daten_2022 <- na.omit(daten_2022)

ggplot(daten_2022, aes(x = Total_Fertility_Rate, y = Life_Expectancy_Overall, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +  
  labs(x = "Fertilitätsrate (in Kinder pro Frau)", 
       y = "Allgemeine Lebenserwartung (in Jahren)", 
       color = "Region") +  
  scale_color_discrete(labels = c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")) +
  theme_gray() +  
  theme(plot.title = element_blank(),  # Titel entfernen
        legend.position = "right",
        axis.title = element_text(face = "bold", size = 15),  # Achsentitel größer machen
        axis.text = element_text(face = "bold", size = 11),  # Achsentext größer machen
        legend.text = element_text(size = 14),  # Legendentext größer machen
        legend.title = element_text(face = "bold", size = 14))  # Legendentitel größer machen

## Frage 3:


library(ggplot2)
library(dplyr)


daten_2022 <- daten_2022 %>%
  mutate(
    Region = factor(Region, levels = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                    labels = c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")),
    Subregion = factor(Subregion, 
                       levels = c("Northern Africa", "Western Africa", "Eastern Africa", "Southern Africa", "Middle Africa",  # Afrika
                                  "Northern America", "South America", "Central America", "Caribbean",                      # Amerika
                                  "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia", "South-Central Asia", # Asien
                                  "Western Europe", "Northern Europe", "Eastern Europe", "Southern Europe",                # Europa
                                  "Australia/New Zealand", "Polynesia", "Micronesia", "Melanesia"),                       # Ozeanien
                       labels = c("Nordafrika", "Westafrika", "Ostafrika", "Südliches Afrika", "Mittleres Afrika",
                                  "Nordamerika", "Südamerika", "Mittelamerika", "Karibik",
                                  "Ostasien", "Südostasien", "Südasien", "Westasien", "Süd-Zentralasien",
                                  "Westeuropa", "Nordeuropa", "Osteuropa", "Südeuropa",
                                  "Australien/Neuseeland", "Polynesien", "Mikronesien", "Melanesien"))
  )

# Create the plot with ggplot2
ggplot(daten_2022, aes(x = Subregion, y = Life_Expectancy_Overall, fill = Region)) + 
  geom_boxplot(width = 0.6) +  # Narrower boxplots for more space
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "italic"),  # Rotate and format x-axis labels
    axis.text.y = element_text(size = 12),  # Increase y-axis label size
    axis.title.x = element_text(face = "bold", size = 14),  # Bold and enlarge x-axis title
    axis.title.y = element_text(face = "bold", size = 12),  # Bold and enlarge y-axis title
    legend.title = element_text(face = "bold", size = 12),  # Format legend title
    legend.text = element_text(size = 12)  # Increase legend text size
  ) +  
  labs(x = "Subregion", y = "Allgemeine Lebenserwartung (in Jahren)") + 
  scale_fill_brewer(palette = "Set3") +  # Colors for regions
  scale_x_discrete(expand = expansion(add = c(0.2, 0.2)))





###############################################

## Frage 4:

# Erstellen des Streudiagramms mit ggplot2
library(ggplot2)
library(dplyr)

# Regionen auf Deutsch umbenennen und in der gewünschten Reihenfolge setzen
daten$Region <- factor(daten$Region, 
                       levels = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                       labels = c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien"))

# Erstellen des Streudiagramms mit ggplot2
ggplot(daten, aes(x = Total_Fertility_Rate, y = Life_Expectancy_Overall, color = Region)) + 
  geom_point(size = 3, alpha = 0.6, shape = 16) +  # Verwenden von Kreisen für beide Jahre
  facet_wrap(~ Year) +  # Zwei Plots, einen für jedes Jahr
  labs(x = "Fertilitätsrate (in Kinder pro Frau)", 
       y = "Allgemeine Lebenserwartung (in Jahren)", 
       color = "Region") +  # Entfernen des "shape" Eintrags in der Legende
  theme_gray() +
  theme(
    axis.title.x = element_text(face = "bold", size = 18),  # Größere Schrift für X-Achse
    axis.title.y = element_text(face = "bold", size = 17),  # Größere Schrift für Y-Achse
    axis.text = element_text(size = 16),  # Größere Schrift für Achsenbeschriftungen
    legend.title = element_text(face = "bold", size = 14),  # Vergrößert und fettet den Legendentitel
    legend.text = element_text(size = 17)  # Vergrößert den Text der Legende
  )


####### Anhang:

# Erstelle das Diagramm mit ggplot2
ggplot(daten_2022, aes(x = Subregion, y = Total_Fertility_Rate, fill = Region)) + 
  geom_boxplot(width = 0.6) +  # Engere Boxplots für mehr Platz
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "italic"),  # Rotate and format x-axis labels
    axis.text.y = element_text(size = 12),  # Increase y-axis label size
    axis.title.x = element_text(face = "bold", size = 14),  # Bold and enlarge x-axis title
    axis.title.y = element_text(face = "bold", size = 12),  # Bold and enlarge y-axis title
    legend.title = element_text(face = "bold", size = 12),  # Format legend title
    legend.text = element_text(size = 12)  # Increase legend text size
  ) +  
  labs(x = "Subregion", y = "Fertilitätsrate (in Kinder pro Frau)") + 
  scale_fill_brewer(palette = "Set3") +  # Farben für Regionen
  scale_x_discrete(expand = expansion(add = c(0.2, 0.2)))  # Platz auf beiden Seiten der x-Achse hinzufügen


