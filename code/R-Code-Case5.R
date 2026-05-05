library(tidyverse)
library(patchwork)
library(caret)
library(pROC)
library(ggplot2)
library(leaps)
library(bestglm)





### Frage 1

# Loading the dataset and preprocessing some of the variables
df <- read_delim(
  "C:\\Users\\salim\\Downloads\\Fallstudien 1\\Projekt_5\\US_election_2024.csv",
  ";",
  locale = locale(decimal_mark = ",")
) |>
  janitor::clean_names() |>
  mutate(
    state = tolower(state),
    target = ifelse(leading_candidate == "Trump", 0, 1)
  )


df$unemployment_rate <- df$unemployment_rate * 100
df$health_insurance_coverage <- df$health_insurance_coverage * 100



# Summarizing the dataset
summary(select(df, -state))
round(sapply(select(df, -c(state, leading_candidate)), sd), 4)

df_standardized <- df |>
  mutate(
    total_area = log(total_area),
    population = log(population),
    target = ifelse(leading_candidate == "Trump", 0, 1)
  )

df_standardized <- dplyr::select(df_standardized, -population_density)

# Logistic regression model
logit_model <- glm(
  target ~
    total_area +
    population +
    median_age +
    birth_rate +
    hdi +
    unemployment_rate +
    health_insurance_coverage +
    median_rent,
  data = df_standardized,
  family = "binomial"
)
summary(logit_model)
# Call:
#   glm(formula = target ~ total_area + population + median_age + 
#         birth_rate + hdi + unemployment_rate + health_insurance_coverage + 
#         median_rent, family = "binomial", data = df_standardized)
# 
# Coefficients:
#                              Estimate Std. Error z value Pr(>|z|)
# (Intercept)               -1.325e+02  8.332e+01  -1.590    0.112
# total_area                -1.060e+00  1.495e+00  -0.709    0.478
# population                -1.667e+00  1.071e+00  -1.557    0.119
# median_age                 2.686e-01  5.962e-01   0.450    0.652
# birth_rate                -5.353e-02  2.768e-01  -0.193    0.847
# hdi                        6.299e+01  6.179e+01   1.019    0.308
# unemployment_rate          2.392e+02  1.733e+02   1.381    0.167
# health_insurance_coverage  9.046e+01  6.638e+01   1.363    0.173
# median_rent                7.343e-03  5.643e-03   1.301    0.193
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 68.310  on 50  degrees of freedom
# Residual deviance: 16.198  on 42  degrees of freedom
# AIC: 34.198
# 
# Number of Fisher Scoring iterations: 8

AIC(logit_model)
# 34.19821

### Frage 2
library(leaps)
library(bestglm)



# Installieren und laden Sie das glmulti-Paket
if (!require("glmulti")) install.packages("glmulti", dependencies=TRUE)
library(glmulti)

# Erstellen Sie ein glmulti-Objekt für die automatische Modellauswahl
glmulti_obj <- glmulti(target ~ total_area + population + median_age + birth_rate + hdi + 
                         unemployment_rate + health_insurance_coverage + median_rent, 
                       data = df_standardized, 
                       family = binomial(link="logit"),
                       crit = "aic",
                       method="h", # Hierarchische Kombination
                       level=1) # Maximal ein Prädiktor kann entfernt werden



# Anzeigen der besten Modelle
summary(glmulti_obj)

# Auswahl und Anpassung des besten Modells
best_model <- glmulti_obj@objects[[1]]  # Nehmen Sie das beste Modell
summary(best_model)
# Call:
#   fitfunc(formula = as.formula(x), family = ..1, data = data)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)               -1.431e+02  5.250e+01  -2.725  0.00642 **
#   population                -1.672e+00  9.192e-01  -1.819  0.06887 . 
# median_age                 3.783e-01  2.961e-01   1.278  0.20129   
# unemployment_rate          1.818e+00  1.274e+00   1.427  0.15345   
# health_insurance_coverage  1.404e+00  5.540e-01   2.534  0.01126 * 
#   median_rent                1.163e-02  4.348e-03   2.676  0.00745 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 68.310  on 50  degrees of freedom
# Residual deviance: 17.524  on 45  degrees of freedom
# AIC: 29.524
# 
# Number of Fisher Scoring iterations: 8



# Berechnung und Ausgabe des AIC-Wertes für das beste Modell
best_model_aic <- AIC(best_model)
print(best_model_aic)
# 29.5241



confint.default(best_model)
#                                2.5 %       97.5 %
#   (Intercept)               -2.459699e+02 -40.18578546
# population                -3.473717e+00   0.12929658
# median_age                -2.019457e-01   0.95863441
# unemployment_rate         -6.784163e+01 431.53963693
# health_insurance_coverage  3.182086e+01 248.97290664
# median_rent                3.113086e-03   0.02015508











### Frage 3

# Cross-validation setup
set.seed(123)
cv_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,
  savePredictions = "final" # Save predictions for ROC plotting
)

# Ensure the target variable is a factor
df_standardized$Leading_Candidate_Factor <- factor(
  df_standardized$target, 
  levels = c(0, 1), 
  labels = c("Trump", "Harris")
)

# Full model with cross-validation
cv_logit_model <- train(
  Leading_Candidate_Factor ~ total_area + population + median_age + birth_rate + hdi + 
    unemployment_rate + health_insurance_coverage + median_rent,
  data = df_standardized, 
  method = "glm", 
  family = "binomial", 
  trControl = cv_control, 
  metric = "ROC"
)

# Reduced (best) model with cross-validation
cv_best_model <- train(
  Leading_Candidate_Factor ~ health_insurance_coverage + median_rent,
  data = df_standardized, 
  method = "glm", 
  family = "binomial", 
  trControl = cv_control, 
  metric = "ROC"
)

# # Print Cross-Validated AUC for both models
# cat("Cross-Validated AUC (Logit Model): ", max(cv_logit_model$results$ROC), "\n")
# cat("Cross-Validated AUC (Best Model): ", max(cv_best_model$results$ROC), "\n")

# Generate ROC data from cross-validated predictions
roc_logit_cv <- roc(
  response = cv_logit_model$pred$obs,
  predictor = cv_logit_model$pred$Harris,
  levels = c("Trump", "Harris")
)

roc_best_cv <- roc(
  response = cv_best_model$pred$obs,
  predictor = cv_best_model$pred$Harris,
  levels = c("Trump", "Harris")
)

# Create dataframes for plotting ROC curves (Cross-Validation)
roc_logit_cv_df <- data.frame(
  TPR = c(0, rev(roc_logit_cv$sensitivities), 1), 
  FPR = c(0, rev(1 - roc_logit_cv$specificities), 1)
)

roc_best_cv_df <- data.frame(
  TPR = c(0, rev(roc_best_cv$sensitivities), 1), 
  FPR = c(0, rev(1 - roc_best_cv$specificities), 1)
)
# Add labels to each ROC dataframe
roc_logit_cv_df$Model <- "Logit Model"
roc_best_cv_df$Model <- "Best Model"

# Combine dataframes for plotting
roc_cv_df <- rbind(roc_logit_cv_df, roc_best_cv_df)


# Print Cross-Validated AUC values
auc_logit_cv <- auc(roc_logit_cv)
auc_best_cv <- auc(roc_best_cv)
# Plot ROC curves with geom_step for stepped appearance and AUC annotations
ggplot(roc_cv_df, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_step(linewidth = 0.7) +  # Stepped lines
  labs(
    title = "",
    x = "False positive rate",
    y = "True positive rate"
  ) +
  scale_color_manual(values = c("Logit Model" = "blue", "Best Model" = "red")) +
  scale_linetype_manual(values = c("Logit Model" = "dashed", "Best Model" = "solid")) +
  annotate(
    "text", 
    x = 0.2, 
    y = 0.2, 
    label = paste0("Logit Model AUC: ", round(auc_logit_cv, 3)),
    color = "blue",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "text", 
    x = 0.2, 
    y = 0.1, 
    label = paste0("Best Model AUC: ", round(auc_best_cv, 3)),
    color = "red",
    size = 4,
    hjust = 0
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14),  # Legendentitel vergrößern
    legend.text = element_text(size = 12),   # Legendentext vergrößern
    axis.title = element_text(size = 14, face = "bold"),  # Achsentitel vergrößern und fett
    axis.text = element_text(size = 12)  # Achsentext vergrößern
  )


cat("Cross-Validated AUC (Logit Model): ", auc_logit_cv, "\n")
# Cross-Validated AUC (Logit Model):  0.8620968 

cat("Cross-Validated AUC (Best Model): ", auc_best_cv, "\n")
# Cross-Validated AUC (Best Model):  0.933871 






########### Deskriptive Analyse ##################


# Load necessary library
library(ggplot2)

# Daten einlesen
daten <- read.csv("C:\\Users\\salim\\Downloads\\Fallstudien 1\\Projekt_5\\US_election_2024.csv", sep=";")

# Mapping der Variablennamen von Englisch auf Deutsch
namen_mapping <- c("Population_Density" = "Bevölkerungsdichte", 
                   "Median_Age" = "Median Alter", 
                   "HDI" = "HDI", 
                   "Unemployment_Rate" = "Arbeitslosenrate", 
                   "Health_Insurance_Coverage" = "Versicherungsrate", 
                   "Median_Rent" = "Mediane Miete",
                   "Total_Area"  = "Gesamtfläche",
                   "Population" = "Einwohnerzahl",
                   "Birth_Rate" = "Geburtenrate")

# Spaltennamen anpassen
colnames(daten) <- sapply(colnames(daten), function(x) ifelse(x %in% names(namen_mapping), namen_mapping[x], x))

# Spalten mit Komma als Dezimaltrennzeichen identifizieren
spalten_umwandeln <- c("Bevölkerungsdichte", "Median Alter", "HDI", "Arbeitslosenrate", 
                       "Versicherungsrate", "Mediane Miete", "Gesamtfläche", 
                       "Einwohnerzahl", "Geburtenrate")

# Dezimaltrennzeichen von Komma zu Punkt ändern und in numerische Werte umwandeln
daten[spalten_umwandeln] <- lapply(daten[spalten_umwandeln], function(x) as.numeric(gsub(",", ".", x)))

daten$Arbeitslosenrate <- daten$Arbeitslosenrate * 100
daten$Versicherungsrate <- daten$Versicherungsrate * 100

# Plotting histograms
par(mfrow=c(3,3))  # Sets the layout for 3x3 plots, with one empty space

# Set font size for axis labels and numbers
axis_label_size <- 1.4  # Adjust this as needed
axis_number_size <- 1.4  # Adjust this as needed
title_size <- 1.4

# Total Area
hist(daten$Gesamtfläche, main="Gesamtfläche ", ylab="Häufigkeit", col="lightblue", 
     xlab="Gesamtfläche (in 1000 sq mi)", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# Population
hist(daten$Einwohnerzahl, main="Einwohnerzahl", xlab="Einwohnerzahl (in Mio)", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# Empty plot for Population Density
plot.new()  # Creates an empty plot space

# Median Age
hist(daten$`Median Alter`, main="Medianes Alter", xlab="Medianes Alter (in Jahren)", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# Birth Rate
hist(daten$Geburtenrate, main="Geburtenrate", xlab="Geburtenrate (Anzahl Frauen)", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# HDI
hist(daten$HDI, main="HDI", xlab="Human Development Index", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# Unemployment Rate
hist(daten$Arbeitslosenrate, main="Arbeitslosenrate", xlab="Arbeitslosenrate (in %)", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# Health Insurance Coverage
hist(daten$Versicherungsrate, main="Versicherungsrate", xlab="Versicherungsrate (in %)", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)

# Median Rent
hist(daten$`Mediane Miete`, main="Median Miete", xlab="Mediane Miete (in US-Dollar)", 
     ylab="Häufigkeit", col="lightblue", breaks=10, cex.lab=axis_label_size, cex.axis=axis_number_size, cex.main=title_size)




# Balkendiagramm für 'Leading_Candidate' erstellen
ggplot(data, aes(x = Leading_Candidate, fill = Leading_Candidate)) +
  geom_bar(stat = "count", color = "black") +  # Zählfunktion und Randfarbe hinzufügen
  scale_fill_manual(values = c("Trump" = "#FF6666", "Harris" = "skyblue"), 
                    name = "Leading Candidate") +  # Hellrot für Trump und Himmelblau für Harris, Legendentitel anpassen
  theme_minimal(base_size = 16) +  # Minimalistisches Thema mit größerer Basisschrift
  theme(
    axis.title = element_text(size = 18),  # Größere Achsentitel
    axis.text = element_text(size = 16),  # Größere Achsentexte
    plot.title = element_blank(),  # Kein Titel
    panel.grid.major.x = element_blank(),  # Keine vertikalen Hauptgitterlinien
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"),  # Horizontale Hauptgitterlinien
    panel.grid.minor = element_blank(),  # Keine untergeordneten Gitterlinien
    axis.ticks = element_line(color = "black"),  # Schwarze Achsenticks
    legend.title = element_text(size = 16)  # Legendentitel Größe anpassen
  ) +
  labs(
    x = "Kandidat",  # X-Achsenbeschriftung auf Deutsch
    y = "Anzahl der Staaten",  # Y-Achsenbeschriftung auf Deutsch
    fill = "Leading Candidate"  # Legendentitel setzen
  ) +
  scale_y_continuous(breaks = seq(0, 50, 5), limits = c(0, 50))  # Y-Achse von 0 bis 50 mit Schritten von 5



########### Tabelle ############

# Ersetzen von Kommas durch Punkte und Umwandlung in numerische Werte
data <- data %>%
  mutate(across(where(is.character), ~ as.numeric(gsub(",", ".", .))))

# Auswahl der spezifischen Spalten für die deskriptive Analyse
data_selected <- data %>% 
  select(Total_Area, Population, Median_Age, Birth_Rate, HDI, Unemployment_Rate, Health_Insurance_Coverage, Median_Rent)

# Deskriptive Statistik für ausgewählte numerische Variablen berechnen
deskriptiv <- data_selected %>%
  summarise(across(everything(), 
                   list(Mittelwert = mean, 
                        SD = sd, 
                        Minimum = min, 
                        Maximum = max,
                        Median = median),
                   na.rm = TRUE))

# Ergebnisse als DataFrame umwandeln und anzeigen
deskriptiv <- do.call(data.frame, deskriptiv)
print(deskriptiv)
# Total_Area_Mittelwert Total_Area_SD Total_Area_Minimum
# 1              74445.96       96933.1                 68
# Total_Area_Maximum Total_Area_Median Population_Mittelwert
# 1             665384             56273               6499006
# Population_SD Population_Minimum Population_Maximum
# 1       7408023             576851           39538223
# Population_Median Median_Age_Mittelwert Median_Age_SD
# 1           4505836               39.4451      2.201937
# Median_Age_Minimum Median_Age_Maximum Median_Age_Median
# 1               32.3               44.9              39.3
# Birth_Rate_Mittelwert Birth_Rate_SD Birth_Rate_Minimum
# 1              52.01961      5.500873                 39
# Birth_Rate_Maximum Birth_Rate_Median HDI_Mittelwert     HDI_SD
# 1                 66                52      0.9199804 0.02462072
# HDI_Minimum HDI_Maximum HDI_Median Unemployment_Rate_Mittelwert
# 1       0.858       0.956      0.925                    0.0394902
# Unemployment_Rate_SD Unemployment_Rate_Minimum
# 1          0.007684719                     0.026
# Unemployment_Rate_Maximum Unemployment_Rate_Median
# 1                     0.055                     0.04
# Health_Insurance_Coverage_Mittelwert
# 1                            0.9281569
# Health_Insurance_Coverage_SD Health_Insurance_Coverage_Minimum
# 1                    0.0268152                             0.836
# Health_Insurance_Coverage_Maximum
# 1                             0.974
# Health_Insurance_Coverage_Median Median_Rent_Mittelwert
# 1                            0.935               1299.961
# Median_Rent_SD Median_Rent_Minimum Median_Rent_Maximum
# 1       311.7747                 850                1992
# Median_Rent_Median
# 1               1238





