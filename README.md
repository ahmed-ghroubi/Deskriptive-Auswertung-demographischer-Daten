# Data Analysis Case Studies

This repository contains five statistical case studies completed as part of the course **Fallstudien I**.  
The projects cover descriptive statistics, comparison of distributions, contingency table analysis, and logistic regression.

All analyses were carried out using R and include statistical methods, visualizations, interpretation of results, and final PDF reports.

## Overview

| Case Study | Topic | Main Statistical Focus |
|---|---|---|
| Case Study 1 | Demographic Data Analysis | Descriptive statistics, correlation, visual analysis |
| Case Study 2 | Comparison of Two Distributions | t-tests, Shapiro-Wilk test, F-test |
| Case Study 3 | Comparison of Several Distributions | ANOVA, pairwise t-tests, Bonferroni-Holm correction |
| Case Study 4 | Olympic Medal Table Analysis | Contingency tables, Chi-square test, Fisher's exact test |
| Case Study 5 | Logistic Regression | Binary classification, model selection, ROC/AUC |

---

## Case Study 1: Descriptive Analysis of Demographic Data

This case study analyzes demographic census data from the years 2002 and 2022.  
The main goal is to investigate life expectancy and fertility rates across countries, regions, and subregions.

### Main Questions

- How is life expectancy distributed in 2022?
- Are there differences between male and female life expectancy?
- Is there a relationship between fertility rate and life expectancy?
- How do life expectancy and fertility rate differ between subregions?
- How did the relationship between fertility rate and life expectancy change from 2002 to 2022?

### Methods

- Descriptive statistics
- Mean, median, quantiles, and correlation
- Histograms
- Scatterplots
- Boxplots
- Pearson correlation

### Graphs

The project includes histograms of male and female life expectancy, scatterplots of fertility rate and life expectancy, and boxplots comparing life expectancy and fertility rate across subregions.

### Summary

The results show that women generally have a higher life expectancy than men.  
There is also a strong negative relationship between fertility rate and life expectancy, meaning that countries with higher fertility rates often have lower life expectancy.  
Between 2002 and 2022, life expectancy generally increased while fertility rates decreased.

---

## Case Study 2: Comparison of Two Distributions

This case study analyzes an experimental concentration test.  
Participants completed two concentration tests, and the project investigates whether repetition improves concentration performance and processing time.

Two test types are compared:

- **GU test**: first digit even, second digit odd
- **UG test**: first digit odd, second digit even

### Main Questions

- Are the GU and UG tests different in the first round?
- Does repetition improve concentration performance?
- Does repetition reduce processing time?
- Is repeating the same test better than switching from UG to GU?

### Methods

- Descriptive statistics
- Histogram analysis
- Shapiro-Wilk test for normality
- One-sample t-test
- Two-sample t-test
- F-test for equality of variances

### Graphs

The project includes histograms of concentration performance and processing time.  
These graphs show the distribution of participants' performance and how long they needed to complete the test.

### Summary

The results show no significant difference between the GU and UG tests in the first round.  
However, concentration performance improved significantly in the second round, and processing time was significantly reduced.  
Repeating the exact same test did not lead to a significantly stronger improvement than switching from UG to GU.

---

## Case Study 3: Comparison of Several Distributions

This case study analyzes the lengths of cuckoo eggs found in the nests of different host bird species.  
The goal is to determine whether cuckoo egg lengths differ between host species.

The host bird species are:

- Wiesenpieper (WP)
- Baumpieper (BP)
- Rotkehlchen (RK)
- Zaunkönig (ZK)

### Main Questions

- Do cuckoo egg lengths differ between host bird species?
- Which host species show significant differences?
- Are the results robust after correcting for multiple testing?

### Methods

- Descriptive statistics
- Boxplots
- QQ-plots
- ANOVA
- Pairwise t-tests
- Bonferroni-Holm correction
- Multiple testing procedure

### Graphs

The project includes boxplots of cuckoo egg lengths by host bird species and QQ-plots to check normality assumptions.

### Summary

The ANOVA showed significant differences in cuckoo egg lengths between the host bird species.  
Pairwise comparisons showed that several groups differ significantly, while some pairs do not show significant differences.  
The Bonferroni-Holm correction was used to reduce the risk of false-positive results due to multiple testing.

---

## Case Study 4: Contingency Table Analysis of the Olympic Medal Table

This case study analyzes medal results from the Olympic Games 2024.  
The focus is on relationships between countries, sports, and medal types.

The analysis considers medal counts for selected countries across different sport categories.

### Main Questions

- Is there a relationship between country and sport category?
- Is there a relationship between medal color and country within each sport?
- Is there a relationship between medal color and sport within each country?
- Which countries or sports show noticeable medal patterns?

### Methods

- Descriptive statistics
- Contingency tables
- Chi-square test of independence
- Fisher's exact test
- Bonferroni-Holm correction

### Graphs

The project includes bar charts showing the distribution of gold, silver, and bronze medals by country and sport category.

### Summary

The analysis shows that medal distributions differ across countries and sports.  
There is a significant relationship between country and sport category in terms of total medal counts.  
For some smaller contingency tables, Fisher's exact test was used because expected frequencies were too small for the Chi-square test.  
After p-value adjustment, most medal-color relationships were no longer statistically significant, which shows the importance of correcting for multiple testing.

---

## Case Study 5: Logistic Regression

This case study analyzes the 2024 United States presidential election using demographic and socioeconomic variables from U.S. states.  
The goal is to model the election outcome using logistic regression and identify important predictors.

The binary target variable is the leading candidate:

- Harris
- Trump

### Main Questions

- Which demographic and socioeconomic variables are related to the election outcome?
- Can logistic regression be used to classify the winning candidate by state?
- Which variables remain important after model selection?
- Does a reduced model perform better than the full model?

### Methods

- Descriptive statistics
- Data transformation
- Logistic regression
- Best subset selection
- Akaike Information Criterion
- Wald tests
- Wald confidence intervals
- 10-fold cross-validation
- ROC curves
- AUC comparison

### Graphs

The project includes a bar chart of states won by each candidate, histograms of the metric variables, and ROC curves comparing the full logistic regression model with the reduced model.

### Summary

The full logistic regression model did not show clear significant effects for all variables.  
After variable selection, the reduced model identified health insurance coverage and median rent as important predictors.  
The reduced model achieved a higher AUC than the full model, meaning that it had better classification performance in this analysis.

---

## Tools and Software

The projects mainly use R and common statistical packages such as:

- `ggplot2`
- `dplyr`
- `tidyverse`
- `gridExtra`
- `patchwork`
- `caret`
- `pROC`
- `leaps`
- `bestglm`

## General Conclusion

Together, these five case studies demonstrate different statistical methods for analyzing real-world data.  
The projects begin with descriptive and visual analysis, then move toward hypothesis testing, contingency table analysis, and predictive modeling.

The case studies show how statistical methods can be used to answer practical questions, compare groups, test relationships, and evaluate model performance.
