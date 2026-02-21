---
title: "STAT512: Case Study"
author: "Emily Tumacder, Thomas Varghese, Viet Hai Tran, Andrew Tang"
date: ""
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

``` {r load libraries}
library(dplyr)
```
'''
Research question: Which factor contributes more to crime rates
'''
```{r load dataset}
# Load demographics dataset
demographics <- read.csv("crime_data_with_poverty_tiers.csv")
df_demographics <- data.frame(demographics)
sprintf("Number of Observations (Demographics): %d", nrow(df_demographics))
variables_demographics <- paste(names(df_demographics), collapse = ", ")
sprintf("Categorical Variables: %s", variables_demographics)

# Load crime dataset
crimes <- read.csv("crime_data_with_poverty_tiers.csv")
df_crimes <- data.frame(crimes)
sprintf("Number of Observations (Crimes): %d", nrow(crimes))
variables_crime <- paste(names(df_demographics), collapse = ", ")
sprintf("Categorical Variables: %s", variables_crime)
```

```{r clean dataset}
# Compute average percent of population under the poverty line (Crimes dataset)
per_poverty_by_state <- df_crimes |>
  filter(!is.na(PctPopUnderPov)) |>
  group_by(state) |>
  summarise(state_percent_poverty = mean(PctPopUnderPov))

# Compute the average unemployment rate by state (Demographics dataset)
unemployment_by_state <- df_demographics |>
  filter(!is.na(PctUnemployed)) |>
  group_by(state) |>
  summarise(state_unemployment  = mean(PctUnemployed, na.rm = TRUE))

# Compute the average median income by state (Demographics dataset)
median_income_by_state <- df_demographics |>
  filter(!is.na(medIncome)) |>
  group_by(state) |>
  summarise(state_median_income = mean(medIncome, na.rm = TRUE))

# # Compute the average percent of population with less than high school education by state (Demographics dataset)
hs_incomplete_by_state <- df_demographics |>
  filter(!is.na(PctNotHSGrad)) |>
  group_by(state) |>
  summarise(state_hs_incomplete = mean(PctNotHSGrad, na.rm = TRUE))

# Compute the average crime rate by state (Crimes dataset)
crime_rate_by_state <- df_crimes |>
  filter(!is.na(totalCrimesPerPop)) |>
  group_by(state) |>
  summarise(state_crime_rate = mean(totalCrimesPerPop, na.rm = TRUE))
```

'''
H0: Poverty contributes more to crime rates than unemployment, median income, and education level.
H1: All roughly contribute equally to crime rates
'''
```{r compute descriptive statistics + correlations}
y <- demographics$ViolentCrimesPerPop
 
x1 <- demographics$medIncome        # median income
x2 <- demographics$PctUnemployed    # unemployment rate
x3 <- demographics$PctPopUnderPov   # poverty rate
x4 <- demographics$PctNotHSGrad     # high school incomplete (% not HS grad)
 

png("crime_vs_predictors.png", width=1200, height=900)
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(x1, y, xlab="medIncome", ylab="ViolentCrimesPerPop", main="Crime vs Median Income")
plot(x2, y, xlab="PctUnemployed", ylab="ViolentCrimesPerPop", main="Crime vs Unemployment")
plot(x3, y, xlab="PctPopUnderPov", ylab="ViolentCrimesPerPop", main="Crime vs Poverty Rate")
plot(x4, y, xlab="PctNotHSGrad", ylab="ViolentCrimesPerPop", main="Crime vs High School Incomplete")
dev.off()
 
 # Remove missing values pairwise
cor.test(x1, y, use="complete.obs")   # Median income vs Crime
cor.test(x2, y, use="complete.obs")   # Unemployment vs Crime
cor.test(x3, y, use="complete.obs")   # Poverty vs Crime
cor.test(x4, y, use="complete.obs")   # HS Incomplete vs Crime

```

```{r create plots}
```
