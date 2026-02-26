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
Research question: Which factor provides the most weight in predicting crime rates?
X1: % population under the poverty line
X2: unemployment rate
X3: median income 
X4: % population with less than high school education

Y: crime rate (per capita) 
'''
```{r load dataset}
# Load demographics dataset
demographics <- read.csv("./raw_data.csv")
df_demographics <- data.frame(demographics)
sprintf("Number of Observations (Demographics): %d", nrow(df_demographics))
variables_demographics <- paste(names(df_demographics), collapse = ", ")
sprintf("Categorical Variables: %s", variables_demographics)

# Load crime dataset
crimes <- read.csv("./crime_data_with_poverty_tiers.csv")
df_crimes <- data.frame(crimes)
sprintf("Number of Observations (Crimes): %d", nrow(crimes))
variables_crime <- paste(names(df_demographics), collapse = ", ")
sprintf("Categorical Variables: %s", variables_crime)
```

```{r clean dataset}
# Compute average percent of population under the poverty line (Crimes)
per_poverty_by_state <- df_crimes |>
  filter(!is.na(PctPopUnderPov)) |>
  group_by(state) |>
  summarise(percent_poverty = mean(PctPopUnderPov))

# Compute the average unemployment rate by state (Demographics)
unemployment_by_state <- df_demographics |>
  filter(!is.na(PctUnemployed)) |>
  group_by(state) |>
  summarise(unemployment  = mean(PctUnemployed, na.rm = TRUE))

# Compute the average median income by state (Demographics)
median_income_by_state <- df_demographics |>
  filter(!is.na(medIncome)) |>
  group_by(state) |>
  summarise(median_income = mean(medIncome, na.rm = TRUE))

# Compute % population with less than hs education by state (Demographics)
hs_incomplete_by_state <- df_demographics |>
  filter(!is.na(PctNotHSGrad)) |>
  group_by(state) |>
  summarise(hs_incomplete = mean(PctNotHSGrad, na.rm = TRUE))

# Compute the average crime rate by state (Crimes)
crime_rate_by_state <- df_crimes |>
  filter(!is.na(totalCrimesPerPop)) |>
  group_by(state) |>
  summarise(crime_rate = mean(totalCrimesPerPop, na.rm = TRUE))

# Merge all state summaries into one dataframe
dataset <- per_poverty_by_state |>
  left_join(unemployment_by_state, by = "state") |>
  left_join(median_income_by_state, by = "state") |>
  left_join(hs_incomplete_by_state, by = "state") |>
  left_join(crime_rate_by_state, by = "state")

poverty <- dataset$percent_poverty
unemployment <- dataset$unemployment
hs_incomplete <- dataset$hs_incomplete
median_income <- dataset$median_income
crime_rate <- dataset$crime_rate

```

'''
H0: All roughly contribute equally to crime rates (beta1 = beta2 = beta3 = beta4)
H1: One factor contributes more than the others to crime rate
'''
```{r compute descriptive statistics + correlations}
# By state statistics: see if there is variability among states 
# (Breush-Pagan: Heteroscedacity in the future)
summary(median_income_by_state)
summary(unemployment_by_state)
summary(per_poverty_by_state)
summary(hs_incomplete_by_state)

# Correlation Coefficients
cor(dataset[, -1], method = "pearson", use = "complete.obs")
cor.test(percent_poverty, crime_rate, method = "pearson")
cor.test(unemployment, crime_rate, method = "pearson")
cor.test(hs_incomplete, crime_rate, method = "pearson")
cor.test(median_income, crime_rate, method = "pearson")
```

```{r create scatter plots}
# Scatter plots that represent the crime rate against different factors
png("crime_scatterplots.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(poverty, crime_rate, main = "Poverty vs Crime Rate",
     xlab = "Poverty Rate", ylab = "Crime Rate", pch = 16)
abline(lm(crime_rate ~ poverty, data = dataset), col = "blue")

plot(unemployment, crime_rate, main = "Unemployment vs Crime Rate",
     xlab = "Unemployment %", ylab = "Crime Rate", pch = 16)
abline(lm(crime_rate ~ unemployment, data = dataset), col = "blue")

plot(hs_incomplete, crime_rate, main = "% of Pop w/HS Incomplete vs Crime Rate",
     xlab = "% Unfinished HS", ylab = "Crime Rate", pch = 16)
abline(lm(crime_rate ~ hs_incomplete, data = dataset), col = "blue")

plot(median_income, crime_rate, main = "Median Income vs Crime Rate",
     xlab = "Median Income", ylab = "Crime Rate", pch = 16)
abline(lm(crime_rate ~ median_income, data = dataset), col = "blue")
par(mfrow = c(1, 1))
dev.off()
```

```{r create models and plots}
# Make SLR models for each
model_poverty <- lm(crime_rate ~ percent_poverty, data = dataset)
model_unemployment <- lm(crime_rate ~ unemployment, data = dataset)
model_income <- lm(crime_rate ~ median_income, data = dataset)
model_hs <- lm(crime_rate ~ hs_incomplete, data = dataset)

models <- list(
  poverty = model_poverty,
  unemployment = model_unemployment,
  income = model_income,
  hs = model_hs
)

# Then plot all residual plots
for (name in names(models)) {
  png(paste0("residuals_", name, ".png"), width = 800, height = 800)
  par(mfrow = c(2, 2))
  plot(models[[name]], main = name)
  par(mfrow = c(1, 1))
  dev.off()
}
# Multiple Linear Regression
mlr <- lm(crime_rate ~ percent_poverty + unemployment + median_income + hs_incomplete, data = dataset)

# Generate all 4 diagnostic plots in a 2x2 grid
png("MLR residuals.png")
par(mfrow = c(2, 2))
plot(mlr)
par(mfrow = c(1, 1))
dev.off()
```

