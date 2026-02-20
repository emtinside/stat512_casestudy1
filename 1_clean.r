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
per_poverty_by_state <- df_crimes[PctPopUnderPov] |>
  group_by(state) |>
  summarise(state_percent_poverty = mean(PctPopUnderPov, na.rm = TRUE))

# Compute the average unemployment rate by state (Demographics dataset)
unemployment_by_state <- df_demographics |>
  group_by(state) |>
  summarise(state_unemployment  = mean(PctUnemployed, na.rm = TRUE))

# Compute the average median income by state (Demographics dataset)
median_income_by_state <- df_demographics |>
  group_by(state) |>
  summarise(state_median_income = mean(medIncome, na.rm = TRUE))

# Compute the average percent of population with less than high school education by state (Demographics dataset)
hs_incomplete_by_state <- df_demographics |>
  group_by(state) |>
  summarise(state_hs_incomplete = mean(PctNotHSGrad, na.rm = TRUE))

# Compute the average crime rate by state (Crimes dataset)
crime_rate_by_state <- df_crimes |>
  group_by(state) |>
  summarise(state_crime_rate = mean(totalCrimesPerPop, na.rm = TRUE))
```

'''
H0: Poverty contributes more to crime rates than unemployment, median income, and education level.
H1: All roughly contribute equally to crime rates
'''
```{r compute descriptive statistics + correlations}

```

```{r create plots}
```
