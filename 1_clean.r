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
Research question: What is the relationship between poverty and crime rates? 
'''
```{r load dataset}
# Load demographics dataset
demographics <- read.csv("stat512_casestudy1\\raw_data.csv")
df_demographics <- data.frame(demographics)
sprintf("Number of Observations (Demographics): %d", nrow(df_demographics))
variables_demographics <- paste(names(df_demographics), collapse = ", ")
sprintf("Categorical Variables: %s", variables_demographics)

# Load crime dataset
crimes <- read.csv("stat512_casestudy1\\crime_data_with_poverty_tiers.csv")
df_crimes <- data.frame(crimes)
sprintf("Number of Observations (Crimes): %d", nrow(crimes))
variables_crime <- paste(names(df_demographics), collapse = ", ")
sprintf("Categorical Variables: %s", variables_crime)
```

```{r clean dataset}
# Compute average median income by state (Demographics dataset))
income_by_state <- df_demographics |>
  group_by(state) |>
  summarise(state_median_income = mean(medIncome, na.rm = TRUE))
head(income_by_state)

# Compute the average crime rate by state (Crimes dataset)
crime_rate_by_state <- df_crimes |>
  group_by(state) |>
  summarise(state_crime_rate = mean(crime_rate, na.rm = TRUE))
head(crime_rate_by_state)
```

```{r compute descriptive statistics + correlations}
```

```{r create plots}
```
