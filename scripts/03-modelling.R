# Uncomment these (by deleting the #) if you need to install the packages
#### Preamble ####
# Purpose: Prepare predictive model using individual survey data We will be downloading the ACS data from the aforementioned site
# Author: Anees Shaikh, Jaffa Romain, Lu Mu, Cameron Fryer
# Data: 02 November 2020
# Contact: anees.shaikh@mail.utoronto.ca or jaffa.romain@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/raw_data/ACS
# - Need to have loaded data in scripts folder; 01-data_cleaning_post_strat for post-stratification data and 01-data_cleaning-survey for survey data
# in order to get cleaned data set.

library(broom) # Helps make the regression results tidier
library(here) # Helps make file referencing easier.
library(tidyverse) # Helps make programming with R easier
library(haven)
library(stringr)
library(brms)
library(gtsummary)

# loading in individual_data

survey_data <- readRDS(here::here("inputs/cleaned_data","individual-survey.rds"))

# Look at some summary statistics to make sure the data seem reasonable


summary(survey_data)

# change vote_2020 results to binary result
# 1: Trump, 0: Biden




survey_data %>% 
  summarise(raw_prop = sum(vote_2020) / nrow(survey_data))  # no class bias for our response variable - results are fairy equal

# building model
model <- glm(vote_2020 ~ sex + age + race + hispan, family=binomial(link=logit),
            data = survey_data)


saveRDS(model, file = "outputs/model/final_model.rds")



# Looking for indications of collinearity using correlation matrix of estimated coefficients
collin <- car::vif(model) # no collinearity 
c <- broom::tidy(model)

saveRDS(c, file = "outputs/model/coefficients.rds")
  

