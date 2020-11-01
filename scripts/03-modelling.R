# Uncomment these (by deleting the #) if you need to install the packages
#### Preamble ####
# Purpose: Prepare predictive model using individual survey data We will be downloading the ACS data from the aforementioned site
# Author: Anees Shaikh, Jaffa Romain, Lu Mu
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
# Look at some summary statistics to make sure the data seem reasonable
summary(survey_data)

# change vote_2020 results to binary result
# 1: Trump, 0: Biden
survey_data_reduced <-
  survey_data_reduced %>%
  mutate(vote_2020 = ifelse(vote_2020 == "Donald Trump", 1, 0))


survey_data_reduced %>% 
  summarise(raw_prop = sum(vote_2020) / nrow(survey_data_reduced))  # no class bias for our response variable - results are fairy equal

# building model
model <- glm(vote_2020 ~ sex + age + race + household_income + hispan + state, family=binomial,
            data = survey_data_reduced)
final_model <- saveRDS(model, file = "outputs/model/final_model.rds")
# Looking for indications of collinearity using correlation matrix of estimated coefficients
collin <- car::vif(model) # no collinearity 
cor_test <- saveRDS(model, file = "outputs/model/cor.rds")


coefficients <- broom::tidy(model)
coef <- saveRDS(model, file = "outputs/model/coefficients.rds")



### TTraining on indivual survey
set.seed(10)
train <- survey_data_reduced %>% sample_frac(.70)
predict <- predict(model,train, type = 'response')
table_mat <- table(train$vote_2020, predict > 0.5)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
# 63% - could be improved


