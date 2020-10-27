# Uncomment these (by deleting the #) if you need to install the packages

library(broom) # Helps make the regression results tidier
library(here) # Helps make file referencing easier.
library(tidyverse) # Helps make programming with R easier
library(haven)

library(stringr)


# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("C:/Users/lumuc/Desktop/ps4/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
data <- 
  raw_data %>% 
  select(vote_2020,
         employment,
         gender,
         race_ethnicity,
         household_income,
         state,
         age)

head(raw_data)

# Look at some summary statistics to make sure the data seem reasonable
summary(raw_data)

data <-
  data %>%
    filter(vote_2020 != "I am not sure/don't know")


data <-
  data %>%
  filter(vote_2020 != "I would not vote")

data <-
  data %>%
  mutate(vote_2020 = ifelse(vote_2020 == "Donald Trump", 1, 0))

data %>% 
  summarise(raw_prop = sum(vote_2020) / nrow(reduced_data))  

model <- lm(vote_2020 ~ gender + age + employment + race_ethnicity + household_income + state,
            data = data
)

broom::tidy(model)

ACS_data <- read_dta("C:/Users/lumuc/Desktop/ps4/ns20200625/usa_00001.dta")
head(ACS_data)

ACS_data$estimate <-
  model %>%
  predict(newdata = ACS_data)

