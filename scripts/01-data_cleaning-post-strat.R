#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS. We will be downloading the ACS data from the aforementioned site
# Author: Anees Shaikh, Jaffa Romain, Lu Mu
# Data: 02 November 2020
# Contact: anees.shaikh@mail.utoronto.ca or jaffa.romain@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/raw_data/ACS
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("inputs/raw_data/ACS/usa_00001.dta") %>% filter(age >= 18)

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)


names(raw_data)

raw_data

reduced_data <- 
  raw_data %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         marst, 
         bpl,
         citizen,
         educd,
         labforce,
         hhincome)
rm(raw_data)
         


#### What's next? ####

##let us now create our different kinds of stratification
##all filtering of the data based on the variables is done prior to this step



#counts by all vars
reduced_data %>% group_by_all()
  tally()

  
#cell count by age
reduced_data %>% 
group_by(age) %>% 
  tally()

#cell count by region
reduced_data %>% 
  group_by(region) %>% 
  tally()

#cell count by sex
reduced_data %>% 
  group_by(sex) %>% 
  tally()

#cell count by race
reduced_data %>% 
  group_by(race) %>% 
  tally()

#cell count by education
reduced_data %>% 
  group_by(educd) %>% 
  tally()

#cell count by bpl
reduced_data %>% 
  group_by(bpl) %>% 
  tally()

#cell count by labforce
reduced_data %>% 
  group_by(labforce) %>% 
  tally()

