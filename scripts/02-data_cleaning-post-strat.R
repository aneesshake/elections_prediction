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





#creating this list to determine whether an individual is a foreign or domestic born
list_of_states <- tolower(state.name) %>% append("district of columbia")

race_to_be_rewritten = c("black/african american/negro")

races_to_be_filtered = c("three or more major races","two major races")



# Read in the raw data. 
raw_data <- 
  read_dta("inputs/raw_data/ACS/usa_00001.dta") %>% 
  filter(age >= 18 & hhincome <9999999 & hhincome >= 0) 
  

# Add the labels
raw_data <- labelled::to_factor(raw_data) %>% filter(!citizen == "not a citizen")


reduced_data <- 
  raw_data %>% 
  select(
         stateicp,
         sex, 
         age, 
         race, 
         hispan, 
         bpl,
         hhincome) %>% 
  drop_na()



##let us now create our different kinds of stratification
##all filtering of the data based on the variables is done prior to this step







#determining whether a person is born in the US or outside.
reduced_data <- 
  reduced_data %>% 
  mutate(foreign_born = if_else(bpl %in% list_of_states, "The United States", "Another country"))




#removing respondents that have more than one race ascribed to them, as well as standardizing the black race name here
reduced_data <- reduced_data %>% 
  mutate(race = if_else(race %in% race_to_be_rewritten, "black", as.character(reduced_data$race))) %>% 
  filter(!race %in% races_to_be_filtered)



#grouping by age
reduced_data<- reduced_data %>% 
  mutate(age = case_when(as.numeric(age) <= 24 ~ "18-24", 
                         as.numeric(age) <= 44 ~ "25-44",
                         as.numeric(age) <= 64 ~ "45-64",  
                         as.numeric(age) >= 65 ~ "65+"))





#income grouping, to match nationscape grouping
reduced_data <- reduced_data %>%
  mutate(hhincome_group = case_when(hhincome <= 14999 ~ "Less than $14,999",
                                    hhincome <= 19999 ~ "$15,000 to $19,999",
                                    hhincome <= 24999 ~ "$20,000 to $24,999",
                                    hhincome <= 29999 ~ "$25,000 to $29,999",
                                    hhincome <= 34999 ~ "$30,000 to $34,999",
                                    hhincome <= 39999 ~ "$35,000 to $39,999",
                                    hhincome <= 44999 ~ "$40,000 to $44,999",
                                    hhincome <= 49999 ~ "$45,000 to $49,999",
                                    hhincome <= 54999 ~ "$50,000 to $54,999",
                                    hhincome <= 59999 ~ "$55,000 to $59,999",
                                    hhincome <= 64999 ~ "$60,000 to $64,999",
                                    hhincome <= 69999 ~ "$65,000 to $69,999",
                                    hhincome <= 74999 ~ "$70,000 to $74,999",
                                    hhincome <= 79999 ~ "$75,000 to $79,999",
                                    hhincome <= 84999 ~ "$80,000 to $84,999",
                                    hhincome <= 89999 ~ "$85,000 to $89,999",
                                    hhincome <= 94999 ~ "$90,000 to $94,999",
                                    hhincome <= 99999 ~ "$95,000 to $99,999",
                                    hhincome <= 124999 ~ "$100,000 to $124,999",
                                    hhincome <= 149999 ~ "$125,000 to $149,999",
                                    hhincome <= 174999 ~ "$150,000 to $174,999",
                                    hhincome <= 199999 ~ "$175,000 to $199,999",
                                    hhincome <= 249999 ~ "$200,000 to $249,999",
                                    hhincome >= 250000 ~ "$250,000 and above"))









saveRDS(reduced_data, file = "inputs/cleaned_data/post-strat.rds")





#counts by all vars


cell_counts <- reduced_data %>% 
  group_by(stateicp,sex,age,race,hhincome_group,foreign_born) %>% 
  summarise(num_records = n()) %>% ungroup() %>% 
  mutate(proportion = num_records/sum(num_records),
         total_num = sum(num_records)) %>% arrange(desc(proportion))


saveRDS(cell_counts,file = "inputs/cleaned_data/cell_counts.rds")

  
#cell count by age
reduced_data %>% 
group_by(age) %>% 
  summarise(num_records = n()) %>% 
  mutate(proportion = num_records/sum(num_records),
         total_num = sum(num_records))

#cell count by state
reduced_data %>% 
  group_by(stateicp) %>% 
  summarise(num_records = n()) %>% 
  mutate(proportion = num_records/sum(num_records),
         total_num = sum(num_records))


#cell count by sex
reduced_data %>% 
  group_by(sex) %>% 
  summarise(num_records = n()) %>% 
  mutate(proportion = num_records/sum(num_records),
         total_num = sum(num_records))


#cell count by race
reduced_data %>% 
  group_by(race)%>% 
  summarise(num_records = n()) %>% 
  mutate(proportion = num_records/sum(num_records),
         total_num = sum(num_records))


#cell count by bpl
reduced_data %>% 
  group_by(foreign_born) %>% 
  summarise(num_records = n()) %>% 
  mutate(proportion = num_records/sum(num_records),
         total_num = sum(num_records))


