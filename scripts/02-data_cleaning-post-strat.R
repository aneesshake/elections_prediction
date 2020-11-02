#### Preamble ####
# Purpose: Prepare and clean the data downloaded from IPUMS for our post-stratification cell counts. We will be downloading the ACS data from https://usa.ipums.org/usa-action/variables/group
# Author: Anees Shaikh, Jaffa Romain, Lu Mu, and Cameron Fryer
# Date: 02 November 2020
# Contact: anees.shaikh@mail.utoronto.ca or jaffa.romain@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/raw_data/ACS
# - Don't forget to gitignore it and cite it!
# - It would also be helpful for you to know how the values for variables differ between both datasets. This way you can lay out a simple plan to match them.

#### Workspace setup ####
library(haven)
library(tidyverse)




## Defined static Variables ##
#These variables may change according to your needs, you may find that you have no interest in the foreign born status, adjust the variables accordingly

#creating this list to determine whether an individual is a foreign or domestic born
list_of_states <- tolower(state.name) %>% append("district of columbia")

#the black/african american race is defined differently in the nationscape dataset, so we're going to use this variable for transformation later
race_to_be_rewritten = c("black/african american/negro")

#For our analysis, we're not looking at people who may be defined by multiple races, this classification doesn't mesh well with the nationscape data
races_to_be_filtered = c("three or more major races","two major races")



# Read in the raw data. You may choose to use the here::here() function if it works better with your setup. Do remember that this won't work unless you have the file extracted into this folder.
#Individuals below 18 can't vote and are filtered accordingly
#according to the IPUMS codes, an income of "9999999" is used to signify a N/A value. we are also removing negative values for income as that didn't make sense to us. There didn't seem to be any information about it either in the codes
raw_data <- 
  read_dta("inputs/raw_data/ACS/usa_00001.dta") %>% 
  filter(age >= 18 & hhincome <9999999 & hhincome >= 0) 
  

# Add the labels and filter for citizens. Non-citizens can't vote so it would make sense to omit them
raw_data <- labelled::to_factor(raw_data) %>% filter(!citizen == "not a citizen")



#selecting the variables we want, the dataset comes with a lot of variables that are not meaningful for our purposes and drop the null rows
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


## Data Munging ##

##let us now create our different kinds of stratification. To do this, we are going to also be standardizing the values with the Nationscape dataset.


#determining whether a person is born in the US or outside. This becomes a simple value where you're either born in the US or Another country
reduced_data <- 
  reduced_data %>% 
  mutate(foreign_born = if_else(bpl %in% list_of_states, "The United States", "Another country"))



#removing respondents that have more than one race ascribed to them, 
#as well as standardizing the black race name here. the logic simply states that if the race value is the same as our race_to_be_rewritten value, then it should be re-written as "black", otherwise just take the existing race value
#additionally, the ipums data has a few multi-racial categories that don't quite work well with nationscape data, we are going to be omitting them. They don't constitute a huge number either.
reduced_data <- 
  reduced_data %>% 
  mutate(race = if_else(race %in% race_to_be_rewritten, "black", as.character(reduced_data$race))) %>% 
  filter(!race %in% races_to_be_filtered)



#creating age groups here. This grouping is equivalent to the grouping we have done in the 01-data_cleaning-survey.R
reduced_data<- reduced_data %>% 
  mutate(age = case_when(as.numeric(age) <= 24 ~ "18-24", 
                         as.numeric(age) <= 44 ~ "25-44",
                         as.numeric(age) <= 64 ~ "45-64",  
                         as.numeric(age) >= 65 ~ "65+"))





#income grouping, to match nationscape grouping
reduced_data <- reduced_data %>%
  mutate(household_income = case_when(hhincome <= 14999 ~ "Less than $14,999",
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






#saving this output in the folder for additional plotting that will be done in the final paper
saveRDS(reduced_data, file = "inputs/cleaned_data/post-strat.rds")





#the actual post-stratification cell counts. We first group by state,sex,age,race,hispan. These are our modelling variables(except for state). We then get the counts by the state to get the proportion of each different kind of pairing of age * sex * race * hispan.
#this is then saved in a folder for us to predict on using our final model
cell_counts <- reduced_data %>% 
  group_by(stateicp, sex,age,race,hispan) %>% 
  summarise(num_records = n()) %>%  
  group_by(stateicp) %>% 
  mutate(prop = num_records/sum(num_records),state_count = sum(num_records)) %>% ungroup() %>% rename(state_name = stateicp)


saveRDS(cell_counts,file = "inputs/cleaned_data/cell_counts.rds")

 





