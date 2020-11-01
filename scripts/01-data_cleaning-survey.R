#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Nationscape. We will be downloading the nationscape data from the aforementioned site
# Author: Anees Shaikh, Jaffa Romain, Lu Mu
# Data: 02 November 2020
# Contact: anees.shaikh@mail.utoronto.ca or jaffa.romain@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the Nationscape survey data and saved it to inputs/raw_data/Nationscape
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(ggplot2)
library(maps)
library(ggthemes)
library(mapproj)

us <- map_data("state")
us <- us %>% mutate(region = case_when(region == "district of columbia"~"washington", TRUE ~ region))
party_colors <- c("#FF4040","#0000FF")


#the little mapping below allows us to easily match state abbreviations to state names, thank you professor rohan alexander for suggesting this neat method.
us_states_mapping <-  tibble(state.abb, state_name = tolower(state.name)) %>% 
  rename(state = state.abb)



                                   


# Read in the raw data (You might need to change this if you use a different dataset)
survey_data<- read_dta("inputs/raw_data/Nationscape/ns20200625.dta")

# Add the labels
survey_data <- labelled::to_factor(survey_data)

# Just keep some variables
survey_data_reduced <- 
  survey_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age, ) %>% 
  drop_na()



#creating this little ethnicity mapping to make the lower code cleaner

race_ethnicity <- attr(survey_data_reduced$race_ethnicity, "levels")
us_ethnicity_mapping <- data.frame("race" = c("white",
                                              "black",
                                              "american indian or alaska native",
                                              "other asian or pacific islander",
                                              "chinese",
                                              "other asian or pacific islander",
                                              "japanese",
                                              "other asian or pacific islander",
                                              "other asian or pacific islander",
                                              "other asian or pacific islander",
                                              "other asian or pacific islander",
                                              "other asian or pacific islander",
                                              "other asian or pacific islander",
                                              "other asian or pacific islander",
                                              "other race, nec"),race_ethnicity)


survey_data_reduced <-survey_data_reduced %>% 
  filter(vote_intention == "Yes, I will vote") %>% # Keep observations of those who intend to vote in the upcoming 2020 election
  filter(vote_2020 == "Donald Trump"| vote_2020 == "Joe Biden") %>% # Since we're only looking for a binary result, we're filtering for respondents that would've voted for Trump or Biden. By nature of the filter conditions, we'd be removing all other types of responses.
  filter(vote_2016 == "Donald Trump"| vote_2016 == "Hillary Clinton")

#In the code chunk below, we are doing a few thingS:
#1) matching state names to that of the post-stratification data, .
#2) matching ethnicities to those that are found in the post-stratification data
#3) matching hispanic definitions to post-stratification data
#4) matching gender
#5) creating age groupings that are similar to that of the census


survey_data_reduced <- survey_data_reduced %>% 
  left_join(us_states_mapping, by = c("state" = "state")) %>% 
  left_join(us_ethnicity_mapping, by = c("race_ethnicity" = "race_ethnicity")) %>% 
  mutate(hispan = ifelse(hispanic == "Not Hispanic", "not hispanic", 
                         ifelse(hispanic == "Mexican", "mexican", 
                                ifelse(hispanic == "Cuban", "cuban", 
                                       ifelse(hispanic == "Puerto Rican", "puerto rican", "other"))))) %>% 
  mutate( sex = case_when(gender == "Female" ~ "female", gender == "Male"~ "male")) %>% 
  mutate(age = case_when(age <= 24 ~ "18-24", 
                         age <= 44 ~ "25-44",
                         age <= 64 ~ "45-64",  
                         age >= 65 ~ "65+")) 

### GRAPHING
s <- survey_data_reduced %>% 
  select(vote_2016, race) %>% 
  group_by(race, vote_2016) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))


## 2016 vote by race
s %>% ggplot(aes(x = vote_2016, fill = race, group = race)) + 
  geom_bar(aes(y = freq), stat = "identity", position = "dodge", alpha = 2/3) + 
  labs(x = "Race", title = " Percentage of 2016 Presidential Election Votes by Race")

# Voter intention by Income
survey_data_reduced %>% 
  drop_na(household_income) %>% 
  ggplot(aes(x = household_income, fill  = vote_2020), color = "black") + 
  geom_bar(alpha = 0.7, color = "black") + 
  scale_fill_manual(values = c("#FF4040", "#0000FF")) + 
  labs(title = "Income Vote Distribution" , 
       subtitle =  " A look into how eligible voters intend to vote based on income.", 
       x = "Income", 
       y= "Number of votes", 
       fill = "Nominee") +   
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5))

# Voter intention by employment status
survey_data_reduced %>% 
  drop_na(employment) %>% ggplot(aes(x = employment, fill  = vote_2020), color = "black") + 
  geom_bar(alpha = 0.7, color = "black") + 
  scale_fill_manual(values = c("#FF4040", "#0000FF")) + 
  labs(title = " Vote Distribution by Employment Status" , 
       subtitle =  " A look into how eligible voters intend to vote based on employment status.", 
       x = "Employment Status", y= "Number of votes", fill = "Nominee") +   
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5)) + coord_flip()
                                                           
# Get proportion of those who voted for Trump by state in 2016
avg_repub_votes <-survey_data_reduced %>% 
  select(state, vote_2016) %>% 
  filter(vote_2016 == "Donald Trump") %>% 
  group_by(state, vote_2016) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct_trump = n/1948 * 100) %>% 
  drop_na(state)

avg_repub_votes$region <- tolower(avg_repub_votes$state)

trump_votes <- left_join(us, avg_repub_votes)

# Visual of republican support by State
trump_votes %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = pct_trump)) +
  coord_map(projection = "albers" ,lat0 = 39, lat1 = 45) + geom_polygon(color = "grey", size = 0.1) + 
  labs(title = "Trump Votes by State", subtitle = "Distribution of Republican vote in 2016 election by state.") + 
  theme_map() + 
  labs(fill = "Percentage") + 
  scale_fill_gradient(low = "white", high = "red")

# Get proportion of those who voted for Clinton by state in 2016
survey_data_reduced %>% 
  filter(vote_2016 == "Hillary Clinton") # get total number of democratic voters

avg_dem_votes <-
  survey_data_reduced %>% 
  select(state, vote_2016) %>% 
  filter(vote_2016 == "Hillary Clinton") %>% 
  group_by(state, vote_2016) %>% 
  count() %>% 
  ungroup() %>%
  mutate(pct_clinton = n/1758 * 100) %>% 
  drop_na(state)

avg_dem_votes$region <- tolower(avg_dem_votes$state)
dem_votes <- left_join(us, avg_dem_votes)

# Visual of democratic support by State
dem_votes %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = pct_clinton)) + 
  coord_map(projection = "albers" ,lat0 = 39, lat1 = 45) + 
  geom_polygon(color = "black", size = 0.1) + 
  labs(title = "Clinton Votes by State", subtitle = "Distribution of Democratic vote in 2016 election by state.") + 
  theme_map() + 
  labs(fill = "Percentage") + 
  scale_fill_gradient(low = "gray", high = "navy blue")










# visual of age differences between 2016 and 2020

survey_data_reduced %>% group_by(age)






