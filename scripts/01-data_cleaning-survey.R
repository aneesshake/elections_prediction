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
         age, )

# Keep observations of those who intend to vote in the upcoming 2020 election
survey_data_reduced <-survey_data_reduced %>% filter(vote_intention == "Yes, I will vote")

# Remove observations of those who are unsure of who they will vote for in 2020
survey_data_reduced <-survey_data_reduced %>% filter(vote_2020 != "I am not sure/don't know")

# We are predicting a binary outcome for vote; either Trump or Biden to win majority vote.
# Keep vote results of either Democrat vs. Republican nominee
survey_data_reduced <-survey_data_reduced %>% filter(vote_2020 == "Donald Trump"| vote_2020 == "Joe Biden")

# matching state names to that of the post-stratification data
survey_data_reduced <- survey_data_reduced %>% mutate(state = case_when(state == "AL" ~ "alabama", state == "AK" ~ "alaska", state == "AZ" ~ "arizona", state == "AR" ~ "arkansas", state == "CA" ~ "california",
                                                                        state == "CO" ~ "colorado", state == "CT" ~ "connecticut", state == "DE" ~ "delaware", state == "FL"~ "florida", state == "GA" ~ "georgia", 
                                                                        state == "HI" ~ "hawaii", state == "ID" ~ "idaho", state == "IL" ~ "illinois", state == "IN" ~ "indiana", state == "IA" ~ "iowa",state == "KS" ~ "kansas", 
                                                                        state == "KY" ~ "kentucky", state == "LA" ~ "louisiana", state == "ME"~ "maine", state == "MD"~"maryland", state == "MA" ~ "massachusetts", state == "MI" ~ "michigan", 
                                                                        state == "MN" ~ "minnesota", state == "MS" ~ "mississippi", state == "MO" ~ "missouri",
                                                                        state == "MT" ~ "montana", state == "NE" ~ "nebraska", state == "NV" ~ "nevada", state == "NH"~ "new hampshire", state == "NJ"~"new jersey",
                                                                        state == "NM" ~ "new mexico", state == "NY" ~ "new york", state == "NC" ~ "north carolina", state == "ND" ~ "north dakota", state == "OH" ~ "ohio",
                                                                        state == "OK" ~ "oklahoma", state == "OR" ~ "oregon", state == "PA" ~ "pennsylvania", state == "RI"~ "rhode island", state == "SC"~"south carolina",
                                                                        state == "SD" ~ "south dakota", state == "TN" ~ "tennessee", state == "TX" ~ "texas", state == "UT" ~ "utah", state == "VT" ~ "vermont",
                                                                        state == "VA" ~ "virginia", state == "WA" ~ "washington", state == "WV" ~ "west virginia", state == "WI"~ "wisconsin", state == "WY"~"wyoming"))
# matching hispanic variable to post stratification data set
survey_data_reduced <- survey_data_reduced %>% mutate(hispan = ifelse(hispanic == "Not Hispanic", "not hispanic", ifelse(hispanic == "Mexican", "mexican", 
                                              ifelse(hispanic == "Cuban", "cuban", ifelse(hispanic == "Puerto Rican", "puerto rican", "other")))))
# matching race to post-stratification data set
survey_data_reduced <- survey_data_reduced %>% mutate(race = ifelse(race_ethnicity == "White", "white", ifelse(race_ethnicity == "Black, or African American", "black", 
    ifelse(race_ethnicity == "American Indian or Alaska Native", "american indian or alaska native",ifelse(race_ethnicity == "Asian (Asian Indian)", "asian", 
    ifelse(race_ethnicity == "Asian (Chinese)", "asian",  ifelse(race_ethnicity == "Asian (Filipino)", "asian",  
    ifelse(race_ethnicity == "Asian (Japanese)", "asian",  ifelse(race_ethnicity == "Asian (Korean)", "asian",  ifelse(race_ethnicity == "Asian (Vietnamese)", "asian", 
    ifelse(race_ethnicity == "Asian (Other)", "asian",  ifelse(race_ethnicity == "Pacific Islander (Native Hawaiian)" , "asian", ifelse(race_ethnicity == "Pacific Islander (Guamanian)", "asian", 
    ifelse(race_ethnicity == "Pacific Islander (Samoan)", "asian", ifelse(race_ethnicity == "Pacific Islander (Other)", "asian", "other" )))))))))))))))
survey_data_reduced<- survey_data_reduced %>% mutate(age = case_when(age <= 24 ~ "18-24", age <= 44 ~ "25-44",
    age <= 64 ~ "45-64",  age >= 65 ~ "65+"))

survey_data_reduced <- survey_data_reduced %>% mutate( sex = case_when(gender == "Female" ~ "female", gender == "Male"~ "male"))


### GRAPHING
s <- survey_data_reduced %>% select(vote_2016, race) %>% filter(vote_2016 != "Did not vote, but was eligible", vote_2016 != "Was not eligible to vote", vote_2016 != "Don't recall") %>% group_by(race, vote_2016) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
## 2016 vote by race
s %>% ggplot(aes(x = vote_2016, fill = race, group = race)) + geom_bar(aes(y = freq), stat = "identity", position = "dodge", alpha = 2/3) + labs(x = "Race", title = " Percentage of 2016 Presidential Election Votes by Race")

# Voter intention by Income
survey_data_reduced %>% drop_na(household_income) %>% ggplot(aes(x = household_income, fill  = vote_2020), color = "black") + geom_bar(alpha = 0.7, color = "black") + 
  scale_fill_manual(values = c("#FF4040", "#0000FF")) + labs(title = "Income Vote Distribution" , subtitle =  " A look into how eligible voters intend to vote based on income.", x = "Income", y= "Number of votes", fill = "Nominee") +   theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5))

# Voter intention by employment status
survey_data_reduced %>% drop_na(employment) %>% ggplot(aes(x = employment, fill  = vote_2020), color = "black") + geom_bar(alpha = 0.7, color = "black") + 
  scale_fill_manual(values = c("#FF4040", "#0000FF")) + labs(title = " Vote Distribution by Employment Status" , subtitle =  " A look into how eligible voters intend to vote based on employment status.", x = "Employment Status", y= "Number of votes", fill = "Nominee") +   theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5))
                                                           
# Get proportion of those who voted for Trump by state in 2016
avg_repub_votes <-survey_data_reduced %>% select(state, vote_2016) %>% filter(vote_2016 == "Donald Trump") %>% group_by(state, vote_2016) %>% count() %>% ungroup() %>% mutate(pct_trump = n/1948 * 100) %>% drop_na(state)
avg_repub_votes$region <- tolower(avg_repub_votes$sttate)
trump_votes <- left_join(us, avg_repub_votes)

# Visual of republican support by State
trump_votes %>% ggplot(aes(x = long, y = lat, group = group, fill = pct_trump)) + coord_map(projection = "albers" ,lat0 = 39, lat1 = 45) + geom_polygon(color = "grey", size = 0.1) + labs(title = "Trump Votes by State", subtitle = "Distribution of Republican vote in 2016 election by state.") + 
  theme_map() + labs(fill = "Percentage") + scale_fill_gradient(low = "white", high = "red")

# Get proportion of those who voted for Clinton by state in 2016
survey_data_reduced %>% filter(vote_2016 == "Hillary Clinton") # get total number of democratic voters
avg_dem_votes <-survey_data_reduced %>% select(state, vote_2016) %>% filter(vote_2016 == "Hillary Clinton") %>% group_by(state, vote_2016) %>% count() %>% ungroup() %>% mutate(pct_clinton = n/1758 * 100) %>% drop_na(state)

avg_dem_votes$region <- tolower(avg_dem_votes$state)
dem_votes <- left_join(us, avg_dem_votes)

# Visual of democratic support by State
dem_votes %>% ggplot(aes(x = long, y = lat, group = group, fill = pct_clinton)) + coord_map(projection = "albers" ,lat0 = 39, lat1 = 45) + geom_polygon(color = "black", size = 0.1) + labs(title = "Clinton Votes by State", subtitle = "Distribution of Democratic vote in 2016 election by state.") + 
  theme_map() + labs(fill = "Percentage") + scale_fill_gradient(low = "gray", high = "navy blue")


