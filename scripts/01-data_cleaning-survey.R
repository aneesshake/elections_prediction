#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS. We will be downloading the ACS data from the aforementioned site
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
library(state)


us_states <- map_data("state")
party_colors <- c("#FF4040","#0000FF")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/raw_data/Nationscape/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
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
         age)

# Keep observations of those who intend to vote in the upcoming 2020 election
reduced_data <- reduced_data %>% filter(vote_intention == "Yes, I will vote")

# Remove observations of those who are unsure of who they will vote for in 2020
reduced_data <- reduced_data %>% filter(vote_2020 != "I am not sure/don't know")

# We are predicting a binary outcome for vote; either Trump or Biden to win majority vote.
# Keep vote results of either Democrat vs. Republican nominee
reduced_data <- reduced_data %>% filter(vote_2020 == "Donald Trump"| vote_2020 == "Joe Biden")

# Age distribution
reduced_data %>% ggplot(aes(x = age, fill  = vote_2020), color = "black") + geom_histogram(alpha = 0.7, color = "black", bins = 25) + 
  scale_fill_manual(values = c("#FF4040", "#0000FF")) + labs(title = "Age Distribution" , subtitle =  "Comparing age distribution of those who intend to vote for Trump and Biden.", x = "Age", y= "Number of votes", fill = "Nominee")

# Get proportions of those who intend to vote for Trump by state
avg_repub_votes <- reduced_data %>% select(state, vote_2020) %>% filter(vote_2020 == "Donald Trump") %>% group_by(state, vote_2020) %>% count() %>% ungroup() %>% mutate(prop_repub = n/sum(n) * 100)
ggplot(data = avg_repub_votes, mapping = aes(x = us_states$long, y = us_states$lat, group = us_states$group, fill = avg_repub_votes$prop_repub))
# Voter intention by Income

reduced_data %>% drop_na(household_income) %>% ggplot(aes(x = household_income, fill  = vote_2020), color = "black") + geom_bar(alpha = 0.7, color = "black") + 
  scale_fill_manual(values = c("#FF4040", "#0000FF")) + labs(title = "Income Vote Distribution" , subtitle =  " A look into how eligible voters intend to vote based on income.", x = "Income", y= "Number of votes", fill = "Nominee") +   theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5))

# Looking at political views depending on political interest
reduced_data %>% drop_na(ideo5, interest) %>% ggplot(aes(x= ideo5, y=interest)) + 
  geom_count(aes(size=..prop..), colour="lightgrey") +
  geom_count(aes(size=..prop.., group=ideo5), colour="cornflowerblue")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2)) +
  coord_fixed() +
  theme_minimal()








