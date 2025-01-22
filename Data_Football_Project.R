library(dplyr)
library(tidyr)

Data_Football <- read.csv("C:/Users/joeph/OneDrive/Documents/University/Datasets/Data_Football.csv")

#inspect the variables (i.e, columns) using the glimpse() function
glimpse(Data_Football)

# Make sure all column names do not contain spaces.
Data_Football <- Data_Football %>% 
  rename(CurrentClub = `Current.Club`)

# Remove all rows that have missing data, if there are any.
Data_Football <- Data_Football %>% 
  drop_na()

glimpse(Data_Football)

# Convert league, season, position, and current club into factors
Data_Football <- Data_Football %>% 
  mutate(league = as.factor(league),
         season = as.factor(season),
         position = as.factor(position),
         CurrentClub = as.factor(CurrentClub))

glimpse(Data_Football)

# Remove the player that has an implausible value on the variable “age”.
Data_Football <- Data_Football %>% 
  filter(age > 15)

#Create a new variable (called sumCard) that is the sum of yellow and red cards.
Data_Football <- Data_Football %>% 
  mutate(medianSplit = ifelse(age > median(age), 1, 0),
         sumCard = yellow_cards_overall + red_cards_overall)

# Remove three columns “rank_in_league_top_attackers”, “rank_in_league_top_midfielders”, and “rank_in_league_top_defenders” from the dataset.
Data_Football <- Data_Football %>% 
  select(-rank_in_league_top_attackers,
         -rank_in_league_top_midfielders,
         -rank_in_league_top_defenders)

glimpse(Data_Football)

# Using the summarize function in dplyr, compute the mean, median and SD of the sumCard variable.
Data_Football %>% 
  summarise(mean = mean(sumCard),
            median = median(sumCard),
            sd = sd(sumCard))

# Using the summarize function in dplyr, compute the mean, median and SD of the sumCard variable for 
# player who are older and younger than the median age.
Data_Football %>% 
  group_by(medianSplit) %>% 
  summarise(mean = mean(sumCard),
            median = median(sumCard),
            sd = sd(sumCard))

# Create a table summarizing the mean number of goals scored for each position.
Data_Football %>% 
  group_by(position) %>% 
  summarise(mean = mean(goals_overall))

#Identify the max number of goals.
Data_Football %>% 
  summarise(mean = max(goals_overall))

# Find the number of rows in the dataset.
Data_Football %>% count()

# Save the cleaned dataset in a .csv file.
write.csv(Data_Football, "Data_Football_Cleaned.csv", row.names=FALSE)

#Use the seq() function to generate the sequence  9, 18, 27, 36, 45
seq(from = 9, to = 45, by = 9)

#Using the rep() function generate the sequence: 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4
rep(1:4, each = 3)


