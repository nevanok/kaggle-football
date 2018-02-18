### R Setup
library("RSQLite")
library(tidyverse)
library(VIM)
library(ggthemes)
library(GGally)
library(gridExtra)
library(lubridate)

### Import data
sqlite    <- dbDriver("SQLite")
football <- dbConnect(sqlite,"C:/Users/Nevn/OneDrive/Documents/machineLearning/project2/database.sqlite")
dbListTables(football) # IMPORTANT lists tables in db object

matchTbl = dbGetQuery(football, "SELECT * FROM match")
teamTbl = dbGetQuery(football, "SELECT * FROM Team") # Want to cross reference APIs with team name

### Variables
# id is index (int)
# country_id links to country table (int)
# league_id links to league table  (int)
# season describes season, like 2016/2017 (char)
# stage, not sure of, might assign an incrementing number to each game, 1st game is 1, 2nd is 2, etc. (int)
# date, describes time & date in form 2008-08-17 00:00:00, need to clean this (char)
# match_api_id (int)
# home_team_api_id, away_team_api_id links to team table (int)
# home_team_goal, away_team_goal contain goals scored by each team (int)
# home_player_... & away_player_... can be dropped, contain info on players that I don't need now (int)
# goal, shoton, shotoff, foulcommit, card, cross, corner, possession are event logs, need cleaning (char)
# Remaining variables are bookies odds on the games

lapply(matchTbl, class) # wanted to know class of each column

matchTbl1 <- matchTbl %>% 
  separate(date, c('date','time'), " ", convert=TRUE) %>% # Separate date column into date & time
  filter(season == '2015/2016') %>% # only include 15/16 season
  select(-starts_with('home_player')) %>% # Wanted to remove all 'home_player_...' variables
  select(-starts_with('away_player')) %>%
  select(1:12) %>%
  select(-contains('time')) # Drop time since it is always 00:00:00, better way to drop it
  
  #select(-c('goal', 'shoton', 'shotoff', 'foulcommit', 'card', 'cross', 'corner', 'possession')) # gives error
  # ASK

# Now down to 11 variables of interest

matchTbl1 %>% count(country_id) 
matchTbl1 %>% count(country_id) %>% count(n)
matchTbl1 %>% count(league_id)
matchTbl1 %>% count(league_id) %>% count(n) # Count how many leagues had that count of matches

# Transforming variables
matchTbl1 <- matchTbl1 %>%
  mutate(date = ymd(date)) %>% # Want to transform date column from character to date object
  # select(-12) # Accidentally created a new column and had to  drop it, no need to run this again
  mutate(home_res = home_team_goal - away_team_goal) %>% # Compute score for home team
  mutate(outcome = ifelse(home_res > 0, 'h',
                          ifelse(home_res == 0, 'd', 'a'))) # Compute result for home team

head(matchTbl1)

ggplot(matchTbl1, aes(date, home_res)) +
  #geom_point()
  geom_line(aes(group = outcome)) # Most interested of the line graphs I tried
  #geom_line(aes(group = league_id))
  #geom_line(aes(group = countrty_id)) # Very messy plots
 
### Focus
# Pull in league table to cross ref IDs
leagueTbl = dbGetQuery(football, "SELECT * FROM league")

# Integrating league names
matchTbl2 <- matchTbl1 %>% # Wanted to avoid breaking table so created new one
  left_join(leagueTbl, by = 'country_id') %>% # Wanted to bring in league name
  select(-c(country_id, league_id,id.y)) %>% # Then wanted to drop unnecessary variables
  rename(league_name = name)

# Mean home result by league
matchTbl2 %>%
  group_by(league_name) %>%
  summarise(mean = mean(home_res)) %>%
  arrange(desc(mean))

# Mean goals scored by league (in 2015/2016 season)
matchTbl2 %>%
  group_by(league_name) %>%
  summarise(mean = mean(home_team_goal+away_team_goal)) %>%
  arrange(desc(mean))


# Mean home goals scored by team in premier league
# First use joins to get home and away team names in
matchTbl3 <- matchTbl2 %>% # Wanted to avoid breaking table so created new one
  left_join(teamTbl[,c(2,5)], by = c('home_team_api_id' = 'team_api_id')) %>% # Only wanted 1 column from teamTable
  rename(home_team_name = team_short_name ) %>%
  left_join(teamTbl[,c(2,5)], by = c('away_team_api_id' = 'team_api_id')) %>% # Now away names
  rename(away_team_name = team_short_name)

# Want average number of goals scored by each team at home
matchTbl3 %>%
  filter(league_name == 'England Premier League') %>%
  group_by(home_team_name) %>%
  summarise(mean = mean(home_team_goal)) %>%
  arrange(desc(mean))
 
# Average number of goals conceded by each team at home
matchTbl3 %>%
  filter(league_name == 'England Premier League') %>%
  group_by(home_team_name) %>%
  summarise(mean = mean(away_team_goal)) %>%
  arrange(desc(mean))

### More plots
# In a better position to plot now because of above experience
# Plotting premier league home team results over time
prem16 = matchTbl3 %>%
  filter(league_name == 'England Premier League')
ggplot(prem16, aes(date, home_res)) +
  geom_line(aes(group = home_team_name), alpha = .2) 

# Now want united's home results over time
ggplot(prem16 %>% filter(home_team_name == 'MUN'), aes(date, home_res)) +
  geom_line(aes(group = home_team_name), alpha = .5) 

# United vs City performance
ggplot(prem16 %>% filter(home_team_name == c('MUN','MCI')), aes(date, home_res)) +
  geom_point(aes(group = home_team_name), alpha = .5) +
  geom_line(aes(group = home_team_name), alpha = .5) +
  theme_solarized() 
  
