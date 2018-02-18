library("RSQLite")
library(tidyverse)
library(nycflights13)
library(VIM)
library(ggthemes)
library(GGally)
library(gridExtra)

sqlite    <- dbDriver("SQLite")
football <- dbConnect(sqlite,"C:/Users/Nevn/OneDrive/Documents/machineLearning/project2/database.sqlite")

dbListTables(football) # IMPORTANT lists tables in db object

typeof(football)
select(football)

# Put data in a df ('S4') from a SQL query
countryTbl = dbGetQuery(football, "SELECT * FROM country")
leagueTbl = dbGetQuery(football, "SELECT * FROM league")
matchTbl = dbGetQuery(football, "SELECT * FROM match")
playerTbl = dbGetQuery(football, "SELECT * FROM league")
teamTbl = dbGetQuery(football, "SELECT * FROM Team")
league = dbGetQuery(football, "SELECT * FROM league")

head(matchTbl)
summary(matchTbl)
colnames(matchTbl)
nrow(matchTbl)
matchTblSamp = matchTbl[1:1000,]

# Percentage of games won by home team
matchTblSamp$home_team_api_id[1:10]
matchTblSamp$home_team_goal[1:10]

gamesWonHomeSamp = length(matchTblSamp[matchTblSamp$home_team_goal > matchTblSamp$away_team_goal,1])

gamesWonHome = length(matchTbl[matchTbl$home_team_goal > matchTbl$away_team_goal,1])
gamesPlayed = length(matchTbl[,1])
percentageHomeWin = 100*gamesWonHome/gamesPlayed

# Looking at team table, relates att. in match table a team name
matchTblSamp[1:10,1:6]
teamTbl[1:10,]
# Finding man united
which(teamTbl$team_short_name == 'MUN')
teamTbl[26,]
unitedID = teamTbl$team_api_id[26]

#############################
# Broke the dataframe at this point. Over wrote the entire home_team_api with the unitedID. Trek.
a1 = length(matchTbl[(matchTbl$home_team_api_id = unitedID)&(matchTbl$home_team_goal > matchTbl$away_team_goal),1])
a2 = length(matchTbl[(matchTbl$home_team_api_id = unitedID),1])
p1 = 100*a1/a2
