# Exploring match table
library(stringr)
# Firstly, need easy way of checking team_api_id against team_long_name
# Could just replace team_api_id with team_long_name? team_short_name has duplicates
length(unique(teamTbl$team_long_name))
length(teamTbl$team_long_name) # There are 3 duplicate names

which(duplicated(teamTbl$team_long_name)) # rows 25,184, 200 are duplicates
teamTbl[c(25,184,200),]

which(teamTbl$team_long_name == 'Polonia Bytom') # 183,184
which(teamTbl$team_long_name == 'Royal Excel Mouscron') # 16, 25
which(teamTbl$team_long_name == 'Widzew Lódz') # Return none??? Issue with the accent?
which(teamTbl$team_fifa_api_id == 301) # 190, 200

teamTblDupeIndex = teamTbl[c(16,25,183,184,190,200),2] # Looks like duplicates arose because of new team_short_name
which(teamTbl$team_short_name == 'LOD')
which(teamTbl$team_short_name == 'POB')
which(teamTbl$team_short_name == 'MOU')

teamTbl[c(25,184,190),]

# Not sure whether to drop duplicates, they may be associated with matches

matchTbl = dbGetQuery(football, "SELECT * FROM Match") # Read match table in as df

unique(matchTbl[(matchTbl$home_team_api_id %in% teamTblDupeIndex),8]) # 9996 274581 8031 8244 8020 8024
unique(matchTbl[(matchTbl$away_team_api_id %in% teamTblDupeIndex),9]) # 9996 274581 8031 8244 8020 8024
# So  the duplicates are associate with matches. Could leave them in as I may not be looking at the 
# affected leagues.
# If I want to deal with them I reckon the best idea is to reassign the two api values as one value
# TO DO

##########################################
##########################################
summary(matchTbl)

# There's alot of missing data, 11762 rows. Might be based on league or date???
# Weird, event log columns: goal, shoton, shotoff, foulcommit, card, cross, corner, possession
# Bookies odds are columns 86:115
# TO DO
# Looking at goal variable, seems to be event log for goals scored.
sum(is.na(matchTbl$goal)) 
which(complete.cases(matchTbl$goal))
length(matchTbl$goal)
matchTbl[2728,5:14]

which(colnames(matchTbl)=="away_team_goal")
matchTbl[which(complete.cases(matchTbl$goal)),c(10,11)]
matchTbl[which(is.na(matchTbl$goal)),c(3,4)]
matchTbl$possession[which(complete.cases(matchTbl$goal))]


##########################################
##########################################
# Exploring results
# Compute result col, homeGoals - awayGoals
# Create subset first, can drop most columns
matchTblSubset = matchTbl[, 1:11]
matchTblSubset$home_result = matchTblSubset$home_team_goal - matchTblSubset$away_team_goal

resultsNumBar <- ggplot(matchTblSubset, aes(x=home_result, fill = result)) + geom_bar() + theme_solarized(light=TRUE) +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(title="Bar Chart", subtitle="Aggregate home goals", caption="Source: kaggle.com/hugomathien/soccer", 
      x = 'Home Goals - Away Goals', y = 'Number of Matches') + 
  scale_x_continuous(breaks = seq(min(matchTblSubset$home_result), max(matchTblSubset$home_result), 1),
                   labels = seq(min(matchTblSubset$home_result), max(matchTblSubset$home_result), 1))

# Compute new column to indicate home win, draw or away win (h,d,a)
# Nested ifelse, ifelse(test, yes, no)
matchTblSubset$result = ifelse(matchTblSubset$home_result > 0, 'h',
                               ifelse(matchTblSubset$home_result == 0, 'd', 'a'))

resultsCatBar <- ggplot(matchTblSubset, aes(x=result, fill=result)) + geom_bar() + theme_solarized(light=TRUE) +
  labs(title="Bar Chart", subtitle="Results of matches", caption="Source: kaggle.com/hugomathien/soccer", 
       x = 'Result', y = 'Number of Matches') +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  scale_x_discrete(labels = c('a' = 'Away Win', 'd' = 'Draw', 'h' = 'Home Win'))

grid.arrange(resultsNumBar,resultsCatBar, nrow=1)

##########################################
##########################################
# Look at one team: Manchester United, team_api_id = 10260
# Compare united performance to everyone else
teamTbl[which(teamTbl$team_short_name == 'MUN'),] # Gives team_api_id for united
teamTbl[teamTbl$team_short_name == 'MUN',]

matchTblSubset[matchTblSubset$home_team_api_id == 10260,] # This is the subset of united games

resultsNumBarMUN <- ggplot(matchTblSubset[matchTblSubset$home_team_api_id == 10260,], 
                           aes(x=home_result, fill = result)) +
  geom_bar() + theme_solarized(light=TRUE) + scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(title="Bar Chart", subtitle="Man United aggregate home goals", caption="Source: kaggle.com/hugomathien/soccer", 
       x = 'Home Goals - Away Goals', y = 'Number of Matches') + 
  scale_x_continuous(breaks = seq(min(matchTblSubset$home_result), max(matchTblSubset$home_result), 1),
                     labels = seq(min(matchTblSubset$home_result), max(matchTblSubset$home_result), 1))

resultsCatBarMUN <- ggplot(matchTblSubset[matchTblSubset$home_team_api_id == 10260,], 
                           aes(x=result, fill=result)) + geom_bar() + theme_solarized(light=TRUE) +
  labs(title="Bar Chart", subtitle="Results of matches", caption="Source: kaggle.com/hugomathien/soccer", 
       x = 'Result', y = 'Number of Matches') +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  scale_x_discrete(labels = c('a' = 'Away Win', 'd' = 'Draw', 'h' = 'Home Win'))

# Comparison of home results between united and all other teams
grid.arrange(resultsNumBar,resultsCatBar, resultsNumBarMUN, resultsCatBarMUN, nrow=2, name = 'test')


##########################################
##########################################
# Subset further to get one season, '2015/2016'
# Remember this only includes home games
united2016 = matchTblSubset[((matchTblSubset$home_team_api_id == 10260) | (matchTblSubset$away_team_api_id == 10260)) 
                            & (matchTblSubset$season == '2015/2016'),]
typeof(united2016$date) # date column is of class character, need to remove time

united2016$date = str_replace(united2016$date, ' 00:00:00', '') # Remove time detail from date column
# All times were the same

# Now need to convert date character string in to date object for time series plot

##########################################
##########################################
# Match Plots for interesting questions, TO DO
# Home & away win/loss/draw graphic
# Time series for team over season, each match plotted as a point.
# matchTblSubset = matchTbl[1:300,]
homeWins = nrow(matchTbl[matchTbl$home_team_goal > matchTbl$away_team_goal,])
draws = nrow(matchTbl[matchTbl$home_team_goal == matchTbl$away_team_goal,])
homeLosses = nrow(matchTbl[matchTbl$home_team_goal < matchTbl$away_team_goal,])

# Function to plot home win/loss/draw for specified team in specified season
teamSelectAPI = 10260
seasonSelect = '2015/2016'

homeRatios <- function(team_api, season_select){
  homeWins = nrow(matchTbl[(matchTbl$season == season_select) & (matchTbl$home_team_api_id == team_api) & 
                             (matchTbl$home_team_goal > matchTbl$away_team_goal),])
  homeDraws = nrow(matchTbl[(matchTbl$season == season_select) & (matchTbl$home_team_api_id == team_api) & 
                              (matchTbl$home_team_goal == matchTbl$away_team_goal),])
  homeLosses = nrow(matchTbl[(matchTbl$season == season_select) & (matchTbl$home_team_api_id == team_api) & 
                               (matchTbl$home_team_goal < matchTbl$away_team_goal),])
  teamName = teamTbl[teamTbl$team_api_id == team_api, 2]
  tempDF = cbind(data.frame(row.names = c('homeWins', 'homeDraws','homeLosses')), 
                 c(homeWins,homeDraws,homeLosses))
  ggplot(tempDF, aes(x = tempDF[,1])) + geom_bar()
}

homeRatios(teamSelectAPI,seasonSelect)

