# Exploring Country table
dbListTables(football) 
countryTbl = dbGetQuery(football, "SELECT * FROM country")
typeof(countryTbl)
summary(countryTbl)
head(countryTbl)

# Exploring League table
# 11 rows, 3 columns
dbListTables(football)
leagueTbl = dbGetQuery(football, "SELECT * FROM league")
summary(leagueTbl)
nrow(leagueTbl)

# Exploring Team table
# 299 rows, 5 columns
teamTbl = dbGetQuery(football, "SELECT * FROM Team")
summary(teamTbl)
nrow(teamTbl)
length(unique(teamTbl$team_short_name))
length(teamTbl$team_short_name)
