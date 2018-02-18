# Exploring Team_Attribute table
teamAttTbl = dbGetQuery(football, "SELECT * FROM Team_Attributes")
summary(teamAttTbl)
ncol(teamAttTbl)