library("RSQLite")
library(tidyverse)
library(VIM)
library(ggthemes)
library(GGally)
library(gridExtra)

sqlite    <- dbDriver("SQLite")
football <- dbConnect(sqlite,"C:/Users/Nevn/OneDrive/Documents/machineLearning/project2/database.sqlite")

dbListTables(football) # IMPORTANT lists tables in db object
