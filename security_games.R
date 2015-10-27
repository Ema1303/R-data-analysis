# R-data-analysis
# R code used to analyze data extracted for the survey on security games and plot the corresponding charts


# libraries needed
# one library is needed to rename the columns of the dataset
library(plyr)

# DATA IMPORT

# Data is imported and stored into a data.frame game_theory.
# Because we only need data about the types of information used in games, game types, number of players, and application purpose, a subset of the corresponding columns is extracted and stored into a data.frame called ``games''.  
# a column is renamed in the data.frame (Nr..of.players to Players)
game_theory <- read.csv("import.csv", sep=";")
games <- game_theory[,c("Information", "Nr..of.players", "Games", "Domain")]
games <- rename(games, c("Nr..of.players"="Players"))

# DATA TRANSFORMATION

# types of information (incomplete, complete) and percentage calculation
information <- tapply(games$Information, games$Information, length)
pct <- round(information/sum(information)*100)
# game types (non-cooperative, cooperative) and percentage calculation
NC <- length(grep("B", games$Games))
noncooperative <- length(grep("Non", games$Games))
cooperative <- length(grep("C", games$Games))
NA_games <- length(grep("NA", games$Games))
game_type <- c(noncooperative, cooperative, NA_games, NC)
game_type <- setNames(game_type, c("Noncoop.", "Cooperative", "NA", "(Non)cooperative"))
pct <- round(game_type/sum(game_type)*100)
# number of players (2, N) and percentage calculation
players_2N <- length(grep("2, N", games$Players))
players_2 <- length(grep("2", games$Players))-players_2N
players_N <- length(grep("N", games$Players))-players_2N
players <- c(players_2N, players_2, players_N)
players <- setNames(players, c("2/N-player", "2-player", "N-player"))
pct <- round(players/sum(players)*100)
# application of games (4 application types: defense against attacks, foster cooperation among nodes, preserve privacy, other) and percentage calculation
collaboration <- length(grep("cooperation", games$Domain, ignore.case=TRUE))
attacks <- length(grep("attack", games$Domain, ignore.case=TRUE))
privacy <- length(grep("privacy", games$Domain, ignore.case=TRUE))
risk <- length(grep("risk", games$Domain, ignore.case=TRUE))
mcommerce <- length(grep("m-commerce", games$Domain, ignore.case=TRUE))
other <- risk + mcommerce
domain <- c(collaboration, attacks, privacy, other)
domain <- setNames(domain, c("Node cooperation", "Attack defense", "Privacy", "Other"))
pct <- round(domain/sum(domain)*100)

# REPORT

# Information types are presented in a pie chart. 
# Set the labels to show percentages
lbls <- paste(names(information), pct)
lbls <- paste(lbls,"%",sep="")
lblsg <- paste(names(game_type), pct)
lblsg <- paste(lblsg, "%", sep="")
lblsp <- paste(names(players), pct)
lblsp <- paste(lblsp, "%", sep="")
lblsd <- paste(names(domain), pct)
lblsd <- paste(lblsd, "%", sep="")

# Draw 4 pie charts in one frame
par(mfrow=c(2,2), mar=c(3,2,3,3), las=1)
pie(information, label=lbls, col=gray.colors(4, start=0.1), main="a) Information type")
pie(game_type, label=lblsg, col=gray.colors(4, start=0.6), main="b) Game type")
pie(players, label=lblsp, col=gray.colors(3, start=0.3), main="c) Number of players")
pie(domain, label=lblsd, col=gray.colors(4, start=0.5), main="d) Application of games")
