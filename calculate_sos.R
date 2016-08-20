## disclaimer that there's probably a better way to factor this
library(dplyr)

## use the massey ratings to determine strength of schedule for each team

## read in ratings, results, and schedule
ratings <- data.frame(read.csv('massey_ratings.csv', stringsAsFactors = FALSE))
results <- data.frame(read.csv('xg_results.csv', stringsAsFactors = FALSE))
schedule <- data.frame(read.csv('schedule16/2016.csv', stringsAsFactors = FALSE))[, c("Game.Time", "Home", "Away")]
## add an id so we can preserve ordering later
schedule$id <- seq(1, nrow(schedule))

## map teams as they are in the schedule, to teams as they are in the rating
mapping <- cbind.data.frame(schedule.name = c("New York Red Bulls", "Orlando City", "Chicago Fire", "FC Dallas", "Houston Dynamo", "San Jose Earthquakes", "Portland Timbers", "Vancouver Whitecaps FC", "Seattle Sounders FC", "LA Galaxy", "New England Revolution", "Real Salt Lake", "Colorado Rapids", "Montreal Impact", "Columbus Crew SC", "Sporting Kansas City", "New York City FC", "Philadelphia Union", "D.C. United", "Toronto FC"), rating.name = c("New York", "Orlando City", "Chicago", "FC Dallas", "Houston", "San Jose", "Portland", "Vancouver", "Seattle", "L.A. Galaxy", "New England", "Salt Lake", "Colorado", "Montreal", "Columbus", "Kansas City", "New York City FC", "Philadelphia", "DC United", "Toronto"))

## join mapping with ratings
mapping <- merge(mapping, ratings, by.x = "rating.name", by.y = "teams")
## join ratings with schedule
schedule <- merge(schedule, mapping, by.x = "Home", by.y = "schedule.name")
schedule <- merge(schedule, mapping, by.x = "Away", by.y = "schedule.name")
schedule <- schedule[, c("Away", "Home", "Game.Time", "id", "ratings.x", "ratings.y")]
## rename columns and rearrange to re-presever chronological order
schedule <- arrange(setNames(schedule, c("Away", "Home", "Game.Time", "id", "home.rating", "away.rating")), id)

## adjust home team ratings for home field advantage
kHomeFieldAdv <- ratings[ratings$teams == 'Home Field Adv.', ]$ratings
schedule$home.rating <- schedule$home.rating + kHomeFieldAdv


## given a team and a schedule, calc the strength of schedule
CalcSos <- function(name, team.schedule) {
    ## strength of schedule for the team's home games
    homeSos <- sum(filter(team.schedule, Home == name)$away.rating)
    ## strength of schedule for the team's away games
    awaySos <- sum(filter(team.schedule, Away == name)$home.rating)
    return((homeSos + awaySos)/nrow(team.schedule))
}

## calculate average ratings of previous schedule and remaining schedule
SosSplit <- function(schedule.name, team.name, schedule) {
    team.schedule <- filter(schedule, Home == schedule.name | Away == schedule.name)
    games.played <- nrow(filter(results, Home == team.name | Away == team.name))
    prev.games <- team.schedule[1:games.played, ]
    remaining.games <- team.schedule[(games.played + 1):nrow(team.schedule), ]
    prev.sos <- CalcSos(schedule.name, prev.games)
    remaining.sos <- CalcSos(schedule.name, remaining.games)
    return(c(prev.sos, remaining.sos))
}

## now get the SOS splits for each team


