## TODO- some refactoring

## results are grabbed from http://www.americansocceranalysis.com/by-game-2016/
results <- data.frame(read.csv('xg_results.csv', stringsAsFactors = FALSE))[, c("Home", "Away", "xGD")]
kTeamCount <- length(unique(results$Home))
kGamesPlayed <- nrow(results)

## data prep - there's probably a better way to do this solely in dplyr
## create a mapping from team name to number
team.mapping <- cbind.data.frame(team = unique(results$Home), number = seq(1, kTeamCount))
## merge mapping with results data, and drop actual team names
results <- merge(results, team.mapping, by.x = "Home", by.y = "team", all.x = TRUE)
results <- merge(results, team.mapping, by.x = "Away", by.y = "team", all.x = TRUE)
results <- results[, c("xGD", "number.x", "number.y")]
results <- setNames(results, c("xgd", "home", "away"))
## add a game number to each row
results$game.num <- seq(1, kGamesPlayed)

## initialize matrix with rows = number of games played, columns = number of teams + 1 (for home field advantage)
M <- matrix(nrow = kGamesPlayed, ncol = kTeamCount + 1, data = 0)
## now instantiate it with 1s and -1s for each game
AddGame <- function(game) {
    M[game[4], game[2]] <<- sign(game[1])
    M[game[4], game[3]] <<- -1 * sign(game[1])
}
## update M for each game
apply(results, 1, AddGame)
## update M for the home field advantage column
M[, kTeamCount + 1] <- rep(1, kGamesPlayed)
## grab our xgd column
xgd <- results$xgd

## now solve the linear equations M * ratings = xgd
fit <-fastLm(X = M, y = xgd) 
ratings <- fit$coefficients
## remove the na, replace with 0 since that's the baseline
ratings[is.na(ratings)] <- 0

## spit out the mean absolute error
mae <- sum(abs((M %*% ratings) - xgd))/kGamesPlayed
print(paste("The mean absolute error is:", mae))

## now write the team mapping and ratings to a csv
teams <- c(as.vector(team.mapping$team), "Home Field Adv.")
ratings <- cbind.data.frame(teams, ratings)
write.csv(ratings, "massey_ratings.csv")
