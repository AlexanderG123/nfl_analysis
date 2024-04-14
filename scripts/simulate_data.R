#### Preamble ####
# Purpose: Simulates Data
# Author: Alexander Guarasci
# Date: 18 April, 2024
# Contact: alexander.guarasci@mail.utoronto.ca
# License: MIT
# Pre-requisites: install nflverse and tidyverse

# Simulate some data for NFL games
set.seed(123) # Ensure reproducibility

# Define the number of games to simulate
n_games <- 285

# Teams
teams <- c("ARI", "WAS", "BUF", "NYJ", "CAR", "ATL", "CIN", "CLE", "DAL", "NYG", "DET", "KC", "GB", "CHI", "HOU", "BAL")

# Generate game IDs
game_ids <- paste(2023, seq_len(n_games), sep = "_")

# Simulate data for each game
simulated_data <- do.call(rbind, lapply(seq_len(n_games), function(i) {
  home_team <- sample(teams, 1)
  away_team <- sample(teams[teams != home_team], 1)

  home_RushingYards <- sample(50:200, 1)
  away_RushingYards <- sample(50:200, 1)

  home_PassingYards <- sample(150:400, 1)
  away_PassingYards <- sample(150:400, 1)

  home_TotalYards <- home_RushingYards + home_PassingYards
  away_TotalYards <- away_RushingYards + away_PassingYards

  home_score <- sample(0:50, 1)
  away_score <- sample(0:50, 1)

  Winner <- ifelse(home_score > away_score, home_team, ifelse(home_score < away_score, away_team, "TIE"))
  Win_home <- ifelse(home_score > away_score, 1, 0)
  Win_away <- ifelse(away_score > home_score, 1, 0)

  Q4_home_RushingYards <- sample(20:100, 1)
  Q4_away_RushingYards <- sample(20:100, 1)

  Q4_home_PassingYards <- sample(50:200, 1)
  Q4_away_PassingYards <- sample(50:200, 1)

  Q4_home_TotalYards <- Q4_home_RushingYards + Q4_home_PassingYards
  Q4_away_TotalYards <- Q4_away_RushingYards + Q4_away_PassingYards

  rbind(
    c(
      game_id = game_ids[i], posteam = home_team, RushingYards = home_RushingYards,
      PassingYards = home_PassingYards, TotalYards = home_TotalYards, home_score = home_score,
      away_score = away_score, Winner = Winner, Win = Win_home, Q4_RushingYards = Q4_home_RushingYards,
      Q4_PassingYards = Q4_home_PassingYards, Q4_TotalYards = Q4_home_TotalYards
    ),
    c(
      game_id = game_ids[i], posteam = away_team, RushingYards = away_RushingYards,
      PassingYards = away_PassingYards, TotalYards = away_TotalYards, home_score = home_score,
      away_score = away_score, Winner = Winner, Win = Win_away, Q4_RushingYards = Q4_away_RushingYards,
      Q4_PassingYards = Q4_away_PassingYards, Q4_TotalYards = Q4_away_TotalYards
    )
  )
}))

# Convert the data to a data frame and fix types
simulated_data_df <- as.data.frame(simulated_data, stringsAsFactors = FALSE)
numerical_columns <- c("RushingYards", "PassingYards", "TotalYards", "home_score", "away_score", "Win", "Q4_RushingYards", "Q4_PassingYards", "Q4_TotalYards")
simulated_data_df[numerical_columns] <- lapply(simulated_data_df[numerical_columns], as.numeric)

write_csv(simulated_data_df, file = "~/nfl_analysis/data/analysis_data/sim_data.csv")
