#### Preamble ####
# Purpose: Cleans and writes the data
# Author: Alexander Guarasci
# Date: 18 April, 2024
# Contact: alexander.guarasci@mail.utoronto.ca
# License: MIT
# Pre-requisites: install nflverse and tidyverse

library(tidyverse)
library(nflverse)

# load dta
pbp_data <- load_pbp(2023)

# construct variiables for rushing yards, passing yards and total yards
team_game_stats <- pbp_data |>
  group_by(game_id, posteam) |>
  summarize(
    RushingYards = sum(rushing_yards, na.rm = TRUE),
    PassingYards = sum(passing_yards, na.rm = TRUE),
    TotalYards = RushingYards + PassingYards,
    .groups = "drop"
  )

# construct variiables for q4 rushing yards, passing yards and total yards
q4_team_game_stats <- pbp_data |>
  filter(qtr == 4) |>
  group_by(game_id, posteam) |>
  summarize(
    Q4_RushingYards = sum(rushing_yards, na.rm = TRUE),
    Q4_PassingYards = sum(passing_yards, na.rm = TRUE),
    Q4_TotalYards = Q4_RushingYards + Q4_PassingYards,
    .groups = "drop"
  )

# construct variiables for winner
game_scores <- pbp_data |>
  select(game_id, home_team, away_team, home_score, away_score) |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(Winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE ~ "Tie"
  ))

# put it all together
final_data_with_q4 <- team_game_stats |>
  left_join(q4_team_game_stats, by = c("game_id", "posteam")) |>
  left_join(game_scores, by = "game_id") |>
  mutate(Win = ifelse(posteam == Winner, 1, 0)) |>
  select(game_id, posteam, RushingYards, PassingYards, TotalYards, home_score, away_score, Winner, Win, Q4_RushingYards, Q4_PassingYards, Q4_TotalYards)


cleaned_data <- final_data_with_q4 |>
  filter(!is.na(posteam))

write_csv(cleaned_data, file = here::here("data/analysis_data/cleaned_data.csv"))
