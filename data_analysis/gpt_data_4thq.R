library(tidyverse)
library(nflreadr)
library(nflverse)

# Load play-by-play data for a specific season, e.g., 2023
pbp_data <- load_pbp(2023)

# Filter for fourth quarter plays only
q4_data <- pbp_data %>%
  filter(qtr == 4)

# Calculate team statistics for the fourth quarter (rushing yards, passing yards, total yards) per game
q4_team_game_stats <- q4_data %>%
  group_by(game_id, posteam) %>%
  summarize(
    Q4_RushingYards = sum(rushing_yards, na.rm = TRUE),
    Q4_PassingYards = sum(passing_yards, na.rm = TRUE),
    Q4_TotalYards = Q4_RushingYards + Q4_PassingYards,
    .groups = 'drop'
  )

# Assuming home_score and away_score represent final scores, extract these at the game level from the original dataset
game_scores <- pbp_data %>%
  select(game_id, home_team, away_team, home_score, away_score) %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(Winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE ~ "Tie" # or NA_character_ to indicate a tie differently
  ))

# Merge the fourth quarter team statistics with the winners and scores to include comprehensive game information
q4_final_stats_with_winners <- q4_team_game_stats %>%
  left_join(game_scores, by = "game_id") %>%
  select(game_id, posteam, Q4_RushingYards, Q4_PassingYards, Q4_TotalYards, home_score, away_score, Winner)

# Prepare the final data for modeling or analysis, including determining the win/loss status
q4_final_data_for_modeling <- q4_final_stats_with_winners %>%
  mutate(Win = ifelse(posteam == Winner, 1, 0))

q4_final_data_for_modeling <- q4_final_data_for_modeling %>%
  filter(!is.na(posteam))


# Output the final dataset for Q4 analysis
write_csv(q4_final_data_for_modeling, file = "~/nfl_analysis/data/analysis_data/q4_cleaned_data.csv")

# Note: You may need to adjust the file path for 'write_csv' to match your directory structure
