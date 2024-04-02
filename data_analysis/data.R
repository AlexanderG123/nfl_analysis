library(tidyverse)
library(nflverse)

# Load play-by-play data for a specific season
pbp_data <- load_pbp(2023) # Adjust the season as needed

# Calculate overall team statistics per game
team_game_stats <- pbp_data %>%
  group_by(game_id, posteam) %>%
  summarize(RushingYards = sum(rushing_yards, na.rm = TRUE),
            PassingYards = sum(passing_yards, na.rm = TRUE),
            TotalYards = RushingYards + PassingYards,
            .groups = 'drop')

# Calculate Q4 team statistics per game
q4_team_game_stats <- pbp_data %>%
  filter(qtr == 4) %>%
  group_by(game_id, posteam) %>%
  summarize(Q4_RushingYards = sum(rushing_yards, na.rm = TRUE),
            Q4_PassingYards = sum(passing_yards, na.rm = TRUE),
            Q4_TotalYards = Q4_RushingYards + Q4_PassingYards,
            .groups = 'drop')

# Extract game scores and determine the winner
game_scores <- pbp_data %>%
  select(game_id, home_team, away_team, home_score, away_score) %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(Winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE ~ "Tie"
  ))

# Merge overall stats, Q4 stats, and game scores to create a comprehensive dataset
final_data_with_q4 <- team_game_stats %>%
  left_join(q4_team_game_stats, by = c("game_id", "posteam")) %>%
  left_join(game_scores, by = "game_id") %>%
  mutate(Win = ifelse(posteam == Winner, 1, 0)) %>%
  select(game_id, posteam, RushingYards, PassingYards, TotalYards, home_score, away_score, Winner, Win, Q4_RushingYards, Q4_PassingYards, Q4_TotalYards)

# Filtering step to remove any rows where 'posteam' might be NA, ensuring cleanliness
cleaned_data_1 <- final_data_with_q4 %>%
  filter(!is.na(posteam))

# Display the structure of the cleaned data to confirm column arrangements
glimpse(cleaned_data)


write_csv(cleaned_data, file = "~/nfl_analysis/data/analysis_data/cleaned_data.csv")