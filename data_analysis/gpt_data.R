library(tidyverse)
library(nflreadr)
library(nflverse)

# Load play-by-play data for a specific season, e.g., 2022
pbp_data <- load_pbp(2023) # Adjust the season as needed
view(pbp_data)
# Calculate team statistics (rushing yards, passing yards, total yards) per game
team_game_stats <- pbp_data %>%
  group_by(game_id, posteam) %>%
  summarize(
    RushingYards = sum(rushing_yards, na.rm = TRUE),
    PassingYards = sum(passing_yards, na.rm = TRUE),
    TotalYards = RushingYards + PassingYards,
    .groups = 'drop'
  )

# Assuming home_score and away_score represent final scores, extract these at the game level
game_scores <- pbp_data %>%
  select(game_id, home_team, away_team, home_score, away_score) %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(Winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE ~ "Tie" # or NA_character_ to indicate a tie differently
  ))

# Merge the team statistics with the winners and scores to include comprehensive game information
final_stats_with_winners <- team_game_stats %>%
  left_join(game_scores, by = "game_id") %>%
  select(game_id, posteam, RushingYards, PassingYards, TotalYards, home_score, away_score, Winner)


# Assuming you have already loaded your play-by-play data into a variable called pbp_data
# Remove rows where 'posteam' is NA
cleaned_data <- final_stats_with_winners %>%
  filter(!is.na(posteam))

final_data_for_modeling <- cleaned_data %>%
  mutate(Win = ifelse(posteam == Winner, 1, 0))
# Now, you can proceed with your data analysis or modeling with cleaned_data
final_data_for_modeling 
# View the final dataset
write_csv(x = final_data_for_modeling, file = "~/nfl_analysis/data/analysis_data/gay_cleaned_data.csv")


