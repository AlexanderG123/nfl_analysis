library(dplyr)

# Read the dataset
df <- read.csv('~/nfl_analysis/data/analysis_data/cleaned_data.csv')

# Create a helper function to identify wins with fewer rushing or passing yards
analyze_wins <- function(df) {
  # Pair each team with its opponent within the same game
  df <- df %>%
    group_by(game_id) %>%
    mutate(opponent_rushing_yards = lead(RushingYards, order_by = game_id, default = first(RushingYards)),
           opponent_passing_yards = lead(PassingYards, order_by = game_id, default = first(PassingYards))) %>%
    ungroup()
  
  # Count wins with fewer rushing yards than opponent
  wins_with_fewer_rushing_yards <- sum(df$Win == 1 & df$RushingYards < df$opponent_rushing_yards, na.rm = TRUE)
  
  # Count wins with fewer passing yards than opponent
  wins_with_fewer_passing_yards <- sum(df$Win == 1 & df$PassingYards < df$opponent_passing_yards, na.rm = TRUE)
  
  return(list(wins_with_fewer_rushing_yards = wins_with_fewer_rushing_yards,
              wins_with_fewer_passing_yards = wins_with_fewer_passing_yards))
}

# Analyze the data
results <- analyze_wins(df)

# Print the results
print(paste("Number of wins with fewer rushing yards than opponent:", results$wins_with_fewer_rushing_yards))
print(paste("Number of wins with fewer passing yards than opponent:", results$wins_with_fewer_passing_yards))


library(dplyr)

# Assuming df is your dataframe loaded from cleaned_data.csv
# First, create opponent columns for rushing and passing yards to compare within the same game
df <- df %>%
  group_by(game_id) %>%
  mutate(
    opponent_rushing_yards = if_else(posteam == lead(posteam, order_by = game_id), lag(RushingYards), lead(RushingYards)),
    opponent_passing_yards = if_else(posteam == lead(posteam, order_by = game_id), lag(PassingYards), lead(PassingYards))
  ) %>%
  ungroup()

# Now, filter for conditions where a team won with more rushing AND more passing yards than their opponent
wins_with_more_rushing_and_passing_yards <- df %>%
  filter(Win == 1 & RushingYards > opponent_rushing_yards & PassingYards > opponent_passing_yards) %>%
  nrow()

# Print the result
print(paste("Number of wins with more rushing and passing yards than opponent:", wins_with_more_rushing_and_passing_yards))

library(dplyr)

# Assuming df is your dataframe loaded from cleaned_data.csv
# Adjusting the code to consider losses where the team had more of both rushing and passing yards
losses_with_more_rushing_and_passing_yards <- df %>%
  filter(Win == 0 & RushingYards > opponent_rushing_yards & PassingYards > opponent_passing_yards) %>%
  nrow()

# Print the result
print(paste("Number of losses with more rushing and passing yards than opponent:", losses_with_more_rushing_and_passing_yards))

