library(tidyverse)

ggplot(final_data_for_modeling, aes(x = RushingYards, y = PassingYards, color = as.factor(Win))) +
  geom_point() +
  labs(title = "Rushing Yards vs. Passing Yards by Win/Loss",
       x = "Rushing Yards",
       y = "Passing Yards",
       color = "Outcome") +
  theme_minimal()

ggplot(final_data_for_modeling, aes(x = as.factor(Win), y = TotalYards, fill = as.factor(Win))) +
  geom_boxplot() +
  labs(title = "Distribution of Total Yards by Win/Loss",
       x = "Outcome (0 = Loss, 1 = Win)",
       y = "Total Yards",
       fill = "Outcome") +
  theme_minimal()

ggplot(final_data_for_modeling, aes(x = TotalYards)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Total Yards",
       x = "Total Yards",
       y = "Count") +
  theme_minimal()

final_data_for_modeling %>%
  group_by(Win) %>%
  summarize(AvgTotalYards = mean(TotalYards, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(Win), y = AvgTotalYards, fill = as.factor(Win))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Total Yards by Win/Loss",
       x = "Outcome (0 = Loss, 1 = Win)",
       y = "Average Total Yards",
       fill = "Outcome") +
  theme_minimal()

ggplot(final_data_for_modeling, aes(x = RushingYards, fill = as.factor(Win))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Rushing Yards by Win/Loss",
       x = "Rushing Yards",
       y = "Density",
       fill = "Outcome") +
  theme_minimal()

ggplot(final_data_for_modeling, aes(x = PassingYards, fill = as.factor(Win))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Passing Yards by Win/Loss",
       x = "Rushing Yards",
       y = "Density",
       fill = "Outcome") +
  theme_minimal()

ggplot(final_data_for_modeling, aes(x = RushingYards, y = TotalYards)) +
  geom_point(aes(color = as.factor(Win)), alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Rushing Yards vs. Total Yards with Regression Line",
       x = "Rushing Yards",
       y = "Total Yards",
       color = "Outcome") +
  theme_minimal()


# Calculate average yards for wins and losses
avg_yards_by_outcome <- final_data_for_modeling %>%
  group_by(Win) %>%
  summarize(AvgRushingYards = mean(RushingYards, na.rm = TRUE),
            AvgPassingYards = mean(PassingYards, na.rm = TRUE)) %>%
  pivot_longer(cols = -Win, names_to = "YardType", values_to = "AverageYards")

# Plot
ggplot(avg_yards_by_outcome, aes(x = as.factor(Win), y = AverageYards, fill = YardType)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("AvgRushingYards" = "skyblue", "AvgPassingYards" = "orange")) +
  labs(title = "Average Rushing and Passing Yards for Wins and Losses",
       x = "Outcome (0 = Loss, 1 = Win)",
       y = "Average Yards",
       fill = "Type of Yards") +
  theme_minimal()

q4_final_data_for_modeling <-  read.csv("~/nfl_analysis/data/analysis_data/cleaned_data.csv") 
#4th q data Q4_RushingYards,Q4_PassingYards
ggplot(q4_final_data_for_modeling, aes(x = Q4_RushingYards, y = Q4_PassingYards, color = as.factor(Win))) +
  geom_point() +
  labs(title = "Rushing Yards vs. Passing Yards by Win/Loss 4th Q",
       x = "Rushing Yards",
       y = "Passing Yards",
       color = "Outcome") +
  theme_minimal()

ggplot(q4_final_data_for_modeling, aes(x = Q4_PassingYards, fill = as.factor(Win))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Passing Yards by Win/Loss 4th q",
       x = "Passing Yards",
       y = "Density",
       fill = "Outcome") +
  theme_minimal()

ggplot(q4_final_data_for_modeling, aes(x = Q4_RushingYards, fill = as.factor(Win))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Passing Yards by Win/Loss 4th q",
       x = "Rushing Yards",
       y = "Density",
       fill = "Outcome") +
  theme_minimal()



# Assuming your Q4 data is loaded into a variable called q4_data
# First, read the Q4 data from the given structure
q4_data <- q4_final_data_for_modeling

# Calculate average Q4 yards for wins and losses
avg_q4_yards_by_outcome <-  read.csv("~/nfl_analysis/data/analysis_data/cleaned_data.csv") %>%
  group_by(Win) %>%
  summarize(AvgRushingYards = mean(Q4_RushingYards, na.rm = TRUE),
            AvgPassingYards = mean(Q4_PassingYards, na.rm = TRUE)) %>%
  pivot_longer(cols = -Win, names_to = "YardType", values_to = "AverageYards")

# Plot the average Q4 rushing and passing yards for wins and losses
ggplot(avg_q4_yards_by_outcome, aes(x = as.factor(Win), y = AverageYards, fill = YardType)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("AvgRushingYards" = "skyblue", "AvgPassingYards" = "orange")) +
  labs(title = "Average Q4 Rushing and Passing Yards for Wins and Losses",
       x = "Outcome (0 = Loss, 1 = Win)",
       y = "Average Yards in Q4",
       fill = "Type of Yards") +
  theme_minimal()

data <- read.csv("~/nfl_analysis/data/analysis_data/cleaned_data.csv")
ggplot(data, aes(x = as.factor(Win), y = Q4_TotalYards, fill = as.factor(Win))) +
  geom_violin()+
  labs(title = "Distribution of Total Yards by Win/Loss",
       x = "Outcome (0 = Loss, 1 = Win)",
       y = "Total Yards",
       fill = "Outcome") +
  theme_minimal()
