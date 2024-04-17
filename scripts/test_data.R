#### Preamble ####
# Purpose: Tests to make sure all my data is appropriate
# Author: Alexander Guarasci
# Date: 18 April, 2024
# Contact: alexander.guarasci@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "data_cleaning.R"


#### Workspace setup ####
library(tidyverse)

# Load the data
data <- read.csv(here::here("data/analysis_data/cleaned_data.csv"))

# Function to print test results
print_test_result <- function(test_name, result) {
  if (result) {
    message(test_name, ": PASS")
  } else {
    message(test_name, ": FAIL")
  }
}

# Test 'posteam' and 'Winner' are all 3-character words
print_test_result("posteam has 3-character words", all(nchar(data$posteam) == 3))
print_test_result("Winner has 3-character words", all(nchar(data$Winner) == 3))

# Test specific columns are all integers
integer_columns <- c(
  "RushingYards", "PassingYards", "TotalYards",
  "home_score", "away_score", "Q4_RushingYards",
  "Q4_PassingYards", "Q4_TotalYards"
)

for (col in integer_columns) {
  test_result <- all(data[[col]] == floor(data[[col]]))
  print_test_result(paste(col, "is integers"), test_result)
}

# Test 'Win' is only 0's and 1's
print_test_result("Win is only 0's and 1's", all(data$Win %in% c(0, 1)))
