library(tidyverse)
library(nflverse)
library(caret)

df <- read.csv("~/nfl_analysis/data/analysis_data/cleaned_data.csv")  # Update the path to your CSV file


set.seed(10) 
trainingIndex <- createDataPartition(df$Win, p = .8, list = FALSE)
train_data <- df[trainingIndex, ]
test_data <- df[-trainingIndex, ]


model_1 <- glm(Win ~ RushingYards + PassingYards, data = train_data, family = binomial())


summary(model_1)


predictions_prob <- predict(model_1, newdata = test_data, type = "response")
predictions_class <- ifelse(predictions_prob > 0.5, 1, 0)


confusionMatrix(factor(predictions_class), factor(test_data$Win))

save(model_1, confusion_matrix_q4, file = "~/nfl_analysis/models/model_1.RData")