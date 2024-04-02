library(tidyverse)
library(caret)


df <- read.csv("~/nfl_analysis/data/analysis_data/cleaned_data.csv")  # Update the path to your CSV file


set.seed(10)
trainingIndex <- createDataPartition(df$Win, p = .8, list = FALSE)
train_data <- df[trainingIndex, ]
test_data <- df[-trainingIndex, ]

model_q4 <- glm(Win ~ Q4_RushingYards + Q4_PassingYards, family = binomial(), data = train_data)

predictions_prob <- predict(model_q4, newdata = test_data, type = "response")
predictions_class <- ifelse(predictions_prob > 0.5, 1, 0)


confusion_matrix_q4 <- confusionMatrix(factor(predictions_class), factor(test_data$Win))


summary(model_q4)
confusion_matrix_q4

save(model_q4, confusion_matrix_q4, file = "~/nfl_analysis/models/model_q4.RData")

