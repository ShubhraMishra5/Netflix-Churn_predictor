library(randomForest)
library(caret)
library(ggplot2)

data <- read.csv("netflix_customer_churn.csv")
print(head(data))

data$customer_id <- NULL
data$churned <- as.factor(data$churned)

set.seed(123)

trainIndex <- createDataPartition(data$churned, p=0.8, list=FALSE)

train <- data[trainIndex,]
test <- data[-trainIndex,]

model <- randomForest(churned ~ ., data=train, ntree=100)
print(model)

pred <- predict(model, test)
print(confusionMatrix(as.factor(pred), as.factor(test$churned)))

importance(model)

varImpPlot(model)

ggplot(data, aes(x = churned)) +
  geom_bar(fill="steelblue") +
  labs(title="Churn Distribution")

ggplot(data, aes(x=watch_hours, fill=churned)) +
  geom_histogram(bins=30, alpha=0.7)