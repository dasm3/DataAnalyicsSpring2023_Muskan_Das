#titanic- lab_9
#rpart, ctree, hclust, randomForest 

data1 <- data("Titanic")
df <- data.frame(Titanic)

library(rpart)
library(randomForest)

set.seed(123)
train_index <- sample(nrow(Titanic), 0.7*nrow(Titanic))
train_index
train_data1 <- Titanic[train_index]
test_data1 <- Titanic[-train_index]

train_data12 <- data.frame(train_data1)
test_data12 <- data.frame(test_data1)
head(train_data12)
library(party)
m1 <- rpart(Survived ~ ., data = df)
c1 <- ctree(Survived ~ ., data = df)
dist_matrix <- dist(Titanic)
h1 <- hclust(dist_matrix)
plot(h1)

# Build the random forest model
library(randomForest)
model <- randomForest(Survived ~ ., data = df, ntree = 500)

# Evaluate the model performance
library(caret)
