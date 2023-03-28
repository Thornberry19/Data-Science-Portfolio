# project 3 dummy script
# Data: https://www.kaggle.com/ronitf/heart-disease-uci

##### OBECTIVE: Predict the presence of heart disease #####

# Libraries
library(ROCR)
library(e1071)
library(randomForest)

# Loading data
data <- read.csv("C:\\Users\\Allen\\Desktop\\Data Science in R\\project3\\heart.csv")
View(data)

# Clustering/eda, cause it needs to be done (using gaussian mixture)
# kmclusters <- kmeans(data[,1:13], centers = 1)
# data$km <- kmclusters$cluster
# View(data)

### machine learning modeling

# data cleaning
names(data) <- c("age", colnames(data[,2:14]))

# cross validation preperation and modeling variables
model_var <- c("sex", "cp", "trestbps", "thalach", "exang", "oldpeak", "ca", "thal")
data$group <- sample(1:10, nrow(data), replace = TRUE)

# SVM modeling ---------------------------------------------------------------
# cross validation modeling loop
for (group in 1:10){
  print(paste("Training Group: ", as.character(group)))
  train <- data[data$group != group, c(model_var, "target")]
  test <- data[data$group == group, model_var]
  
  model <- svm(target ~ ., train, cost = .01, tolerance = .01)

  data$prediction[data$group == group] <- predict(model, test, type = 'response')
  train$prediction <- predict(model, train)
}

# auc rating stuff for end result and all cross validation
pr <- ROCR::prediction(data$prediction, data$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# auc rating stuff for training set
pr <- ROCR::prediction(train$prediction, train$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# randomForest modeling ------------------------------------------------------
# cross validation modeling loop
for (group in 1:10){
  print(paste("Training Group: ", as.character(group)))
  train <- data[data$group != group, c(model_var, "target")]
  test <- data[data$group == group, model_var]
  
  model <- randomForest(target ~ ., data=train, ntree=6000, maxnodes = 5)
  
  data$prediction[data$group == group] <- predict(model, test, type = 'response')
  train$prediction <- predict(model, train)
}

# auc rating stuff for end result and all cross validation
pr <- ROCR::prediction(data$prediction, data$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# auc rating stuff for training set
pr <- ROCR::prediction(train$prediction, train$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# glm modeling --------------------------------------------------------------
# cross validation modeling loop
for (group in 1:10){
  print(paste("Training Group: ", as.character(group)))
  train <- data[data$group != group, c(model_var, "target")]
  test <- data[data$group == group, model_var]

  model <- glm(target ~ ., train, family = binomial(link = 'logit'))
  #step(model, direction = "both", trace = 1)
  
  data$prediction[data$group == group] <- predict(model, test, type = 'response')
  train$prediction <- predict(model, train)
}

# auc rating stuff for end result and all cross validation
pr <- ROCR::prediction(data$prediction, data$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# auc rating stuff for training set
pr <- ROCR::prediction(train$prediction, train$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)