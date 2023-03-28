# Project 1 R script
# data loading
# link to webpage with data: https://www.lendingclub.com/info/download-data.action
mainData <- read.csv(file = "C:\\Users\\Allen\\Desktop\\Data Science in R\\project1\\LoanStats3a.csv")

# Libraries used
library(gbm)
library(ROCR)
library(stringr)
library(e1071)
library(randomForest)

# data cleaning
mainData$member_id <- NULL
mainData$url <- NULL
mainData$desc <- NULL
mainData$purpose <- NULL
mainData$title <- NULL
mainData$zip_code <- NULL
mainData$addr_state <- NULL
mainData$mths_since_last_record <- NULL
mainData$initial_list_status <- NULL
mainData$out_prncp_inv <- NULL
mainData$total_pymnt <- NULL
mainData$recoveries <- NULL
mainData$collection_recovery_fee <- NULL
mainData$last_credit_pull_d <- NULL
mainData$collections_12_mths_ex_med <- NULL
mainData$mths_since_last_major_derog <- NULL
mainData$policy_code <- NULL
mainData$application_type <- NULL
mainData$annual_inc_joint <- NULL
mainData$dti_joint <- NULL
mainData$verification_status_joint <- NULL
mainData$acc_now_delinq <- NULL
mainData$tot_coll_amt <- NULL
mainData$tot_cur_bal <- NULL
mainData$open_acc_6m <- NULL
mainData[82:145] <- NULL
mainData[35:80] <- NULL
mainData$percent_laon_rec <- mainData$funded_amnt/mainData$loan_amnt

# collecting only the complete cases
mainData <- mainData[complete.cases(mainData),]

# imputing(kind of)
mainData$id <- 1:length(mainData$id)

# removing whitespace
mainData$int_rate <- trimws(str_split_fixed(str_split_fixed(mainData$int_rate, "%", 2)[,1], " ",2)[,2], which = c("both", "left", "right"))

# turning variabl data into number values only
mainData$home_ownership <- as.integer(mainData$home_ownership == "OWN")
mainData$a_grade <- as.integer(mainData$grade == "A")
mainData$b_grade <- as.integer(mainData$grade == "B")
mainData$c_grade <- as.integer(mainData$grade == "C")
mainData$d_grade <- as.integer(mainData$grade == "D")
mainData$e_grade <- as.integer(mainData$grade == "E")
mainData$term <- as.integer(str_split_fixed(mainData$term, " ", 3)[,2])
mainData$group <- sample(1:10, nrow(mainData), replace = TRUE)

model_var <- c("loan_amnt", "term", "annual_inc", "dti", "open_acc", "total_acc", "pub_rec_bankruptcies", "a_grade", "b_grade", "c_grade", "d_grade", "e_grade")

mainData <- mainData[1:1000,]

#mainData$prediction <- NA
for (group in 1:10){
  print(paste("Training Group: ", as.character(group)))
  train <- mainData[mainData$group != group, c(model_var, "home_ownership")]
  test <- mainData[mainData$group == group, model_var]
  
  model <- svm(home_ownership ~ ., train, cost = .5, tolerance = .01)
  
  # predictions placed at end of the data sets
  mainData$prediction[mainData$group == group] <- predict(model, test, type = 'response')
  train$prediction <- predict(model, train)

#  raindf$prediction[raindf$Group == Group] <- predict(model, test, type = 'response')
#  train$prediction <- predict(model, train)
}

# auc rating stuff for end result and all cross validation
pr <- ROCR::prediction(mainData$prediction, mainData$home_ownership)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# auc rating stuff for training set
pr <- ROCR::prediction(train$prediction, train$home_ownership)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

for (group in 1:10){
  print(paste("Training Group: ", as.character(group)))
  train <- mainData[mainData$group != group, c(model_var, "home_ownership")]
  test <- mainData[mainData$group == group, model_var]
  
  model <- glm(home_ownership ~ ., train, family=binomial(link='logit'))
  
  # predictions placed at end of the data sets
  mainData$prediction[mainData$group == group] <- predict(model, test, type = 'response')
  train$prediction <- predict(model, train)
}

# auc rating stuff for end result and all cross validation
pr <- ROCR::prediction(mainData$prediction, mainData$home_ownership)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# auc rating stuff for training set
pr <- ROCR::prediction(train$prediction, train$home_ownership)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)


for (group in 1:10){
  print(paste("Training Group: ", as.character(group)))
  train <- mainData[mainData$group != group, c(model_var, "home_ownership")]
  test <- mainData[mainData$group == group, model_var]
  
  model <- randomForest(home_ownership ~ ., train, ntree = 10000)
  # predictions placed at end of the data sets
  mainData$prediction[mainData$group == group] <- predict(model, test, type = 'response')
  train$prediction <- predict(model, train)
}

# auc rating stuff for end result and all cross validation
pr <- ROCR::prediction(mainData$prediction, mainData$home_ownership)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# auc rating stuff for training set
pr <- ROCR::prediction(train$prediction, train$home_ownership)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)