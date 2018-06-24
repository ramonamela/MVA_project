rm(list=ls())

###########################################
# Reading Data
###########################################
filenames <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS")
setwd("/home/ramela/Documents/Master/MVA/MVA_project")
dataset  <- read.csv('all_stocks_7.csv',sep = ';',row.names = 1)
source("usefulFunctions.R")

###########################################
# Partitioning 
###########################################

positions <- c()
for (i in filenames){
  positions <- c(positions,which(colnames(dataset)==paste0('IncrementDayCategorical',i) | colnames(dataset)==paste0('IncrementPreCategorical',i) | colnames(dataset)==paste0('MaxDay',i) | colnames(dataset)==paste0('MinDay',i) | colnames(dataset)==paste0('IncrementDay',i) | colnames(dataset)==paste0('IncrementPre',i) ))
}
X <- dataset[,-positions]
Y <- dataset[,positions]

n <- nrow(X)   

set.seed(250)
amount_train <- 0.8
amount_test <- 0.2

ntest <- round(amount_test * n)
ntrain <- round(amount_train * n)

X.test <- data.frame(X[(n - ntest + 1):n,])
Y.test <- data.frame(Y[(n - ntest + 1):n,])

# in case CV
X.train.val <- data.frame(X[1:(n-ntest),])
Y.train.val <- data.frame(Y[1:(n-ntest),])

# in case validation error. Val 10% of the total data, 12.5% of training data (80%)
amount_val <- 0.3
nval <- round(amount_val*n)
val_ind <- sample(1:nrow(X.train.val), nval)
X.train <- data.frame(X.train.val[-val_ind,])
Y.train <- data.frame(Y.train.val[-val_ind,])
X.val <- data.frame(X.train.val[val_ind,])
Y.val <- data.frame(Y.train.val[val_ind,])

# in case I do not need PCA, I need to partitionate also dataset
dataset.train.val <- dataset[1:(n-ntest),]
dataset.test <- dataset[(n - ntest + 1):n,]

##########################################
# PCA
##########################################

library(FactoMineR)
pca.results <- PCA(X,ncp = ncol(X))

# deciding the number of principal components we want to take
cum.sum.eig <- cumsum(pca.results$eig[,1])
cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
# we decide to retain as many eigenvalues as needed in order to have 80% of the total 
# Inertia
nd <- which(cum.sum.eig.norm>=0.95)[1] 

# redefining X in PCA space
X.test.pca <- as.data.frame(predict(pca.results, X.test)$coord[,1:nd])
X.train.val.pca <- as.data.frame(predict(pca.results, X.train.val)$coord[,1:nd])
X.train.pca <- as.data.frame(predict(pca.results, X.train)$coord[,1:nd])
X.val.pca <- as.data.frame(predict(pca.results, X.val)$coord[,1:nd])

dataset <- createCompaniesAsRowsDataset(filenames, "/home/ramela/Documents/Master/MVA/MVA_project/")
dataset_center <- scale(dataset)

count.0.1 <- generate_outliers(dataset_center, perc_outliers = 0.1)
count.0.2 <- generate_outliers(dataset_center, perc_outliers = 0.2)
count.0.3 <- generate_outliers(dataset_center, perc_outliers = 0.3)
count.0.4 <- generate_outliers(dataset_center, perc_outliers = 0.4)
count.0.5 <- generate_outliers(dataset_center, perc_outliers = 0.5)


###########################################
# DECISION TREE WITH THE WHOLE DATASET
###########################################

library(rpart)

## Categorical decision tree
model.dtcat.train <- list()
model.test.prob_pred.dtcat.val <- list()
for (i in 1:length(filenames)) {
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor <- as.factor(Y.train[,pos])
  model.dtcat.train[[i]] <- rpart(curr_factor ~ ., data = X.train.pca,method="class",control = rpart.control(cp=0.001))
  model.test.prob_pred.dtcat.val[[i]] <- predict(model.dtcat.train[[i]], newdata=X.val.pca)
  Y.val <- addColumn(Y.val, model.test.prob_pred.dtcat.val[[i]], paste0('PredTreeCatTrain',filenames[i]))
}

performance_stocks_dtcat <- list()
for (i in 1:10) {
  performance_stocks_dtcat[[i]] <- generateBenefitContinuousDay(Y.val, "PredTreeCatTrain", filenames, i)
}

amount_mean_dtcat <- which(performance_stocks_dtcat == max(unlist(performance_stocks_dtcat)))[1]

model.dtcat.train.val <- list()
model.test.prob_pred.dtcat.test <- list()
for (i in 1:length(filenames)) {  
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor <- as.factor(Y.train.val[,pos])
  model.dtcat.train.val[[i]] <- rpart(curr_factor ~ ., data = X.train.val.pca,method="class",control = rpart.control(cp=0.001))
  model.test.prob_pred.dtcat.test[[i]] <- predict(model.dtcat.train.val[[i]], newdata=X.test.pca)
  Y.test <- addColumn(Y.test, model.test.prob_pred.dtcat.test[[i]], paste0('PredTreeCatTest',filenames[i]))
}

1 - generateBenefitContinuousDay(Y.test, "PredTreeCatTest", filenames, amount_mean_dtcat)

## Continouous decision tree
model.dtcon.train <- list()
model.test.prob_pred.dtcon.val <- list()
for (i in 1:length(filenames)) {
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDay',filenames[i]))
  model.dtcon.train[[i]] <- rpart(Y.train[,pos] ~ ., data = X.train.pca,method="anova",control = rpart.control(cp=0.001))
  model.test.prob_pred.dtcon.val[[i]] <- predict(model.dtcon.train[[i]], newdata=X.val.pca)
  Y.val <- addColumn(Y.val, model.test.prob_pred.dtcon.val[[i]], paste0('PredTreeConTrain',filenames[i]))
}

performance_stocks_dtcon <- list()
for (i in 1:10) {
  performance_stocks_dtcon[[i]] <- generateBenefitContinuousDay(Y.val, "PredTreeConTrain", filenames, i)
}

amount_mean_dtcon <- which(performance_stocks_dtcon == max(unlist(performance_stocks_dtcon)))[1]

model.dtcon.train.val <- list()
model.test.prob_pred.dtcon.test <- list()
for (i in 1:length(filenames)) {  
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDay',filenames[i]))
  model.dtcon.train.val[[i]] <- rpart(Y.train.val[,pos] ~ ., data = X.train.val.pca,method="anova",control = rpart.control(cp=0.001))
  model.test.prob_pred.dtcon.test[[i]] <- predict(model.dtcon.train.val[[i]], newdata=X.test.pca)
  Y.test <- addColumn(Y.test, model.test.prob_pred.dtcon.test[[i]], paste0('PredTreeConTest',filenames[i]))
}

1 - generateBenefitContinuousDay(Y.test, "PredTreeConTest", filenames, amount_mean_dtcon)


###########################################
# RANDOM FOREST WITH THE WHOLE DATASET
###########################################

library(randomForest)

## Random forest
model.rf.train <- list()
model.test.prob_pred.rf.val <- list()
for (i in 1:length(filenames)) {

  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor <- as.factor(Y.train[,pos])
  model.rf.train[[i]] <- randomForest(curr_factor ~ . , data = X.train.pca)
  model.test.prob_pred.rf.val[[i]] <- predict(model.rf.train[[i]], newdata=X.val.pca)
  Y.val <- addColumn(Y.val, model.test.prob_pred.rf.val[[i]], paste0('PredRandomTrain',filenames[i]))
}

performance_stocks_rf <- list()
for (i in 1:10) {
  performance_stocks_rf[[i]] <- generateBenefitContinuousDay(Y.val, "PredRandomTrain", filenames, i)
}

amount_mean_rf <- which(performance_stocks_rf == max(unlist(performance_stocks_rf)))[1]

model.rf.train.val <- list()
model.test.prob_pred.rf.test <- list()
for (i in 1:length(filenames)) {  
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor <- as.factor(Y.train.val[,pos])
  model.rf.train.val[[i]] = randomForest(curr_factor ~ ., data=X.train.val.pca)
  model.test.prob_pred.rf.test[[i]] <- predict(model.rf.train.val[[i]], newdata=X.test.pca)
  Y.test <- addColumn(Y.test, model.test.prob_pred.rf.test[[i]], paste0('PredRandomTest',filenames[i]))
}

1 - generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, amount_mean_rf)


generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, 1)
generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, 2)
generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, 3)
generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, 4)
generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, 5)

###########################################
# kNN WITH THE WHOLE DATASET
###########################################

library(class)

## kNN
model.knn.train <- list()
amountCentersKNN <- 10
limit_choices <- 10
for (i in 1:length(filenames)) {
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor_train <- as.factor(Y.train[,pos])
  for (centers in 1:amountCentersKNN){
    print(centers)
    predicted_labels <- knn(train = X.train.pca, test = X.val.pca,cl = curr_factor_train, k=centers)
    Y.val <- addColumn(Y.val, predicted_labels, paste0('PredKNNTrain',centers,filenames[i]))
  }
}

benefits_KNN <- matrix(, nrow=amountCentersKNN, ncol=10)

for (centers in 1:amountCentersKNN){
  for (choices in 1:limit_choices){
    curr_col_label <- paste0("PredKNNTrain", centers)
    benefits_KNN[centers, choices] <- generateBenefitContinuousDay(Y.val, curr_col_label, filenames, choices)
  }
}

total_pos <- which(benefits_KNN == max(benefits_KNN))
prevChoicesKNN <- (total_pos %% limit_choices) + 1
prevCentersKNN <- ((total_pos - prevChoicesKNN + 1) / limit_choices) + 1

for (i in 1:length(filenames)) {
  print(paste0(i, " ", filenames[i]))
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor_train <- as.factor(Y.train.val[,pos])
  
  predicted_labels <- knn(train = X.train.val.pca, test = X.test.pca,cl = curr_factor_train, k=prevCentersKNN)
  Y.test <- addColumn(Y.test, predicted_labels, paste0('PredKNNTest',prevCentersKNN,filenames[i]))
}

1 - generateBenefitContinuousDay(Y.test, paste0('PredKNNTest', prevCentersKNN), filenames, prevChoicesKNN)


