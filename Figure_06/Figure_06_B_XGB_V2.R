# Model Comparer Script-Many Runs To Get Accuracy and AUC
# Jason M. Grayson
# Started 02-28-2023

library(e1071)
library(caret)
library(randomForest)
library(xgboost)

# Clear workspace
rm(list=ls())

# Load data
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/D1530_times_upsampled_10.RDa")

model_runs <- 1  # Reduced to 1 for debugging
data_for_model <- seq(from=1, to=10, by=1)

NB_ACC <- matrix(nrow=length(data_for_model), ncol=length(model_runs))
NB_AUC <- matrix(nrow=length(data_for_model), ncol=length(model_runs))

for (i in data_for_model) {
    df <- upsampled_data[[i]]
    df$class <- as.factor(df$class)
    
    for (j in model_runs) {
        set.seed(j)
        trainIndex <- createDataPartition(df$class, p = .8, list = FALSE, times = 1)
        train <- df[trainIndex,]
        test  <- df[-trainIndex,]
        
        data_matrix <- as.matrix(train[,-13])
        label_vector <- as.numeric(train$class) - 1
        
        xgbModel <- xgboost(data = data_matrix, label = label_vector, nrounds = 100, objective = "binary:logistic")
        
        xgbPredictions <- predict(xgbModel, as.matrix(test[,-13]))
        
        # Assuming the predictions are probabilities, we'll threshold at 0.5 for binary classification
        test_predictions <- ifelse(xgbPredictions > 0.5, 1, 0)
        test_labels <- as.numeric(test$class) - 1
        
        NB_ACC[i, j] <- sum(test_predictions == test_labels) / length(test_labels)
        ROCurve <- roc(test_labels, xgbPredictions)
        NB_AUC[i, j] <- ROCurve$auc
    }
}

A <- rowMeans(NB_ACC)
B <- apply(NB_ACC, 1, sd)
NB_Average_Acc <- mean(A)
NB_SD_Acc <- mean(B)

A2 <- rowMeans(NB_AUC)
B2 <- apply(NB_AUC, 1, sd)
NB_Average_Auc <- mean(A2)
NB_SD_Auc <- mean(B2)
