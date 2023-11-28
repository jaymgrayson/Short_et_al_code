# Model Comparer Script-Many Runs To Get Accuaracy and AUC
# Jason M. Grayson
# Started 02-28-2023


library(e1071)
library(caret)
library(randomForest)
library(ggplot2)
library(pROC)
rm(list=ls())
set.seed(1234)

# Predict the probabilities of the test data using the trained model
model_runs<-seq(from=1, to=100, by=1)
data_for_model<-seq(from=1, to=10, by=1)
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/D1530_times_upsampled_10.RDa")
NB_ACC<-matrix(nrow=length(data_for_model),ncol=length(model_runs))
NB_AUC<-matrix(nrow=length(data_for_model),ncol=length(model_runs))
holding_train<-list()
holding_test<-list()
holding_rf_preds<-list()

for (i in data_for_model)
  {
    df<-upsampled_data[[i]]
    df$class<-as.factor(df$class)
        for (j in model_runs)
        {
          set.seed(j)
          trainIndex <- createDataPartition(df$class, p = .8, list = FALSE,times = 1)
          train <- df[trainIndex,]
          test <- df[-trainIndex,]
          rf_model <- randomForest(class ~ ., data = train, ntree = 500)
          rf_preds <- predict(rf_model, newdata = test)
          rf_accuracy <- mean(rf_preds == test$class)
          conf_matrix <- confusionMatrix(rf_preds, test$class)
          conf_matrix$table
          NB_ACC[[i,j]] <- rf_accuracy
          test_labels<-test$class
          ROCurve<-roc(test_labels,as.numeric(rf_preds))
          NB_AUC[[i,j]] <- ROCurve$auc
          holding_train[[i]]<-train
          holding_test[[i]]<-test
          holding_rf_preds[[i]]<-rf_preds
}
}
A<-rowMeans(NB_ACC)
B<-apply(NB_ACC,1,sd)
NB_Average_Acc<-mean(A)
NB_SD_Acc<-mean(B)
A2<-rowMeans(NB_AUC)
B2<-apply(NB_AUC,1,sd)
NB_Average_Acuc<-mean(A2)
NB_SD_Auc<-mean(B2)

