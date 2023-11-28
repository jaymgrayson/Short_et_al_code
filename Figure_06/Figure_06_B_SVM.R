# Model Comparer Script-Many Runs To Get Accuaracy and AUC
# Jason M. Grayson
# Started 02-28-2023


library(e1071)
library(caret)
library(randomForest)
library(xgboost)
rm(list=ls())
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/D1530_times_upsampled_10.RDa")
# Predict the probabilities of the test data using the trained model
model_runs<-seq(from=1, to=100, by=1)
data_for_model<-seq(from=1, to=10, by=1)

NB_ACC<-matrix(nrow=length(data_for_model),ncol=length(model_runs))
NB_AUC<-matrix(nrow=length(data_for_model),ncol=length(model_runs))
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
          svm_model <- svm(class ~ ., data = train)
          svm_preds <- predict(svm_model, newdata = test)
          svm_accuracy <- mean(svm_preds == test$class)
          
          NB_ACC[[i,j]] <- svm_accuracy
          test_labels<-test$class
          ROCurve<-roc(test_labels,as.numeric(svm_preds))
          NB_AUC[[i,j]] <- ROCurve$auc
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
