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
load("~/Desktop/AML_Relapse_JG/AML (Sam's Data)/JG_Model_Stuff/Automated_Analyses/Condensed_Clusters/Upsampled_Multi/D1530_times_upsampled_10.RDa")
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
          rf_model <- randomForest(class ~ ., data = train, ntree = 500)
          rf_preds <- predict(rf_model, newdata = test)
          rf_accuracy <- mean(rf_preds == test$class)
          conf_matrix <- confusionMatrix(rf_preds, test$class)
          conf_matrix$table
          NB_ACC[[i,j]] <- rf_accuracy
          test_labels<-test$class
          ROCurve<-roc(test_labels,as.numeric(rf_preds))
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

conf_matrix$table

rf_pred <- predict(rf_model, newdata = test,type="prob")
ROC_rf <- roc(test$class, as.numeric(rf_pred[,2]))
plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN) ")
varImpPlot(rf_model)

#Alternative variable importance plot
df<-rf_model$importance
df<-as.data.frame(df)
data <- data.frame(x = rownames(df), y = df$MeanDecreaseGini)
# Create the needle plot
g1<-ggplot(data, aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = 0), color = "black") +
  geom_point(color = "red", size = 2) +
  labs(title = "Variable Importance",
       x = "Population",
       y = "Mean Decrease Gini Coefficient") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1))
g1

#Alternative ROC curve plot
Sensitivity<-ROC_rf$sensitivities
Specificity<-ROC_rf$specificities
Threshold<-ROC_rf$threshold
ROC_data<-cbind(Sensitivity,Specificity,Threshold)
ROC_data<-as.data.frame(ROC_data)
ROC_data<-ROC_data%>%mutate(FPR=1-Specificity)
