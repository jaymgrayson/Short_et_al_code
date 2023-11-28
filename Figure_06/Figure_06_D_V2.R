# Code to generate ROC Curve for Figure 7D, note only one of many models examined
#JMG


rm(list=ls())

# Load required libraries
library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(pROC)
library(ggplot2)


# Load data
load("/Users/jasongrayson/Desktop/rf_model.RDa")
load("/Users/jasongrayson/Desktop/test.RDa")


rf_preds <- predict(rf_model, newdata = test)
rf_accuracy <- mean(rf_preds == test$class)
conf_matrix <- confusionMatrix(rf_preds, test$class)
conf_matrix$table

# Print results

cat("Random forest accuracy:", rf_accuracy, "\n")

# Use RF predictions to make  ROC Curve

rf_pred <- predict(rf_model, newdata = test,type="prob")
ROC_rf <- roc(test$class, rf_pred[,2],percent=TRUE,)
fpr <- 100-ROC_rf$specificities
sensitivity <- ROC_rf$sensitivities
plot(fpr, sensitivity, type = "l", xlim = c(0, 100), ylim = c(0, 100),
     xlab = "False Positive Rate", ylab = "Sensitivity", main = "ROC Curve", col = "green")
abline(a = 0, b = 1, col = "black", lty = 1)
