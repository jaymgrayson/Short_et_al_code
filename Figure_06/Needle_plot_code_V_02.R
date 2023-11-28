rm(list=ls())
library(ggplot2)
load("~/Desktop/AML_Relapse_JG/AML (Sam's Data)/JG_Model_Stuff/Automated_Analyses/Scripts/D15_30_rf_model.RDa")
df<-rf_model$importance
df<-as.data.frame(df)
data(mtcars)

# Or use an existing dataset, such as the built-in 'mtcars' dataset
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
