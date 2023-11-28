# Script to combine all manual data
# Jason M. Grayson
# Started 12-06-21

rm(list=ls())
library(tidyverse)
library(dplyr)
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_03/CD8_Cy_percentages_clusters.Rda")
#okay here we will look at each cluster compared to day.
#Each day's data will be tested for normality and analyzed between relapse and not
#Change columns as needed for more or less variables
df<-df_percentages
rm(df_percentages)
table(df$`Day Rounded`)
df<-filter(df,df$`Day Rounded`=="60")
df_for_stats<-df[,c(4:27)]

#Filter out all zero columns
df_for_stats<-df_for_stats%>%select(which(!colSums(df_for_stats)%in% 0))

# Determine which variables are normally distributed use appropriate methods
normality_test<-lapply(df_for_stats,shapiro.test)
pvalues<-rep(0,23)
for (i in seq_along(normality_test)){
  pvalues[i]<-normality_test[[i]][[2]]
  
}
variables<-colnames(df_for_stats)
normality_test_results<-cbind(variables,pvalues)
normality_test_results<-as.data.frame(normality_test_results)
normality_test_results$pvalues<-as.numeric(normality_test_results$pvalues)
normally_distributed_variables<-filter(normality_test_results,normality_test_results$pvalues>.05)
non_normally_distributed_variables<-filter(normality_test_results,normality_test_results$pvalues<=.05)
# Grab labels
Labels<-df[,c(1,2)]
colnames(Labels)[2]<-"Relapse"
colnames(df)[2]<-"Relapse"
Normal_Pull<-normally_distributed_variables$variables
Abnormal_Pull<-non_normally_distributed_variables$variables
Normal_numbers<-df[,c(Normal_Pull)]
Abnormal_numbers<-df[,c(Abnormal_Pull)]
Normal_data<-cbind(Labels,Normal_numbers)
Abnormal_data<-cbind(Labels,Abnormal_numbers)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_03/Stat_Runner/Day_60")
save(Normal_data,file="Normal_data_Surface.RDa")
save(Abnormal_data,file="Abnormal_data_Surface.RDa")

rm(list=ls())
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_03/Stat_Runner/Day_60/Normal_data_Surface.RDa")
data_TBA<-Normal_data[,-c(1)]

data_TBA$Relapse<-as.factor(data_TBA$Relapse)


result_holder<-lapply(data_TBA[,c(2:7)],function(x)wilcox.test(x~data_TBA$Relapse))
pvalues<-rep(0,length(result_holder))
for (i in seq_along(result_holder)){
  pvalues[i]<-result_holder[[i]][[3]]
  
}
variables<-colnames(data_TBA)
variables<-variables[-1]
Normal_by_Relapse<-cbind(variables,pvalues)
Normal_by_Relapse<-as.data.frame(Normal_by_Relapse)
Normal_by_Relapse$pvalues<-as.numeric(Normal_by_Relapse$pvalues)
Normal_by_Relapse<-Normal_by_Relapse[order(pvalues),]
save(Normal_by_Relapse,file="Normal_by_Relapse_Surface.RDa")

rm(list=ls())
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_03/Stat_Runner/Day_60/Abnormal_data_Surface.RDa")
data_TBA<-Abnormal_data[,-c(1)]
colnames(data_TBA)[1]<-"Relapse"
data_TBA$Relapse<-as.factor(data_TBA$Relapse)
result_holder<-lapply(data_TBA[,c(2:18)],function(x)kruskal.test(x~data_TBA$Relapse))
pvalues<-rep(0,length(result_holder))
for (i in seq_along(result_holder)){
  pvalues[i]<-result_holder[[i]][[3]]
  
}
variables<-colnames(data_TBA)
variables<-variables[-1]
Abnormal_by_Relapse<-cbind(variables,pvalues)
Abnormal_by_Relapse<-as.data.frame(Abnormal_by_Relapse)
Abnormal_by_Relapse$pvalues<-as.numeric(Abnormal_by_Relapse$pvalues)
Abnormal_by_Relapse<-Abnormal_by_Relapse[order(pvalues),]
save(Abnormal_by_Relapse,file="Abnormal_by_Relapse_Surface.RDa")
