# Cluster Quantitation Script
# Jason M. Grayson and Nuri Park
# Started Dec 4,2019

rm(list=ls())

# We need something that is Tidy! Hence Tidyverse
library(tidyverse)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_05")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_05/CD8Rel_UMAP_all.RDa")
df<-filter(CD8Rel_UMAP_all,CD8Rel_UMAP_all$Stimulation=="None")
df$phenograph_cluster<-as.factor(df$phenograph_cluster)
df$MRN<-as.factor(df$MRN)
df$DPT<-as.factor(df$DPT)
df$Relapse<-as.factor(df$Relapse)

# Now need percentage of each phenograph cluster for each patient
# Need to know k for kmeans or cluster number for phenograph
table(df$phenograph_cluster)
# Okay here k=37, change for your analysis
k=24
##create a function to calculate percentage of cells in each cluster
r=0
cluster_ratio<-function(x){
  for (i in 1:k){
    count<-length(x[x==i])
    ratio<-round(count/length(x),4)*100
    r[i]=ratio
  }
  return(r)
}

#apply the cluster_ratio function to calculate the percentage of cell in each cluster
result_of_all<-as.matrix(aggregate(df$phenograph_cluster, by=list(df$MRN,df$DPT,df$Relapse), FUN=cluster_ratio))


print(result_of_all)
cluster_info<-result_of_all[,-c(1:3)]
class(cluster_info)<-"numeric"
#Generate a vector containing Cluster labels, depending on the the number of clusters
#used in the analysis, e.g. “Cluster 1, Cluster 2, Cluster 3...”
ClustNum<-c(1:k)
ClustLabels<-NULL
for(i in 1:k){
  ClustLabels<-c(ClustLabels, paste("Cluster",as.character(ClustNum[i])))
}
cluster_info<-as.data.frame(cluster_info)
colnames(cluster_info)<-ClustLabels
Label<-result_of_all[,c(1:3)]
##create infection label
Label<-as.data.frame(Label)

colnames(Label)<-c("MRN","DPT","Relapse")
df_percentages<-cbind(Label,cluster_info)
df_percentages$MRN<-as.factor(df_percentages$MRN)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_05/Stat_Runner/No_Stim")
save(df_percentages,file="CD8_no_perc_pheno_cluster.Rda")


