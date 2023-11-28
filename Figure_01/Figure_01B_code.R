#We need a script to generate cluster variable levels
# Jason M. Grayson
# Started 08/15/22
rm(list=ls())
library(tidyverse)
library(ggradar)
library(dplyr)
library(scales)
library(splitstackshape)
# Set working directory
#as needed
#Load flow data
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/Pan_UMAP_filtered.RDa")
df<-Pan_UMAP_filtered
df<-df[,c(1:4,6:16)]
antigens<-colnames(df[,c(1:11)])
df2<-df%>%group_by(phenograph_cluster)%>%summarise_at(vars(antigens),list(name=mean))
clusters<-as.vector(df2[,1])
row.names(df2)<-clusters
df2<-df2[,-1]
Pan_radar<- df2%>%add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  select(1:12)%>%slice(2,10,14,22)
Pan_col_names<-c("group","HLA_DR","CD8a","CD4","CD14","CD38","CD16","CD45RA","CD3","CD11c","CD27","CD66")
colnames(Pan_radar)<-Pan_col_names
#ggradar(Surface_radar,values.radar = "",legend.title = "Phenograph Cluster")
ggradar(Pan_radar,values.radar = "",legend.title = "",legend.text.size =12,plot.title = "",axis.label.size = 4,base.size = 6)+theme(legend.title = element_text(size=6)) 

