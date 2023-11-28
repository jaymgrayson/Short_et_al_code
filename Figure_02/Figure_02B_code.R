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
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_02/CD4Cy_UMAP_all.RDa")
df<-CD4Cy_UMAP_all
df<-df[,c(1:13,18,21)]
antigens<-colnames(df[,c(1:13)])
df2<-df%>%group_by(phenograph_cluster)%>%summarise_at(vars(antigens),list(name=mean))
clusters<-as.vector(df2[,1])
row.names(df2)<-clusters
df2<-df2[,-1]
Pan_radar<- df2%>%add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  select(1:13)%>%slice(3,17,26,33)
Pan_col_names<-c("group","IL-9","IL-17","IL-2","GzB","TNFa","T-Bet","IL-4","IL-10","CD45RA","HLA-DR","IFNg","FoxP3","CD127")
colnames(Pan_radar)<-Pan_col_names
#ggradar(Surface_radar,values.radar = "",legend.title = "Phenograph Cluster")
ggradar(Pan_radar,values.radar = "",legend.title = "",legend.text.size =12,plot.title = "",axis.label.size = 4,base.size = 6)+theme(legend.title = element_text(size=6)) 

