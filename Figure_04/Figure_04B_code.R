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
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Outputs/Figure_04")

load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/demo_data.RDa")
load("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_04/Good_Stuff_CD4R copy.RDa")

demo_data <- distinct(demo_data, MRN, .keep_all = TRUE)
df<-df_scaled_with_label
df<-df[,c(1:13,19)]
df$MRN<-as.factor(df$MRN)
demo_data_filtered <- demo_data %>% filter(MRN %in% levels(df$MRN))
demo_data_filtered$MRN<-as.factor(demo_data_filtered$MRN)
CD4Rel_UMAP_all<-left_join(df,demo_data_filtered,by=c("MRN"))

set.seed(1234)
df<-df[,c(1:11,14)]
antigens<-colnames(df[,c(1:11)])
df2<-df%>%group_by(phenograph_cluster)%>%summarise_at(vars(antigens),list(name=mean))
clusters<-as.vector(df2[,1])
row.names(df2)<-clusters
df2<-df2[,-1]
Pan_radar<- df2%>%add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  select(1:11)%>%slice(20,21)
Pan_col_names<-c("group","Tox","GzB","IFNg","CD69","T-Bet","CD39","PD-1","CD45RA","TCF-1","CD103","LAG-3")
colnames(Pan_radar)<-Pan_col_names
#ggradar(Surface_radar,values.radar = "",legend.title = "Phenograph Cluster")
ggradar(Pan_radar,values.radar = "",legend.title = "",legend.text.size =12,plot.title = "",axis.label.size = 4,base.size = 6)+theme(legend.title = element_text(size=6)) 
