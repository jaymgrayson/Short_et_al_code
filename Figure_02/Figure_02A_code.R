#CD4 Cytokine Immune Plotting Script
# Jason M. Grayson
# 01-20-23

rm(list=ls())
library(tidyverse)
library(splitstackshape)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Outputs/Figure_02")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/demo_data.RDa")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_02/dfx copy.RDa")

df<-df[,c(1:13,15,23,29:31)]
df$MRN<-as.factor(df$MRN)
demo_data$MRN<-as.factor(demo_data$MRN)
demo_data <- distinct(demo_data, MRN, .keep_all = TRUE)

CD4Cy_UMAP_all<-left_join(df,demo_data,by=c("MRN"))

set.seed(1234)
df2<-stratified(CD4Cy_UMAP_all,c("Relapse","Day_Rounded"), size=500)
df2$Day_Rounded<-factor(df2$Day_Rounded,levels=c("0","15","30","45","60","100","180"))

df2<-df2[!is.na(df2$Relapse=="NA"),]
g1<-ggplot(df2, aes(x=V1,y=V2,color=phenograph_cluster))+geom_point(size=0.1)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))
g1<-g1+facet_grid(vars(Relapse),vars(Day_Rounded))+ggtitle("CD4 Cytokines")
g1

