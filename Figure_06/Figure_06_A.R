# Code used to create UMAP plots in Figure 7A
# Jason M Grayson
library(tidyverse)
rm(list=ls())
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Cluster_Data.RDa")

load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/demo_data.RDa")
demo_data <- distinct(demo_data, MRN, .keep_all = TRUE)
demo_data<-demo_data[,c(3,10,32)]
demo_data$Patient_ID<-as.factor(demo_data$Patient_ID)
df$MRN<-as.factor(df$MRN)
colnames(df)[63]<-"Patient_ID"
df<-left_join(df,demo_data,by="Patient_ID")

df<-filter(df,df$Relapse=="Yes"|df$Relapse=="No")
df$`Day Rounded.x`<-factor(df$`Day Rounded.x`,levels=c("0","15","30","45","60","100","180"))
g1<-ggplot(df, aes(x=UMAP_1,y=UMAP_2, color=Relapse))+geom_point(size=2.5)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))

g1<-g1+facet_wrap(vars(`Day Rounded.x`))
g1
