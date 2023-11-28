#CD4 Relapse Immune Plotting Script
# Jason M. Grayson
# 01-20-23

rm(list=ls())
library(tidyverse)
library(splitstackshape)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Outputs/Figure_04")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/demo_data.RDa")
load("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_04/Good_Stuff_CD4R copy.RDa")
demo_data <- distinct(demo_data, MRN, .keep_all = TRUE)
df<-df_scaled_with_label
df<-df[,c(1:12,14,15,17:19)]
df$MRN<-as.factor(df$MRN)
demo_data_filtered <- demo_data %>% filter(MRN %in% levels(df$MRN))
demo_data_filtered$MRN<-as.factor(demo_data_filtered$MRN)

df<-filter(df,df$V2<11)
df<-filter(df,df$MRN!="3362830")
df<-filter(df,df$DPT<102)
CD4Rel_UMAP_all<-left_join(df,demo_data_filtered,by=c("MRN"))
set.seed(1234)
df2<-stratified(CD4Rel_UMAP_all,c("Relapse","Stimulation"), size=500)
df2$Day_Rounded<-factor(df2$Day_Rounded,levels=c("0","15","30","45","60","100","180"))

df2<-df2[!is.na(df2$Relapse=="NA"),]
df2<-filter(df2,df2$V1>-12)
g1<-ggplot(df2, aes(x=V1,y=V2,color=phenograph_cluster))+geom_point(size=0.1)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ guides(color = guide_legend(override.aes = list(size=2)))
g1<-g1+facet_grid(vars(Relapse),vars(Stimulation))+ggtitle("CD4 Relapse")
g1
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_03")
save(CD4Rel_UMAP_all,file="CD4Rel_UMAP_all.RDa")
