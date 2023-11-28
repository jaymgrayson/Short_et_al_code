# Script to generate Boxplots for Figure 2C
# Jason M Grayson
# Started 04-04-22

rm(list=ls())
library(tidyverse)
#set your wd and load datafile
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Outputs/Figure_05")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_05/Stat_Runner/PI_Stim/CD8_PI_perc_pheno_cluster.Rda")
df<-df_percentages
Pop_Of_Interest<-ggplot(df, aes(x=df$Relapse, y=df$`Cluster 5`)) +theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Cluster 5") + theme(plot.title = element_text(hjust = 0.5))+geom_boxplot()
Pop_Of_Interest
#Plot saved as a pdf and inputted to Canvas to make Figure