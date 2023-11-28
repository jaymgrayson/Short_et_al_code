# Script to generate Boxplots for Figure 2C
# Jason M Grayson
# Started 04-04-22

rm(list=ls())
library(tidyverse)
#set your wd and load datafile
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Outputs/Figure_01")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_03/CD8_Cy_percentages_clusters.Rda")
df_percentages$`Day Rounded`<-as.factor(df_percentages$`Day Rounded`)
#Filter Day as Interested
df2<-filter(df_percentages,df_percentages$`Day Rounded`=="0")
# Adjust and plot for each population that was statistically significant

Pop_Of_Interest<-ggplot(df2, aes(x=df2$Relapse, y=df2$`Cluster 4`)) +theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ ggtitle("Day 0-Cluster 4") + theme(plot.title = element_text(hjust = 0.5))+geom_boxplot()
Pop_Of_Interest
#Plot saved as a pdf and inputted to Canvas to make Figure