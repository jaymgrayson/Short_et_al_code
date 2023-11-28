# Upsampler- We need script to up sample each dataset 10-times
#Jason M. Grayson
# Started Feb 28,2023

rm(list=ls())
library(tidyverse)
library(smotefamily)

#Set number of upsample replicates
data_upsamples<-seq(from=1, to=10, by=1)

#Load the data
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Condensed_Data.RDa")
table(Condensed_Data$Relapse)
Condensed_Data<-Condensed_Data[c(1:13,18)]
Condensed_Data<-Condensed_Data %>% select("Day Rounded.x", everything())
Condensed_Data <- Condensed_Data[, c(setdiff(names(Condensed_Data)[1], "Relapse"), "Relapse", setdiff(names(Condensed_Data)[-1], "Relapse"))]
Condensed_Data <- Condensed_Data[!is.na(Condensed_Data$Relapse), ]
upsampled_data<-list()
for (i in data_upsamples)
{
    set.seed(i)
    df<-Condensed_Data
    df<-df[,-c(1)]
    balanced_data_ADAS <- ADAS(df[,-c(1)],df[,1],K=3)
    df_balanced<-balanced_data_ADAS$data
    upsampled_data[[i]]<-df_balanced
}
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
save(upsampled_data,file="D1530_times_upsampled_10.RDa")
