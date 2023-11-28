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
table(Condensed_Data$`Day Rounded.x`)
Condensed_Data$Day<-as.character(Condensed_Data$Day)
Condensed_Data<-filter(Condensed_Data,Condensed_Data$`Day Rounded.x`=="15"|Condensed_Data$`Day Rounded.x`=="30")
Condensed_Data<-Condensed_Data[,c(1:11,17)]
Relapse<-Condensed_Data$Relapse
Condensed_Data<-Condensed_Data[,c(1:11)]
Condensed_Data<-cbind(Relapse,Condensed_Data)
Condensed_Data <- na.omit(Condensed_Data, subset = "relapse")
upsampled_data<-list()


for (i in 1:10) {
set.seed(i)  # Set seed for reproducibility
    
# Perform ADAS upsampling
# Assuming `Relapse` is the first column and the rest are features.
balanced_data_ADAS <- ADAS(Condensed_Data[, -1], Condensed_Data[, 1], K = 5)
df_balanced <- balanced_data_ADAS$data
    
# Add the upsampled dataset to the list
upsampled_data[[i]] <- df_balanced
}
save(upsampled_data,file="D1530_times_upsampled_10.RDa")
