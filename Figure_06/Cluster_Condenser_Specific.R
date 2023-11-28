# Collapsing cluster script
# Jason M. Grayson
# Started 02-17-2023
library(tidyverse)
rm(list=ls())
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Cluster_Data.RDa")
df<-filter(df,df$`Day Rounded.x`==0|df$`Day Rounded.x`==15|df$`Day Rounded.x`==30)
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/demo_data.RDa")
demo_data <- distinct(demo_data, MRN, .keep_all = TRUE)
demo_data<-demo_data[,c(3,10,32)]
demo_data$Patient_ID<-as.factor(demo_data$Patient_ID)
df$MRN<-as.factor(df$MRN)
colnames(df)[63]<-"Patient_ID"
df<-left_join(df,demo_data,by="Patient_ID")
Relapse<-df$Relapse
Key_Info<-df[,c(1,60:65)]
df<-df[,c(2:35)]
Cluster<-colnames(df)
New_Col_Names<-vector(mode="character",length=0)
for (i in seq_along(Cluster)){
    string<-Cluster[[i]]
    new_string <- gsub("\\s", "_", string)
    New_Col_Names<-append(New_Col_Names,new_string)
}
colnames(df)<-New_Col_Names
rm(i,New_Col_Names,new_string,Relapse,string)
colnames(df)
df_condense<-df[,c(1:34)]
colnames(df_condense)
# Get non-cytokine producing CD4 cells
CD4_Cl3_df<-select(df_condense,c(3))
CD4_Cl3_Pop<-CD4_Cl3_df$CD4_Cluster_3
rm(CD4_Cl3_df)
CD4_Cl17_df<-select(df_condense,c(17))
CD4_Cl17_Pop<-CD4_Cl17_df$CD4_Cluster_17
rm(CD4_Cl17_df)
CD4_Cl26_df<-select(df_condense,c(26))
CD4_Cl26_Pop<-CD4_Cl26_df$CD4_Cluster_26
rm(CD4_Cl26_df)
CD4_Cl33_df<-select(df_condense,c(33))
CD4_Cl33_Pop<-CD4_Cl33_df$CD4_Cluster_33
rm(CD4_Cl33_df)
CD4_condensed<-cbind(CD4_Cl3_Pop,CD4_Cl17_Pop,CD4_Cl26_Pop,CD4_Cl33_Pop )
CD4_condensed<-as.data.frame(CD4_condensed)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
save(CD4_condensed,file="CD4_condensed_spec.RDa")

# Now do CD8 condensing
rm(list=ls())
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Cluster_Data.RDa")
df<-filter(df,df$`Day Rounded.x`==0|df$`Day Rounded.x`==15|df$`Day Rounded.x`==30)
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/demo_data.RDa")
demo_data <- distinct(demo_data, MRN, .keep_all = TRUE)
demo_data<-demo_data[,c(3,10,32)]
demo_data$Patient_ID<-as.factor(demo_data$Patient_ID)
df$MRN<-as.factor(df$MRN)
colnames(df)[63]<-"Patient_ID"
df<-left_join(df,demo_data,by="Patient_ID")
Relapse<-df$Relapse
Key_Info<-df[,c(1,60:65)]
df<-df[,c(36:59)]
Cluster<-colnames(df)
New_Col_Names<-vector(mode="character",length=0)
for (i in seq_along(Cluster)){
    string<-Cluster[[i]]
    new_string <- gsub("\\s", "_", string)
    New_Col_Names<-append(New_Col_Names,new_string)
}
colnames(df)<-New_Col_Names
rm(i,New_Col_Names,new_string,Relapse,string)
colnames(df)
df_condense<-df
colnames(df_condense)
# Get non-cytokine producing CD4 cells
CD8_Cl1_df<-select(df_condense,c(1))
CD8_Cl1_Pop<-CD8_Cl1_df$CD8_Cluster_1
rm(CD8_Cl1_df)
CD8_Cl4_df<-select(df_condense,c(4))
CD8_Cl4_Pop<-CD8_Cl4_df$CD8_Cluster_4
rm(CD8_Cl4_df)
CD8_Cl12_df<-select(df_condense,c(12))
CD8_Cl12_Pop<-CD8_Cl12_df$CD8_Cluster_12
rm(CD8_Cl12_df)
CD8_Cl14_df<-select(df_condense,c(14))
CD8_Cl14_Pop<-CD8_Cl14_df$CD8_Cluster_14
rm(CD8_Cl14_df)
CD8_condensed<-cbind(CD8_Cl1_Pop,CD8_Cl4_Pop,CD8_Cl12_Pop,CD8_Cl14_Pop )
CD8_condensed<-as.data.frame(CD8_condensed)
save(CD8_condensed,file="CD8_condensed_spec.RDa")
rm(list=ls())
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/CD4_condensed_spec.RDa")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/CD8_condensed_spec.RDa")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Key_Info_Spec.RDa")
Condensed_Data<-cbind(CD4_condensed,CD8_condensed,Key_Info)
save(Condensed_Data,file="Condensed_Data_Spec.RDa")
