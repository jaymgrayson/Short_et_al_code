# Collapsing cluster script
# Jason M. Grayson
# Started 02-17-2023
library(tidyverse)
rm(list=ls())
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Cluster_Data.RDa")
df<-filter(df,df$`Day Rounded.x`==0|df$`Day Rounded.x`==15|df$`Day Rounded.x`==30|df$`Day Rounded.x`==45|df$`Day Rounded.x`==60)
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
CD4_None_df<-select(df_condense,c(1:3,5:8,10,13:15,17,19,20,21,23,25:27,30:32))
CD4_None_df<-mutate(CD4_None_df,CD4_None_Pop=rowSums(CD4_None_df))
CD4_None_Pop<-CD4_None_df$CD4_None_Pop
rm(CD4_None_df)
# Get Treg like CD4 cells
CD4_TReg_df<-select(df_condense,c(4,28))
CD4_TReg_df<-mutate(CD4_TReg_df,CD4_TReg_Pop=rowSums(CD4_TReg_df))
CD4_TReg_Pop<-CD4_TReg_df$CD4_TReg_Pop
rm(CD4_TReg_df)
#Get Naive Like
CD4_Naive_like_df<-select(df_condense,c(9,16))
CD4_Naive_like_df<-mutate(CD4_Naive_like_df,CD4_Naive_like_Pop=rowSums(CD4_Naive_like_df))
CD4_Naive_like_Pop<-CD4_Naive_like_df$CD4_Naive_like_Pop
rm(CD4_Naive_like_df)
# Get Cytotoxic CD4+ T Cells
CD4_Cyto_df<-select(df_condense,c(18,34))
CD4_Cyto_df<-mutate(CD4_Cyto_df,CD4_Cyto_Pop=rowSums(CD4_Cyto_df))
CD4_Cyto_Pop<-CD4_Cyto_df$CD4_Cyto_Pop
rm(CD4_Cyto_df)
# Get Effector Like CD4+ T cells (HLA-DR+)
CD4_Eff_df<-select(df_condense,c(11,22))
CD4_Eff_df<-mutate(CD4_Eff_df,CD4_Eff_Pop=rowSums(CD4_Eff_df))
CD4_Eff_Pop<-CD4_Eff_df$CD4_Eff_Pop
rm(CD4_Eff_df)
# Get memory like cytokine producing CD4+ T cells (RAlow HLA-DR low, cyto)
CD4_Mem_df<-select(df_condense,c(24,29))
CD4_Mem_df<-mutate(CD4_Mem_df,CD4_Mem_Pop=rowSums(CD4_Mem_df))
CD4_Mem_Pop<-CD4_Mem_df$CD4_Mem_Pop
rm(CD4_Mem_df)
#TNFhi CD4 producing T cells
CD4_TNFa_df<-select(df_condense,c(33))
CD4_TNFa_df<-mutate(CD4_TNFa_df,CD4_TNFa_Pop=rowSums(CD4_TNFa_df))
CD4_TNFa_Pop<-CD4_TNFa_df$CD4_TNFa_Pop
rm(CD4_TNFa_df)
CD4_condensed<-cbind(CD4_Cyto_Pop,CD4_Mem_Pop,CD4_Naive_like_Pop,CD4_None_Pop,CD4_TNFa_Pop,CD4_TReg_Pop )
CD4_condensed<-as.data.frame(CD4_condensed)
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
save(CD4_condensed,file="CD4_condensed.RDa")

# Now do CD8 condensing
rm(list=ls())
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Cluster_Data.RDa")
df<-filter(df,df$`Day Rounded.x`==0|df$`Day Rounded.x`==15|df$`Day Rounded.x`==30|df$`Day Rounded.x`==45|df$`Day Rounded.x`==60)
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
CD8_None_df<-select(df_condense,c(1:5,7,11,12,14,15,9,17,18,21,23))
CD8_None_df<-mutate(CD8_None_df,CD8_None_Pop=rowSums(CD8_None_df))
CD8_None_Pop<-CD8_None_df$CD8_None_Pop
rm(CD8_None_df)
# Now get IL-2 hi CD8
CD8_IL2_df<-select(df_condense,c(6))
CD8_IL2_df<-mutate(CD8_IL2_df,CD8_IL2_Pop=rowSums(CD8_IL2_df))
CD8_IL2_Pop<-CD8_IL2_df$CD8_IL2_Pop
rm(CD8_IL2_df)
# Now get effector like (HLA-DR+RAlow) CD8
CD8_Eff_df<-select(df_condense,c(8))
CD8_Eff_df<-mutate(CD8_Eff_df,CD8_Eff_Pop=rowSums(CD8_Eff_df))
CD8_Eff_Pop<-CD8_Eff_df$CD8_Eff_Pop
rm(CD8_Eff_df)
#CD8 T reg like
CD8_TReg_df<-select(df_condense,c(10))
CD8_TReg_df<-mutate(CD8_TReg_df,CD8_TReg_Pop=rowSums(CD8_TReg_df))
CD8_TReg_Pop<-CD8_TReg_df$CD8_TReg_Pop
rm(CD8_TReg_df)
#More Mem-like CD8(cyto pos, CD45RA low, HLA-DR low)
CD8_Mem_df<-select(df_condense,c(13:24))
CD8_Mem_df<-mutate(CD8_Mem_df,CD8_Mem_Pop=rowSums(CD8_Mem_df))
CD8_Mem_Pop<-CD8_Mem_df$CD8_Mem_Pop
rm(CD8_Mem_df)
#Highest GZB
CD8_Cyto_df<-select(df_condense,c(22))
CD8_Cyto_df<-mutate(CD8_Cyto_df,CD8_Cyto_Pop=rowSums(CD8_Cyto_df))
CD8_Cyto_Pop<-CD8_Cyto_df$CD8_Cyto_Pop
rm(CD8_Cyto_df)
CD8_condensed<-cbind(CD8_Cyto_Pop,CD8_IL2_Pop,CD8_Mem_Pop,CD8_None_Pop,CD8_TReg_Pop)
CD8_condensed<-as.data.frame(CD8_condensed)
save(CD8_condensed,file="CD8_condensed.RDa")
rm(list=ls())
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/CD4_condensed.RDa")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/CD8_condensed.RDa")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Key_Info.RDa")
Condensed_Data<-cbind(CD4_condensed,CD8_condensed,Key_Info)
save(Condensed_Data,file="Condensed_Data.RDa")
