## Sam Short
## Table 1
## 3-18-21

## Clear Workspace
rm(list=ls())
## Libraries
library(tidyverse)
library(table1)
library(htmltools)
library(forcats)
library(officer)
library(visdat)
#set output directory
setwd("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Outputs/Figure_01")
load("~/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_01/df.RDa")
df<- df[!duplicated(df$MRN), ]
df$Molecular_Landscape[df$Molecular_Landscape == "Abnomal"] <- "Abnormal"
df$Molecular_Landscape<-as.factor(df$Molecular_Landscape)
df$Molecular_Landscape<-droplevels(df$Molecular_Landscape)
df$`Disease at HCT`[df$`Disease at HCT` == "CR"]<- "CR1"
df$`Disease at HCT`[df$`Disease at HCT` == "CR 1"]<- "CR1"
df$`Disease at HCT`[df$`Disease at HCT` == "CRi/CR1"]<- "CRi"
df$`Disease at HCT`[df$`Disease at HCT` == "CRi/CR2"]<- "CRi"
df$`Disease at HCT`<-as.factor(df$`Disease at HCT`)
df$`Disease at HCT`<-droplevels(df$`Disease at HCT`)
df$Max_Acute_GVHD <- factor(df$Max_Acute_GVHD, levels = c("None","1","2","3","4"))
df$Max_Chronic_GVHD<- factor(df$Max_Chronic_GVHD, levels = c("None","Mild","Moderate","Severe"))
levels(df$`Disease Risk Index (DRI) per CIBMTR`)[levels(df$`Disease Risk Index (DRI) per CIBMTR`) == "unknown"] <- "Unknown"
levels(df$Max_Acute_GVHD)[levels(df$Max_Acute_GVHD) == "1"] <- "1 & 2"
levels(df$Max_Acute_GVHD)[levels(df$Max_Acute_GVHD) == "2"] <- "1 & 2"
levels(df$Max_Acute_GVHD)[levels(df$Max_Acute_GVHD) == "3"] <- "3 & 4"
levels(df$Max_Acute_GVHD)[levels(df$Max_Acute_GVHD) == "4"] <- "3 & 4"
levels(df$Max_Chronic_GVHD)[levels(df$Max_Chronic_GVHD) == "Mild"] <- "Mild to Moderate"
levels(df$Max_Chronic_GVHD)[levels(df$Max_Chronic_GVHD) == "Moderate"] <- "Mild to Moderate"
cols_to_convert<-c(1,3:17,22:33,35)
df[cols_to_convert] <- lapply(df[cols_to_convert], as.factor)
## Rename labels for use in table
table1::label(df$Age)<- "Age"
table1::label(df$Sex)<- "Sex"
table1::label(df$`donor type`)<- "Donor Source"
table1::label(df$`conditioning intensity`)<- "Conditioning Intensity"
table1::label(df$`Conditioning regimen`)<- "Conditioning Regimen"
table1::label(df$`GVHD Prophylaxis.x`)<- "GVHD Prophylaxis"
table1::label(df$Match_Data)<- "# of matches"
table1::label(df$Cytogenetics)<- "Cytogenetics"
table1::label(df$Molecular_Landscape)<- "Molecular Landscape"
table1::label(df$FLT_3_TKD_Positive)<- "FLT3 TKD Positive"
table1::label(df$FLT_3ITD)<- "FLT3 ITD"
table1::label(df$DNMT3A)<- "DNMT3A Mutation"
table1::label(df$NPM_1)<- "NPM1 Mutation"
table1::label(df$IDH2)<- "IDH2 Mutation"
table1::label(df$C_D30)<- "Day 30 Chimerism"
table1::label(df$C_D60)<- "Day 60 Chimerism"
table1::label(df$C_D100)<- "Day 100 Chimerism"
table1::label(df$C_D180)<- "Day 180 Chimerism"
table1::label(df$`ASBMT Disease Classification`)<- "ASBMT Disease Classification"
table1::label(df$`Disease Risk Index (DRI) per CIBMTR`)<- "Disease Risk Index (CIBMTR)"
table1::label(df$`Disease at HCT`)<-"Disease at Transplant"
table1::label(df$`MRD pre transplant`)<-"MRD Pre-Transplant"
table1::label(df$`PT Cytoxan?`)<-"Post-Transplant Cytoxan"
table1::label(df$Acute_GVHD_Binned)<-"Acute GVHD"
table1::label(df$Max_Acute_GVHD)<-"Maximum Acute GVHD Score"
table1::label(df$Chronic_GVHD_Binned)<-"Chronic GVHD"
table1::label(df$Max_Chronic_GVHD)<-"Maximum Chronic GVHD Score"
table1::label(df$`2022 European LeukemiaNet (ELN)`)<-"2022 European LeukemiaNet (ELN)"
table1::label(df$`Days from BMT to Relapse`)<-"Time from HSCT To Relapse"
## makes age in units of years
units(df$Age)<-"years"
units(df$`Days from BMT to Relapse`)<-"days"
units(df$C_D30)<-"%"
units(df$C_D60)<-"%"
units(df$C_D100)<-"%"
units(df$C_D180)<-"%"
pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
    } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
    }
    # Format the p-value, using an HTML entity for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
vis_dat(df)
AML_table<-table1(~ Sex+Age+`donor type`+`conditioning intensity`+`Conditioning regimen`+`GVHD Prophylaxis.x`+Match_Data+`Cell Source`+`Disease Risk Index (DRI) per CIBMTR`+`Disease at HCT`+`MRD pre transplant`+Acute_GVHD_Binned+Max_Acute_GVHD+Chronic_GVHD_Binned+Max_Chronic_GVHD|Relapse,data=df,overall=F, extra.col=list(`P-value`=pvalue))
AML_table



AML_table2<-table1(~ df$Cytogenetics+df$Molecular_Landscape+df$FLT_3_TKD_Positive+df$FLT_3_TKD_Positive+df$FLT_3ITD+df$DNMT3A+df$NPM_1+df$IDH2|Relapse,data=df,overall=F, extra.col=list(`P-value`=pvalue))
AML_table2

table1::label(df$C_D30)<-"Day 30 Chimerism"
table1::label(df$C_D60)<-"Day 60 Chimerism"
table1::label(df$C_D100)<-"Day 100 Chimerism"
table1::label(df$C_D180)<-"Day 180 Chimerism"
AML_table3<-table1(~df$C_D30+df$C_D60+df$C_D100+df$C_D180+df$`Days from BMT to Relapse`|Relapse,data=df,overall=F, extra.col=list(`P-value`=pvalue))
AML_table3
