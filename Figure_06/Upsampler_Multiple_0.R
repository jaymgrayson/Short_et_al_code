
rm(list=ls())
# Load required libraries
library(tidyverse)
library(smotefamily)

# Set number of upsample replicates
data_upsamples <- seq(from = 1, to = 10, by = 1)

# Load the data
load("/Users/jasongrayson/Desktop/Critical Docs/Manuscripts/AML_Paper/JI_Version/JI_Paper_Project/Key_Data_Files/Figure_06/Condensed_Data.RDa")

# Remove the 7th column from the dataframe
Condensed_Data <- Condensed_Data[,-c(7,14:17,19)]

# Now proceed with filtering rows based on 'Day Rounded.x' values of 0, 15, and 30
Condensed_Data <- Condensed_Data %>%
    filter(`Day Rounded.x` %in% c("0", "15", "30"))
Condensed_Data<-Condensed_Data[,-c(12)]
# Assuming your dataframe is named df
Condensed_Data <- Condensed_Data[, c(12, 2:11, 1)]

# Initialize an empty list to store upsampled data
upsampled_data <- list()

# Upsampling loop
for (i in data_upsamples) {
    set.seed(i) # Set seed for reproducibility
    df <- Condensed_Data # Copy the data to a new variable to avoid altering the original dataframe
    
    # Check that all predictor variables are numeric
    df[,-1] <- lapply(df[,-1], function(x) if(is.numeric(x) | is.factor(x)) as.numeric(as.character(x)) else x)
    
    # Perform ADAS upsampling, assuming the first column is the target variable
    balanced_data_ADAS <- ADAS(df[,-1], df[,1], K = 3) # Adjust the columns if necessary
    df_balanced <- balanced_data_ADAS$data # Get the balanced data
    
    # Check if the number of rows matches, otherwise report an error or warning
    if (nrow(df_balanced) != nrow(df)) {
        warning(paste("Iteration", i, "produced a dataset with a different number of rows. Expected:", nrow(df), "Got:", nrow(df_balanced)))
        next # Skip this iteration
    }
    
    upsampled_data[[i]] <- df_balanced # Store in the list
}

# Save the upsampled data
save(upsampled_data, file = "/Users/jasongrayson/Desktop/upsampled_data.RDa")
