# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# 1. READ IN DATA: -------------------------------------------------------------
source("scripts/scripts_2groups/config_2_groups.R")
# Read input_file as dataframe and save as object
data <- read.csv(file.path(input_file), stringsAsFactors = FALSE)

# 2. DEFINE MARKERS AND TREATMENT GROUPS: --------------------------------------

# markers and treatment groups were defined in config script above

# 3. ORGANIZE AND SUMMARIZE DATA: ----------------------------------------------

# Define function for summarizing data *****************************************

summ_data_function <- function(data_df, marker_col) { 
  data_summ_df <- data_df %>% 
    group_by(treatment) %>% 
    summarise(
      mean_cells = mean(.data[[marker_col]]), 
      sd_cells = sd(.data[[marker_col]]), 
      n_cells = n(), 
      SE_cells = sd(.data[[marker_col]]) / sqrt(n())  
    ) %>%
    ungroup()
  
  return(data_summ_df) 
}
# ******************************************************************************
# Here, we will create dataframes for the summarized data for each marker,
# and save them as .csv files in the 'results/tables/' directory.

# initiate list
summarized_data <- list()

# iterate through each marker and call function
for (marker in markers) {
  summarized_data[[marker]] <- summ_data_function(data, marker)
  print(summarized_data[[marker]])
  
  # write a csv file for each summarized dataset for each marker
  write.csv(summarized_data[[marker]], 
            file.path(
              output_dir_tables, paste0(marker,".csv")), 
            row.names = FALSE)
  
  # Print to let us know that script was completed successfully
  print(paste("Data for", marker, "has been preprocessed."))
  
}

