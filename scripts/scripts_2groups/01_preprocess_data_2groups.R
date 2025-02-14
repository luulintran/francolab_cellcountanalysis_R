# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# 1. READ IN DATA: -------------------------------------------------------------

# Read in as dataframe and save as object
data <-
  read.csv("data/2groups_data/2021-05-06_cd1_e15-18_dnrbpj_olig2_pdgfra.csv")

# 2. DEFINE MARKERS AND TREATMENT GROUPS: --------------------------------------

# Define markers
# Change as needed based on your column names!
markers <- c("olig2", "olig2_pdgfra", "olig2_pdgfraneg")


# Define control and mutant groups
# Change as needed based on "treatments"!
control_group <- "pcig"     
mutant_group <- "dnrbpj"    

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

# Initiate a list for the summarized_data
summarized_data <- list()

# Iterate through each "marker" in the vector markers defined earlier
for (marker in markers) {
  # Select specific columns from the original data dataframe
  # and store it in a new dataframe for each marker
  data_marker <- data %>%
    select(treatment, brain_id, all_of(marker))
  
  # Call summ_data_function to summarize data,
  # and store results in the summarized_data list we initiated earlier
  summarized_data[[marker]] <- summ_data_function(data_marker, marker)
  
  # Print summarized data for each marker in its own dataframe
  print(summarized_data[[marker]])
  print(paste("Data for", marker, "has been processed."))
}
