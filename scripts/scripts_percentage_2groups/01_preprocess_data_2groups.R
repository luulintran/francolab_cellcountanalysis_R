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
  read.csv("data/percentage_data/2021-05-06_cd1_e15-18_dnrbpj_olig2_pdgfra.csv")

# 2. ORGANIZE AND SUMMARIZE DATA: ----------------------------------------------

# Separate dataframes
data.olig2 <- data %>%
  select(treatment, brain_id, olig2)

data.olig2.pdgfra <- data %>%
  select(treatment, brain_id, olig2_pdgfra)

data.olig2.pdgfraneg <- data %>%
  select(treatment, brain_id, olig2_pdgfraneg)

# Define function for summarizing data *****************************************

# define the variables of the function, here the dataframe and the marker column
summ_data_function <- function(data_df, marker_col) { 
  # make a new dataframe containing the summarized data
  data_summ_df <- data_df %>% 
    # group by the treatment variable, so pcig and dnrbpj are the groups
    group_by(treatment) %>% 
    summarise(
      # calculate the mean for each group
      mean_cells = mean( {{marker_col}} ), 
      # calculate the standard deviation
      sd_cells = sd( {{marker_col}} ), 
      # the number of samples per group
      n_cells = n(), 
      # calculate the standard error for each group
      SE_cells = sd( {{marker_col}}) /sqrt(n())  
    ) %>%
    ungroup()
  
  # return the summarized data as a dataframe
  return(data_summ_df) 
}
# ******************************************************************************

# Call function and save each marker as its own dataframe 
data_summ_olig2 <- summ_data_function(
  data.olig2, 
  olig2)

data_summ_olig2_pdgfra <- summ_data_function(
  data.olig2.pdgfra, 
  olig2_pdgfra)

data_summ_olig2_pdgfraneg <- summ_data_function(
  data.olig2.pdgfraneg, 
  olig2_pdgfraneg)

print(data_summ_olig2)
print(data_summ_olig2_pdgfra)
print(data_summ_olig2_pdgfraneg)

print("Data has been preprocessed")