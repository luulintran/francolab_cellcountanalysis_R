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
  read.csv(
    "data/4groups_data/2023-05-01_cd_e15-18_dnrbpj-ascl1-nicd_olig2_pdgfra.csv"
    )

# 2. DEFINE MARKERS AND TREATMENT GROUPS: --------------------------------------

# Define markers
# Change as needed based on your column names!
markers <- c("olig2", "olig2_pdgfra")

# Define treatment groups
# Change as needed based on your column names!
control_group <- "pcig"  

# define all other unique treatments as "mutant"
mutant_groups <- setdiff(unique(data$treatment), control_group)
all_groups <- c(control_group, mutant_groups)

# 3. ORAGANIZE AND SUMMARIZE DATA: ---------------------------------------------

# Define function for summarizing data *****************************************

summ_data_function <- function(data_df, marker_col) { 
  data_summ_df <- data_df %>% 
    group_by(treatment) %>% 
    summarise(
      mean_cells = mean(.data[[marker_col]], na.rm = TRUE), 
      sd_cells = sd(.data[[marker_col]], na.rm = TRUE), 
      n_cells = sum(!is.na(.data[[marker_col]])),  # Count non-NA values
      SE_cells = sd(.data[[marker_col]], na.rm = TRUE) / sqrt(n_cells)  
    ) %>%
    ungroup()
  
  # Ensure treatment order (control first)
  data_summ_df$treatment <- factor(data_summ_df$treatment, levels = all_groups)
  return(data_summ_df)
}

# ******************************************************************************
# initiate list
summarized_data <- list()

# iterate through each marker and call function
for (marker in markers) {
  summarized_data[[marker]] <- summ_data_function(data, marker)
  print(summarized_data[[marker]])
  print(paste("Data for", marker, "has been preprocessed."))
}

