# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# RUN ALL SCRIPTS IN ORDER: ----------------------------------------------------
# These will run all lines of code in each script listed below:

# MAKE SURE YOU EDIT THIS CONFIG SCRIPT WITH THE CORRECT DATA AND VARIABLES FOR YOUR EXPERIMENT
source("scripts/scripts_4groups/config_4_groups.R")
print("Ran config_4_groups.R script and defined variables.")
# ******************************************************************************

# 1. Preprocess data
source("scripts/scripts_4groups/01_preprocess_data_4groups.R")
print("Ran script 01 successfully. Data has been processed.")

# 2. Perform stats tests on 2 groups
source("scripts/scripts_4groups/02_statstests_percentage_4groups.R")
print("Ran script 02 successfully. Stats tests were performed.")

# 3. Make barplots based on data and stats results
source("scripts/scripts_4groups/03_make_barplots_percentage_4groups.R")
print("Ran script 03 successfully. Barplots were generated.")