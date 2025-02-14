# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# LOAD OBJECTS FROM 02_statstests_2groups.R
source("scripts/scripts_2groups/01_preprocess_data_2groups.R")

# 4. PERFORM STATS ANALYSIS: ---------------------------------------------------

# Define stats test function: **************************************************
stats_test <- function(data_df, marker_col, control_group, mutant_group) {
  # Check that the variable is numeric
  variable_data <- data_df[[marker_col]]
  if (!is.numeric(variable_data)) {
    stop("The specified variable is not numeric.")
  }
  
  # Split data by treatment group
  ctrl_data <- variable_data[data_df$treatment == control_group] 
  mut_data <- variable_data[data_df$treatment == mutant_group] 
  
  # Check if the the splitting worked correctly
  if (length(ctrl_data) == 0 | length(mut_data) == 0) {
    stop("No data found for one of the groups.")
  }
  
  # Test for normality (Shapiro-Wilk test)
  shapiro_ctrl <- shapiro.test(ctrl_data)
  shapiro_mut <- shapiro.test(mut_data)
  
  print(paste("Shapiro test for", control_group, "p-value:", shapiro_ctrl$p.value))
  print(paste("Shapiro test for", mutant_group, "p-value:", shapiro_mut$p.value))
  
  # Test for equality of variances (Levene's test)
  levene_test <- leveneTest(variable_data ~ data_df$treatment)
  
  print(paste("Levene test p-value:", levene_test$`Pr(>F)`[1]))
  
  # Determine which statistical test to use
  if (shapiro_ctrl$p.value > 0.05 && shapiro_mut$p.value > 0.05) { 
    # Normal distribution
    if (levene_test$`Pr(>F)`[1] > 0.05) { 
      test_result <- t.test(ctrl_data, mut_data, var.equal = TRUE) 
      test_used <- "Student's t-test" # if data is normal and variance is equal
    } else { 
      test_result <- t.test(ctrl_data, mut_data, var.equal = FALSE) 
      test_used <- "Welch's t-test" # if data is normal but variance is not equal
    }
  } else { 
    test_result <- wilcox.test(ctrl_data, mut_data) 
    test_used <- "Wilcoxon rank-sum test" #if data is not normally distributed
  }
  
  # Return results
  stats_test_result <- list(
    Normality_CTRL = shapiro_ctrl,
    Normality_MUT = shapiro_mut,
    Variance_result = levene_test,
    Test_Used = test_used,
    Test_Result = test_result,
    Mean_CTRL = mean(ctrl_data),
    Mean_MUT = mean(mut_data)
  )
  
  return(stats_test_result)
}

# ******************************************************************************

# Perform statistical tests

# Initiate a list to store the stats_results
stats_results <- list()

for (marker in markers) {
  # Call stats_test function and then store results in
  # stats_results list initiated earlier
  stats_results[[marker]] <- stats_test(data, 
                                        marker, 
                                        control_group, 
                                        mutant_group)
  
  print(paste("Stats results for", marker, "has been processed."))
}