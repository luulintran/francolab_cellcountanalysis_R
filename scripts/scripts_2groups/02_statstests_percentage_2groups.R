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
stats_test <- function(data_df, marker_col) {
  # Ensure the variable is numeric
  variable_data <- data[[ marker_col ]]
  if (!is.numeric(variable_data)) {
    stop("The specified variable is not numeric.")
  }
  
  # Split data by treatment group
  ctrl_data <- variable_data[data$treatment == "pcig"] 
  # change group name if needed
  mut_data <- variable_data[data$treatment == "dnrbpj"] 
  # change group name if needed
  
  # Check if the subsetting worked correctly
  if (length(ctrl_data) == 0 | length(mut_data) == 0) {
    stop("No data found for one of the groups.")
  }
  
  # Test for normality (Shapiro-Wilk test)
  shapiro_ctrl <- shapiro.test(ctrl_data)
  shapiro_mut <- shapiro.test(mut_data)
  
  # Print normality test results
  print(paste("Shapiro test for CTRL: p-value =", shapiro_ctrl$p.value))
  print(paste("Shapiro test for MUTANT: p-value =", shapiro_mut$p.value))
  
  # Test for equality of variances (Levene's test)
  levene_test <- leveneTest(variable_data ~ data$treatment)
  
  # Print Levene's test result
  print(paste("Levene test p-value =", levene_test$`Pr(>F)`[1]))
  # Determine which statistical test to use based on normality and variance
  if (shapiro_ctrl$p.value > 0.05 && shapiro_mut$p.value > 0.05) { 
    # Normal distribution
    if (levene_test$`Pr(>F)`[1] > 0.05) { # Equal variances
      test_result <- t.test(ctrl_data, mut_data, var.equal = TRUE) 
      # Student's t-test
      test_used <- "Student's t-test"
    } else { # Unequal variances
      test_result <- t.test(ctrl_data, mut_data, var.equal = FALSE) 
      # Welch's t-test
      test_used <- "Welch's t-test"
    }
  } else { # Non-normal distribution
    test_result <- wilcox.test(ctrl_data, mut_data) 
    # Wilcoxon rank-sum test
    test_used <- "Wilcoxon rank-sum test"
  }
  
  # Return the results
  stats_test_result <- list(
    Test_Used = test_used,
    Test_Result = test_result,
    Mean_CTRL = mean(ctrl_data),
    Mean_MUT = mean(mut_data)
  )
  
  # Print results
  print(stats_test_result)
  
  return(stats_test_result)
}

# ******************************************************************************

# Call function to perform stats tests
olig2_test_results <- stats_test(data.olig2, 
                                 "olig2")

olig2_pdgfra_test_results <- stats_test(data.olig2.pdgfra, 
                                        "olig2_pdgfra")

olig2_pdgfraneg_test_results <- stats_test(data.olig2.pdgfraneg, 
                                           "olig2_pdgfraneg")

print("All stats tests were performed successfully")