# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# LOAD OBJECTS FROM 02_statstests_2groups.R
source("scripts/scripts_4groups/01_preprocess_data_4groups.R")

# 5. PERFORM STATS ANALYSIS: ---------------------------------------------------

# Define stats test function: **************************************************
stats_test_4_groups <- function(data_df, marker_col, control_group, mutant_groups) {
  # Check that the variable is numeric
  variable_data <- data_df[[marker_col]]
  if (!is.numeric(variable_data)) {
    stop("The specified variable is not numeric.")
  }
  
  # Check that all the groups exist
  all_groups <- c(control_group, mutant_groups)
  # Make a function to extract values of each group and then store in group_data
  group_data <- lapply(all_groups, function(group)
    variable_data[data_df$treatment == group])
  
  # Check if any group has no data
  if (any(sapply(group_data, length) == 0)) {
    stop("No data found for one or more groups.")
  }
  
  # Test for normality (Shapiro-Wilk test)
  shapiro_results <- lapply(group_data, shapiro.test)
  names(shapiro_results) <- all_groups
  
  # Print Shapiro-Wilk test results
  for (group in all_groups) {
    cat(paste("Shapiro test for", group, "p-value:", shapiro_results[[group]]$p.value, "\n"))
  }
  
  # Test for equality of variances (Levene's test)
  levene_test <- leveneTest(variable_data ~ data_df$treatment)
  cat(paste("Levene test p-value:", levene_test$`Pr(>F)`[1], "\n"))
  
  # Determine which statistical test to use
  all_normal <- all(sapply(shapiro_results, function(res) res$p.value > 0.05))
  equal_variances <- levene_test$`Pr(>F)`[1] > 0.05
  
  if (all_normal) {
    if (equal_variances) {
      test_result <- aov(variable_data ~ data_df$treatment)  
      test_used <- "One-way ANOVA"  # Normal distribution & equal variances
    } else {
      test_result <- oneway.test(variable_data ~ data_df$treatment, var.equal = FALSE)  
      test_used <- "Welch's ANOVA"  # Normal distribution but unequal variances
    }
  } else {
    test_result <- kruskal.test(variable_data ~ data_df$treatment)  
    test_used <- "Kruskal-Wallis test"  # Non-normal distribution
  }
  
  # Print results
  cat(paste("Test used:", test_used, "\n"))
  print(summary(test_result))
  
  # Compute group means
  group_means <- sapply(group_data, mean, na.rm = TRUE)
  
  # Return results as a list
  stats_test_result <- list(
    Normality_Results = shapiro_results,
    Variance_Result = levene_test,
    Test_Used = test_used,
    Test_Result = test_result,
    Group_Means = group_means
  )
  
  return(stats_test_result)
}
# ******************************************************************************

# initiate a list to store stats results
stats_results <- list()

# Iterate through each marker and call function to perform stats tests
for (marker in markers) {
  stats_results[[marker]] <- stats_test_4_groups(data,
                                                 marker, 
                                                 control_group, 
                                                 mutant_groups)
  print(paste("Stats results for", marker, "has been processed."))
}

# Define Post Hoc Tukey's test function: ***************************************
posthoc_tukey_test <- function(data_df, marker_col, group_col) {
  
  # Check that the specified columns exist
  if (!(marker_col %in% colnames(data_df)) || !(group_col %in% colnames(data_df))) {
    stop("One or more specified columns do not exist in the dataset.")
  }
  
  # Remove rows with NA values in the selected columns
  data_clean <- na.omit(data_df[, c(marker_col, group_col)])
  
  # Maure sure the grouping variable is a factor
  data_clean[[group_col]] <- as.factor(data_clean[[group_col]])
  
  # Perform one-way ANOVA
  model <- aov(as.formula(paste(marker_col, "~", group_col)), data = data_clean)
  
  # Extract ANOVA summary
  summary_posthoc <- summary(model)
  
  # Extract ANOVA p-value
  anova_p_value <- summary_posthoc[[1]]$`Pr(>F)`[1]
  
  # Perform Tukey HSD test for pairwise comparisons
  tukey_results <- TukeyHSD(model, conf.level = 0.95)
  
  # Extract p-values
  tukey_p_values <- tukey_results[[group_col]][,"p adj"]
  
  # Format p-values with up to 10-digits
  formatted_p_values <- signif(tukey_p_values, digits = 10)
  
  # Plot Tukey HSD results
  plot(tukey_results, las = 2)
  
  # Return results as a list
  return(list(
    "ANOVA_p_value" = anova_p_value,
    "Tukey_P_Values" = formatted_p_values,
    "Tukey_Summary" = tukey_results
  ))
}


# ******************************************************************************
# initiate list
posthoc_tukey_results <- list()

# Iterate through markers vector
for (marker in markers) {
  posthoc_tukey_results[[marker]] <- posthoc_tukey_test(data,
                                                        marker, 
                                                        "treatment")
  
  print(paste("Posthoc Tukey results for", marker, "have been processed."))
}