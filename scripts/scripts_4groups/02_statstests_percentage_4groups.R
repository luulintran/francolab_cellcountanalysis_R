# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# LOAD OBJECTS FROM 02_statstests_2groups.R
source("scripts/scripts_4groups/01_preprocess_data_4groups.R")

# 4. PERFORM STATS ANALYSIS: ---------------------------------------------------

# Define stats test function: **************************************************
stats_test_4_groups <- function(data, variable) {
  # Ensure the variable exists and is numeric
  if (!variable %in% names(data)) {
    stop("The specified variable does not exist in the dataframe.")
  }
  
  variable_data <- data[[variable]]
  if (!is.numeric(variable_data)) {
    stop("The specified variable is not numeric.")
  }
  
  # Split the data by treatment groups. Change names as needed
  group1_data <- variable_data[data$treatment == "pcig"]
  group2_data <- variable_data[data$treatment == "ascl1"]
  group3_data <- variable_data[data$treatment == "nicd_ascl1"]
  group4_data <- variable_data[data$treatment == "dnrbpj_ascl1"]
  
  # Validate the groups
  if (any(length(group1_data) == 0, length(group2_data) == 0, 
          length(group3_data) == 0, (length(group4_data) == 0))) {
    stop("No data found for one or more groups.")
  }
  
  # Test for normality (Shapiro-Wilk test)
  shapiro_results <- list(
    PCIG = shapiro.test(group1_data), #change names as needed
    ASCL1 = shapiro.test(group2_data),
    NICD_ASCL1 = shapiro.test(group3_data),
    DNRBPJ_ASCL1 = shapiro.test(group4_data)
  )
  
  # Print normality test results
  for (group in names(shapiro_results)) {
    cat(paste("Shapiro test for", group, ": p-value =", 
              shapiro_results[[group]]$p.value, "\n"))
  }
  
  # Test for equal variances (Levene's test)
  levene_test <- leveneTest(variable_data ~ data$treatment)
  
  # Print Levene's test result
  cat(paste("Levene's test p-value =", levene_test$`Pr(>F)`[1], "\n"))
  
  # Determine the appropriate test
  all_normal <- all(sapply(shapiro_results, function(res) res$p.value > 0.05))
  equal_variances <- levene_test$`Pr(>F)`[1] > 0.05
  
  if (all_normal) {
    if (equal_variances) {
      test_result <- aov(variable_data ~ data$treatment)  
      # One-way ANOVA
      test_used <- "One-way ANOVA"
    } else {
      test_result <- oneway.test(variable_data ~ data$treatment, 
                                 var.equal = FALSE) 
      # Welch ANOVA
      test_used <- "Welch ANOVA"
    }
  } else {
    test_result <- kruskal.test(variable_data ~ data$treatment)  
    # Kruskal-Wallis test
    test_used <- "Kruskal-Wallis test"
  }
  
  # Return results as a list
  stats_test_result <- list(
    Normality_Results = shapiro_results,
    Variance_Result = levene_test,
    Test_Used = test_used,
    Test_Result = test_result
  )
  
  # Print results
  cat(paste("Test used:", test_used, "\n"))
  print(summary(test_result))
  
  return(stats_test_result)
}
# ******************************************************************************

# Call function to perform stats tests
olig2_test_results <- stats_test_4_groups(data.olig2, 
                                          "olig2")

olig2_pdgfra_test_results <- stats_test_4_groups(data.olig2.pdgfra, 
                                                 "olig2_pdgfra")


# Define Post Hoc Tukey's test function: ***************************************
posthoc_tukey_test <- function(marker_col, data_df) {
  
  # Remove rows with NA values in either 'marker_col' or 'treatment' columns
  data_clean <- na.omit(data_df[, c(marker_col, "treatment")])
  
  # one-way ANOVA
  model <- aov(as.formula(paste(marker_col, "~ treatment")), data = data_clean)
  
  # Make ANOVA model
  summary_posthoc <- summary(model)
  
  # Extract ANOVA p-value from model above
  anova_p_value <- summary_posthoc[[1]]$`Pr(>F)`[1] 
  
  # Perform Tukey HSD test for pairwise comparisons
  tukey_results <- TukeyHSD(model, conf.level = 0.95)
  
  # Extract p-values from Tukey results
  p_values <- tukey_results$treatment[,"p adj"]
  
  # Format p-values in scientific notation
  formatted_p_values <- format(p_values, scientific = TRUE)
  
  # Return significant p-values with 10 digits precision
  significant_p_values <- signif(p_values, digits = 10)
  
  # Plot Tukey HSD results
  plot(tukey_results, las = 2)
  
  # Return p-values and summary of Tukey HSD in a list
  return(list("Tukey_P_Values" = significant_p_values, 
              "Tukey_Summary" = tukey_results, 
              "ANOVA_pvalue" = anova_p_value))
}

# ******************************************************************************

# Call function to perform post hoc Tukey's test
olig2_posthoctukey_results <- posthoc_tukey_test("olig2", 
                                                 data.olig2)
olig2_pdgfra_posthoctukey_results <- posthoc_tukey_test("olig2_pdgfra", 
                                                        data.olig2.pdgfra)


print("All stats tests were performed successfully")