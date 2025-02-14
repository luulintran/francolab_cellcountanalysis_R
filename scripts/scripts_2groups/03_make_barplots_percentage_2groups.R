# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# LOAD OBJECTS FROM 02_statstests_2groups.R
source("scripts/scripts_2groups/02_statstests_percentage_2groups.R")

# Define custom colors for treatment groups
custom_colors <- c(pcig = "#bcbcbc", dnrbpj = "#c4586f")

# 6. GENERATE BAR PLOTS: -------------------------------------------------------

# Function to create a bar plot
create_barplot <- function(data_summ, 
                           data_marker, 
                           treatment_col = "treatment",
                           marker_col, 
                           title, 
                           test_used, 
                           p_value, 
                           colors) {  
  
  scaleFUN <- function(x) sprintf("%.1f", x)  # Format y-axis labels to one decimal point
  
  # Make the order of treatments control_group then mutant-group 
  data_summ[[treatment_col]] <- factor(data_summ[[treatment_col]], 
                                       levels = c(control_group, mutant_group))
  data_marker[[treatment_col]] <- factor(data_marker[[treatment_col]], 
                                         levels = c(control_group, mutant_group))
  
  # Create the plot
  barplot <- ggplot(data_summ, 
                    aes(x = !!sym(treatment_col), 
                        y = mean_cells)) + 
    geom_col(width = 0.5, 
             aes(fill = !!sym(treatment_col)), 
             color = 'black', 
             size = 0.5) + 
    geom_errorbar(aes(ymin = mean_cells - SE_cells, 
                      ymax = mean_cells + SE_cells), 
                  width = 0.3, 
                  size = 0.5) + 
    geom_dotplot(data = data_marker, 
                 aes(x = !!sym(treatment_col), 
                     y = !!sym(marker_col), 
                     fill = !!sym(treatment_col)), 
                 binaxis = 'y', 
                 stackdir = 'center', 
                 dotsize = 0.8, 
                 show.legend = FALSE) + 
    theme_classic(base_size = 12) + 
    ylab("% of GFP+ cells") + 
    scale_x_discrete(limits = c(control_group, mutant_group)) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_line(size = 0.5, color = "black"), 
      axis.text.x = element_text(colour = 'black', angle = 45, hjust = 1), 
      axis.text.y = element_text(hjust = 1, colour = 'black'), 
      axis.title.x = element_blank(), 
      aspect.ratio = 2 / 1, 
      legend.key.size = unit(0.7, "line"), 
      text = element_text(size = 14), 
      plot.title = element_text(size = 14)) + 
    scale_y_continuous(expand = c(0, 0), labels = scaleFUN) + 
    scale_fill_manual(values = colors) +  # Use custom colors defined earlier
    ggtitle(title) + 
    theme(legend.position = "none") + 
    annotate("text", x = 1.5, y = max(data_summ$mean_cells, na.rm = TRUE) + 0.5, 
             label = paste(test_used, "\np-value =", format(p_value, digits = 3)), 
             size = 2, hjust = 0.5)
  
  return(barplot)
}

# ******************************************************************************


# Iterate through each marker and generate plots
for (marker in markers) {
  
  # Extract marker cell count data
  data_marker <- data %>%
    select(treatment, brain_id, all_of(marker))
  
  # Extract summarized data
  data_summ <- summarized_data[[marker]]
  
  # define order of treatments on barplot
  data_summ$treatment <- factor(data_summ$treatment, 
                                levels = c(control_group, mutant_group))
  data_marker$treatment <- factor(data_marker$treatment, 
                                  levels = c(control_group, mutant_group))
  
  # Extract stats test results
  test_results <- stats_results[[marker]]
  p_value <- test_results$Test_Result$p.value
  
  # Generate the barplot with function made earlier
  barplot <- create_barplot(
    data_summ = data_summ, 
    data_marker = data_marker, 
    marker_col = marker, 
    title = paste0("% ", toupper(marker), "+ cells"), 
    test_used = test_results$Test_Used, 
    p_value = p_value,
    colors = custom_colors # based on the custome_colors defined earlier
  )
  
  # Print barplots
  print(barplot)
  
  # Save barplots as pdf
  ggsave(filename = paste0("results/2groups_results/", marker, "_barplot.pdf"), 
         plot = barplot, 
         width = 5, 
         height = 5) # can change the width and height as needed
  
  print(paste(marker, "barplot generated and saved in results/"))
}
