# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# 6. GENERATE BAR PLOTS: -------------------------------------------------------

# define custom colors for each treatment group
custom_colors <- c('#bcbcbc', '#f4bfbd', '#e590a0', '#c4586f')

# Define function for making barplots: *****************************************
create_barplot_4_groups <- function(data_summ, 
                                    data_marker, 
                                    treatment_col = "treatment", 
                                    marker_col = "marker", 
                                    title = "Title", 
                                    positions = NULL, 
                                    test_used, 
                                    p_value, 
                                    fill_colors = NULL,
                                    dotsize_value = 1, 
                                    fixed_divisor = 40,  
                                    y_label = "% of EP'd cells") {
  
  scaleFUN <- function(x) sprintf("%.1f", x) 
  
  # Default position order
  if (is.null(positions)) {
    positions <- unique(data_marker[[treatment_col]])
  }
  
  # Default fill colors if not defined
  if (is.null(fill_colors)) {
    fill_colors <- scales::hue_pal()(length(positions)) 
  }
  
  # Calculate max value of y-axis and add buffer (for annotation and y-axis scale)
  max_marker_value <- max(data_marker[[marker_col]], na.rm = TRUE) * 1.1 
  
  # Define binwidth based on range of marker values/y axis
  binwidth_value <- (max(data_marker[[marker_col]], na.rm = TRUE) - 
                       min(data_marker[[marker_col]], na.rm = TRUE)) / fixed_divisor
  
  # Plot
  barplot <- ggplot(data_summ, 
                    aes(x = !!sym(treatment_col), 
                        y = mean_cells)) + 
    geom_col(width = 0.5, 
             aes(fill = !!sym(treatment_col)), 
             color = 'black', 
             size = 0.25) + 
    geom_errorbar(aes(ymin = mean_cells - SE_cells, 
                      ymax = mean_cells + SE_cells), 
                  width = 0.3, 
                  size = 0.25) + 
    geom_dotplot(data = data_marker, 
                 aes(x = !!sym(treatment_col), 
                     y = !!sym(marker_col), 
                     fill = !!sym(treatment_col)), 
                 binaxis = 'y', 
                 stackdir = 'center', 
                 dotsize = dotsize_value,   
                 binwidth = binwidth_value, 
                 show.legend = FALSE) + 
    theme_classic(base_size = 10) + 
    ylab(y_label) + 
    scale_x_discrete(limits = positions) + 
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.25, 
                               color = rgb(0, 0, 0, max = 255)), 
      axis.text.x = element_text(colour = 'black', angle = 45, hjust = 1), 
      axis.text.y = element_text(hjust = 1, colour = 'black'), 
      axis.title.x = element_blank(), 
      aspect.ratio = 2 / 1, 
      legend.key.size = unit(0.7, "line"), 
      text = element_text(size = 10), 
      plot.title = element_text(size = 10)) + 
    scale_y_continuous(expand = c(0, 0), labels = scaleFUN, limits= c(0, max_marker_value)) + 
    scale_fill_manual(values = fill_colors, 
                      name = "", 
                      labels = positions) + 
    ggtitle(title) + 
    theme(legend.position = "none") + 
    annotate("text", x = median(1:length(positions)), 
             y = max_marker_value * 0.95,  
             label = paste(test_used, "\n", format(p_value, digits = 3)), 
             size = 3, hjust = 0.5)
  
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
  
  # Define order of treatments on barplot
  data_summ$treatment <- factor(data_summ$treatment, 
                                levels = c(control_group, mutant_groups))
  data_marker$treatment <- factor(data_marker$treatment, 
                                  levels = c(control_group, mutant_groups))
  
  # Extract stats test results
  test_results <- stats_results[[marker]]
  posthoc_tukey_test_results <- posthoc_tukey_results[[marker]]
  
  # Generate the barplot
  barplot <- create_barplot_4_groups( 
    data_summ = data_summ, 
    data_marker = data_marker, 
    marker_col = marker, 
    title = paste0("% ", toupper(marker), "+ cells"), 
    test_used = test_results$Test_Used, 
    p_value = posthoc_tukey_test_results$ANOVA_p_value,
    fill_colors = custom_colors
  )
  
  # Print barplots
  print(barplot)
  
  # Save barplots as PDF files
  ggsave(filename = file.path(output_dir_figures, paste0(marker, "_barplot.pdf")), 
         plot = barplot, 
         width = 5, 
         height = 5) # Adjust width and height as needed
  
  print(paste(marker, "barplot generated and saved in results/figures/"))
}