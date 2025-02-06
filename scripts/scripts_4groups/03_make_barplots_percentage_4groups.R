# SET UP: ----------------------------------------------------------------------
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rlang)

# LOAD OBJECTS FROM 02_statstests_2groups.R
source("scripts/scripts_4groups/02_statstests_percentage_4groups.R")

# 5. GENERATE BAR PLOTS: -------------------------------------------------------

# Define function for making barplots: *****************************************
create_barplot_4_groups <- function(data_summ, 
                                    data_marker, 
                                    treatment_col = "treatment", 
                                    marker_col = "marker", 
                                    title = "Title", 
                                    positions = c("pcig", "ascl1", "nicd_ascl1", "dnrbpj_ascl1"), 
                                    test_used, 
                                    p_value, 
                                    fill_colors = c('#bcbcbc', '#f4bfbd', '#e590a0', '#c4586f'),
                                    dotsize_value,
                                    binwidth_value) {
  
  # Function to format y-axis
  scaleFUN <- function(x) sprintf("%.1f", x) 
  
  # Calculate max value of the marker column for consistent y-axis scaling
  max_marker_value <- max(data_marker[[marker_col]], na.rm = TRUE) + 5
  
  # Creating the plot
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
    ylab("% of EP'd cells") + 
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
    scale_y_continuous(expand = c(0, 0), labels = scaleFUN, limits= c(0,max_marker_value)) + 
    scale_fill_manual(values = fill_colors, 
                      name = "", 
                      labels = positions) + 
    ggtitle(title) + 
    theme(legend.position = "none") + 
    annotate("text", x = 2.5, y = max(data_marker[[marker_col]]) + 2, 
             label = paste(test_used, "\n", format(p_value, digits = 3)), 
             size = 3, hjust = 0.5)
  
  return(barplot)
}

# ******************************************************************************

# Call function to generate bar plots and save as PDF

# OLIG2+ ***********************************************************************
olig2_barplot_4groups <- create_barplot_4_groups (
  data_summ_olig2, 
  data.olig2, 
  treatment_col = "treatment", 
  marker_col = "olig2", 
  title = "% OLIG2+ Cells", 
  positions = c("pcig", "ascl1", "nicd_ascl1", "dnrbpj_ascl1"), 
  test_used = olig2_test_results$Test_Used,
  p_value = olig2_posthoctukey_results$ANOVA_pvalue,
  fill_colors = c('#bcbcbc', '#f4bfbd', '#e590a0', '#c4586f'),
  dotsize_value = 0.7,
  binwidth_value = 0.7)


# set up filename and save
filename = "results/4groups_results/ascl1-nicd-dnrbpj_olig2_barplot.pdf"
pdf(filename, width = 5, height = 5)
print(olig2_barplot_4groups)

dev.off()

print("OLIG2+ barplot generated and saved")

# OLIG2+ PDGFRA+ ***************************************************************
olig2_pdgfra_barplot_4groups <- create_barplot_4_groups (
  data_summ_olig2_pdgfra, 
  data.olig2.pdgfra, 
  treatment_col = "treatment", 
  marker_col = "olig2_pdgfra", 
  title = "% OLIG2+ PDGFRA+ Cells", 
  positions = c("pcig", "ascl1", "nicd_ascl1", "dnrbpj_ascl1"), 
  test_used = olig2_pdgfra_test_results$Test_Used,
  p_value = olig2_pdgfra_posthoctukey_results$ANOVA_pvalue,
  fill_colors = c('#bcbcbc', '#f4bfbd', '#e590a0', '#c4586f'),
  dotsize_value = 0.3,
  binwidth_value = 0.7)

# set up filename and save
filename = "results/4groups_results/ascl1-nicd-dnrbpj_olig2_pdgfra_barplot.pdf"
pdf(filename, width = 5, height = 5)
print(olig2_pdgfra_barplot_4groups)

dev.off()

print("OLIG2+ PDGFRA+ barplot generated and saved")

