
# 1. DEFINE FILES AND PATHS: ---------------------------------------------------

input_file <- "data/4groups_data/2023-05-01_cd_e15-18_dnrbpj-ascl1-nicd_olig2_pdgfra.csv"
output_dir_tables <- "results/4groups_results/tables/"
output_dir_figures <- "results/4groups_results/figures/"

# Make sure output directory exists, and if it doesn't, create one
if (!dir.exists(output_dir_tables)) {
  dir.create(output_dir_tables, recursive = TRUE)
}

if (!dir.exists(output_dir_figures)) {
  dir.create(output_dir_figures, recursive = TRUE)
}

# 2. DEFINE MARKERS AND CONTROL GROUP: --------------------------------------

# Define markers based on sample sheet columns
markers <- c("olig2", "olig2_pdgfra")

# Define control group based on sample sheet rows
control_group <- "pcig"  

