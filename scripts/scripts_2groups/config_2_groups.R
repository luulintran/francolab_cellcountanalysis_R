
# 1. DEFINE FILES AND PATHS: ---------------------------------------------------

input_file <- "data/2groups_data/2021-05-06_cd1_e15-18_dnrbpj_olig2_pdgfra.csv"
output_dir_tables <- "results/2groups_results/tables"
output_dir_figures <- "results/2groups_results/figures/"

# Make sure output directory exists, and if it doesn't, create one
if (!dir.exists(output_dir_tables)) {
  dir.create(output_dir_tables, recursive = TRUE)
}

if (!dir.exists(output_dir_figures)) {
  dir.create(output_dir_figures, recursive = TRUE)
}

# 2. DEFINE MARKERS AND TREATMENT GROUPS: --------------------------------------

# Define markers
markers <- c("olig2", "olig2_pdgfra", "olig2_pdgfraneg")

# Define control and mutant groups
control_group <- "pcig"     
mutant_group <- "dnrbpj"  
