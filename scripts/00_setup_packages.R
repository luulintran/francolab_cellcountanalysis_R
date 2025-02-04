# Set CRAN repository
options(repos = c(CRAN = "https://cran.rstudio.com"))

# List of required packages
packages <- c("tidyr", "readr", "dplyr", "ggplot2", "car")

# Install only missing packages
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, ask = FALSE)
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Print message
message("All required packages are installed and loaded!")