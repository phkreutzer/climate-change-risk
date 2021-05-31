# Author: Philipp Kreutzer 
# Master Thesis 2021
# Titel: Would You Think We Are Doomed Because of Climate Change? 
#        Risk Perception, Taking Action and Trusting Others in the
#        Face of Different Climate Change Scenarios

getwd()
setwd('../')
script_path <- './scripts/'
tables_path <- './tables/'
figures_path <- './figures/'
dir.create(file.path('../master-thesis/', tables_path))  
dir.create(file.path('../master-thesis/', figures_path))  

# Main Analysis
source(paste0(script_path, "import.R"))
source(paste0(script_path, "descriptive-data.R"))
source(paste0(script_path, "SE-function.R"))
source(paste0(script_path, "analysis.R"))
source(paste0(script_path, "graphs-out.R"))

# Additional analysis including responses under 6 minutes
source(paste0(script_path, "import_no_dropped_cases.R"))
source(paste0(script_path, "analysis_no_dropped_cases.R"))

