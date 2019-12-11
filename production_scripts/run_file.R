# ----- Run File -----

# ----- Set the Working Directory -----
if(Sys.info()[['nodename']] %in% c('966916-dci1-adw-002.ofg.local')){
  # - path on server
  setwd("/home/peter.tomko/concept---data-science")
}else{
  # - path on local
  setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
}

# ----- 0_data_download.R -----
source("production_scripts/0_data_download.R")

# ----- 1_tipsport_feedxml.R ----- 
source("production_scripts/1_tipsport_feedxml.R")

# ----- 2_variable_calculation.R ----- 
source("production_scripts/2_variable_calculation.R")

# ----- 3_modelling_data.R ----- 
source("production_scripts/3_modelling_data.R")

# ----- 4_xml_preprocessing.R ----- 
source("production_scripts/4_xml_preprocessing.R")

# ----- 5_model_estimation.R ----- 
source("production_scripts/5_model_Estiamtion.R")
