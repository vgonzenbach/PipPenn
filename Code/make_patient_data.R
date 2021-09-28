# Author: Virgilio Gonzenbach
# 
# This script makes individual datasets from the raw data in order to 
setwd(rprojroot::find_rstudio_root_file()) 

make_patient_data = function(csv.path){
  #Save patients in individual datasets
  df = read.csv(sprintf("Data/%s", csv.path))
  
  df_list = split(df, df$PatientID)
  
  for (name in names(df_list)){
    saveRDS(df_list[[name]], sprintf("Data/patient_%s.rds", name))
  }

  return(NULL)
}

patient_datasets = dir("Data")[grep("Patients", dir("Data"))]

sapply(patient_datasets, FUN = make_patient_data)
