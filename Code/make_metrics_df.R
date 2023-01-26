source("Code/functions.R")
source("Code/summary.R")
library(dplyr)

setwd(rprojroot::find_rstudio_root_file()) 
make_metrics_df = function(params){
  
  df = expand.grid(params)
  colnames(df) = names(params)
  
  # Exclude non-existent params combinations
  df = df[( (df$pca == 'dominanthand' | df$pca == 'nondominanthand') & df$npc == 6) | (df$pca == 'bothhands' & df$npc %in% c(3,4,5)),  ]
  
  # for each patient, ses combination get metrics from confusion matrix + percent_correct at each cut_off
  metrics_df = df |> apply(1, function(x){
    pred = load_prediction(patient = x["patient"], pca = x['pca'], npc = x['npc'], ses = x["session"], smooth = x["smooth"])
    row = get_metrics(pred$test$Accuracy$table)
    
    percent_correct = mutate(pred$test$Session, percent_correct = correct_med_pred / med_session)$percent_correct
    names(percent_correct) = paste0("correct_at_",pred$test$Session$cut_off)
    row = c(row, percent_correct)
    return(row)
  }) |> t() |> as.data.frame()
  
  metrics_df = cbind(df, metrics_df)
  metrics_df$smooth = ifelse(metrics_df$smooth, "Post-Smoothing", "Pre-Smoothing") # Fix name
  metrics_df$smooth = factor(metrics_df$smooth, levels=rev(unique(metrics_df$smooth))) # Reverse order
  
  return(metrics_df)
}

params = list(
  patient = c(19, 20, 28, 32, 40, 52, 75, 86, 88, 89),
  pca = c('bothhands', 'dominanthand', 'nondominanthand'),
  npc = c(3, 4, 5, 6),
  smooth = c(TRUE, FALSE),
  session = c(5, 6, 8, 9, 11, 12)
)
# Prepare smoothing indicator variable for plotting
metrics_df = make_metrics_df(params)

# Load data for all training modes -----
params_n2 = params
params_n2$session = combn(params$session, 2) |> t() |> apply(1, function(x) paste(x, collapse=","))
metrics_df_n2 = make_metrics_df(params_n2)

params_n3 = params
params_n3$session = combn(params$session, 3) |> t() |> apply(1, function(x) paste(x, collapse=","))
metrics_df_n3 = make_metrics_df(params_n3)

metrics_df_all_n = metrics_df |>
  rbind(metrics_df_n2, metrics_df_n3)

# make n_train dataframe
metrics_df_all_n = metrics_df_all_n |> tibble::add_column(n_train = stringr::str_count(metrics_df_all_n$session, ",") + 1, .before = "session") 

# write 
write.csv(metrics_df, 'metrics_df.csv')
write.csv(metrics_df_all_n, 'metrics_df_all_n.csv')
