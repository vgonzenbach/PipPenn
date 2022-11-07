# Summary functions

load_prediction = function(patient, pca, npc, smooth, ses){
  # Loads Prediction Objects (nested lists)
  args = ls()
  args = lapply(args, function(x) gsub(" ", "", get(x), fixed = TRUE))
  trim = function(x) gsub(" ", "", x)
  resPred = readRDS(sprintf("Results/patient_%s_pred_pca-%s_npc-%s_smooth-%s_ses-%s.rds", 
                            trim(patient), trim(pca), trim(npc), trim(smooth), trim(ses)))
  return(resPred)
}

get_metrics = function(table){
  #' Calculates metrics based on any confusion Matrix

  metrics = vector("numeric", 7)
  names(metrics) = c("Sensitivity", "Specificity", "Precision", "NPV", "F1.score", "Inv.F1.score", "Accuracy")
  metrics[1] = unname(table[1,1]/colSums(table)[1]) # Sensitivity (Recall) = TP / (TP + FN)
  metrics[2] = unname(table[2,2]/colSums(table)[2]) # Specificity = TN / (TN + FP)
  metrics[3] = unname(table[1,1]/rowSums(table)[1]) # Precision (PPV) = TP / (TP + FP) same as PPV
  metrics[4] = unname(table[2,2]/rowSums(table)[2]) # NPV = TN / (TN + FN)
  metrics[5] = (2*(metrics[3]*metrics[1]))/(metrics[3]+metrics[1]) # F1 score: Harmonic mean of Recall, Precision
  metrics[6] = (2*(metrics[4]*metrics[2]))/(metrics[4] + metrics[2]) # Inv. F1 score: Harmonice mean of Specificity and NPV
  metrics[7] = unname((table[1,1] + table[2,2])/sum(table))
  return(metrics)
}

aggregate_tables = function(resPreds){
  #' Aggregates confusion Matrix and cut_off data_frame from a list of prediction objects
  table = matrix(c(0,0,0,0),2,2)
  
  test.session = data.frame(cut_off = vector("numeric", 6),
                            correct_med_pred = vector("integer", 6),
                            med_session = vector("numeric", 6))
  for(pred in resPreds){
    table = table + pred$test$Accuracy$table
    test.session = test.session + pred$test$Session 
  }
  
  test.session$cut_off = test.session$cut_off / length(resPreds)
  test.session$percent_correct =  test.session$correct_med_pred / test.session$med_session
  
  return(list(confMatrix = table, Session = test.session))
}