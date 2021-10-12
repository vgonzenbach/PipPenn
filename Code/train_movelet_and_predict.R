# Train and run prediction on patient

setwd(rprojroot::find_rstudio_root_file()) 
source("Code/functions.R")
library(dplyr)
library(caret)

args = commandArgs(trailingOnly = TRUE)
data.path = args[1]
dat <- readRDS(data.path)

## clean up data and perform pca
pca.dat <- run_pca(dat)

splitWalk = function(df){
  # Rename Activity 
  ends = range(which(df$ActivityName == "Walk"))
  mid = round(diff(ends)/2)
  df$ActivityName[ends[1]:mid] = "Walk_train"
  df$ActivityName[(mid + 1):ends[2]] = "Walk_test"
  return(df)
}

pca.dat = splitWalk(pca.dat)

med.ses = args[2]
# turn digit into label
label.ses = function(med.ses){
  # split 
  sessions = unlist(strsplit(med.ses, ","))
  labels = unique(pca.dat$ActivityName)
  
  ses.label = labels[sapply(sessions, function(x) grep(paste0("^", x), labels))]
  return(ses.label)
}

## train chapters
train.label =  c(label.ses(med.ses), "Walk_train", "Wash_Hands","Look_around_in_bag_or_purse")

movelet.dat <- Acceleration_Create2(pca.dat,subjectName= dat$PatientID[1],
                                    n_axes = 6,frequency=50) # 50 obs per second

for(i in 1:length(train.label)){
  from <- min(which(movelet.dat$Label == train.label[i])) 
  to <- max(which(movelet.dat$Label == train.label[i])) 
  assign(paste0("Ch.",train.label[i],".",movelet.dat$Subject), Movelet_Create2(FROM = from ,TO = to, 
                                                                               DATA = movelet.dat, n_axes = 6,
                                                                               ChapName = train.label[i]))
}

## create movelet-ready data 
test.dat <- subset(pca.dat, ActivityName %in% c("Walk_test", "Turn_Key","Open_Door","Eat_From_Bag",
                                                "4._Med_Taking","7._Med_Taking", "10._Med_Taking", "13._Med_Taking"))

test.movelet.dat <- Acceleration_Create2(test.dat,subjectName=dat$PatientID[1],n_axes = 6,frequency=50) 

#expand the prediction activity labels to include all trained chapters
pred <- Movelet_Pred2(test.movelet.dat, Act.Names=train.label, vote=TRUE)
pred$Pred2 <- recode(pred$Pred, `1`=train.label[1], `2`=train.label[2],`3`=train.label[3],`4`=train.label[4], `5`=train.label[5])

## Evaluate prediction
pred$test <- eval_prediction(test.movelet.dat, pred, n_control_acts = 4)

# Save it
saveRDS(pred, file = sprintf("Results/%s_pred_ses-%s.rds", tools::file_path_sans_ext(basename(data.path)), med.ses))

