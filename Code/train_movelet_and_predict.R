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

mode = args[2]

if(mode == "1_ses_train"){
  train.label <- c("6._Med_Taking",
                   #"9._Med_Taking",
                   #"12._Med_Taking",
                   "Wash_Hands",
                   "Look_around_in_bag_or_purse")
  
  test.dat <- subset(pca.dat, ActivityName %in% c("Turn_Key","Open_Door","Eat_From_Bag","4._Med_Taking","5._Med_Taking","7._Med_Taking","8._Med_Taking",
                                                  "10._Med_Taking","11._Med_Taking", "13._Med_Taking", "14._Med_Taking", "15._Med_Taking"))
}else if(mode == "5_ses_train"){
  train.label <- c("3._Patient_Choice",
                   "6._Med_Taking",
                   "9._Med_Taking",
                   "12._Med_Taking",
                   "15._Med_Taking",
                   "Walk",
                   "Wash_Hands",
                   "Look_around_in_bag_or_purse")
  
  test.dat <- subset(pca.dat, ActivityName %in% c("Turn_Key","Open_Door","Eat_From_Bag","4._Med_Taking","5._Med_Taking","7._Med_Taking","8._Med_Taking",
                                                  "10._Med_Taking","11._Med_Taking", "13._Med_Taking", "14._Med_Taking"))
  
} else if(mode == "4_ses_train"){
  train.label <- c("3._Patient_Choice",
                   "6._Med_Taking",
                   "9._Med_Taking",
                   "12._Med_Taking",
                   "Walk",
                   "Wash_Hands",
                   "Look_around_in_bag_or_purse")
  
  test.dat <- subset(pca.dat, ActivityName %in% c("Turn_Key","Open_Door","Eat_From_Bag","4._Med_Taking","5._Med_Taking","7._Med_Taking","8._Med_Taking",
                                                  "10._Med_Taking","11._Med_Taking", "13._Med_Taking", "14._Med_Taking", "15._Med_Taking"))
} else if(mode == "3_ses_train"){
  train.label <- c("6._Med_Taking",
                   "9._Med_Taking",
                   "12._Med_Taking",
                   "Walk",
                   "Wash_Hands",
                   "Look_around_in_bag_or_purse")
  
  test.dat <- subset(pca.dat, ActivityName %in% c("Turn_Key","Open_Door","Eat_From_Bag","4._Med_Taking","5._Med_Taking","7._Med_Taking","8._Med_Taking",
                                                  "10._Med_Taking","11._Med_Taking", "13._Med_Taking", "14._Med_Taking", "15._Med_Taking"))
}

## create movelet-ready data 
movelet.dat <- Acceleration_Create2(pca.dat,subjectName= dat$PatientID[1],
                                    n_axes = 6,frequency=50) # 50 obs per second
## train chapters
for(i in 1:length(train.label)){
  from <- min(which(movelet.dat$Label == train.label[i])) 
  to <- max(which(movelet.dat$Label == train.label[i])) 
  assign(paste0("Ch.",train.label[i],".",movelet.dat$Subject), Movelet_Create2(FROM = from ,TO = to, 
                                                                               DATA = movelet.dat, n_axes = 6,
                                                                               ChapName = train.label[i]))
}

## test data
test.dat <- subset(pca.dat, ActivityName %in% c("Turn_Key","Open_Door","Eat_From_Bag","4._Med_Taking","5._Med_Taking","7._Med_Taking","8._Med_Taking",
                                                "10._Med_Taking","11._Med_Taking"))
test.movelet.dat <- Acceleration_Create2(test.dat,subjectName=dat$PatientID[1],n_axes = 6,frequency=50) 

#expand the prediction activity labels to include all trained chapters
pred <- Movelet_Pred2(test.movelet.dat, Act.Names=train.label, vote=TRUE)
pred$Pred2 <- recode(pred$Pred, `1`=train.label[1], `2`=train.label[2],`3`=train.label[3],`4`=train.label[4], `5`=train.label[5])

## Evaluate prediction
pred$test <- eval_prediction(test.movelet.dat, pred, n_control_acts = 3)

# Save it
saveRDS(pred, file = sprintf("Results/%s_pred_%s.rds", tools::file_path_sans_ext(basename(data.path)), mode))

