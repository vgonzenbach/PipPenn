# Train and run prediction on patient

setwd(rprojroot::find_rstudio_root_file()) 
source("Code/functions.R")
library(dplyr) |> suppressMessages()
library(caret) |> suppressMessages()
library(tvR) |> suppressMessages()
library(argparser) |> suppressMessages()

options(error = quote({dump.frames(to.file = TRUE); q(status = 1)}))
# Read in argument
p <- arg_parser("Run Movelet Prediction for Apple Watch Medication-Taking", hide.opts = FALSE)
p <- add_argument(p, "data", help = "Subject .rds file")
p <- add_argument(p, "--ses", help = "All Medication-Taking session to train on separated by commas")
p <- add_argument(p, "--smooth", flag=TRUE, help='Option to apply smoothing')
p <- add_argument(p, "--hand", default='bothhands', help='Choose `dominanthand`, `nondominanthand` or `bothhands` (default)')
p <- add_argument(p, "--npc", default=3, help="N of PCs to include for each hand")
p <- add_argument(p, "--force", flag=TRUE, help='Option to force re-run if output file is found. Otherwise')

argv <- parse_args(p)

data.path <- argv$data
med.ses <- argv$ses
hand <- argv$hand
smoothing <- argv$smooth
npc <- argv$npc
force <- argv$force

outfile = sprintf("Results/%s_pred_pca-%s_npc-%s_smooth-%s_ses-%s.rds", 
                  tools::file_path_sans_ext(basename(data.path)), 
                  hand,
                  ifelse(hand %in% c('nondominant', 'dominanthand') , 6, npc),
                  smoothing,
                  med.ses)

if(file.exists(outfile) & !force) stop(sprintf("Output %s found. Stopping computation. Use `--force` to overwrite.", outfile))

## clean up data and perform pca
dat <- readRDS(data.path)

# assign 
if (hand == 'dominanthand'){
  # patient 19 is left-handed
  if (!grepl("19", data.path)){
    message('dominant: right')
    pca.dat <- run_pca_dominant_hand(dat, dominant_hand="right")
  } else if (grepl("19", data.path)) {
    message('dominant: left')
    pca.dat <- run_pca_dominant_hand(dat, dominant_hand="left")
  }
} else if (hand == 'nondominanthand'){
  # patient 19 is left-handed
  if (!grepl("19", data.path)){
    message('nondominant: left')
    # just switch which hand we call 'dominant'
    pca.dat <- run_pca_dominant_hand(dat, dominant_hand="left") 
  } else if (grepl("19", data.path)) {
    message('nondominant: right')
    pca.dat <- run_pca_dominant_hand(dat, dominant_hand="right")
  }
} else if (hand == 'bothhands') {
  pca.dat <- run_pca(dat, npc=npc)
}

n_axes <- ifelse(hand %in% c('dominanthand', 'nondominanthand'), npc, npc*2)

splitWalk = function(df){
  # Rename Activity 
  ends = range(which(df$ActivityName == "Walk"))
  mid = round(diff(ends)/2)
  df$ActivityName[ends[1]:mid] = "Walk_train"
  df$ActivityName[(mid + 1):ends[2]] = "Walk_test"
  return(df)
}

pca.dat = splitWalk(pca.dat)

# turn digit into label
label.ses = function(med.ses){
  # split 
  sessions = unlist(strsplit(med.ses, ","))
  labels = unique(pca.dat$ActivityName)
  
  ses.label = labels[sapply(sessions, function(x) grep(paste0("^", x), labels))]
  return(ses.label)
}

## train chapters
train_control = c("Walk_train", "Wash_Hands","Look_around_in_bag_or_purse")
train.label =  c(label.ses(med.ses), train_control)

message("Preparing Training data...")
system.time(movelet.dat <- Acceleration_Create2(pca.dat,subjectName= dat$PatientID[1],
                                    n_axes = n_axes,frequency=50)) # 50 obs per second

for(i in 1:length(train.label)){
  from <- min(which(movelet.dat$Label == train.label[i])) 
  to <- max(which(movelet.dat$Label == train.label[i])) 
  assign(paste0("Ch.",train.label[i],".",movelet.dat$Subject), Movelet_Create2(FROM = from ,TO = to, 
                                                                               DATA = movelet.dat, n_axes = n_axes,
                                                                               ChapName = train.label[i]))
}

## create movelet-ready data 
test_control = c("Walk_test", "Turn_Key","Open_Door","Eat_From_Bag")
test.dat <- subset(pca.dat, ActivityName %in% c(test_control,
                                                "4._Med_Taking","7._Med_Taking", "10._Med_Taking", "13._Med_Taking"))

test.movelet.dat <- Acceleration_Create2(test.dat,subjectName=dat$PatientID[1],n_axes = n_axes,frequency=50) 

#expand the prediction activity labels to include all trained chapters
message("Running prediction...")
system.time(pred <- Movelet_Pred2(test.movelet.dat, Act.Names=train.label, n_axes=n_axes, vote=TRUE))
pred$Pred2 <- train.label[pred$Pred]

## Evaluate prediction
message("Evaluating prediction...")
system.time(pred$test <- eval_prediction(test.movelet.dat, pred, train_control, test_control, smoothing = smoothing))

# Save it
saveRDS(pred, file = outfile)
message("Prediction saved.")
