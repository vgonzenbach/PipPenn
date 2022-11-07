run_pca = function(dat, npc = 3)
{
  dat$Activitylabel<-match(dat$ActivityName,unique(dat$ActivityName))
  
  #convert time series and reorder data by time
  dat$timestamp<-substr(dat$activity_timestamp,12,23)
  dat$time<- as.numeric(lubridate::hms(dat$timestamp))
  dat <- dat[sort.int(dat$time, index.return = TRUE)$ix, ]
  dat$time<-dat$time-dat$time[1]
  
  ## Relabel names for wrist location
  dat$Wrist_Location <- ifelse(dat$Wrist_Location =="right     ","right","left")
  
  ## remove duplicate left and right wrist to create balance data
  dat$Wrist_Location2 <- ifelse(dat$Wrist_Location == "left", 0, 1)
  keep_idx <- c()
  diff_time <- diff(dat$time)
  diff_wrist <- diff(dat$Wrist_Location2)
  
  for (i in 1:length(diff_time)){
    if (diff_time[i] <= 0.01 & diff_wrist[i] != 0)
    {
      keep_idx = c(keep_idx, i, i + 1) 
    }
    i = i + 2
  }
  dat <- dat[keep_idx, ]

  ## extract 3 pcs for the left hand
  vars_left <- dat[dat$Wrist_Location=="left",
                         c("accel_x_coord","accel_y_coord","accel_z_coord",
                           "rotate_x_coord","rotate_y_coord", "rotate_z_coord",
                           "gravity_x_coord","gravity_z_coord","gravity_y_coord",
                           "magneto_x_coord","magneto_y_coord","magneto_z_coord",
                           "attitude_x_coord","attitude_y_coord","attitude_z_coord")]
  varsleft.pca <- prcomp(vars_left, center=TRUE, scale.=TRUE)
  #summary(varsleft.pca)
  pcs_left <- varsleft.pca$x[,1:npc]
  colnames(pcs_left) <- paste( colnames(pcs_left),"_left", sep ="")
  
  ## extract 3 pcs for the right hand
  vars_right <- dat[dat$Wrist_Location=="right",
                          c("accel_x_coord","accel_y_coord","accel_z_coord",
                            "rotate_x_coord","rotate_y_coord", "rotate_z_coord",
                            "gravity_x_coord","gravity_z_coord","gravity_y_coord",
                            "magneto_x_coord","magneto_y_coord","magneto_z_coord",
                            "attitude_x_coord","attitude_y_coord","attitude_z_coord")]
  varsright.pca <- prcomp(vars_right, center=TRUE, scale.=TRUE)
  #summary(varsright.pca)
  pcs_right <- varsright.pca$x[,1:npc]
  colnames(pcs_right) <- paste( colnames(pcs_right),"_right", sep ="")
  
  pcs_combined <- cbind(pcs_left,pcs_right)
  #colnames(pcs_combined) <- c("PC1_left","PC2_left","PC3_left","PC1_right","PC2_right","PC3_right")
  
  ## combine data with Pcs
  ActivityName <- dat$ActivityName[dat$Wrist_Location=="left"]
  movelet.dat <- data.frame(pcs_combined, ActivityName)
  movelet.dat$ActivityName <- gsub(" ", "_", movelet.dat$ActivityName)
  return(movelet.dat)
}

run_pca_dominant_hand = function(dat, dominant_hand = NULL)
{
  dat$Activitylabel<-match(dat$ActivityName,unique(dat$ActivityName))
  
  #convert time series and reorder data by time
  dat$timestamp<-substr(dat$activity_timestamp,12,23)
  dat$time<- as.numeric(lubridate::hms(dat$timestamp))
  dat <- dat[sort.int(dat$time, index.return = TRUE)$ix, ]
  dat$time<-dat$time-dat$time[1]
  
  ## Relabel names for wrist location
  dat$Wrist_Location <- ifelse(dat$Wrist_Location =="right     ","right","left")
  
  ## remove duplicate left and right wrist to create balance data
  dat$Wrist_Location2 <- ifelse(dat$Wrist_Location == "left", 0, 1)
  keep_idx <- c()
  diff_time <- diff(dat$time)
  diff_wrist <- diff(dat$Wrist_Location2)
  
  for (i in 1:length(diff_time)){
    if (diff_time[i] <= 0.01 & diff_wrist[i] != 0)
    {
      keep_idx = c(keep_idx, i, i + 1) 
    }
    i = i + 2
  }
  dat <- dat[keep_idx, ]
  
  if (dominant_hand == "left"){
  ## extract 6 pcs for the left hand
  vars_left <- dat[dat$Wrist_Location=="left",
                   c("accel_x_coord","accel_y_coord","accel_z_coord",
                     "rotate_x_coord","rotate_y_coord", "rotate_z_coord",
                     "gravity_x_coord","gravity_z_coord","gravity_y_coord",
                     "magneto_x_coord","magneto_y_coord","magneto_z_coord",
                     "attitude_x_coord","attitude_y_coord","attitude_z_coord")]
  varsleft.pca <- prcomp(vars_left, center=TRUE, scale.=TRUE)
  #summary(varsleft.pca)
  pcs <- varsleft.pca$x[,1:6]
  } else {
  ## extract 6 pcs for the right hand
  vars_right <- dat[dat$Wrist_Location=="right",
                    c("accel_x_coord","accel_y_coord","accel_z_coord",
                      "rotate_x_coord","rotate_y_coord", "rotate_z_coord",
                      "gravity_x_coord","gravity_z_coord","gravity_y_coord",
                      "magneto_x_coord","magneto_y_coord","magneto_z_coord",
                      "attitude_x_coord","attitude_y_coord","attitude_z_coord")]
  varsright.pca <- prcomp(vars_right, center=TRUE, scale.=TRUE)
  #summary(varsright.pca)
  pcs <- varsright.pca$x[,1:6]
  }
  #pcs_combined <- cbind(pcs_left,pcs_right)
  #colnames(pcs_combined) <- c("PC1_left","PC2_left","PC3_left","PC1_right","PC2_right","PC3_right")
  
  ## combine data with Pcs
  ActivityName <- dat$ActivityName[dat$Wrist_Location== dominant_hand]
  movelet.dat <- data.frame(pcs, ActivityName)
  movelet.dat$ActivityName <- gsub(" ", "_", movelet.dat$ActivityName)
  return(movelet.dat)
}

####### Modify movelet_dist to use more variables
## modify movelet packages to have more axes
Acceleration_Create2=function(Data,subjectName,frequency,n_axes,label=TRUE)
{
  if (label==FALSE)
  {Result=list(Subject=subjectName,N=nrow(Data),SampleRate=frequency,Data=Data[,c(1:n_axes)])}
  else {Result=list(Subject=subjectName,N=nrow(Data),SampleRate=frequency,Data=Data[,c(1:n_axes)],Label=Data[,n_axes+1])}
  class(Result)="AcceleData"
  return(Result)
}

Movelet_Create2=function(FROM, TO, DATA, ChapName, n_axes, Length=NULL)
{
  if (is.null(Length)==TRUE)
  {
    Length=DATA$SampleRate
  }
  # define dimension of the movelet collection (number of movelets per collection)
  Dim.Collection = TO-Length+1-FROM+1
  
  # make sure there the time between first and last is greater than Length
  if(Dim.Collection < 1){stop("Movelet length longer than time used")}
  
  # construct dictionary
  # Collection=list(AX1=array(0,c(Dim.Collection,Length)),
  #                 AX2=array(0,c(Dim.Collection,Length)),
  #                 AX3=array(0,c(Dim.Collection,Length)))
  #n_axes <- 6
  Collection <- list()
  for(i in 1:n_axes){
    name <- paste('AX',i,sep='')
    tmp <- array(0,c(Dim.Collection,Length))
    Collection[[name]] <- tmp
  }
  
  temp=c(1:(TO-FROM+1))
  IndexMatrix=c()
  for (t in (0:(Length-1)))
  {
    IndexMatrix=c(IndexMatrix,temp[(1+t):(length(temp)-(Length-t)+1)])
  }
  for (j in 1:n_axes)
  {
    Collection[[j]]=matrix(DATA$Data[IndexMatrix+FROM-1,j],ncol=Length)
  }
  Result=list(Subject=DATA$Subject,Chapter=ChapName,Length=Length,SampleRate=DATA$SampleRate,MvltNum=Dim.Collection,From=FROM, To=TO, Movelet=Collection,Data=DATA$Data[ifelse(FROM-50>0,FROM-50,FROM):ifelse(TO+50<nrow(DATA$Data),TO+50,TO),])
  class(Result)="MvltChap"
  return(Result)
}  

## modify movelet_dist
Movelet_Dist2=function(Chapter, Unlabeled, n_axes = NULL, index=FALSE)
{
  # Check if the class of "Chapter" is MvltChap
  if(class(Chapter)!="MvltChap"){stop("Please use appropriate object for Chapter")}
  # Check if the class of "Unlabeled" is AcceleData
  if(class(Unlabeled)!="AcceleData"){stop("Please use appropriate object for Unlabeled")}  
  N = Unlabeled$N				# length of unlabeled timeseries
  Length = Chapter$Length 	# length of movelets in chapter
  N_Chapter=Chapter$MvltNum	# number of chapter elements
  
  # decompose unlabeled timeseries into movelets
  Unlabeled_Movelets=Movelet_Create2(FROM=1, TO=N, DATA=Unlabeled, 
                                     Length=Length, n_axes = n_axes, ChapName="Unlabeled")
  
  N_Sample=Unlabeled_Movelets$MvltNum	# number of unlabeled movelets
  Distance=array(0,c(N_Sample,N_Chapter))
  
  if (n_axes == 6){
    for (i in 1:N_Chapter)
    {
      Dist1=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX1)-Chapter$Movelet$AX1[i,]))^2,1,sum))
      Dist2=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX2)-Chapter$Movelet$AX2[i,]))^2,1,sum))
      Dist3=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX3)-Chapter$Movelet$AX3[i,]))^2,1,sum))
      Dist4=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX4)-Chapter$Movelet$AX4[i,]))^2,1,sum))
      Dist5=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX5)-Chapter$Movelet$AX5[i,]))^2,1,sum))
      Dist6=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX6)-Chapter$Movelet$AX6[i,]))^2,1,sum))
      Dist=(Dist1+Dist2+Dist3+Dist4+Dist5+Dist6)/6
      Distance[,i]=Dist
    }
  } else if (n_axes == 8){
    for (i in 1:N_Chapter)
    {
      Dist1=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX1)-Chapter$Movelet$AX1[i,]))^2,1,sum))
      Dist2=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX2)-Chapter$Movelet$AX2[i,]))^2,1,sum))
      Dist3=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX3)-Chapter$Movelet$AX3[i,]))^2,1,sum))
      Dist4=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX4)-Chapter$Movelet$AX4[i,]))^2,1,sum))
      Dist5=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX5)-Chapter$Movelet$AX5[i,]))^2,1,sum))
      Dist6=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX6)-Chapter$Movelet$AX6[i,]))^2,1,sum))
      Dist7=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX7)-Chapter$Movelet$AX7[i,]))^2,1,sum))
      Dist8=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX8)-Chapter$Movelet$AX8[i,]))^2,1,sum))
      Dist=(Dist1+Dist2+Dist3+Dist4+Dist5+Dist6+Dist7+Dist8)/8
      Distance[,i]=Dist
    }
  } else {
    for (i in 1:N_Chapter)
    {
      Dist1=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX1)-Chapter$Movelet$AX1[i,]))^2,1,sum))
      Dist2=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX2)-Chapter$Movelet$AX2[i,]))^2,1,sum))
      Dist3=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX3)-Chapter$Movelet$AX3[i,]))^2,1,sum))
      Dist4=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX4)-Chapter$Movelet$AX4[i,]))^2,1,sum))
      Dist5=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX5)-Chapter$Movelet$AX5[i,]))^2,1,sum))
      Dist6=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX6)-Chapter$Movelet$AX6[i,]))^2,1,sum))
      Dist7=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX7)-Chapter$Movelet$AX7[i,]))^2,1,sum))
      Dist8=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX8)-Chapter$Movelet$AX8[i,]))^2,1,sum))
      Dist9=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX9)-Chapter$Movelet$AX9[i,]))^2,1,sum))
      Dist10=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX10)-Chapter$Movelet$AX10[i,]))^2,1,sum))
      Dist=(Dist1+Dist2+Dist3+Dist4+Dist5+Dist6+Dist7+Dist8+Dist9+Dist10)/10
      Distance[,i]=Dist
    }
  } #n_axes = 10
    
  Result=array(0,c(2,N_Sample))
  Result[1,]=apply(Distance,1,which.min)
  Result[2,]=apply(Distance,1,min)
  if (index==FALSE)
  {
    return(Result[2,])
  } else 
  {
    return(Result)
  }
}

Movelet_Pred2=function(x,Act.Names,vote=FALSE, n_axes = NULL)
{
  Ch.Names = paste("Ch.", Act.Names, ".",x$Subject,sep="")
  Distance = c()
  Length=get(Ch.Names[1])$Length
  for(i in 1:length(Act.Names))
  {
    Distance = cbind(Distance, Movelet_Dist2(get(Ch.Names[i]),x, n_axes = n_axes))
  }
  
  Distance=as.data.frame(Distance)
  colnames(Distance) = Act.Names
  
  ## estimate predicited activity as using the minimum distance
  Predicted.Labels=sapply(1:dim(Distance)[1], function(u) which.min(Distance[u,]))
  Result=Predicted.Labels
  ## Voting ##
  if (vote==TRUE)
  {
    for (i in 1:length(Predicted.Labels))
    {
      Start=max(1,i-Length+1)
      End=min(length(Predicted.Labels),i)
      Table=table(Predicted.Labels[Start:End])
      #       W=1
      #       Table[which(as.character(Predicted.Labels[Start+3])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+3])==names(table(Predicted.Labels[Start:End])))]+W
      #       Table[which(as.character(Predicted.Labels[Start+4])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+4])==names(table(Predicted.Labels[Start:End])))]+W
      #       Table[which(as.character(Predicted.Labels[Start+5])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+5])==names(table(Predicted.Labels[Start:End])))]+W
      #       Table[which(as.character(Predicted.Labels[Start+6])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+6])==names(table(Predicted.Labels[Start:End])))]+W
      Result[i]=as.numeric(names(which.max(Table)))
    }
  }
  return(list(Dist=Distance, Pred=Result))  
}

## Evaluate prediction
eval_prediction = function(test.movelet.dat, pred, train_control, test_control, smoothing = TRUE) 
{
  ## Predict number of med taking session!
  ## test.movelet.dat is the result of accel_create2 function
  ## pred is the result of the movelet_pred2 function
  ## test_control is the test activities that are not med-taking
  ## n_control_acts is the number of test activities that are not med-taking
  temp <- data.frame(cbind(test.movelet.dat$Label[1:length(pred$Pred2)], pred$Pred2, 
                           pred$Pred))
  colnames(temp) <- c("True","Pred","Pred_num")
  temp$Pred2 <- factor(ifelse(temp$Pred %in% train_control, "Not Med Taking","Med Taking"))
  temp$Pred2_num <- ifelse(temp$Pred2 =="Not Med Taking",2,1)
  true <- rle(as.character(temp$True))
  med_taking_start_idx <- cumsum(true$lengths)[(grep("Med_Taking",true$values)-1)] + 1
  #med_taking_start_idx <- c(med_taking_start_idx,cumsum(true$lengths)[length(true$lengths)])
  med_session <- length(med_taking_start_idx) 
  med_length <- true$lengths[grep("Med_Taking",true$values)]
  
  cut_off <- seq(0.50,0.9,0.05)
  correct_med_pred <- c()
  for(j in 1:length(cut_off)){
    ## define cut off
    cut_off_median <- median(cut_off[j]*med_length)
    pred_res <- c()
    for(i in 1:(length(med_taking_start_idx))){
      if(length(which(temp$Pred2[med_taking_start_idx[i]:(med_taking_start_idx[i] + med_length[i])] == "Med Taking")) > cut_off_median){
        pred_res <- c(pred_res,"Med-taking session")
      } else pred_res <- c(pred_res,"Not a full med-taking session")
    }
    correct_med_pred[j] <- length(which(pred_res == "Med-taking session"))
  }
  
  ## Accuracy
  if(smoothing){
    temp$Pred2_num <- round(tvR::denoise1(temp$Pred2_num, lambda = 35, method = "TVL2.MM"))    
  }
  temp2 <- data.frame(cbind(test.movelet.dat$Label[1:length(temp$Pred2_num)], temp$Pred2_num))
  temp2$True_label <- factor(ifelse(temp2$X1 %in% test_control, "Not Med Taking","Med Taking"))
  temp2$Pred_label <- factor(ifelse(temp2$X2 == 2, "Not Med Taking","Med Taking"))
  conf_mat <- confusionMatrix(data = temp2$Pred_label, reference = temp2$True_label)
  
  return(list(Session = data.frame(cut_off,correct_med_pred, med_session), Accuracy = conf_mat))
}

## wrapper 
run_movelet <- function(filename){
  dat <- readRDS(file = paste0("~/Movelets/Data/",filename))
  ## clean up data and perform pca
  pca.dat <- run_pca(dat)
  ## create movelet-ready data 
  movelet.dat <- Acceleration_Create2(pca.dat,subjectName= dat$PatientID[1],
                                      n_axes = 6,frequency=50) 
  ## train chapters
  train.label <- c(train.control, train.med)
  for(i in 1:length(train.label)){
    from <- min(which(movelet.dat$Label == train.label[i])) 
    to <- max(which(movelet.dat$Label == train.label[i])) 
    assign(paste0("Ch.",train.label[i],".",movelet.dat$Subject),
           Movelet_Create2(FROM = from ,TO = to, 
                           DATA = movelet.dat, n_axes = 6,
                           ChapName = train.label[i]), envir = .GlobalEnv)
  }
  ## test data
  test.dat <- subset(pca.dat, ActivityName %in% c(test.control,test.med))
  test.movelet.dat <- Acceleration_Create2(test.dat,subjectName=dat$PatientID[1],n_axes = 6,frequency=50)
  #expand the prediction activity labels to include all trained chapters
  pred <- Movelet_Pred2(test.movelet.dat, Act.Names=train.label, vote=TRUE)
  pred$Pred2 <- recode(pred$Pred, `1`=train.label[1], `2`=train.label[2], `3`=train.label[3], `4`=train.label[4],`5`=train.label[5])
  ## Evaluate prediction
  pred$test <- eval_prediction(test.movelet.dat, pred, n_control_acts = 3)
  # Save it
  saveRDS(pred, file = paste0("Results_",filename,".rds"))
  print(paste0(filename,"done"))
  return(pred)
}



