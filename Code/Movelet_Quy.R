## Author: Quy Cao
## Modifies Movelet functions

library(Movelet)
####### Modify movelet_dist to use more variables
## modify movelet packages to have more axes
Acceleration_Create2 = function(Data, subjectName, frequency, n_axes, label=TRUE){
  if (label==FALSE){
    Result = list(Subject=subjectName, N=nrow(Data), SampleRate=frequency, Data=Data[,c(1:n_axes)])
  } else {
    Result = list(Subject=subjectName, N=nrow(Data), SampleRate=frequency, Data=Data[,c(1:n_axes)], Label=Data[,n_axes+1])
  }
  class(Result)="AcceleData"
  return(Result)
}

Movelet_Create2 = function(FROM, TO, DATA, ChapName, n_axes, Length=NULL){
  if (is.null(Length)==TRUE){
    Length=DATA$SampleRate
  }
  # define dimension of the movelet collection (number of movelets per collection)
  Dim.Collection = TO-Length+1-FROM+1
  
  # make sure there the time between first and last is greater than Length
  if(Dim.Collection < 1) stop("Movelet length longer than time used")
  
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
  
  temp = c(1:(TO-FROM+1))
  IndexMatrix = c()
  for (t in 0:(Length-1)){
    IndexMatrix=c(IndexMatrix,temp[(1+t):(length(temp)-(Length-t)+1)])
  }
  for (j in 1:n_axes){
    Collection[[j]] = matrix(DATA$Data[IndexMatrix+FROM-1, j], ncol=Length)
  }
  Result = list(Subject=DATA$Subject, Chapter=ChapName, Length=Length, SampleRate=DATA$SampleRate, 
                MvltNum=Dim.Collection, From=FROM, To=TO, Movelet=Collection, Data=DATA$Data[ifelse(FROM-50>0,FROM-50,FROM):ifelse(TO+50<nrow(DATA$Data),TO+50,TO),])
  class(Result)="MvltChap"
  return(Result)
}  
## modify movelet_dist
Movelet_Dist2=function(Chapter, Unlabeled, index=FALSE){
  # Check if the class of "Chapter" is MvltChap
  if(class(Chapter) != "MvltChap") stop("Please use appropriate object for Chapter")
  # Check if the class of "Unlabeled" is AcceleData
  if(class(Unlabeled) != "AcceleData") stop("Please use appropriate object for Unlabeled") 
  N = Unlabeled$N				# length of unlabeled timeseries
  Length = Chapter$Length 	# length of movelets in chapter
  N_Chapter=Chapter$MvltNum	# number of chapter elements
  
  # decompose unlabeled timeseries into movelets
  Unlabeled_Movelets = Movelet_Create2(FROM=1, TO=N, DATA=Unlabeled, 
                                       Length=Length, n_axes=6, ChapName="Unlabeled")
  
  N_Sample = Unlabeled_Movelets$MvltNum	# number of unlabeled movelets
  Distance = array(0, c(N_Sample,N_Chapter))
  for (i in 1:N_Chapter){
    Dist1=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX1)-Chapter$Movelet$AX1[i,]))^2,1,sum))
    Dist2=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX2)-Chapter$Movelet$AX2[i,]))^2,1,sum))
    Dist3=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX3)-Chapter$Movelet$AX3[i,]))^2,1,sum))
    Dist4=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX4)-Chapter$Movelet$AX4[i,]))^2,1,sum))
    Dist5=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX5)-Chapter$Movelet$AX5[i,]))^2,1,sum))
    Dist6=sqrt(apply((t(t(Unlabeled_Movelets$Movelet$AX6)-Chapter$Movelet$AX6[i,]))^2,1,sum))
    Dist=(Dist1+Dist2+Dist3+Dist4+Dist5+Dist6)/6
    Distance[,i]=Dist
  }
  
  Result=array(0, c(2,N_Sample))
  Result[1,] = apply(Distance, 1, which.min)
  Result[2,] = apply(Distance, 1, min)
  
  if (index==FALSE){
    return(Result[2,])
  } else {
    return(Result)
  }
}

Movelet_Pred2=function(x, Act.Names, vote=FALSE){
  Ch.Names = paste("Ch.", Act.Names, ".", x$Subject,sep="")
  Distance = c()
  Length= get(Ch.Names[1])$Length
  for(i in 1:length(Act.Names)){
    Distance = cbind(Distance, Movelet_Dist2(get(Ch.Names[i]),x))
  }
  
  Distance = as.data.frame(Distance)
  colnames(Distance) = Act.Names
  
  ## estimate predicted activity as using the minimum distance
  Predicted.Labels = sapply(1:dim(Distance)[1], function(u) which.min(Distance[u,]))
  Result = Predicted.Labels
  ## Voting ##
  if (vote==TRUE){
    for (i in 1:length(Predicted.Labels)){
      Start = max(1,i-Length+1)
      End = min(length(Predicted.Labels),i)
      Table = table(Predicted.Labels[Start:End])
      #       W=1
      #       Table[which(as.character(Predicted.Labels[Start+3])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+3])==names(table(Predicted.Labels[Start:End])))]+W
      #       Table[which(as.character(Predicted.Labels[Start+4])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+4])==names(table(Predicted.Labels[Start:End])))]+W
      #       Table[which(as.character(Predicted.Labels[Start+5])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+5])==names(table(Predicted.Labels[Start:End])))]+W
      #       Table[which(as.character(Predicted.Labels[Start+6])==names(table(Predicted.Labels[Start:End])))]=Table[which(as.character(Predicted.Labels[Start+6])==names(table(Predicted.Labels[Start:End])))]+W
      Result[i] = as.numeric(names(which.max(Table)))
    }
  }
  return(list(Dist=Distance, Pred=Result))  
}