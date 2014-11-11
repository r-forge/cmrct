print.CMRCTData <-
function(x, ...){
  cmrctdata <- x
  cat("\n\nIndividuals :\n=============\n\n")
  print(attributes(cmrctdata)$indsum)

  cat("\n\nCo_Variables :\n=============\n\n")
  tab2 <-lapply(attributes(cmrctdata)$varlist, class)
  print(data.frame(var=names(tab2), type=unlist(tab2), row.names=NULL))

  cat("\n\nCaptures :\n==========\n\n")
  if(nrow(cmrctdata$capt) > 15){
    AA <- as.matrix(rbind(head(cmrctdata$capt, 7), tail(cmrctdata$capt)))
    AA[7,] <- ""
    rownames(AA)[7] <- "..."
    print(AA[,1:3], quote=FALSE)
  }
  else{
    print(cmrctdata$capt)
  }

  cat("\n\nCapture_Occasions :\n===================\n\n")
  if(nrow(cmrctdata$captocc) > 15){
    AA <- as.matrix(rbind(head(cmrctdata$captocc, 7), tail(cmrctdata$captocc)))
    AA[7,] <- ""
    rownames(AA)[7] <- "..."
    print(AA[,1:3], quote=FALSE)
  }
  else{
    print(cmrctdata$captocc)
  }

}
