print.CMRCTVars <-
function(x, ...){
  cmrctvars <- x
  cat("\n\nIndividuals :\n=============\n\n")

  ft <- cmrctvars$indvar
  ft <- ft[,colnames(ft)[!colnames(ft) %in% c("ind","date")], drop=FALSE]
  if(!is.null(cmrctvars$clage)){
    ft$age <- as.factor(findInterval(ft$age, cmrctvars$clage))
  }
  pos.fac <- which(unlist(lapply(ft, class) %in% c("factor", "character")))
  print(ftable(ft[,pos.fac]))

  cat("\n\nCo_Variables :\n=============\n\n")
  if(is.null(cmrctvars$sptvar)){
    print("No temporal or spatio-temporal covariates.")
  }
  else{
    print(paste(names(cmrctvars$sptvar), collapse=", "))
  }

  cat("\n\nCapture_Occasions :\n===================\n\n")
  if(nrow(cmrctvars$captocc) > 15){
    AA <- as.matrix(rbind(head(cmrctvars$captocc, 7), tail(cmrctvars$captocc)))
    AA[7,] <- ""
    rownames(AA)[7] <- "..."
    print(AA[,1:3], quote=FALSE)
  }
  else{
    print(cmrctvars$captocc)
  }

}
