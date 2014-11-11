plot.CMRCTVars <-
function(x, main="capture occasions", ...){
    cmrvars <- x
    plotoccasions(cmrvars$captocc, occonly=TRUE, bufpop=0.4, main=main, ...)
  }
