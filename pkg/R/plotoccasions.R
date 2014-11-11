plotoccasions <-
function(captocc, bufpop=0.4, occonly=TRUE, main="", ...){
  yylim0 <- 0.1
  if(occonly){
    yylim0 <- 1 - bufpop
  }
  bufpop1 <- 1-bufpop
  captocc$pop2 <- as.numeric(captocc$pop)
  npop <- max(captocc$pop2)
  deb <- min(captocc$start)
  fin <- max(captocc$end)
  plot(captocc$start[1],-1, ylim=c(yylim0, max(captocc$pop2)+bufpop), xlim=c(min(captocc$start), max(captocc$end)),
  axes=FALSE,  ylab="", xlab="time", xaxs="i", yaxs="i", main=main, ...)
  rect(par("usr")[1], par("usr")[3],par("usr")[2],par("usr")[4], col=grey(0.8))
  ats <- axis.POSIXct(1, x=captocc$start)
  axis(2, at=1:nlevels(captocc$pop), labels=paste("pop", 1:nlevels(captocc$pop)), las=2)
  abline(v=ats, lty=2)
  box()

  for(i in unique(captocc$pop2)){
    ll <- i
    occ <- captocc[captocc$pop2==i,]
    polygon(x=c(deb, deb, fin, fin), y=c(ll-bufpop, ll+bufpop, ll+bufpop, ll-bufpop), col=grey(0.4), border=NA)
    for(j in 1:nrow(occ)){
      deb0 <- occ$start[j]
      end0 <- occ$end[j]
      polygon(x=c(deb0, deb0, end0, end0), y=c(ll-bufpop, ll+bufpop, ll+bufpop, ll-bufpop), col="yellow", border=NA)
    }

  }
}
