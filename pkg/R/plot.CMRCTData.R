plot.CMRCTData <-
function(x, main="capture histories", ...){
    cmrdata <- x
    plotoccasions(cmrdata$captocc, occonly=FALSE, bufpop=0.03, main=main, ...)
    capts <- cmrdata$capt
    capts$pop2 <- as.numeric(capts$pop)
    cmrdata$capt$pop2 <- as.numeric(cmrdata$capt$pop)
    nc <- tapply(cmrdata$capt$date, cmrdata$capt$ind, length)
    cmrdata$capt$nc <- nc[match(cmrdata$capt$ind, names(nc))]
    nn <- tapply(cmrdata$capt$ind, cmrdata$capt$pop, function(x) length(unique(x)))
    nmax <- max(nn)
    spa <- (1- 0.1) / nmax
    lcapt <- split(cmrdata$capt, cmrdata$capt$pop)
    invisible(lapply(lcapt, function(x){
        x <- x[order(x$date, x$nc),]
        #ids <- names(sort(tapply(x$date, x$ind, min)))
        ids <- unique(x$ind)
        for(i in 1:length(ids)){
            tab <- x[x$ind==ids[i],]
            xx <- unique(x$pop2)
            yy <- xx - 0.03 - (i * spa)
            segments(x0=min(tab$date), x1=max(tab$date), y0=yy, y1=yy, col="white")
            points(tab$date, rep(yy, nrow(tab)), pch=19, cex=0.03)
        }

    }))

}
