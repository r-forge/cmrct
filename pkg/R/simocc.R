simocc <-
function(start, end, length){
    sn <- as.numeric(start)
    en <- as.numeric(seq(end,by=paste("-",length, sep=""), len=2)[2]-1)
    deb <- as.POSIXct(runif(1, sn, en), origin="1970-01-01")
    fin <- seq(deb, by=length, len=2)[2]
    return(c(start=deb, end=fin))
}
