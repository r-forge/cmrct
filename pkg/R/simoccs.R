simoccs <-
function(idpops, limit, length){

    pop <- c()
    start <- c()
    end <- c()
    if(seq(limit[1], by=length, len=2)[2] >= limit[2]) stop("Use a shorter length")

    for(i in 1:length(idpops)){
        for(j in 2:length(limit)){
            sim <- simocc(start=limit[j-1], end=limit[j], length=length)
            pop <- c(pop, idpops[i])
            start <-  c(start, sim[1])
            end <- c(end, sim[2])
        }
    }

    start <- as.POSIXct(start, origin="1970-01-01")
    end <- as.POSIXct(end, origin="1970-01-01")
    start[1] <- limit[1]
    end[1] <- seq(limit[1], by=length, len=2)[2]
    start[length(start)] <- seq(limit[length(limit)], by=paste("-",length, sep=""), len=2)[2]
    end[length(end)] <- limit[length(limit)]-1
    return(data.frame(pop, start, end))
}
