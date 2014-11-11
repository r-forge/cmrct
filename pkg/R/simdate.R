simdate <-
function(captocc, idpops){

    tab <- data.frame(pop=idpops, id=1:length(idpops))
    ltab <- split(tab, tab$pop)

    res <- lapply(ltab, function(x){
        n <- nrow(x)
        occ <- 1
        tabocc <- captocc[captocc$pop==unique(x$pop),]
        if(n>1) occ <- c(occ, sample(1:nrow(tabocc), n-1, replace=TRUE))
        tabocc <- tabocc[occ,]
        dates <- do.call("c", lapply(split(tabocc, 1:nrow(tabocc)), function(x){
            as.POSIXct(runif(1, as.numeric(x[,2]), as.numeric(x[,3])), origin="1970-01-01")
        }))
        return(data.frame(x, date=dates))
    })

    res <- do.call("rbind", res)
    res <- res[order(res$id),]

    return(res$date)

}
