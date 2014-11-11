CMRSimData <-
function(simvars, surv, capt, theta){

    if(!inherits(simvars,"CMRCTVars")) stop("simvars must be an object of calss CMRCTVars")
    cmrdata <- CMRData(indvar=simvars$indvar, sptvar=simvars$sptvar, captocc=simvars$captocc,
                       tps=simvars$tps, unit=simvars$step, clage=simvars$clage)

    ## require(Matrix)
    if(!inherits(surv,"formula")) stop("surv must be a formula")
    if(!inherits(capt,"formula")) stop("capt must be a formula")

    matsurv <- matcstr(surv, cmrdata)
    matcapt <- matcstr(capt, cmrdata)
    unind <- unique(cmrdata$capt$ind)
    dl <- cmrdata$covs
    varind <- cmrdata$capt
    varsessions <- cmrdata$captocc

    matlist <- formatmats2(matsurv, matcapt, unind, cmrdata$covs, cmrdata$capt, cmrdata$captocc)

    nsurv <- nrow(matsurv)
    ncapt <- nrow(matcapt)

    thetasurv <- theta[1:nsurv]
    thetacapt <- theta[(nsurv+1):(nsurv+ncapt)]

    mclsap   <- matlist[[1]]
    mclcap   <- matlist[[2]]

    ltvec <- matlist[[5]]

    insessap <- insess2(ltvec, varind, varsessions)
    facinsess <- unlist(lapply(1:length(ltvec), function(x) rep(x, length(ltvec[[x]])-1)))
    insessap <- split(insessap, facinsess)
    names(insessap) <- names(ltvec)

    pos <- which(unlist(lapply(ltvec, length)) < 3)
    pos2 <- which(unlist(lapply(ltvec, length)) > 2)

    his <- lapply(1:length(mclsap[pos2]), function(x){
        lamsap <- exp(mclsap[pos2][[x]] %*% thetasurv)
         intmat <- mclcap[pos2][[x]] %*% thetacapt
         intmat[intmat>100] <- 100
         lamcap <- exp(intmat) * insessap[pos2][[x]]
         his <- simcapt(lamsap, lamcap, ltvec[pos2][[x]])
         return(his)
    })
    names(his) <- names(pos2)
    his <- c(his, ltvec[pos])

    capts <- do.call("rbind", lapply(1:length(his), function(x) data.frame(ind=names(his)[x], datenum=his[[x]])))
    capts <- data.frame(capts, simvars$ind[match(capts$ind, simvars$indvar$ind), !colnames(simvars$ind) %in% c("ind", "date", "datenum")])
    capts$date <- (capts$datenum * simvars$unitnum) + min(simvars$captocc$start)
    capts <- capts[,colnames(capts)!="datenum"]
    capts <- capts[order(id2num(capts$ind), capts$date),]
    out <- CMRData(indvar=capts, sptvar=simvars$sptvar, captocc=simvars$captocc, clage=simvars$clage,
                   tps=simvars$tps, unit=simvars$step)

    sortedout <- lapply(out, function(x){
        if(inherits(x, "list")){
            int <- lapply(x, function(y){
                if(all(c("ind", "pop") %in% colnames(y))){
                    return(y[order(y$pop, id2num(y$ind), y$datenum),])
                }
                if(all(c("pop", "startnum") %in% colnames(y))){
                    return(y[order(y$pop, y$startnum),])
                }
                if(all(c("pop", "datenum") %in% colnames(y))){
                    return(y[order(y$pop, y$datenum),])
                }
                if(all(c("ind") %in% colnames(y))){
                    return(y[order(id2num(y$ind), y$datenum),])
                }
                return(y[order(y$datenum),])
            })
            return(int)
        }
        else{
            if(all(c("ind", "pop") %in% colnames(x))){
                return(x[order(x$pop, id2num(x$ind), x$datenum),])
            }
            if(all(c("pop", "startnum") %in% colnames(x))){
                return(x[order(x$pop, x$startnum),])
            }
            if(all(c("pop", "datenum") %in% colnames(x))){
                return(x[order(x$pop, x$datenum),])
            }
            if(all(c("ind") %in% colnames(x))){
                return(x[order(id2num(x$ind), x$datenum),])
            }
            return(x[order(x$datenum),])
        }
    })
    attributes(sortedout) <- attributes(out)
    class(sortedout) <- class(out)
    return(sortedout)
}
