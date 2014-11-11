CMRSimVars <-
function(n=10, npop=4, start, step="6 months", nstep=24, length="5 weeks", factind=NULL,
                       numind=NULL, numtps=NULL, numspts=NULL, step.spts="month",
                       clage=NULL, minage=NULL, maxage=NULL){

    ##Verifications des arguments
    check.class(n, "numeric")
    check.class(npop, "numeric")
    check.class(start, "POSIXct")
    if(length(start) > 1) stop("start must be a single date.")
    check.class(nstep, "numeric")
    if(length(nstep) > 1) stop("nstep must be a single value.")
    if(!is.null(clage)){
        if(is.null(minage)) stop("minage must be provided.")
        if(is.null(maxage)) stop("maxage must be provided.")
        check.class(minage, "numeric")
        if(length(minage) > 1) stop("minage must be a single value.")
        check.class(maxage, "numeric")
        if(length(maxage) > 1) stop("maxage must be a single value.")
    }
    if(!is.null(factind)) {
        check.class(factind, "list")
        if(any(names(factind)=="")) stop("factind must be a named list.")
        factind <- lapply(factind, as.character)
    }
    if(!is.null(numind)){
        check.class(numind, "list")
        if(any(names(numind)=="")) stop("numind must be a named list.")
        lapply(1:length(numind), function(x){
            if(!inherits(numind[[x]], "numeric")) stop(names(numind)[x], " in numind must be numeric.")
            if(length(numind[[x]])!=2) stop(names(numind)[x], " in numind must be of length 2.")
        })
    }
    if(!is.null(numtps)){
        check.class(numtps, "list")
        if(any(names(numtps)=="")) stop("numtps must be a named list.")
        lapply(1:length(numtps), function(x){
            if(!inherits(numtps[[x]], "numeric")) stop(names(numtps)[x], " in numtps must be numeric.")
            if(length(numtps[[x]])!=2) stop(names(numtps)[x], " in numtps must be of length 2.")
        })
    }
    if(!is.null(numspts)){
        check.class(numspts, "list")
        if(any(names(numspts)=="")) stop("numspts must be a named list.")
        lapply(1:length(numspts), function(x){
            if(!inherits(numspts[[x]], "numeric")) stop(names(numspts)[x], " in numspts must be numeric.")
            if(length(numspts[[x]])!=2) stop(names(numspts)[x], " in numspts must be of length 2.")
        })
    }

    check.unit(step)
    check.unit(length)
    check.unit(step.spts)
    if(n < npop) stop("n must be greather than npop")

    ##Valeurs par defaut
    tps <- list(deb=start, step=step)

    ##Mise en liste des pops et des classes d'age
    lpops <- paste("pop", 1:npop, sep="")
    facs <- c(pop=list(lpops))
    if(!is.null(clage)){
        facs <- c(facs, list(age=c(1:(length(clage)+1))))
    }
    if(!is.null(factind)){
        facs <- c(facs, factind)
    }

    ## Le temps
    time <- seq(tps$deb, by=tps$step, len=nstep + 1)
    if(length(time)==1) time <- seq(tps$deb, by=tps$step, len=2)
    end <- time[length(time)]

    ####################
    ## On cree captocc #
    ####################

    captocc <- simoccs(idpops=lpops, limit=time, length=length)

    ###################
    ## On cree indvar #
    ###################

    indtab <- as.data.frame(expand.grid(facs))
    indtab <- indtab[sort(rep(1:nrow(indtab), n)),, drop=FALSE]
    indtab$date <- simdate(captocc, indtab$pop)
    indtab$ind <- paste("ind", 1:nrow(indtab), sep="")
    if(!is.null(numind)){
        for(i in 1:length(numind)) indtab[,names(numind)[i]] <- runif(nrow(indtab), numind[[i]][1], numind[[i]][2])
    }
    indtab$ind <- as.factor(indtab$ind)
    indtab$pop <- as.factor(indtab$pop)
    if(!is.null(clage)){
        cls <- c(minage, clage, maxage)
        ncls <- length(cls) - 1
        indtab$age <- runif(nrow(indtab), cls[1:ncls][indtab$age], cls[2:(ncls+1)][indtab$age])
    }

    ###################
    ## On cree sptvar #
    ###################

    unitnum <- as.numeric(time[2]) - as.numeric(time[1])
    varsessions <- data.frame(start=start, end=end)
    timeint <- formattps(varsessions, tps, unitnum)
    spttab <- NULL

    if(!is.null(numtps)){
        spttab <- c(lapply(1:length(numtps), function(x){
            #datetps <- seq(start, end, by=unit)
            datetps <- time[-length(time)]
            int <- expand.grid(datetps, lpops)
            int$val <- runif(nrow(int), numtps[[x]][1], numtps[[x]][2])
            names(int) <- c("date", "pop", names(numtps)[x])
            return(int)
        }))
        names(spttab) <- c(names(spttab), names(numtps))
    }

    if(!is.null(numspts)){
        #undate <- seq(start, end, by=step.spts)
        undate <- seq(time[1], time[length(time)], by=step.spts)
        undate <- undate[undate < max(time)]
        spts <- expand.grid(date=undate, ind=levels(indtab$ind))
        #spts$datenum <- (as.numeric(spts$date) - as.numeric(start)) / unitnum
        for(i in 1:length(numspts)){
            spts0 <- data.frame(spts, runif(nrow(spts), numspts[[i]][1], numspts[[i]][2]))
            names(spts0) <- c(names(spts), names(numspts)[i])
            spttab <- c(spttab, list(spts0))
            names(spttab) <- c(names(spttab)[-length(spttab)], names(numspts)[i])
        }
    }
    if(!is.null(spttab))  spttab <- lapply(spttab, function(x) x[,!colnames(x) %in% "datenum"])
    #captocc <- data.frame(pop=levels(indtab$pop), start=rep(start, nlevels(indtab$pop)), end=rep(end, nlevels(indtab$pop)))
    res <- list(indvar=indtab, sptvar=spttab, captocc=captocc, clage=clage, unitnum=unitnum, tps=tps, step=step)
    class(res) <- c("CMRCTVars", class(res))
    return(res)
}
