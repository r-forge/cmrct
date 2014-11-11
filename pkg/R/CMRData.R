CMRData <-
function(indvar, sptvar=NULL, captocc, tps=NULL, unit="year", clage=NULL){

    ########################################
    ## Verifications de indvar et captocc ##
    ########################################


    ##Verifications des variables reservees dans indvar
    check.class(indvar, "data.frame")
    if(!all(c("ind", "date", "pop") %in% colnames(indvar))) stop("Each element of 'indvar' must contain column(s) 'ind' (factor), 'date' (POSIXct) and 'pop' (factor)")
    check.class(indvar$ind, "factor")
    check.class(indvar$date, "POSIXct")
    check.class(indvar$pop, "factor")
    if("age" %in% colnames(indvar)){
        if(!is.numeric(indvar$age)) stop("age must be numeric")
        if(is.null(clage)) stop("When 'age' is present in 'indvar' then 'clage' must be defined")
    }
    if(!is.null(clage)){
        if(!"age" %in% colnames(indvar)) stop("When 'clage' is given indvar must contain a column 'age'")
    }
    indvar <- indvar[,unique(colnames(indvar))]


    ##Verifications des variables reservees dans captocc
    check.class(captocc, "data.frame")
    if(!all(c("pop", "start", "end") %in% colnames(captocc))) stop("Each element of captocc must contain column(s) pop (factor), start (POSIXct) and end (POSIXct)")
    if(length(colnames(captocc))>3) warning(paste(colnames(captocc)[!colnames(captocc) %in% c("pop", "start", "end")], "were ignored.", collapse=", "))
    check.class(captocc$pop, "factor")
    check.class(captocc$start, "POSIXct")
    check.class(captocc$end, "POSIXct")


    ##Tri des tableaux
    indvar <- indvar[order(indvar$pop, indvar$ind, indvar$date),]
    captocc <- captocc[order(captocc$pop, captocc$start),]


    ##Verification: pas de sessions jointives ou chevauchantes dans captocc
    lcaptocc <- split(captocc, captocc$pop)
    for(i in 1:length(lcaptocc)){
        dates <- c(lcaptocc[[i]]$start, lcaptocc[[i]]$end)
        if(any(table(dates)>1)) warning("Consecutive capture occasions on pop ", unique(lcaptocc[[i]]$pop))
        if(nrow(captocc) > 1){
            start0 <- lcaptocc[[i]]$start
            end0 <- lcaptocc[[i]]$end
            if(any(start0[-1] < end0[-length(end0)])) stop("overlapping capture occasion on pop ", unique(lcaptocc[[i]]$pop))
        }
    }


    ##Verification: toutes les captures sont dans des occasions
    ##Modifiations des dates si elles tombent sur deb ou fin de session
    for(i in 1:nrow(indvar)){
        dd <- indvar$date[i]
        idpop <- indvar$pop[i]
        st0 <- captocc$start[captocc$pop==idpop]
        en0 <- captocc$end[captocc$pop==idpop]

        if(any(dd==st0)) indvar$date[i] <- indvar$date[i] + 1
        if(any(dd==en0)) indvar$date[i] <- indvar$date[i] - 1

        dd <- indvar$date[i]
        bses <- as.numeric(t(as.matrix(data.frame(as.numeric(st0), as.numeric(en0)))))
        if((findInterval(as.numeric(dd), bses) %% 2)==0){
            print(indvar[i,])
            stop("\n This capture fall outside a capture occasion")
        }

    }



    ################################
    ##  Verifications si sptvars  ##
    ################################


    if(!is.null(sptvar)){

        ##Verifications des variables reservees dans sptvar
        check.class(sptvar, "list")
        ##Verification de la coherence des ind et pop entre indvar et sptvar
        ##Verification des dates dans sptvar
        ##Tri de sptvar
        for(i in 1:length(sptvar)){

            if(!any(colnames(sptvar[[i]])=="date")) stop("Each element of sptvar must contain a column date")
            check.class(sptvar[[i]]$date, "POSIXct")

            if("ind" %in% colnames(sptvar[[i]])){
                check.class(sptvar[[i]]$ind, "factor")
                sptvar[[i]] <- sptvar[[i]][order(sptvar[[i]]$ind, sptvar[[i]]$date),]
                if(!all(levels(sptvar[[i]]$ind)==levels(indvar$ind))){
                    stop("levels of ind in ", names(sptvar)[i], " differ from the ones of indvar")
                }
            }

            if("pop" %in% colnames(sptvar[[i]])){
                check.class(sptvar[[i]]$pop, "factor")
                if(!any(colnames(sptvar[[i]])=="ind")) sptvar[[i]] <- sptvar[[i]][order(sptvar[[i]]$pop, sptvar[[i]]$date),]
                if(!all(levels(sptvar[[i]]$pop)==levels(indvar$pop))){
                    stop("levels of pop in sptvar[[", i,"]]", " differ from the ones of indvar")
                }
            }

            if(!any(colnames(sptvar[[i]]) %in% c("ind", "pop"))) sptvar[[i]] <- sptvar[[i]][order(sptvar[[i]]$date),]

            if(sum(!colnames(sptvar[[i]]) %in% c("ind", "pop", "date")) != 1) stop("Each element of sptvar must contain a single covariate")
        }

        ##Verification des doublons
        n.indvar <- colnames(indvar)[!colnames(indvar) %in% c("ind", "pop", "date")]
        n.sptvar <- do.call("c", lapply(sptvar, function(x) colnames(x)[!colnames(x) %in% c("ind", "pop", "date")]))
        if(any(table(c(n.indvar, n.sptvar))>1)) stop("Duplicated variables in indvar and/or sptvar")

    }


    #### #################################
    #### ##  Mise en forme des donnees  ##
    #### #################################


    #########################
    ## Mise en forme de tps #
    #########################

    if(!is.null(tps)){
        check.class(tps, "list")
        check.class(tps$deb, "POSIXct")
        check.class(tps$step, "character")
        check.unit(tps$step)
        if(tps$deb > min(indvar$date)) stop("tps must start before the first capture")
        #tps$step <- match.arg(tps$step, c("sec", "min", "hour", "day", "DSTday", "week", "month", "year"))

    }
    else {
        min.date <- min(captocc$start)
        start.tps <- as.POSIXct(strptime(paste("01/01/", format(min.date, "%Y"), sep=""), "%d/%m/%Y"))
        tps <- list(deb=min(captocc$start), step="year")
    }

    ##Verification de unit
    check.class(unit, "character")
    check.unit(unit)

    ##redef de l'unite de temps en numeric
    unitnum <- diff(as.numeric(seq(tps[[1]], by=unit, len=2)))


    ########################################
    ## Transformation des dates en numeric #
    ########################################

    indvar$datenum <- (as.numeric(indvar$date) - as.numeric(tps[[1]])) / unitnum
    if(!is.null(sptvar)){
        sptvar <- lapply(sptvar, function(x){
            tab <- x
            tab$datenum <- (as.numeric(tab$date) - as.numeric(tps[[1]])) / unitnum
            return(tab)
        })
    }
    captocc$startnum <- (as.numeric(captocc$start) - as.numeric(tps[[1]])) / unitnum
    captocc$endnum <- (as.numeric(captocc$end) - as.numeric(tps[[1]])) / unitnum


    #########################
    ## Definition de t et T #
    #########################


    ftps <- formattps(captocc, tps, unitnum)
    ltps <- list(T=as.factor(1:(length(ftps)-1))[1], t = 1)



    ############################
    ## Tableau des covariables #
    ############################

    ft <- indvar[, colnames(indvar)[!colnames(indvar) %in% c("date", "datenum")]]
    ft <- unique(ft)
    ft <- ft[,colnames(ft)[!colnames(ft) %in% c("ind")], drop=FALSE]
    if(!is.null(clage)){
        ft$age <- as.factor(findInterval(ft$age, clage))
        covs.ind <- as.list(ft[1,colnames(ft)[!colnames(ft) %in% c("age")], drop=FALSE])
    }
    else{
        covs.ind <- as.list(ft[1,])
    }
    pos.fac <- which(unlist(lapply(ft, class) %in% c("factor", "character")))
    ind.sum <- ftable(ft[,pos.fac])
    covs.spt <- lapply(sptvar, function(x){
            ncov <- colnames(x)[!colnames(x) %in% c("ind", "pop", "date", "datenum")]
            x[1, ncov]
        })
    if(!is.null(clage)){
        lage <- list(age=as.factor(c(0,1:length(clage)))[1])
        covs <- data.frame(c(covs.ind, covs.spt, lage, ltps), check.names = FALSE)[1,]
    }
    else{
        covs <- data.frame(c(covs.ind, covs.spt, ltps), check.names = FALSE)[1,]
    }


    ######################################
    ## Mise en forme des donnees CMRData #
    ######################################


    ##Transformation en indicatrices
    indvar0 <- formatvar(indvar)
    dl <- indvar0
    if(!is.null(sptvar)) dl <- c(dl, sptvar0 <- formatvar(sptvar))

    ##Ajout de t et T a dl
    dl <- c(dl, ftps)

    ##Si classe d age alors il faut ajouter les indicatrices
    unind <- as.character(unique(indvar$ind))
    if(!is.null(clage)){
        dl <- c(dl, matage(unind, indvar, clage))
    }

    inds <- data.frame(lapply(dl, function(x){
        int <- x[1,!colnames(x) %in% c("ind", "pop", "date", "datenum")]
    }), check.names = FALSE)

    ltab <- list(covs=dl, capt=indvar[order(indvar$pop, indvar$ind, indvar$datenum), c("ind", "pop", "date", "datenum")],
                 captocc=captocc)

    class(ltab) <- c("CMRCTData", class(ltab))
    attributes(ltab)$varlist <- covs
    attributes(ltab)$indsum <- ind.sum
    attributes(ltab)$indlist <- inds
    return(ltab)

}
