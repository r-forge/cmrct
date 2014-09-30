CMRFit <-
function(surv, capt, cmrdata, start=0, method="BFGS", se.fit=TRUE, latts=FALSE, ...){

    ## require(Matrix)
    if(!inherits(surv,"formula")) stop("surv must be a formula")
    if(!inherits(capt,"formula")) stop("capt must be a formula")
    if(!inherits(cmrdata,"CMRCTData")) stop("cmrdata must be an object of calss CMRData")

    matsurv <- matcstr(surv, cmrdata)
    matcapt <- matcstr(capt, cmrdata)

    unind <- unique(cmrdata$capt$ind)

    cat("Building constraints matrices \n")
    lmats <- formatmats(matsurv, matcapt, unind, cmrdata$covs, cmrdata$capt, cmrdata$captocc)

    ##Valeur initiales de theta
    if(length(start)==1)  theta <- rep(start, nrow(matsurv) + nrow(matcapt))
    else theta <- start

    nsurv <- nrow(matsurv)
    ncapt <- nrow(matcapt)

    ##Optimization
    cat("Starting optimization \n")
    res.fit <- optim(theta, CMRCT.negloglik, nsurv=nsurv, ncapt=ncapt, matlist=lmats, method=method, hessian=se.fit, ...)

    est.surv <- data.frame(parameter=rownames(matsurv), estimation=res.fit$par[1:nsurv], sd=NA)
    est.capt <- data.frame(parameter=rownames(matcapt), estimation=res.fit$par[(nsurv+1):(nsurv+ncapt)], sd=NA)

    if(se.fit){
        sds <- sqrt(diag(solve(res.fit$hessian)))
        est.surv$sd <- sds[1:nsurv]
        est.capt$sd <- sds[(nsurv+1):(nsurv+ncapt)]
    }

    if(res.fit$convergence!=0) warning("Convergence not reached. See ?CMRFit to increase maxit.")

    est <- list(survival=est.surv, capture=est.capt)
    attributes(est)$optim <- res.fit[-1]
    if(latts){
        attributes(est)$mats <- lmats
        attributes(est)$matsurv <- matsurv
        attributes(est)$matcapt <- matcapt
    }
    class(est) <- c("CMRCTFit", "list")
    names(est) <- c("Survival", "Capture")
    return(est)

}
