formatmats2 <-
function(matsurv, matcapt, unind, dl, varind, varsessions){

    ##Creation des matrices
    lmatap <- list()
    ldtvecap <- list()
    lidcapt <- list()
    ltvecap <- list()
    lentvecap <- c()
    nomindap <- c()

    varnames <- unique(c(colnames(matsurv), colnames(matcapt)))
    for(i in 1:length(unind)){
        idind <- unind[i]
        idpop <- as.character(unique(varind$pop[varind$ind==idind]))
        tvecind <- tvec(idind, varind, varsessions, dl, varnames)

        if(length(tvecind$tvecap) > 1){
            vecap <- tvecind$tvecap
            vecap <- vecap[-length(vecap)]
            lmatap <- c(lmatap, list(matfac(vecap, varnames, idind, idpop, dl)))
            ldtvecap <- c(ldtvecap, list(diff(tvecind$tvecap)))
            ltvecap <- aplist(ltvecap, idind, tvecind$tvecap)
            lentvecap <- c(lentvecap, length(vecap))
            nomindap <- c(nomindap, as.character(idind))
            }

    }

    names(lmatap) <- nomindap
    names(ldtvecap) <- nomindap
    names(ltvecap) <-  nomindap

    ##Matcoeflin
    lmclsap <- lapply(lmatap, function(z) coeflin(matsurv,z))
    lmclcap <- lapply(lmatap, function(z) coeflin(matcapt,z))

    ##On concatene les listes
    mclsap <- Matrix::Matrix(do.call("rbind", lmclsap))
    mclcap <- Matrix::Matrix(do.call("rbind", lmclcap))
    idcapt <- do.call("c", lidcapt)

    insessap <- insess(ltvecap, varind, varsessions)
    dtvecap <- do.call("c", ldtvecap)

    return(list(lmclsap, lmclcap, insessap, idcapt,  ltvecap=ltvecap, lmclcap, dl))

}
