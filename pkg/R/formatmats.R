formatmats <-
function(matsurv, matcapt, unind, dl, varind, varsessions){

    ##Creation des matrices
    lmatav <- list()
    lmatap <- list()
    ldtvecav <- list()
    ldtvecap <- list()
    lidcapt <- list()
    ltvecav <- list()
    ltvecap <- list()
    lentvecav <- 0
    lentvecap <- c()
    nomindav <- c()
    nomindap <- c()

    varnames <- unique(c(colnames(matsurv), colnames(matcapt)))
    for(i in 1:length(unind)){
        idind <- unind[i]
        idpop <- as.character(unique(varind$pop[varind$ind==idind]))
        tvecind <- tvec(idind, varind, varsessions, dl, varnames)
        if(length(tvecind$tvecav) > 1){
            vecav <- tvecind$tvecav
            vecav <- vecav[-length(vecav)]
            lmatav <- c(lmatav, list(matfac(vecav, varnames, idind, idpop, dl)))
            ldtvecav <- c(ldtvecav, list(diff(tvecind$tvecav)))
            lidcapt <- c(lidcapt, list(tvecind$idcapt[-1]-1 + sum(lentvecav)))
            ltvecav <- aplist(ltvecav, idind, tvecind$tvecav)
            lentvecav <- c(lentvecav, length(vecav))
            nomindav <- c(nomindav, as.character(idind))
        }

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

    #lentvecav <- lentvecav[-1]
    names(lmatav) <- nomindav
    names(ldtvecav) <- nomindav
    names(lidcapt) <- nomindav
    names(ltvecav) <-  nomindav
    names(lmatap) <- nomindap
    names(ldtvecap) <- nomindap
    names(ltvecap) <-  nomindap

    ##Matcoeflin
    lmclsav <- lapply(lmatav, function(z) coeflin(matsurv,z))
    lmclsap <- lapply(lmatap, function(z) coeflin(matsurv,z))
    lmclcav <- lapply(lmatav, function(z) coeflin(matcapt,z))
    lmclcap <- lapply(lmatap, function(z) coeflin(matcapt,z))

    ##On concatene les listes
    mclsav <- Matrix::Matrix(do.call("rbind", lmclsav))
    mclsap <- Matrix::Matrix(do.call("rbind", lmclsap))
    mclcav <- Matrix::Matrix(do.call("rbind", lmclcav))
    mclcap <- Matrix::Matrix(do.call("rbind", lmclcap))
    idcapt <- do.call("c", lidcapt)

    #return(list(ltvecav=ltvecav, varind=varind, varsessions=varsessions, nomindav, nomindap))

    insessav <- insess(ltvecav, varind, varsessions)
    insessap <- insess(ltvecap, varind, varsessions)
    dtvecav <- do.call("c", ldtvecav)
    dtvecap <- do.call("c", ldtvecap)

    return(list(mclsav, mclsap,
                mclcav, mclcap, insessav, insessap,
                idcapt, dtvecav, dtvecap, lmclcap, dl))
}
