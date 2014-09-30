CMRCT.negloglik <-
function(theta, nsurv, ncapt, matlist){

    thetasurv <- theta[1:nsurv]
    thetacapt <- theta[(nsurv+1):(nsurv+ncapt)]

    mclsav   <- matlist[[1]]
    mclsap   <- matlist[[2]]
    mclcav   <- matlist[[3]]
    mclcap   <- matlist[[4]]
    insessav <- matlist[[5]]
    insessap <- matlist[[6]]
    idcapt   <- matlist[[7]]
    dtvecav  <- matlist[[8]]
    dtvecap  <- matlist[[9]]
    lmclcap  <- matlist[[10]]

    ##Calcul des lambdas
    lamsav <- exp(mclsav %*% thetasurv)
    lamsap <- exp(mclsap %*% thetasurv)
    lamcav <- exp(mclcav %*% thetacapt) *insessav
    intmat <- mclcap %*% thetacapt
    intmat[intmat>100] <- 100
    lamcap <- exp(intmat) *insessap

    #########################
    ## Vraissemblance Avant #
    #########################

    vraisav1 <- -sum((lamsav + lamcav) * dtvecav)
    vraisav2 <- sum(log(lamcav[idcapt]))
    vraisav <-  vraisav1 + vraisav2

    #########################
    ## Vraissemblance Apres #
    #########################

    ##Code Verif donnees pdf
    ## index_apres_nvind <- c(6,10)
    ## lamcap <- c(1,0,0,1,2,3,0,0,4,1,0,5)
    ## lamsap <- c(1,2,1,1,2,1,1,1,2,1,1,3)
    ## dtvecap <- c(1,2,1,1,2,1,4,1,2,1,1,3)
    ## index_apres_derind <- c(index_apres_nvind - 1, length(lamcap))

    ##Calcul de lambda
    lamap <- lamsap + lamcap
    lamap[lamap > 1000] <- 1000
    lamap[lamap==0] <- 1e-15


    ##Calcul de lamda * delaT
    lamfoisdeltat <- lamap * dtvecap

    ##Identifiants des positions de changement d'individus dans les vecteurs de lambda
    index_apres_nvind0 <- do.call("c", lapply(lmclcap, nrow))
    index_apres_nvind0[1] <- index_apres_nvind0[1] + 1
    index_apres_nvind <- cumsum(index_apres_nvind0)
    index_apres_nvind <- index_apres_nvind[-length(index_apres_nvind)]
    #if(length(index_apres_nvind0)>1) index_apres_nvind <- index_apres_nvind[-length(index_apres_nvind)]

    ##Indentifiant de la derniere obs de chaque ind dans les vecteurs de lambda
    index_apres_derind <- index_apres_nvind - 1
    index_apres_derind <- c(index_apres_derind, length(lamap))
    #if(length(index_apres_nvind0)>1) index_apres_derind <- c(index_apres_derind, length(lamap))

    #Cumule partielle
    ghost0 <- cumsum0part(lamfoisdeltat, index_apres_nvind)

    #Calcul de la vraissemeblance Apres
    ghost <- exp(-ghost0)
    vec  <-  ghost * (1 - exp(-lamfoisdeltat)) * lamcap / lamap
    vec2 <- cumsum(vec)
    vec3 <- vec2[index_apres_derind] - c(0, vec2[index_apres_derind[1:(length(index_apres_derind)-1)]])

    if(any(vec3 > 1)){
        vraisap <- -1e16
    }
    else vraisap <- sum(log(1 - vec3))


    ###################
    ## Vraissemblance #
    ###################

    vrais <- vraisav + vraisap

    return(NegLogLik=-vrais)

}
