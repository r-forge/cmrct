simcapt <-
function(lamsap, lamcap, tvec){
    fini <- 0
    Tvec <- tvec
    Tcapt <- Tvec[1]
    for(i in 1:(length(tvec)-1)){
        if(fini==0){
            lambdaM <- lamsap[i]
            lambdaC <- lamcap[i]
            TT <- Tvec[i] - log(runif(1)) / (lambdaM + lambdaC)
            while(TT < Tvec[i+1]){
                if(runif(1) < (lambdaC / (lambdaC + lambdaM))){
                    Tcapt <- c(Tcapt, TT)
                    TT <- TT - log(runif(1)) / (lambdaM + lambdaC)
                }
                else{
                    fini <- 1
                    break
                }
            }
        }
    }
    return(Tcapt)
}
