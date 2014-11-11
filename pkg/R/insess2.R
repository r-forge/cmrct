insess2 <-
function(ltvec, varind, varsessions){
    res <- lapply(1:length(ltvec), function(z){
        idind <- names(ltvec)[z]
        idpop <- as.character(unique(varind$pop[varind$ind==idind]))
        bsess <- as.numeric(t(as.matrix(varsessions[varsessions$pop==idpop,c("startnum", "endnum")])))
        ltvecok <- ltvec[[z]][-length(ltvec[[z]])]
        insess <- findInterval(round(ltvecok, 10), round(bsess, 10)) %% 2
        return(insess)
    })
    res <- do.call("c", res)
    names(res) <- NULL
    return(res)
}
