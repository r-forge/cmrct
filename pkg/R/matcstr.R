matcstr <-
function(formula, cmrdata){
    nl <- data.frame(attributes(cmrdata)$varlist, attributes(cmrdata)$indlist, check.names = FALSE)
    ter <- terms(formula)
    vv <- rownames(attributes(ter)$factors)
    if(!all(vv %in% colnames(nl))){
        print(vv[!vv %in% colnames(nl)])
        stop("unknown variable in formula")
    }
    modmat <- model.matrix(formula, nl)
    terms <- colnames(modmat)
    nterms <- length(terms)
    nvars <- length(vars <- unique(unlist(strsplit(terms, ":"))))
    if(attributes(ter)$intercept) {
        terms <- c("Intercept", terms[-1])
        vars <- c("Intercept", vars[-1])
    }
    mat <- matrix(0, nrow=nterms, ncol=nvars, dimnames=list(terms, vars))
    for(i in 1:nterms){
        ster <- unique(unlist(strsplit(terms[i], ":")))
        for(j in 1:length(ster)){
            pos <- which(vars==ster[j])
            mat[i, pos] <- 1
        }
    }
    return(mat)
}
