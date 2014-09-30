matfac <-
function(tvec, varnames, idind, idpop, dl){
	ids <- c(idind, idpop)
	matav <- matrix(0, ncol=length(varnames), nrow=length(tvec), dimnames=list(tvec, varnames))
	cnames <- colnames(matav)[!colnames(matav) %in% "Intercept"]
	for(nn in cnames){
		dlpos <- which(names(dl)==nn)
		smat <- dl[[dlpos]]
		if("ind" %in% colnames(smat)){
			smat0 <- smat[as.character(smat$ind)==as.character(idind), ]
		}
		else{
			if("pop" %in% colnames(smat)){
				smat0 <- smat[smat$pop==idpop, ]
			}
			else{
				smat0 <- smat
			}
		}
		int <- findInterval(tvec, smat0[, "datenum"])
		matav[,which(colnames(matav)==nn)] <- smat0[int,nn]
	}
	if("Intercept" %in% colnames(matav)) matav[,"Intercept"] <- 1
	return(matav)
}
