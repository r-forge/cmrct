formattps <-
function(varsessions, tps, unitnum){
	datet <- seq(tps[[1]], max(varsessions$end),by=tps[[2]])
	vart <- (as.numeric(datet) - as.numeric(tps[[1]])) / unitnum
	#listt <- list(t=data.frame(date=datet, datenum=vart, t=vart)
        listt <- list(t=data.frame(date=datet, datenum=(1:length(vart))-1, t=(1:length(vart))-1))
	varT <- as.factor(vart)
	levels(varT) <- paste("T", 1:nlevels(varT), sep="")
	varTT <- as.data.frame(diag(nlevels(varT)))
	colnames(varTT) <- as.character(varT)
	vartab <- data.frame(date=datet, datenum=vart,varTT)
	listT <- lapply(1:nlevels(varT), function(x) {
		tab <- data.frame(vartab[,c(1,2,2+x)])
		colnames(tab)[3] <- as.character(varT)[x]
		return(tab)
	})
	names(listT) <- as.character(varT)
	resl <- c(listt, listT)
	return(resl)
}
