matage <-
function(unind, varind, clage){

	matage0 <- function(idind, varind, clage){
		fcapt <- min(varind$datenum[varind$ind==idind])
		fage <- min(varind$age[varind$ind==idind])
		bage <- unique(c(fcapt, fcapt + (clage - fage)))
		keepbage <- bage[bage >= fcapt]
		nage <- as.factor(lage <- paste("age", 0:length(clage), sep=""))
		nage <- as.data.frame(model.matrix(~-1+nage))[(length(clage)-length(keepbage)+2):length(nage),]
		colnames(nage) <- lage
		indage <- data.frame(ind=rep(idind, length(keepbage)), datenum=keepbage, nage)
		return(indage)
	}
	tabage <- do.call("rbind", lapply(unind, function(x) matage0(x, varind, clage)))
	listage <- lapply(1:(ncol(tabage)-2), function(x) tabage[,c(1,2,2+x)])
	names(listage) <- colnames(tabage)[-c(1:2)]
	return(listage)
}
