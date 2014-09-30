tvec <-
function(idind, varind, varsessions, dl, varnames){

	idpop <- as.character(unique(varind$pop[varind$ind==idind]))
	tvec.varind <- varind[varind$ind==idind, "datenum"]
	tvec.varsessions <- as.numeric(unlist(varsessions[varsessions$pop==idpop, c("startnum","endnum")]))
	dlnames <- varnames[!varnames %in% "Intercept"]
	tvec.dl <- as.numeric(unlist(lapply(dl[dlnames], function(x){
		if("ind" %in% colnames(x)){
			return(x$datenum[as.character(x$ind)==idind])
		}
		else{
			if("pop" %in% colnames(x)){
				return(x$datenum[as.character(x$pop)==idpop])
			}
			else {
				return(x$datenum)
			}
		}
	})))

	tvec <- sort(unique(c(tvec.varind, tvec.varsessions, tvec.dl)))
	tvecav <- tvec[(tvec >= min(tvec.varind)) & (tvec <= max(tvec.varind))]
	tvecap <- tvec[(tvec >= max(tvec.varind)) & (tvec <= max(tvec.varsessions))]
	idcapt <-  which(tvecav %in% tvec.varind)
	return(list(tvecav=tvecav, tvecap=tvecap, idcapt=idcapt))
}
