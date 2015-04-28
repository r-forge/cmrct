formatvar0 <-
function(x, exep=c("date", "datenum", "ind", "pop", "age")){
	nn <- colnames(x)[!colnames(x) %in% exep ]
	#if(length(nn)==0) return(x)
	resl <- list()
	for(i in 1:length(nn)){
 		if (is.factor(x[, nn[i]]) & (nlevels(x[,nn[i]]) == 1)) {
      			x[, nn[i]] <- as.numeric(x[, nn[i]])
		}
		if( is.factor(x[,nn[i]])){
			x[,nn[i]] <- as.factor(as.character(x[,nn[i]]))
			mm <- model.matrix(~-1+., data=x[,nn[i], drop=FALSE])
			oldnames <- names(resl)
			resl <- c(resl, lapply(1:ncol(mm), function(z) {
				tab <- data.frame(x[,colnames(x) %in% exep, drop=FALSE], mm[,z])
				colnames(tab) <- c(colnames(tab)[1:(ncol(tab)-1)], colnames(mm)[z])
				return(tab)
			}))
			names(resl) <- c(oldnames, colnames(mm))
		}
		else{
			oldnames <- names(resl)
			resl <- c(resl, list(data.frame(x[,colnames(x) %in% exep, drop=FALSE], x[,nn[i], drop=FALSE])))
			names(resl) <- c(oldnames, nn[i])
		}
	}
	return(resl)
}
