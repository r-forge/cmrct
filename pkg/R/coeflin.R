coeflin <-
function(mat, lmat){
	matcoeflin <- do.call("cbind",lapply(1:nrow(mat), function(x){
		nomsvars <- colnames(mat)[which(mat[x,]==1)]
		prod <- apply(lmat[,nomsvars, drop=FALSE], 1, prod)
		return(prod)
	}))
	colnames(matcoeflin) <- rownames(mat)
	return(matcoeflin)
}
