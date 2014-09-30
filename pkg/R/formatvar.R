formatvar <-
function(x){
	if(is.data.frame(x)){
		newx <- x
		newx <- newx[,!colnames(newx) %in% c("date", "age")]
		#newx$datenum <- 0
		newx <- unique(newx)
		form1 <- formatvar0(newx, exep=c("ind", "datenum"))
		return(form1)
	}
	else{
		form0 <- lapply(x, function(z){
                    zz <- z[,!colnames(z) %in% c("date")]
                    return(formatvar0(zz))
                })
		form1 <- do.call("c", form0)
		names(form1) <- unlist(lapply(strsplit(names(form1), "\\."), function(x) x[-1]))
		return(form1)
	}
}
