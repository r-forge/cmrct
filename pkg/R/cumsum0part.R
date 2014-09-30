cumsum0part <-
function(xx, yy){
    zz <- cumsum(c(0, xx[1:(length(xx)-1)]))
    zz3 <- zz[yy]
    zz3  <-  zz3 - c(0, zz3[1:(length(zz3)-1)])
    zz2 <- rep(0, length(xx))
    zz2[yy] <- zz3
    res  <-  zz - cumsum(zz2)
    return(res)
}
