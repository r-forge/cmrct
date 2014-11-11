id2num <-
function(x, idstr="ind"){

    return(as.numeric(sub(idstr, "", as.character(x))))

}
