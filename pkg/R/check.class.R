check.class <-
function(var, class){

    varname <- deparse(substitute(var))
     if(!inherits(var, class)) stop(varname, " must be of class ", class)

}
