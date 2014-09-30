aplist <-
function(list, name, val, env=parent.frame()){
    c(list, eval(parse(text=paste("list(", name,  "=", deparse(substitute(val)), ")", sep="")), env))
}
