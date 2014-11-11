print.CMRCTFit <-
function(x, ...) {
    cmrctfit <- x
    oldnames <- names(cmrctfit)
    attributes(cmrctfit) <- NULL
    names(cmrctfit) <- oldnames
    print.default(cmrctfit)
}
