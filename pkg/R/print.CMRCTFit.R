print.CMRCTFit <-
function(cmrctfit) {
    oldnames <- names(cmrctfit)
    attributes(cmrctfit) <- NULL
    names(cmrctfit) <- oldnames
    print.default(cmrctfit)
}
