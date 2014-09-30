print.CMRCTData <-
function(cmrctdata) {
    oldnames <- names(cmrctdata)
    attributes(cmrctdata) <- NULL
    names(cmrctdata) <- oldnames
    print.default(cmrctdata)
}
