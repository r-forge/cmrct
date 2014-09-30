check.unit <-
function(by){
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L)
        stop("invalid string for 'unit'")
    valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours",
                                        "days", "weeks", "months", "years", "DSTdays"))
    if (is.na(valid))
        stop("invalid string for 'unit'")
    if (valid <= 5L) {
        by <- c(1, 60, 3600, 86400, 7 * 86400)[valid]
        if (length(by2) == 2L)
            by <- by * as.integer(by2[1L])
    }
    else by <- if (length(by2) == 2L)
        as.integer(by2[1L])
    else 1
}
