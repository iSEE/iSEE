#' @export
filterDTColumn <- function(x, search) {
    if (is.numeric(x)) {
        fragmented <- strsplit(search, " ... ", fixed=TRUE)[[1]]
        x >= as.numeric(fragmented[1]) & x <= as.numeric(fragmented[2])
    } else {
        grepl(search, x)
    }
}
