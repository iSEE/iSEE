.is_brush <- function(x) {
    length(x) && is.null(x$closed)
}
