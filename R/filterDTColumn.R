#' Filter \pkg{DT} columns
#'
#' Filter a vector based on the \pkg{DT} \code{\link{datatable}} widget column search string.
#'
#' @param x A numeric or character vector, usually representing a column of a data.frame.
#' @param search A string specifying the search filter to apply to \code{x}.
#'
#' @return 
#' A logical vector indicating which entries are returned.
#'
#' @details
#' For character \code{x}, \code{search} is treated as a regular expression.
#'
#' For numeric \code{x}, \code{search} should have the form \code{LOWER ... UPPER}
#' where all elements in [LOWER, UPPER] are retained.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{datatable}} and associated documentation for more details about column searches.
#'
#' @examples
#' filterDTColumn(LETTERS, "A|B|C")
#'
#' filterDTColumn(runif(20), "0.1 ... 0.5")
#' @export
filterDTColumn <- function(x, search) {
    if (is.numeric(x)) {
        fragmented <- strsplit(search, " ... ", fixed=TRUE)[[1]]
        x >= as.numeric(fragmented[1]) & x <= as.numeric(fragmented[2])
    } else {
        grepl(search, x)
    }
}
