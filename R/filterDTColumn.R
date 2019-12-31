#' Filter \pkg{DT} columns
#'
#' Filter a data.frame based on the \pkg{DT} \code{\link{datatable}} widget column search string.
#' 
#' @param df A data.frame that was used in the \code{\link{datatable}} widget.
#' @param x A numeric or character vector, usually representing a column of a data.frame.
#' @param search A string specifying the search filter to apply to \code{x}.
#' @param column A character vector of per-column search strings to apply to \code{df}.
#' @param global String containing a regular expression to search for across all columns in \code{df} (and row names, if present).
#'
#' @return 
#' A logical vector indicating which entries of \code{x} or rows of \code{df} are to be retained.
#' For \code{filterDT}, an \code{&} operation is applied to the logical vectors from all columns.
#'
#' @details
#' For character \code{x}, \code{search} is treated as a regular expression.
#'
#' For numeric \code{x}, \code{search} should have the form \code{LOWER ... UPPER}
#' where all elements in [LOWER, UPPER] are retained.
#'
#' Ideally, \code{ncol(df)} and \code{length(searches)} would be the same, but if not,
#' \code{\link{filterDT}} will simply filter on the first N entries where N is the smaller of the two.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{datatable}} and associated documentation for more details about column searches.
#'
#' @examples
#' # Regular expression:
#' filterDTColumn(LETTERS, "A|B|C")
#'
#' # Range query:
#' filterDTColumn(runif(20), "0.1 ... 0.5")
#'
#' # Works on DataFrames:
#' X <- data.frame(row.names=LETTERS, thing=runif(26), 
#'     stuff=sample(letters[1:3], 26, replace=TRUE))
#' filterDT(X, c("0 ... 0.5", "a|b"), "")
#' filterDT(X, "", "A")
#' 
#' @export
filterDTColumn <- function(x, search) {
    if (is.numeric(x)) {
        fragmented <- strsplit(search, " ... ", fixed=TRUE)[[1]]
        x >= as.numeric(fragmented[1]) & x <= as.numeric(fragmented[2])
    } else {
        grepl(search, x)
    }
}

#' @export
#' @rdname filterDTColumn
filterDT <- function(df, column, global) {
    output <- !logical(nrow(df))

    if (global!="") {
        g.out <- logical(nrow(df))
        for (i in seq_len(ncol(df))) {
            g.out <- g.out | grepl(global, df[[i]])
        }
        if (!is.null(rownames(df))) {
            g.out <- g.out | grepl(global, rownames(df))
        }
        output <- output & g.out
    }
   
    for (i in seq_len(min(ncol(df), length(column)))) {
        if (column[i]!="") {
            output <- output & filterDTColumn(df[[i]], column[i])
        }
    }

    output
}
