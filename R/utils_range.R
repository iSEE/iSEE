#' Range utilities
#'
#' \code{.safe_nonzero_range} adds 1 to the upper bound of 0-length ranges.
#' It also subtracts 1 from the lower bound if the range is meant to be centered.
#'
#' @param range A numeric vector of length 2.
#' @param centered A logical scalar indicating if the range is centered.
#'
#' @details 
#' \code{\link{colorRamp2}} does not like all-identical breaks, hence this function.
#' Note that we check for equality of the stringified values as this is what is actually used in the \code{\link{colorRamp2}} call.
#'
#' @return A range with non-zero width.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_range
.safe_nonzero_range <- function(range, centered) {
    converted <- sprintf("%s", range)
    if (identical(converted[1], converted[2])) {
        range[2] <- range[2] + 1
        if (centered) {
            range[1] <- range[1] - 1
        }
    }
    range
}
