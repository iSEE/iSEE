#' Get panel names
#'
#' @param x An instance of a \linkS4class{Panel} class.
#'
#' @return
#' For \code{.getEncodedName}, a string containing the encoded panel name of \code{x}.
#'
#' For \code{.fullName}, a string containing the full (plain-English) name of the class.
#'
#' For \code{.getFullName}, a string containing the full name of \code{x}.
#'
#' @details
#' The encoded name is used internally as the name of various fields in \code{input}, \code{output} and reactive lists.
#'
#' The full name is what should be shown in the interface and visible to the end-user.
#'
#' @author Aaron Lun
#'
#' @export
#' @rdname getEncodedName
.getEncodedName <- function(x) {
    paste0(class(x), slot(x, iSEEslots$organizationId))
}

#' @export
#' @rdname getEncodedName
.getFullName <- function(x) {
    paste(.fullName(x), slot(x, iSEEslots$organizationId))
}

.encodedName <- function(x) {
    class(x)[1]
}

#' Set Difference With Names
#'
#' @param x A vector containing a sequence of named items.
#' @param y A vector containing a sequence of items.
#'
#' @details In contrast to \code{\link{setdiff}}, this function retains names in the return value.
#'
#' @return The vector \code{x} without items associated with a value present in \code{y}.
#'
#' @seealso \code{setdiff}.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_setdiffWithNames
.setdiffWithNames <- function(x, y) {
    x[which(!x %in% y)]
}
