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
    paste0(class(x), x[[.organizationId]])
}

#' @export
#' @rdname getEncodedName
.getFullName <- function(x) {
    paste(.fullName(x), x[[.organizationId]])
}

.encodedName <- function(x) {
    class(x)[1]
}
