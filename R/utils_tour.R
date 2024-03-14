#' UI-specific tour management utilities
#' 
#' Utilities to manage the tours specific to individual UI elements for each \linkS4class{Panel}.
#' This is done via a global tour cache that is updated by each Panel's interface-generating methods,
#' so that developers can easily put the UI documentation next to the element definition.
#' 
#' @param cls String containing the name of the \linkS4class{Panel} class containing the relevant UI element.
#' @param field String containing the slot of the \code{cls} class that is controlled by the UI element.
#' @param fun Function that accepts a string containing the encoded Panel name,
#' and returns a data.frame compatible with \pkg{rintrojs}, i.e., with a \code{element} and \code{intro} column.
#' @param force Logical scalar indicating whether \code{fun} should forcibly overwrite an existing function in the tour cache for \code{cls} and \code{field}.
#' 
#' @return
#' \code{.addSpecificTour} registers the provided function in the tour cache for the provided \code{cls} and \code{field}.
#' A \code{NULL} is invisibly returned.
#' 
#' \code{.getSpecificTours} returns a list of registered tour-generating functions for the specified \code{cls}.
#'
#' \code{.clearSpecificTours} removes all functions in the tour cache.
#' 
#' @details
#' By default, \code{.addSpecificTour} will have no effect if a function is already registered for a particular combination of \code{cls} and \code{field}.
#' However, users can force a replacement with \code{force=TRUE}.
#'
#' \code{.clearSpecificTours} is intended for use by the \pkg{iSEE} app itself and should not be used by Panel methods.
#'
#' @seealso
#' \code{\link{.selectInput.iSEE}} and friends, which provide modified UI elements that can be clicked to launch a helpful tour.
#'
#' @author Aaron Lun
#'  
#' @name specific-tours
NULL

.global_tour_env <- new.env()
.global_tour_env$tours <- list()

#' @export
#' @rdname specific-tours
.addSpecificTour <- function(cls, field, fun, force=FALSE) {
    if (force || is.null(.global_tour_env$tours[[cls]][[field]])) {
        if (is.null(.global_tour_env$tours[[cls]])) {
            .global_tour_env$tours[[cls]] <- list()
        }
        .global_tour_env$tours[[cls]][[field]] <- fun
    }
    invisible(NULL)
}

#' @export
#' @rdname specific-tours
.getSpecificTours <- function(cls) {
    .global_tour_env$tours[[cls]]
}

#' @export
#' @rdname specific-tours
.clearSpecificTours <- function() {
    .global_tour_env$tours <- list()
    invisible(NULL)
}
