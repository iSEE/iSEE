#' Safely use reactive values
#'
#' Initialize and bump reactive variables in a manner that avoids errors if they were not already present in \code{rObjects}.
#' Also avoids creation of links in the Shiny reactive graph when we are only writing to these reactive variables.
#'
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param field String containing the name of the reactive variable.
#' @param value Integer scalar containing the initial value of the variable.
#' @param max Integer scalar specifying the maximum value of the variable, see \code{\link{.increment_counter}}.
#'
#' @return \code{.safe_reactive_init} will add \code{field} to \code{rObjects} with value \code{value},
#' but only if it was not already present; otherwise this is a no-op.
#' It returns \code{rObjects} invisibly.
#'
#' \code{.safe_reactive_bump} will increment \code{field} in \code{rObjects}, initializing it if it was not already present.
#' It returns the incremented value invisibly.
#' 
#' @author Aaron Lun
#' 
#' @rdname INTERNAL_safe_reactive
#' @importFrom shiny isolate
.safe_reactive_init <- function(rObjects, field, value=1L) {
    if (!field %in% isolate(names(rObjects))) {
        rObjects[[field]] <- value
    }
    invisible(rObjects)
}

#' @rdname INTERNAL_safe_reactive
#' @importFrom shiny isolate
.safe_reactive_bump <- function(rObjects, field, max=10000L) {
    .safe_reactive_init(rObjects, field)
    counter <- isolate(rObjects[[field]]) + 1L
    if (counter >= max) {
        counter <- 0L
    }
    rObjects[[field]] <- counter
    invisible(counter)
}

#' Increment a counter
#'
#' Increments the counter for a reactive value to trigger downstream updates.
#'
#' @param counter An integer scalar, usually an isolated reactive variable.
#' @param max An integer scalar specifying the upper bound for the increment.
#'
#' @return Integer scalar of value equal to \code{counter+1L} if this is not greater than \code{max}; zero otherwise.
#'
#' @details
#' This function is primarily designed to increment reactive values to trigger downstream observers, conductors or UI endpoints.
#' The use of \code{max} avoids an integer overflow in (very!) long-running apps.
#'
#' Technically we could have flipped a logical flag instead.
#' The initial worry was that if one observer flips the flag and another observer flips it back, there wouldn't be any net change to trigger downstream events.
#' This is probably not the case, as Shiny links get invalidated upon any change to a reactive value, but nonetheless, here we are.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_increment_counter
#' @seealso
#' \code{\link{iSEE}}
.increment_counter <- function(counter, max=10000L) {
    counter <- counter + 1L
    if (counter >= max) {
        counter <- 0L
    }
    return(counter)
}
