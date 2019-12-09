#' Update lasso information
#'
#' Update the lasso information based on the incoming click object from the application.
#'
#' @param click A Shiny click object.
#' @param previous \code{NULL} if no waypoints exist, otherwise a list produced by this function.
#' @param tol A numeric scalar specifying the tolerance to detect if the next click closes the lasso.
#'
#' @return A list containing:
#' \describe{
#' \item{\code{coord}:}{A numeric matrix of lasso waypoint coordinates, where each row is a waypoint.
#' The first column is the x-axis coordinate and the second column is the y-axis coordinate.}
#' \item{\code{closed}:}{A logical scalar specifying whether the lasso is closed.}
#' \item{\code{panelvar1}, \code{panelvar2}:}{Strings specifying the facet on which the lasso occurs.}
#' \item{\code{mapping}:}{A list containing further information about the variables used on the x- and y-axes and for faceting.}
#' }
#'
#' @details
#' A click close to the first waypoint will close the lasso.
#' The tolerance is defined as a product of \code{tol} and the x-/y-axis range.
#'
#' @author Aaron Lun
#' @seealso \code{\link{.process_selectby_choice}},
#' \code{\link{lassoPoints}}
#' @rdname INTERNAL_update_lasso
.update_lasso <- function(click, previous=NULL, tol=0.01) {
    new_lasso <- list(
        lasso=NULL, closed=FALSE, panelvar1=click$panelvar1,
        panelvar2=click$panelvar2, mapping=click$mapping)

    if (length(previous)) {
        # Closing the lasso if you click close to the starting point, within the same facet.
        xrange <- click$domain$right - click$domain$left
        yrange <- click$domain$top - click$domain$bottom

        if (abs(click$x - previous$coord[1,1]) < xrange * tol
            && abs(click$y - previous$coord[1,2]) < yrange * tol
            && identical(previous$panelvar1, click$panelvar1) # okay for both to be NULL.
            && identical(previous$panelvar2, click$panelvar2)
        ) {
            new_lasso$coord <- rbind(previous$coord, previous$coord[1,])
            new_lasso$closed <- TRUE
        } else {
            # Adding a waypoint, but only to an existing open lasso, otherwise using NULL.
            if (!previous$closed
                && identical(previous$panelvar1, click$panelvar1)
                && identical(previous$panelvar2, click$panelvar2) ) {
                new_lasso$coord <- previous$coord
            }
            new_lasso$coord <- rbind(new_lasso$coord, c(click$x, click$y))
        }
    } else {
        new_lasso$coord <- cbind(click$x, click$y)
    }

    return(new_lasso)
}

#' Is the object a lasso store?
#' 
#' Checks if an object is a lasso or Shiny brush data store.
#'
#' @param x A lasso or Shiny brush object.
#'
#' @return A logical scalar specifying if \code{x} is a lasso.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_is_lasso
.is_lasso <- function(x) {
    !is.null(x$closed)
}

.is_brush <- function(x) {
    length(x) && is.null(x$closed)
}
