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

    if (!is.null(previous)) {
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


#' Find rows of data within a closed lasso
#'
#' Identify the rows of a data.frame lying within a closed lasso polygon, analogous to \code{\link{brushedPoints}}.
#'
#' @param df A data.frame from which to select rows.
#' @param lasso A list containing data from a lasso.
#'
#' @return A subset of rows from \code{df} with coordinates lying within \code{lasso}.
#'
#' @details
#' This function uses \code{\link{in.out}} from the \pkg{mgcv} package to identify points within a polygon.
#' This involves a boundary crossing algorithm that may not be robust in the presence of complex polygons with intersecting edges.
#'
#' @export
#' @seealso \code{\link{brushedPoints}}
#' @author Aaron Lun
#' @examples
#' lasso <- list(coord=rbind(c(0, 0), c(0.5, 0), c(0, 0.5), c(0, 0)),
#'     closed=TRUE, mapping=list(x="X", y="Y"))
#' values <- data.frame(X=runif(100), Y=runif(100),
#'     row.names=sprintf("VALUE_%i", seq_len(100)))
#' lassoPoints(values, lasso)
#'
#' # With faceting information:
#' lasso <- list(coord=rbind(c(0, 0), c(0.5, 0), c(0, 0.5), c(0, 0)),
#'     panelvar1="A", panelvar2="B", closed=TRUE,
#'     mapping=list(x="X", y="Y",
#'     panelvar1="FacetRow", panelvar2="FacetColumn"))
#' values <- data.frame(X=runif(100), Y=runif(100),
#'     FacetRow=sample(LETTERS[1:2], 100, replace=TRUE),
#'     FacetColumn=sample(LETTERS[1:4], 100, replace=TRUE),
#'     row.names=sprintf("VALUE_%i", seq_len(100)))
#' lassoPoints(values, lasso)
#'
#' @importFrom mgcv in.out
lassoPoints <- function(df, lasso) {
    if (!lasso$closed) {
        stop("cannot find points in open lasso")
    }

    keep <- !logical(nrow(df))
    if (!is.null(lasso$panelvar1)) {
        keep <- keep & df[[lasso$mapping$panelvar1]]==lasso$panelvar1
    }
    if (!is.null(lasso$panelvar2)) {
        keep <- keep & df[[lasso$mapping$panelvar2]]==lasso$panelvar2
    }

    retained <- in.out(lasso$coord, cbind(
        as.numeric(df[[lasso$mapping$x]][keep]),
        as.numeric(df[[lasso$mapping$y]][keep])
    ))
    return(df[which(keep)[retained],])
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
