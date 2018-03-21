#' Subset points for faster plotting
#'
#' Subset points using a grid-based system, to avoid unnecessary rendering when plotting.
#' 
#' @param X A numeric vector of x-coordinates for all points.
#' @param Y A numeric vector of y-coordinates for all points, of the same length as \code{X}.
#' @param resolution A positive integer specifying the number of bins on each axis of the grid.
#' 
#' @details
#' This function will define a grid of the specified resolution across the plot.
#' Each point is allocated to a grid location (i.e., pair of bins on the x- and y-axes).
#' If multiple points are allocated to a given location, only the last/right-most point is retained. 
#' This mimics the fact that plotting will overwrite earlier points with later points.
#' In this manner, we can avoid unnecessary rendering of earlier points that would not show up anyway.
#'
#' Note that the \code{resolution} should be a positive integer less than the square root of the maximum integer size,
#' and will be coerced into the valid range if necessary.
#' This enables fast calculation of the grid locations for all points.
#' 
#' For plots where \code{X} and \code{Y} are originally categorical, use the jittered versions as input to this function.
#' 
#' @return A logical vector indicating which points should be retained.
#' 
#' @author Aaron Lun
#' @export 
subsetPointsByGrid <- function(X, Y, resolution=200) {
    # Avoid integer overflow when computing ids.
    resolution <- max(resolution, 1L)
    resolution <- min(resolution, sqrt(.Machine$integer.max))
    resolution <- as.integer(resolution)

    # X and Y MUST be numeric.
    rangeX <- range(X)
    rangeY <- range(Y)

    binX <- (rangeX[2] - rangeX[1])/resolution
    xid <- (X - rangeX[1])/binX
    xid <- as.integer(xid)
        
    binY <- (rangeY[2] - rangeY[1])/resolution
    yid <- (Y - rangeY[1])/binY
    yid <- as.integer(yid)

    # Getting unique IDs, provided resolution^2 < .Machine$integer.max
    # We use fromLast=TRUE as the last points get plotted on top.
    id <- xid + yid * resolution 
    !duplicated(id, fromLast=TRUE)
}
