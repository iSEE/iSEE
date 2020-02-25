#' Subset points for faster plotting
#'
#' Subset points using a grid-based system, to avoid unnecessary rendering when plotting.
#' 
#' @param X A numeric vector of x-coordinates for all points.
#' @param Y A numeric vector of y-coordinates for all points, of the same length as \code{X}.
#' @param resolution A positive integer specifying the number of bins on each axis of the grid.
#'
#' Alternatively, if \code{grouping} is specified, 
#' this may be a named integer vector containing the number of bins to be used for each level.
#' @param grouping A character vector of length equal to \code{X} specifying the group to which each point is assigned.
#' By default, all points belong to the same group.
#' 
#' @details
#' This function will define a grid of the specified resolution across the plot.
#' Each point is allocated to a grid location (i.e., pair of bins on the x- and y-axes).
#' If multiple points are allocated to a given location, only the last/right-most point is retained. 
#' This mimics the fact that plotting will overwrite earlier points with later points.
#' In this manner, we can avoid unnecessary rendering of earlier points that would not show up anyway.
#'
#' If \code{grouping} is specified, redundant points are only identified within each unique level.
#' The resolution of downsampling within each level can be varied by passing an integer vector to \code{resolution}.
#' This can be useful for tuning the downsampling when points differ in importance,
#' e.g., in a MA plot, points corresponding to non-DE genes can be aggressively downsampled 
#' while points corresponding to DE genes should generally be retained.
#' 
#' For plots where \code{X} and \code{Y} are originally categorical, use the jittered versions as input to this function.
#' 
#' @return A logical vector indicating which points should be retained.
#' 
#' @examples
#' X <- rnorm(100000)
#' Y <- X + rnorm(100000)
#'
#' summary(subsetPointsByGrid(X, Y, resolution=100))
#'
#' summary(subsetPointsByGrid(X, Y, resolution=200))
#'
#' summary(subsetPointsByGrid(X, Y, resolution=1000))
#'
#' @author Aaron Lun
#' 
#' @export 
#' @importFrom S4Vectors DataFrame
#' @importFrom BiocGenerics match
subsetPointsByGrid <- function(X, Y, resolution=200, grouping=NULL) {
    if (length(X)!=length(Y)) {
        stop("'X' and 'Y' must be of the same length")
    }

    if (!is.null(grouping)) {
        grouping <- as.character(grouping)
        if (length(grouping)!=length(X)) {
            stop("'X' and 'grouping' must be of the same length")
        }

        ugroups <- unique(grouping)
        if (length(resolution)==1L) {
            resolution <- rep(resolution, length.out=length(ugroups))
            names(resolution) <- ugroups
        }
        if (!all(ugroups %in% names(resolution))) {
            stop("a 'resolution' vector must be named with all levels in 'grouping'")
        }

        output <- logical(length(X))
        for (g in ugroups) {
            current <- grouping==g
            output[current] <- subsetPointsByGrid(X[current], Y[current], resolution=resolution[[g]])
        }
        return(output)
    }

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

    # Taking advantage of the special match,DataFrame-method.
    # We use fromLast=TRUE as the last points get plotted on top.
    id <- DataFrame(X=xid, Y=yid)
    !duplicated(id, fromLast=TRUE)
}
