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

    df[which(keep)[retained],]
}
