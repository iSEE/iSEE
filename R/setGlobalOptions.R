#' \itemize{
#' \item{\code{panel.color}}{Named character vector of colors.
#' The names of the vector should be set to the name of class to be overridden; if a class is not named here, its default color is used.
#' It is highly recommended to define colors as hex color codes (e.g., \code{"#1e90ff"}), for full compatibility with both HTML elements and R plots.}
#' \item{\code{color.maxlevels}}{Maximum number of levels for a categorical variable used for coloring.
#' Variables with more levels are coerced to numeric to avoid problems with an overly-large legend.
#' Defaults to 24.}
#' \item{\code{factor.maxlevels}}{Maximum number of levels for a categorical variable to be used anywhere in the app.
#' Variables with more levels are coerced to numeric to avoid rendering delays.
#' Defaults to 100.}
#' \item{\code{RowTable.select.details}}{A function that takes a string containing the name of a feature (i.e., the current selection in the \linkS4class{RowTable}) and returns a HTML element with more details.} 
#' \item{\code{ColumnTable.select.details}}{A function that takes a string containing the name of a sample (i.e., the current selection in the \linkS4class{ColumnTable}) and returns a HTML element with more details.}
#' }
#' @export
#' @importFrom S4Vectors metadata metadata<-
setGlobalAppOptions <- function(se, ...) {
    metadata(se) <- .setNestedList(metadata(se), c("iSEE", "options"), list(...))
    se
}

global.defaults <- list(
    color.maxlevels=24L,
    factor.maxlevels=100L,
    RowTable.extra.info = NULL,
    ColumnTable.extra.info = NULL
)

#' @export
#' @importFrom S4Vectors metadata
getGlobalAppOption <- function(se, field) {
    available <- metadata(se)[["iSEE"]][["options"]]
    if (!is.null(field)) {
        if (field %in% names(available)) {
            val <- available[[field]]
        } else {
            val <- global.defaults[[field]]
        }
    } else {
        val <- global.defaults
        val[names(available)] <- available
    }
    val
}
