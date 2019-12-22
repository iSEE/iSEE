#' Column statistics table panel
#'
#' Defines a \code{\link{datatable}} panel containing statistics
#' from the \code{\link{colData}} of a \linkS4class{SummarizedExperiment}.
#'
#' @section Constructor:
#' \code{ColStatTable()} creates an instance of a ColStatTable class.
#'
#' @section Panel parameters:
#' \code{\link{.defineInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- ColStatTable()
#' x[["Selected"]]
#' x[["Selected"]] <- 2L
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#'
#' # Search column refinement works as expected.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @name ColStatTable
#' @aliases ColStatTable ColStatTable-class
#' .renderOutput,ColStatTable-method
#' .getEncodedName,ColStatTable-method
NULL

#' @export
ColStatTable <- function() {
    new("ColStatTable")
}

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".cacheCommonInfo", "ColStatTable", function(x, se) {
    if (is.null(.get_common_info(se, "ColStatTable"))) {
        df <- colData(se)
        available <- .find_atomic_fields(df)
        se <- .set_common_info(se, "ColStatTable",
            valid.colData.names=available)
    }

    callNextMethod()
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".refineParameters", "ColStatTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    valid.names <- .get_common_info(se, "ColStatTable")$valid.colData.names
    df <- colData(se)

    # First, expanding out so that we cover all columns.
    search_vals <- x[[.TableColSearch]]
    N <- ncol(df)
    if (length(search_vals)!=N) {
        search_vals <- character(N)
    }

    # Then, contracting only to those columns that survived.
    keep <- match(colnames(df), valid.names)
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    x
})

#' @export
setMethod(".fullName", "ColStatTable", function(x) "Column statistics table")

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".getTableFunction", "ColStatTable", function(x) {
    function(param_choices, se, eval_env) {
        cmds <-"tab <- as.data.frame(colData(se));"

        if (exists("col_selected", envir=eval_env, inherits=FALSE)) {
            cmds <- c(cmds, "tab <- tab[unlist(col_selected),,drop=FALSE]")
        }

        valid.names <- .get_common_info(se, "ColStatTable")$valid.colData.names
        if (!identical(colnames(colData(se)), valid.names)) {
            cmds <- c(cmds, sprintf("tab <- tab[,%s,drop=FALSE]",
                paste(deparse(valid.names), collapse="\n     ")))
        }
        cmds
    }
})
