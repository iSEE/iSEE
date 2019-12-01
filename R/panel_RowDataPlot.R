#' The row data plot panel
#'
#' Plots row data values. What more do I have to say?
#'
#' @section Constructor:
#' \code{RowDataPlot()} creates an instance of a RowDataPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- RowDataPlot()
#' x[["XAxis"]]
#' x[["XAxis"]] <- "Row data"
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' # Spits out a NULL and a warning if is nothing to plot.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' rowData(sce)$Stuff <- runif(nrow(sce))
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases RowDataPlot RowDataPlot-class
#' .defineParamInterface,RowDataPlot-method
#' .createParamObservers,RowDataPlot-method
#' @name RowDataPlot
NULL

#' @export
RowDataPlot <- function() {
    new("RowDataPlot")
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowDataPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .rowDataXAxis, .rowDataXAxisNothingTitle)
    .Object <- .empty_default(.Object, .rowDataXAxisRowData)
    .Object <- .empty_default(.Object, .rowDataYAxis)
    .Object
})

.rowDataXAxisNothingTitle <- "None"
.rowDataXAxisRowDataTitle <- "Row data"

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "RowDataPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names

    if (length(covariates)==0L) {
        warning(sprintf("no atomic 'rowData' fields for '%s'", class(x)[1]))
        return(NULL)
    }

    xchoice <- x[[.rowDataXAxis]]
    if (!xchoice %in% c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle)) {
        x[[.rowDataXAxis]] <- .rowDataXAxisNothingTitle
    }

    xcol <- x[[.rowDataXAxisRowData]]
    if (is.na(xcol) || !xcol %in% covariates) {
        x[[.rowDataXAxisRowData]] <- covariates[1]
    }

    ycol <- x[[.rowDataYAxis]]
    if (is.na(ycol) || !ycol %in% covariates) {
        x[[.rowDataYAxis]] <- covariates[1]
    }

    x
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("RowDataPlot", function(object) {
    msg <- character(0)

    allowable <- c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle)
    if (!object[[.rowDataXAxis]] %in% allowable) {
        msg <- c(msg, sprintf("choice of '%s' should be one of %s", .rowDataXAxis,
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }

    for (field in c(.rowDataXAxisRowData, .rowDataYAxis)) {
        if (!isSingleString(val <- object[[field]])) {
            msg <- c(msg, sprintf("'%s' must be a single string", field))
        }
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineParamInterface", "RowDataPlot", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    row_covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names

    plot.param <- list(
        selectInput(.input_FUN(.rowDataYAxis),
            label="Column of interest (Y-axis):",
            choices=row_covariates, selected=x[[.rowDataYAxis]]),
        radioButtons(.input_FUN(.rowDataXAxis), label="X-axis:", inline=TRUE,
            choices=c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle),
            selected=x[[.rowDataXAxis]]),
        .conditional_on_radio(.input_FUN(.rowDataXAxis),
            .rowDataXAxisRowDataTitle,
            selectInput(.input_FUN(.rowDataXAxisRowData),
                label="Column of interest (X-axis):",
                choices=row_covariates, selected=x[[.rowDataXAxisRowData]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=x[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "RowDataPlot", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_box_observers(plot_name, .dataParamBoxOpen, input, pObjects)

    .define_protected_parameter_observers(plot_name,
        fields=c(.rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "RowDataPlot", function(x) "rowDataPlot") # TODO change to class name.

#' @export
setMethod(".getFullName", "RowDataPlot", function(x) "Row data plot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "RowDataPlot", function(x) .make_rowDataPlot)
