#' The column data plot panel
#'
#' Plots column data values. What more do I have to say?
#'
#' @section Constructor:
#' \code{ColDataPlot()} creates an instance of a ColDataPlot class.
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
#' x <- ColDataPlot()
#' x[["XAxis"]]
#' x[["XAxis"]] <- "Column data"
#' 
#' ##################
#' # For developers #
#' ##################
#' 
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' old_cd <- colData(sce)
#' colData(sce) <- NULL
#' 
#' # Spits out a NULL and a warning if there is nothing to plot.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#' 
#' # Replaces the default with something sensible.
#' colData(sce) <- old_cd
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases ColDataPlot ColDataPlot-class
#' .defineParamInterface,ColDataPlot-method
#' .createParamObservers,ColDataPlot-method
#' @name ColDataPlot
NULL

#' @export
ColDataPlot <- function() {
    new("ColDataPlot")
}

#' @export
setMethod("initialize", "ColDataPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .colDataXAxis, .colDataXAxisNothingTitle)
    .Object <- .empty_default(.Object, .colDataXAxisColData)
    .Object <- .empty_default(.Object, .colDataYAxis)
    .Object
})

.colDataXAxisNothingTitle <- "None"
.colDataXAxisColDataTitle <- "Column data"

#' @export
setMethod(".refineParameters", "ColDataPlot", function(x, se) {
    x <- callNextMethod() # Do this first to trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    if (length(covariates)==0L) {
        warning(sprintf("no valid 'colData' fields for '%s'", class(x)[1]))
        return(NULL)
    }

    xchoice <- x[[.colDataXAxis]]
    if (!xchoice %in% c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle)) { 
        x[[.colDataXAxis]] <- .colDataXAxisNothingTitle
    }

    xcol <- x[[.colDataXAxisColData]]
    if (is.na(xcol) || !xcol %in% covariates) {
        x[[.colDataXAxisColData]] <- covariates[1]
    }

    ycol <- x[[.colDataYAxis]]
    if (is.na(ycol) || !ycol %in% covariates) {
        x[[.colDataYAxis]] <- covariates[1]
    }

    x
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("ColDataPlot", function(object) {
    msg <- character(0)

    allowable <- c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle)
    if (!object[[.colDataXAxis]] %in% allowable) {
        msg <- c(msg, sprintf("choice of '%s' should be one of %s", .colDataXAxis, 
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }

    for (field in c(.colDataXAxisColData, .colDataYAxis)) {
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
setMethod(".defineParamInterface", "ColDataPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    column_covariates <- .get_common_info(se, "ColDataPlot")$valid.colData.names

    plot.param <- list(
        selectInput(.input_FUN(.colDataYAxis),
            label="Column of interest (Y-axis):",
            choices=column_covariates, selected=param_choices[[.colDataYAxis]]),
        radioButtons(.input_FUN(.colDataXAxis), label="X-axis:", inline=TRUE,
            choices=c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle),
            selected=param_choices[[.colDataXAxis]]),
        .conditional_on_radio(.input_FUN(.colDataXAxis),
            .colDataXAxisColDataTitle,
            selectInput(.input_FUN(.colDataXAxisColData),
                label="Column of interest (X-axis):",
                choices=column_covariates, selected=param_choices[[.colDataXAxisColData]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "ColDataPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_plot_parameter_observers(mode, id,
        protected=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "ColDataPlot", function(x) "colDataPlot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "ColDataPlot", function(x) .make_colDataPlot)
