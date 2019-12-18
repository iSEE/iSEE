#' The RowDataPlot panel
#'
#' The RowDotPlot is a panel class for creating a \linkS4class{RowDotPlot} where the y-axis represents a variable from the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment} object.
#' It provides slots and methods for specifying which row metadata variable to use and what to plot on the x-axis.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{YAxis}, a string specifying the row of the \code{\link{rowData}} to show on the y-axis.
#' If \code{NA}, defaults to the first valid field (see \code{?"\link{.refineParameters,RowDotPlot-method}"}).
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"} or \code{"Row data"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisRowData}, string specifying the row of the \code{\link{rowData}} to show on the x-axis.
#' If \code{NA}, defaults to the first valid field.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{RowDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{RowDataPlot(...)} creates an instance of a RowDataPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Contract description:
#' The RowDataPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after replacing any \code{NA} value in \code{YAxis} or \code{XAxisRowData} with the name of the first valid \code{\link{rowData}} field.
#' This will also call the equivalent \linkS4class{RowDotPlot} method for further refinements to \code{x}.
#' If no valid row metadata fields are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineParamInterface}(x, se, active_panels)} defines the user interface for manipulating all slots described above and in the parent classes.
#' This is combined with the interface elements provided by the \linkS4class{RowDotPlot}. 
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{RowDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.getEncodedName}(x)} will return \code{"RowDataPlot"}.
#' \item \code{\link{.getFullName}(x)} will return \code{"Row data plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.getCommandsDataXY}(x)} will return a list of plotting information, including a character vector of commands to construct a data.frame of row metadata variables.
#' }
#'
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{RowDotPlot}, for the immediate parent class.
#'
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
#' .refineParameters,RowDataPlot-method
#' .defineParamInterface,RowDataPlot-method
#' .createParamObservers,RowDataPlot-method
#' .getEncodedName,RowDataPlot-method
#' .getFullName,RowDataPlot-method
#' .getCommandsDataXY,RowDataPlot-method
#' @name RowDataPlot-class
NULL

#' @export
RowDataPlot <- function(...) {
    new("RowDataPlot", ...)
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
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_box_observers(plot_name, .dataParamBoxOpen, input, pObjects)

    .define_protected_parameter_observers(plot_name,
        fields=c(.rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".getEncodedName", "RowDataPlot", function(x) "rowDataPlot") # TODO change to class name.

#' @export
setMethod(".getFullName", "RowDataPlot", function(x) "Row data plot") # TODO change to class name.

#' @export
setMethod(".getCommandsDataXY", "RowDataPlot", function(x, param_choices) {
    data_cmds <- list()

    y_lab <- param_choices[[.rowDataYAxis]]

    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=rowData(se)[, %s], row.names=rownames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    if (param_choices[[.rowDataXAxis]] == .rowDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)))"
    } else {
        x_lab <- param_choices[[.rowDataXAxisRowData]]
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))
    }

    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    return(list(data_cmds=data_cmds, plot_title=plot_title, x_lab=x_lab, y_lab=y_lab))
})
