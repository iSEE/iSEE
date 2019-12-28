#' The ColDataPlot panel
#'
#' The ColDotPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents a variable from the \code{\link{colData}} of a \linkS4class{SummarizedExperiment} object.
#' It provides slots and methods for specifying which column metadata variable to use and what to plot on the x-axis.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{YAxis}, a string specifying the column of the \code{\link{colData}} to show on the y-axis.
#' If \code{NA}, defaults to the first valid field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"}).
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"} or \code{"Column data"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColData}, string specifying the column of the \code{\link{colData}} to show on the x-axis.
#' If \code{NA}, defaults to the first valid field.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{ColDataPlot(...)} creates an instance of a ColDataPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Contract description:
#' The ColDataPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after replacing any \code{NA} value in \code{YAxis} or \code{XAxisColData} with the name of the first valid \code{\link{colData}} field.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no valid column metadata fields are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.fullName}(x)} will return the full name of the panel class.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Column data plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x)} will return a list of plotting information, including a character vector of commands to construct a data.frame of column metadata variables.
#' }
#'
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{ColumnDotPlot}, for the immediate parent class.
#'
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
#' initialize,ColDataPlot-method
#' .refineParameters,ColDataPlot-method
#' .defineDataInterface,ColDataPlot-method
#' .createObservers,ColDataPlot-method
#' .fullName,ColDataPlot-method
#' .panelColor,ColDataPlot-method
#' .generateDotPlotData,ColDataPlot-method
#'
#' @name ColDataPlot-class
NULL

#' @export
ColDataPlot <- function(...) {
    new("ColDataPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColDataPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .colDataXAxis, .colDataXAxisNothingTitle)
    args <- .empty_default(args, .colDataXAxisColData, NA_character_)
    args <- .empty_default(args, .colDataYAxis, NA_character_)
    do.call(callNextMethod, c(list(.Object), args))
})

.colDataXAxisNothingTitle <- "None"
.colDataXAxisColDataTitle <- "Column data"

#' @export
#' @importFrom methods callNextMethod
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

    for (field in c(.colDataXAxisColData, .colDataYAxis)) {
        x <- .replace_na_with_first(x, field, covariates)
    }

    x
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("ColDataPlot", function(object) {
    msg <- character(0)

    msg <- .allowable_choice_error(msg, object, .colDataXAxis,
        c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle))

    msg <- .single_string_error(msg, object,
        c(.colDataXAxisColData, .colDataYAxis))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "ColDataPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names

    list(
        selectInput(.input_FUN(.colDataYAxis),
            label="Column of interest (Y-axis):",
            choices=column_covariates, selected=x[[.colDataYAxis]]),
        radioButtons(.input_FUN(.colDataXAxis), label="X-axis:", inline=TRUE,
            choices=c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle),
            selected=x[[.colDataXAxis]]),
        .conditional_on_radio(.input_FUN(.colDataXAxis),
            .colDataXAxisColDataTitle,
            selectInput(.input_FUN(.colDataXAxisColData),
                label="Column of interest (X-axis):",
                choices=column_covariates, selected=x[[.colDataXAxisColData]]))
    )
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "ColDataPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .create_protected_parameter_observers(plot_name,
        fields=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".fullName", "ColDataPlot", function(x) "Column data plot")

#' @export
setMethod(".panelColor", "ColDataPlot", function(x) "#DB0230")

#' @export
setMethod(".generateDotPlotData", "ColDataPlot", function(x, envir) {
    data_cmds <- list()

    y_lab <- x[[.colDataYAxis]]
    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=colData(se)[, %s], row.names=colnames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    if (x[[.colDataXAxis]] == .colDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    } else {
        x_lab <- x[[.colDataXAxisColData]]
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- colData(se)[, %s];",
            deparse(x_lab)
        )
    }

    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    data_cmds <- unlist(data_cmds)
    .text_eval(data_cmds, envir)

    list(data_cmds=data_cmds, plot_title=plot_title, x_lab=x_lab, y_lab=y_lab)
})
