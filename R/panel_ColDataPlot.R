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
#' @importFrom methods callNextMethod
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

    x <- .replace_na_with_first(x, .colDataXAxisColData, covariates)
    x <- .replace_na_with_first(x, .colDataYAxis, covariates)

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
setMethod(".defineParamInterface", "ColDataPlot", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names

    plot.param <- list(
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

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=x[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "ColDataPlot", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_protected_parameter_observers(plot_name,
        fields=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "ColDataPlot", function(x) "colDataPlot") # TODO change to class name.

#' @export
setMethod(".getFullName", "ColDataPlot", function(x) "Column data plot")

#' @export
setMethod(".getPlottingFunction", "ColDataPlot", function(x) .make_colDataPlot)

#' @export
setMethod(".getCommandsDataXY", "ColDataPlot", function(x, param_choices) {
    # TODO: refactor as a funtion living in a slot of x
    data_cmds <- list()

    y_lab <- param_choices[[.colDataYAxis]]
    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=colData(se)[, %s], row.names=colnames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    if (param_choices[[.colDataXAxis]] == .colDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    } else {
        x_lab <- param_choices[[.colDataXAxisColData]]
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- colData(se)[, %s];",
            deparse(x_lab)
        )
    }

    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    return(list(data_cmds=data_cmds, plot_title=plot_title, x_lab=x_lab, y_lab=y_lab))
})
