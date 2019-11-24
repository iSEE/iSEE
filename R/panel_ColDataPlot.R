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
#'
#' @docType methods
#' @aliases ColDataPlot ColDataPlot-class
#' .defineParamInterface,ColDataPlot-method
#' .createParamObservers,ColDataPlot-method
#' @name ColDataPlot
NULL

ColDataPlot <- function() {
    new("ColDataPlot")
}

#' @export
#' @importFrom shiny selectInput radioButtons
setMethod(".defineParamInterface", "ColDataPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    column_covariates <- colnames(colData(se))

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
