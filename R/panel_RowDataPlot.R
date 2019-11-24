#' The column data plot panel
#'
#' Plots column data values. What more do I have to say?
#'
#' @section Constructor:
#' \code{RowDataPlot()} creates an instance of a RowDataPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases RowDataPlot RowDataPlot-class
#' .defineParamInterface,RowDataPlot-method
#' .createParamObservers,RowDataPlot-method
#' @name RowDataPlot
NULL

RowDataPlot <- function() {
    new("RowDataPlot")
}

#' @export
#' @importFrom shiny selectInput radioButtons
setMethod(".defineParamInterface", "RowDataPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    row_covariates <- colnames(rowData(se))

    plot.param <- list(
        selectInput(.input_FUN(.rowDataYAxis),
            label="Column of interest (Y-axis):",
            choices=row_covariates, selected=param_choices[[.rowDataYAxis]]),
        radioButtons(.input_FUN(.rowDataXAxis), label="X-axis:", inline=TRUE,
            choices=c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle),
            selected=param_choices[[.rowDataXAxis]]),
        .conditional_on_radio(.input_FUN(.rowDataXAxis),
            .rowDataXAxisRowDataTitle,
            selectInput(.input_FUN(.rowDataXAxisRowData),
                label="Column of interest (X-axis):",
                choices=row_covariates, selected=param_choices[[.rowDataXAxisRowData]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "RowDataPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    .define_box_observers(mode, id, .dataParamBoxOpen, input, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "RowDataPlot", function(x) "rowDataPlot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "RowDataPlot", function(x) .make_rowDataPlot)
