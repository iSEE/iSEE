#' The feature assay plot panel
#'
#' Plots feature assay values. What more do I have to say?
#'
#' @section Constructor:
#' \code{SampAssayPlot()} creates an instance of a SampAssayPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases SampAssayPlot SampAssayPlot-class
#' .defineParamInterface,SampAssayPlot-method
#' .createParamObservers,SampAssayPlot-method
#' @name SampAssayPlot
NULL

SampAssayPlot <- function() {
    new("SampAssayPlot")
}

#' @export
#' @importFrom shiny selectInput radioButtons
setMethod(".defineParamInterface", "SampAssayPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    link_sources <- .define_link_sources(active_panels)
    tab_by_col <- c(.noSelection, link_sources$col_tab)

    row_covariates <- colnames(rowData(se))
    sample_names <- .get_internal_info(se, "sample_names")

    all_assays <- .get_internal_info(se, "all_assays")

    xaxis_choices <- c(.sampAssayXAxisNothingTitle)
    if (length(row_covariates)) { # As it is possible for this plot to be _feasible_ but for no row data to exist.
        xaxis_choices <- c(xaxis_choices, .sampAssayXAxisRowDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .sampAssayXAxisSampNameTitle)

    plot.param <- list(
        selectInput(
            .input_FUN(.sampAssayYAxisSampName),
            label="Sample of interest (Y-axis):",
            choices=sample_names, selected=param_choices[[.sampAssayYAxisSampName]]),
        selectInput(
            .input_FUN(.sampAssayYAxisColTable), label=NULL, choices=tab_by_col,
            selected=.choose_link(param_choices[[.sampAssayYAxisColTable]], tab_by_col, force_default=TRUE)),
        selectInput(
            .input_FUN(.sampAssayAssay), label=NULL,
            choices=all_assays, selected=param_choices[[.sampAssayAssay]]),
        radioButtons(
            .input_FUN(.sampAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=param_choices[[.sampAssayXAxis]]),
        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisRowDataTitle,
            selectInput(
                .input_FUN(.sampAssayXAxisRowData),
                label="Row data of interest (X-axis):",
                choices=row_covariates, selected=param_choices[[.sampAssayXAxisRowData]])),
        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisSampNameTitle,
            selectInput(
                .input_FUN(.sampAssayXAxisSampName),
                label="Sample of interest (X-axis):",
                choices=sample_names, selected=param_choices[[.sampAssayXAxisSampName]]),
            selectInput(.input_FUN(.sampAssayXAxisColTable), label=NULL,
                choices=tab_by_col, selected=param_choices[[.sampAssayXAxisColTable]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "SampAssayPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    .define_box_observers(mode, id, .dataParamBoxOpen, input, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.sampAssayAssay, .sampAssayXAxisRowData),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    sample_choices <- seq_len(ncol(se))
    names(sample_choices) <- colnames(se)

    .define_dim_name_observer(mode, id, 
        name_field=.sampAssayXAxisSampName, 
        choices=sample_choices,
        in_use_field=.sampAssayXAxis, 
        in_use_value=.sampAssayXAxisSampNameTitle,
        is_protected=TRUE,
        table_field=.sampAssayXAxisColTable, 
        link_type="xaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(mode, id, 
        name_field=.sampAssayYAxisSampName, 
        choices=sample_choices,
        in_use_field=NA, 
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.sampAssayYAxisColTable, 
        link_type="yaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "SampAssayPlot", function(x) "sampAssayPlot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "SampAssayPlot", function(x) .make_sampAssayPlot)
