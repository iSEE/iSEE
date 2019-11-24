#' The feature assay plot panel
#'
#' Plots feature assay values. What more do I have to say?
#'
#' @section Constructor:
#' \code{FeatAssayPlot()} creates an instance of a FeatAssayPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases FeatAssayPlot FeatAssayPlot-class
#' .defineParamInterface,FeatAssayPlot-method
#' .createParamObservers,FeatAssayPlot-method
#' @name FeatAssayPlot
NULL

FeatAssayPlot <- function() {
    new("FeatAssayPlot")
}

#' @export
#' @importFrom shiny selectInput radioButtons
setMethod(".defineParamInterface", "FeatAssayPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)

    column_covariates <- colnames(colData(se))
    xaxis_choices <- c(.featAssayXAxisNothingTitle)
    if (length(column_covariates)) { 
        # As it is possible for this plot to be _feasible_ but for no column data to exist.
        xaxis_choices <- c(xaxis_choices, .featAssayXAxisColDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .featAssayXAxisFeatNameTitle)

    all_assays <- .get_internal_info(se, "all_assays")

    plot.param <- list(
        selectizeInput(.input_FUN(.featAssayYAxisFeatName),
            label="Y-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(.input_FUN(.featAssayYAxisRowTable), label=NULL, choices=tab_by_row,
            selected=.choose_link(param_choices[[.featAssayYAxisRowTable]], tab_by_row, force_default=TRUE)),
        selectInput(.input_FUN(.featAssayAssay), label=NULL,
            choices=all_assays, selected=param_choices[[.featAssayAssay]]),
        radioButtons(.input_FUN(.featAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=param_choices[[.featAssayXAxis]]),
        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisColDataTitle,
            selectInput(.input_FUN(.featAssayXAxisColData),
                label="X-axis column data:",
                choices=column_covariates, selected=param_choices[[.featAssayXAxisColData]])),
        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisFeatNameTitle,
            selectizeInput(.input_FUN(.featAssayXAxisFeatName),
                label="X-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.featAssayXAxisRowTable), label=NULL,
                choices=tab_by_row, selected=param_choices[[.featAssayXAxisRowTable]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "FeatAssayPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_plot_parameter_observers(mode, id,
        protected=c(.featAssayAssay, .featAssayXAxisColData),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    .define_dim_name_observer(mode, id, 
        name_field=.featAssayXAxisFeatName, 
        choices=feature_choices,
        in_use_field=.featAssayXAxis, 
        in_use_value=.featAssayXAxisFeatNameTitle,
        is_protected=TRUE,
        table_field=.featAssayXAxisRowTable, 
        link_type="xaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(mode, id, 
        name_field=.featAssayYAxisFeatName, 
        choices=feature_choices,
        in_use_field=NA, 
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.featAssayYAxisRowTable, 
        link_type="yaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "FeatAssayPlot", function(x) "featAssayPlot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "FeatAssayPlot", function(x) .make_featAssayPlot)
