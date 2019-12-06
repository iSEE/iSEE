#' Column dot plot panel family
#'
#' The column dot plot panel family covers all plot panel types where each column of the \linkS4class{SummarizedExperiment} is represented by a point.
#' This family is represented by the \code{ColumnDotPlot} virtual class, which has a number of concrete subclasses (e.g., \linkS4class{RedDimPlot}) to direct the construction of specific plots.
#' We provide a number of useful methods on this virtual class to make it easier for developers to define their own subclasses.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for visualization and point selection.
#' More details to be added.
#'
#' @section Output plot:
#' \code{\link{.createOutputElement}} will return the output of \code{\link{plotOutput}} with two-dimensional brushing.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases .defineParamInterface,ColumnDotPlot-method
#' .createParamObservers,ColumnDotPlot-method
#' @name ColumnDotPlot
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnDotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object <- .empty_default(.Object, .facetRowsByColData)
    .Object <- .empty_default(.Object, .facetColumnsByColData)

    .Object <- .empty_default(.Object, .colorByColData)
    .Object <- .empty_default(.Object, .colorByFeatNameAssay)
    .Object <- .empty_default(.Object, .colorBySampNameColor, "red")

    .Object <- .empty_default(.Object, .shapeByColData)

    .Object <- .empty_default(.Object, .sizeByColData)

    .Object
})

#' @importFrom S4Vectors isSingleString setValidity2
setValidity2("ColumnDotPlot", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object,
        c(.facetColumnsByColData, .facetRowsByColData,
            .colorByColData, .colorByFeatNameAssay, .colorBySampNameColor))

    msg <- .allowable_choice_error(msg, object, .colorByField,
        c(.colorByNothingTitle, .colorByColDataTitle, .colorByFeatNameTitle, .colorBySampNameTitle))

    msg <- .allowable_choice_error(msg, object, .shapeByField,
          c(.shapeByNothingTitle, .shapeByColDataTitle))

    msg <- .allowable_choice_error(msg, object, .sizeByField,
          c(.sizeByNothingTitle, .sizeByColDataTitle))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom SummarizedExperiment colData
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "ColumnDotPlot", function(x, se) {
    if (is.null(.get_common_info(se, "ColumnDotPlot"))) {
        df <- colData(se)
        displayable <- .find_atomic_fields(df)

        subdf <- df[,displayable,drop=FALSE]
        discrete <- .which_groupable(subdf)
        continuous <- .which_numeric(subdf)

        se <- .set_common_info(se, "ColumnDotPlot",
            valid.colData.names=displayable,
            discrete.colData.names=displayable[discrete],
            continuous.colData.names=displayable[continuous])
    }

    callNextMethod()
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "ColumnDotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    cdp_cached <- .get_common_info(se, "ColumnDotPlot")
    dp_cached <- .get_common_info(se, "DotPlot")

    discrete <- cdp_cached$discrete.colData.names
    x <- .replace_na_with_first(x, .facetRowsByColData, discrete)
    x <- .replace_na_with_first(x, .facetColumnsByColData, discrete)

    available <- cdp_cached$valid.colData.names
    x <- .replace_na_with_first(x, .colorByColData, available)

    assays <- dp_cached$valid.assay.names
    x <- .replace_na_with_first(x, .colorByFeatNameAssay, assays)

    x <- .replace_na_with_first(x, .shapeByColData, discrete)

    continuous <- cdp_cached$continuous.colData.names
    x <- .replace_na_with_first(x, .sizeByColData, continuous)

    x
})

#' @export
setMethod(".defineParamInterface", "ColumnDotPlot", function(x, se, active_panels) {
    id <- x[[.organizationId]]

    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)
    tab_by_col <- c(.noSelection, link_sources$col_tab)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    mode <- .getEncodedName(x)
    list(
        .create_visual_box_for_column_plots(mode, id, x, tab_by_row, tab_by_col, se),
        .create_selection_param_box(mode, id, x, col_selectable, "column")
    )
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "ColumnDotPlot", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_nonfundamental_parameter_observers(plot_name,
        fields=c(.colorByColData, .colorByFeatNameAssay,
            .shapeByColData, .sizeByColData, .colorBySampNameColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".getCommandsExtra", "ColumnDotPlot", function(x, data_cmds, param_choices, all_memory, all_coordinates, se, ...) {
    # TODO: this signature differs from ColumnDotPlot only for "by_row"
    setup_out <- .extract_plotting_data(data_cmds, param_choices, all_memory, all_coordinates, se, by_row=FALSE)

    xy <- setup_out$envir$plot.data # DO NOT MOVE below .downsample_points, as downsampling will alter the value in 'envir'.

    downsample_cmds <- .downsample_points(param_choices, setup_out$envir)

    plot_out <- .create_plot(setup_out$envir, param_choices, ..., color_lab=setup_out$color_lab, shape_lab=setup_out$shape_lab, size_lab=setup_out$size_lab, by_row=FALSE)

    return(list(cmd_list=c(setup_out$cmd_list, list(plot=c(downsample_cmds, plot_out$cmds))), xy=xy, plot=plot_out$plot))
})
