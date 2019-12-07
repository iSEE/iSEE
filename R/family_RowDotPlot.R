#' Row dot plot panel family
#'
#' The row dot plot panel family covers all plot panel types where each row of the \linkS4class{SummarizedExperiment} is represented by a point.
#' This family is represented by the \code{RowDotPlot} virtual class, which has a number of concrete subclasses (e.g., \linkS4class{SampAssayPlot}) to direct the construction of specific plots.
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
#' @aliases .defineParamInterface,RowDotPlot-method
#' .createParamObservers,RowDotPlot-method
#' @name RowDotPlot
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowDotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object <- .empty_default(.Object, .facetRowsByRowData)
    .Object <- .empty_default(.Object, .facetColumnsByRowData)

    .Object <- .empty_default(.Object, .colorByRowData)
    .Object <- .empty_default(.Object, .colorBySampNameAssay)
    .Object <- .empty_default(.Object, .colorByFeatNameColor, "red")

    .Object <- .empty_default(.Object, .shapeByRowData)

    .Object <- .empty_default(.Object, .sizeByRowData)

    .Object
})

#' @importFrom S4Vectors isSingleString setValidity2
setValidity2("RowDotPlot", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object,
        c(.facetRowsByRowData, .facetColumnsByRowData,
            .colorByRowData, .colorBySampNameAssay, .colorByFeatNameColor))

    msg <- .allowable_choice_error(msg, object, .colorByField,
          c(.colorByNothingTitle, .colorByRowDataTitle, .colorByFeatNameTitle, .colorBySampNameTitle))

    msg <- .allowable_choice_error(msg, object, .shapeByField,
          c(.shapeByNothingTitle, .shapeByRowDataTitle))

    msg <- .allowable_choice_error(msg, object, .sizeByField,
          c(.sizeByNothingTitle, .sizeByRowDataTitle))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom SummarizedExperiment rowData
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "RowDotPlot", function(x, se) {
    if (is.null(.get_common_info(se, "RowDotPlot"))) {
        df <- rowData(se)
        displayable <- .find_atomic_fields(df)

        subdf <- df[,displayable,drop=FALSE]
        discrete <- .which_groupable(subdf)
        continuous <- .which_numeric(subdf)

        se <- .set_common_info(se, "RowDotPlot",
            valid.rowData.names=displayable,
            discrete.rowData.names=displayable[discrete],
            continuous.rowData.names=displayable[continuous])
    }

    callNextMethod()
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "RowDotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    rdp_cached <- .get_common_info(se, "RowDotPlot")
    dp_cached <- .get_common_info(se, "DotPlot")

    discrete <- rdp_cached$discrete.rowData.names
    x <- .replace_na_with_first(x, .facetRowsByRowData, discrete)
    x <- .replace_na_with_first(x, .facetColumnsByRowData, discrete)

    available <- rdp_cached$valid.rowData.names
    x <- .replace_na_with_first(x, .colorByRowData, available)

    assays <- dp_cached$valid.assay.names
    x <- .replace_na_with_first(x, .colorBySampNameAssay, assays)

    x <- .replace_na_with_first(x, .shapeByRowData, discrete)

    continuous <- rdp_cached$continuous.rowData.names
    x <- .replace_na_with_first(x, .sizeByRowData, continuous)

    x
})

#' @export
setMethod(".defineParamInterface", "RowDotPlot", function(x, se, active_panels) {
    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)
    tab_by_col <- c(.noSelection, link_sources$col_tab)
    row_selectable <- c(.noSelection, link_sources$row_plot)

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    list(
        .create_visual_box_for_row_plots(mode, id, x, tab_by_row, tab_by_col, se),
        .create_selection_param_box(mode, id, x, row_selectable, "row")
    )
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "RowDotPlot", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_nonfundamental_parameter_observers(plot_name,
        fields=c(.colorByRowData, .colorBySampNameAssay,
            .shapeByRowData, .sizeByRowData, .colorByFeatNameColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

setMethod(".getCommandsDataColor", "RowDotPlot", function(x, param_choices, se) {
    color_choice <- param_choices[[.colorByField]]

    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        list(
            label=covariate_name,
            cmds=sprintf("plot.data$ColorBy <- rowData(se)[, %s];", deparse(covariate_name))
        )

    } else if (color_choice == .colorByFeatNameTitle) {
        chosen_gene <- param_choices[[.colorByFeatName]]
        list(
            label=.feature_axis_label(se, chosen_gene, assay_id=NULL),
            cmds=sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
                deparse(chosen_gene))
        )

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- param_choices[[.colorBySampName]]
        assay_choice <- param_choices[[.colorBySampNameAssay]]
        list(
            label=.sample_axis_label(se, chosen_sample, assay_choice, multiline=TRUE),
            cmds=sprintf("plot.data$ColorBy <- assay(se, %i, withDimnames=FALSE)[, %i];",
                deparse(assay_choice), deparse(chosen_sample))
        )

    } else {
        NULL
    }
})

setMethod(".getCommandsDataShape", "RowDotPlot", function(x, param_choices, se) {
    shape_choice <- param_choices[[.shapeByField]]

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- param_choices[[.shapeByColData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataSize", "RowDotPlot", function(x, param_choices, se) {
    size_choice <- param_choices[[.sizeByField]]

    if (size_choice == .sizeByRowDataTitle) {
        covariate_name <- param_choices[[.sizeByRowData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$SizeBy <- rowData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})
