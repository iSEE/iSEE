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
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    link_sources <- .define_link_sources(active_panels, exclude=paste0(mode, id))
    row_selectable <- c(.noSelection, link_sources$row)
    col_selectable <- c(.noSelection, link_sources$column)

    list(
        .create_visual_box_for_column_plots(mode, id, x, row_selectable, col_selectable, se),
        .create_selection_param_box(mode, id, x, row_selectable, col_selectable)
    )
})

#' @export
setMethod(".createParamObservers", "ColumnDotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_nonfundamental_parameter_observers(plot_name,
        fields=c(.colorByColData, .colorByFeatNameAssay,
            .shapeByColData, .sizeByColData, .colorBySampNameColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_propagation_observer(plot_name, choices=colnames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)

    .define_selection_effect_observer(plot_name, 
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved, 
        input=input, session=session, pObjects=pObjects, rObjects=rObjects) 
})

#' @export
setMethod(".transmittedDimension", "ColumnDotPlot", function(x) "column")

setMethod(".getCommandsDataColor", "ColumnDotPlot", function(x, param_choices, se) {
    color_choice <- param_choices[[.colorByField]]

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        return(list(
            label=covariate_name,
            cmds=sprintf("plot.data$ColorBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else if (color_choice == .colorByFeatNameTitle) {
        # Set the color to the selected gene
        chosen_gene <- param_choices[[.colorByFeatName]]
        assay_choice <- param_choices[[.colorByFeatNameAssay]]
        return(list(
            label=.feature_axis_label(se, chosen_gene, assay_choice, multiline=TRUE),
            cmds=sprintf("plot.data$ColorBy <- assay(se, %s, withDimnames=FALSE)[%s, ];",
                deparse(assay_choice), deparse(chosen_gene))))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- param_choices[[.colorBySampName]]
        return(list(
            label=.sample_axis_label(se, chosen_sample, assay_id=NULL),
            cmds=sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
                deparse(chosen_sample))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataShape", "ColumnDotPlot", function(x, param_choices, se) {
    shape_choice <- param_choices[[.shapeByField]]

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- param_choices[[.shapeByColData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataSize", "ColumnDotPlot", function(x, param_choices, se) {
    size_choice <- param_choices[[.sizeByField]]

    if (size_choice == .sizeByColDataTitle) {
        covariate_name <- param_choices[[.sizeByColData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$SizeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataFacets", "ColumnDotPlot", function(x, param_choices, se) {
    facet_cmds <- c()

    facet_row <- param_choices[[.facetRowsByColData]]
    if (param_choices[[.facetByRow]]) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- colData(se)[, %s];", deparse(facet_row))
    }

    facet_column <- param_choices[[.facetColumnsByColData]]
    if (param_choices[[.facetByColumn]]) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- colData(se)[, %s];", deparse(facet_column))
    }

    return(facet_cmds)
})
