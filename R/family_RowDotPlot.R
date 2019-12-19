#' Row dot plot panel family
#'
#' The RowDotPlot is a virtual class where each row in the \linkS4class{SummarizedExperiment} is represented by a point (\dQuote{dot}) in a brushable plot.
#' It provides slots and methods to control various aesthetics of the dots and to store the brush or lasso selection.
#'
#' @section Slot overview:
#' The following slots control coloring of the points:
#' \itemize{
#' \item \code{ColorByRowData}, a string specifying the \code{\link{rowData}} field for controlling point color,
#' if \code{ColorBy="Row data"} (see the \linkS4class{Panel} class).
#' Defaults to the first field.
#' \item \code{ColorBySampNameAssay}, a string specifying the assay of the SummarizedExperiment object containing values to use for coloring,
#' if \code{ColorBy="Sample name"}.
#' Defaults to the name of the first assay.
#' \item \code{ColorByFeatNameColor}, a string specifying the color to use for coloring an individual sample on the plot,
#' if \code{ColorBy="Feature name"}.
#' Defaults to \code{"red"}.
#' }
#'
#' The following slots control other metadata-related aesthetic aspects of the points:
#' \itemize{
#' \item \code{ShapeByRowData}, a string specifying the \code{\link{rowData}} field for controlling point shape,
#' if \code{ShapeBy="Row data"} (see the \linkS4class{Panel} class).
#' The specified field should contain categorical values; defaults to the first such field.
#' \item \code{SizeByRowData}, a string specifying the \code{\link{rowData}} field for controlling point size,
#' if \code{SizeBy="Row data"} (see the \linkS4class{Panel} class).
#' The specified field should contain continuous values; defaults to the first such field.
#' }
#'
#' @section Contract description:
#' The RowDotPlot will provide user interface elements to change all above slots and in its parent classes \linkS4class{DotPlot} and \linkS4class{Panel}.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#'
#' Subclasses are expected to implement methods for (at least) \code{\link{.getCommandsDataXY}}.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDotPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"RowDotPlot"} entry containing \code{valid.colData.names}, a character vector of valid column data names (i.e., containing atomic values); \code{discrete.colData.names}, a character vector of names for discrete columns; and \code{continuous.colData.names}, a character vector of names of continuous columns.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{ColorByFeatAssay} with the first valid assay name in \code{se}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineParamInterface}(x, se, active_panels)} defines the user interface for manipulating all slots in the \linkS4class{RowDotPlot}.
#' This will \emph{override} the \linkS4class{Panel} method.
#' \item \code{\link{.hideInterfaceElement}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"SelectRowSource"}, \code{"SelectRowType"} and \code{"SelectRowSaved"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots in the \linkS4class{RowDotPlot}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.transmittedDimension}(x)} returns \code{"column"} to indicate that a column selection is being transmitted.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{DotPlot} and \linkS4class{Panel} are also available.
#'
#' @author Aaron Lun
#' @seealso
#' \linkS4class{DotPlot}, for the immediate parent class that contains the actual slot definitions.
#'
#' @docType methods
#' @aliases
#' initialize,RowDotPlot-method
#' .cacheCommonInfo,RowDotPlot-method
#' .refineParameters,RowDotPlot-method
#' .defineParamInterface,RowDotPlot-method
#' .createParamObservers,RowDotPlot-method
#' .hideInterfaceElement,RowDotPlot-method
#' .transmittedDimension,RowDotPlot-method
#' @name RowDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowDotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

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
        c(.colorByRowData, .colorBySampNameAssay, .colorByFeatNameColor))

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
    x <- .replace_na_with_first(x, .facetByRow, discrete)
    x <- .replace_na_with_first(x, .facetByColumn, discrete)

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
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    link_sources <- .define_link_sources(active_panels, exclude=paste0(mode, id))
    row_selectable <- c(.noSelection, link_sources$row)
    col_selectable <- c(.noSelection, link_sources$column)

    list(
        .create_visual_box_for_row_plots(mode, id, x, row_selectable, col_selectable, se),
        .create_dotplot_selection_param_box(mode, id, x, row_selectable, col_selectable)
    )
})

#' @export
setMethod(".hideInterfaceElement", "RowDotPlot", function(x, field) {
    if (field %in% c(.selectColSource, .selectColType, .selectColSaved)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".createParamObservers", "RowDotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_nonfundamental_parameter_observers(plot_name,
        fields=c(.colorByRowData, .colorBySampNameAssay,
            .shapeByRowData, .sizeByRowData, .colorByFeatNameColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_propagation_observer(plot_name, choices=rownames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)

    .define_selection_effect_observer(plot_name,
        by_field=.selectRowSource, type_field=.selectRowType, saved_field=.selectRowSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".transmittedDimension", "RowDotPlot", function(x) "row")

setMethod(".getCommandsDataColor", "RowDotPlot", function(x, se) {
    color_choice <- x[[.colorByField]]

    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- x[[.colorByRowData]]
        list(
            label=covariate_name,
            cmds=sprintf("plot.data$ColorBy <- rowData(se)[, %s];", deparse(covariate_name))
        )

    } else if (color_choice == .colorByFeatNameTitle) {
        chosen_gene <- x[[.colorByFeatName]]
        list(
            label=.feature_axis_label(se, chosen_gene, assay_id=NULL),
            cmds=sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
                deparse(chosen_gene))
        )

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- x[[.colorBySampName]]
        assay_choice <- x[[.colorBySampNameAssay]]
        list(
            label=.sample_axis_label(se, chosen_sample, assay_choice, multiline=TRUE),
            cmds=sprintf("plot.data$ColorBy <- assay(se, %i, withDimnames=FALSE)[, %i];",
                deparse(assay_choice), deparse(chosen_sample))
        )

    } else {
        NULL
    }
})

setMethod(".getCommandsDataShape", "RowDotPlot", function(x, se) {
    shape_choice <- x[[.shapeByField]]

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- x[[.shapeByColData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataSize", "RowDotPlot", function(x, se) {
    size_choice <- x[[.sizeByField]]

    if (size_choice == .sizeByRowDataTitle) {
        covariate_name <- x[[.sizeByRowData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$SizeBy <- rowData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataFacets", "RowDotPlot", function(x, se) {
    facet_cmds <- c()

    facet_row <- x[[.facetByRow]]
    if (facet_row!=.noSelection) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- rowData(se)[, %s];", deparse(facet_row))
    }

    facet_column <- x[[.facetByColumn]]
    if (facet_column!=.noSelection) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- rowData(se)[, %s];", deparse(facet_column))
    }

    return(facet_cmds)
})
