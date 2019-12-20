#' The ColumnDotPlot virtual class
#'
#' The ColumnDotPlot is a virtual class where each column in the \linkS4class{SummarizedExperiment} is represented by a point (\dQuote{dot}) in a brushable plot.
#' It provides slots and methods to control various aesthetics of the dots and to store the brush or lasso selection.
#'
#' @section Slot overview:
#' The following slots control coloring of the points:
#' \itemize{
#' \item \code{ColorByColData}, a string specifying the \code{\link{colData}} field for controlling point color,
#' if \code{ColorBy="Column data"} (see the \linkS4class{Panel} class).
#' Defaults to the first valid field (see \code{.refineParameters} below).
#' \item \code{ColorByFeatNameAssay}, a string specifying the assay of the SummarizedExperiment object containing values to use for coloring,
#' if \code{ColorBy="Feature name"}.
#' Defaults to the name of the first valid assay (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{ColorBySampNameColor}, a string specifying the color to use for coloring an individual sample on the plot,
#' if \code{ColorBy="Sample name"}.
#' Defaults to \code{"red"}.
#' }
#'
#' The following slots control other metadata-related aesthetic aspects of the points:
#' \itemize{
#' \item \code{ShapeByColData}, a string specifying the \code{\link{colData}} field for controlling point shape,
#' if \code{ShapeBy="Column data"} (see the \linkS4class{Panel} class).
#' The specified field should contain categorical values; defaults to the first such valid field.
#' \item \code{SizeByColData}, a string specifying the \code{\link{colData}} field for controlling point size,
#' if \code{SizeBy="Column data"} (see the \linkS4class{Panel} class).
#' The specified field should contain continuous values; defaults to the first such valid field.
#' }
#'
#' @section Contract description:
#' The ColumnDotPlot will provide user interface elements to change all above slots as well as slots in its parent classes \linkS4class{DotPlot} and \linkS4class{Panel}.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#'
#' Subclasses are expected to implement methods for (at least) \code{\link{.getCommandsDataXY}}.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColumnDotPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"ColumnDotPlot"} entry containing \code{valid.colData.names}, a character vector of names of columns that are valid (i.e., contain atomic values); \code{discrete.colData.names}, a character vector of names for columns with discrete atomic values; and \code{continuous.colData.names}, a character vector of names of columns with continuous atomic values.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{ColorByFeatAssay} with the first valid assay name in \code{se}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineInterface}(x, se, active_panels)} defines the user interface for manipulating all slots described above and in the parent classes.
#' This will \emph{override} the \linkS4class{Panel} method.
#' \item \code{\link{.hideInterfaceElement}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"SelectRowSource"}, \code{"SelectRowType"} and \code{"SelectRowSaved"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"column"} to indicate that a column selection is being transmitted.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{DotPlot} and \linkS4class{Panel} are also available.
#'
#' @seealso
#' \linkS4class{DotPlot}, for the immediate parent class that contains the actual slot definitions.
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases
#' initialize,ColumnDotPlot-method
#' .cacheCommonInfo,ColumnDotPlot-method
#' .refineParameters,ColumnDotPlot-method
#' .defineInterface,ColumnDotPlot-method
#' .createParamObservers,ColumnDotPlot-method
#' .hideInterfaceElement,ColumnDotPlot-method
#' .multiSelectionDimension,ColumnDotPlot-method
#' @name ColumnDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnDotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

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
        c(.colorByColData, .colorByFeatNameAssay, .colorBySampNameColor))

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
    x <- .replace_na_with_first(x, .facetByRow, discrete)
    x <- .replace_na_with_first(x, .facetByColumn, discrete)

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
setMethod(".defineInterface", "ColumnDotPlot", function(x, se, select_info) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    list(
        .create_visual_box_for_column_plots(mode, id, x, select_info$single$row, select_info$single$column, se),
        .create_dotplot_selection_param_box(mode, id, x, select_info$multi$row, select_info$multi$column)
    )
})

#' @export
setMethod(".hideInterfaceElement", "ColumnDotPlot", function(x, field) {
    if (field %in% c(.selectRowSource, .selectRowType, .selectRowSaved)) {
        TRUE
    } else {
        callNextMethod()
    }
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
setMethod(".multiSelectionDimension", "ColumnDotPlot", function(x) "column")

setMethod(".getCommandsDataColor", "ColumnDotPlot", function(x, se) {
    color_choice <- x[[.colorByField]]

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- x[[.colorByColData]]
        return(list(
            label=covariate_name,
            cmds=sprintf("plot.data$ColorBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else if (color_choice == .colorByFeatNameTitle) {
        # Set the color to the selected gene
        chosen_gene <- x[[.colorByFeatName]]
        assay_choice <- x[[.colorByFeatNameAssay]]
        return(list(
            label=.feature_axis_label(se, chosen_gene, assay_choice, multiline=TRUE),
            cmds=sprintf("plot.data$ColorBy <- assay(se, %s, withDimnames=FALSE)[%s, ];",
                deparse(assay_choice), deparse(chosen_gene))))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- x[[.colorBySampName]]
        return(list(
            label=.sample_axis_label(se, chosen_sample, assay_id=NULL),
            cmds=sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
                deparse(chosen_sample))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataShape", "ColumnDotPlot", function(x, se) {
    shape_choice <- x[[.shapeByField]]

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- x[[.shapeByColData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataSize", "ColumnDotPlot", function(x, se) {
    size_choice <- x[[.sizeByField]]

    if (size_choice == .sizeByColDataTitle) {
        covariate_name <- x[[.sizeByColData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$SizeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
})

setMethod(".getCommandsDataFacets", "ColumnDotPlot", function(x, se) {
    facet_cmds <- c()

    facet_row <- x[[.facetByRow]]
    if (facet_row!=.noSelection) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- colData(se)[, %s];", deparse(facet_row))
    }

    facet_column <- x[[.facetByColumn]]
    if (facet_column!=.noSelection) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- colData(se)[, %s];", deparse(facet_column))
    }

    return(facet_cmds)
})
