#' The ColumnDotPlot virtual class
#'
#' The ColumnDotPlot is a virtual class where each column in the \linkS4class{SummarizedExperiment} is represented by no more than one point (i.e., a \dQuote{dot}) in a brushable \link{ggplot} plot.
#' It provides slots and methods to control various aesthetics of the dots and to store the brush or lasso selection.
#'
#' @section Slot overview:
#' The following slots control coloring of the points:
#' \itemize{
#' \item \code{ColorByColumnData}, a string specifying the \code{\link{colData}} field for controlling point color,
#' if \code{ColorBy="Column data"} (see the \linkS4class{Panel} class).
#' Defaults to the first valid field (see \code{.refineParameters} below).
#' \item \code{ColorByFeatureNameAssay}, a string specifying the assay of the SummarizedExperiment object containing values to use for coloring,
#' if \code{ColorBy="Feature name"}.
#' Defaults to the name of the first valid assay (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{ColorBySampleNameColor}, a string specifying the color to use for coloring an individual sample on the plot,
#' if \code{ColorBy="Sample name"}.
#' Defaults to \code{"red"}.
#' }
#'
#' The following slots control other metadata-related aesthetic aspects of the points:
#' \itemize{
#' \item \code{ShapeByColumnData}, a string specifying the \code{\link{colData}} field for controlling point shape,
#' if \code{ShapeBy="Column data"} (see the \linkS4class{Panel} class).
#' The specified field should contain categorical values; defaults to the first such valid field.
#' \item \code{SizeByColumnData}, a string specifying the \code{\link{colData}} field for controlling point size,
#' if \code{SizeBy="Column data"} (see the \linkS4class{Panel} class).
#' The specified field should contain continuous values; defaults to the first such valid field.
#' }
#'
#' In addition, this class inherits all slots from its \linkS4class{DotPlot} and \linkS4class{Panel} classes.
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
#' \item \code{\link{.defineInterface}(x, se, select_info)} defines the user interface for manipulating all slots described above and in the parent classes.
#' It will also create a data parameter box that can respond to specialized \code{\link{.defineDataInterface}}.
#' This will \emph{override} the \linkS4class{Panel} method.
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"RowSelectionSource"}, \code{"RowSelectionType"} and \code{"RowSelectionSaved"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"column"} to indicate that a column selection is being transmitted.
#' \item \code{\link{.singleSelectionDimension}(x)} returns \code{"sample"} to indicate that a sample identity is being transmitted.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{DotPlot} and \linkS4class{Panel} are also available.
#'
#' @section Subclass expectations:
#' Subclasses are expected to implement methods for:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}}
#' \item \code{\link{.fullName}}
#' \item \code{\link{.panelColor}}
#' }
#'
#' The method for \code{\link{.generateDotPlotData}} should create a \code{plot.data} data.frame with one row per column in the \linkS4class{SummarizedExperiment} object.
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
#' .createObservers,ColumnDotPlot-method
#' .hideInterface,ColumnDotPlot-method
#' .multiSelectionDimension,ColumnDotPlot-method
#' .singleSelectionDimension,ColumnDotPlot-method
#' .defineVisualColorInterface,ColumnDotPlot-method
#' .defineVisualShapeInterface,ColumnDotPlot-method
#' .defineVisualSizeInterface,ColumnDotPlot-method
#' .defineVisualFacetInterface,ColumnDotPlot-method
#' .defineVisualPointInterface,ColumnDotPlot-method
#'
#' @name ColumnDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnDotPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .colorByColData, NA_character_)
    args <- .emptyDefault(args, .colorByFeatNameAssay, NA_character_)
    args <- .emptyDefault(args, .colorBySampNameColor, iSEEOptions$get("selected.color"))

    args <- .emptyDefault(args, .shapeByColData, NA_character_)

    args <- .emptyDefault(args, .sizeByColData, NA_character_)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
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
    if (!is.null(.getCachedCommonInfo(se, "ColumnDotPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    df <- colData(se)
    displayable <- .findAtomicFields(df)

    subdf <- df[,displayable,drop=FALSE]
    discrete <- .whichGroupable(subdf)
    continuous <- .whichNumeric(subdf)

    .setCachedCommonInfo(se, "ColumnDotPlot",
        valid.colData.names=displayable,
        discrete.colData.names=displayable[discrete],
        continuous.colData.names=displayable[continuous])
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "ColumnDotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    cdp_cached <- .getCachedCommonInfo(se, "ColumnDotPlot")
    dp_cached <- .getCachedCommonInfo(se, "DotPlot")

    available <- cdp_cached$valid.colData.names
    x <- .replace_na_with_first(x, .colorByColData, available)

    assays <- dp_cached$valid.assay.names
    if (length(assays)) {
        assays <- c(intersect(iSEEOptions$get("assay"), assays), assays)
        x <- .replace_na_with_first(x, .colorByFeatNameAssay, assays)
    } else {
        x[[.colorByFeatNameAssay]] <- NA_character_
    }

    discrete <- cdp_cached$discrete.colData.names
    x <- .replace_na_with_first(x, .shapeByColData, discrete)

    continuous <- cdp_cached$continuous.colData.names
    x <- .replace_na_with_first(x, .sizeByColData, continuous)

    x
})

#' @export
setMethod(".defineInterface", "ColumnDotPlot", function(x, se, select_info) {
    list(
        .create_data_param_box(x, se, select_info),
        .create_visual_box(x, se, select_info$single),
        .create_dotplot_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

#' @export
setMethod(".defineVisualColorInterface", "ColumnDotPlot", function(x, se, select_info) {
    covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(x)
    colorby_field <- paste0(plot_name, "_", .colorByField)

    tagList(
        hr(),
        radioButtons(
            colorby_field, label="Color by:", inline=TRUE,
            choices=.define_color_options_for_column_plots(se, covariates, all_assays),
            selected=x[[.colorByField]]
        ),
        .conditional_on_radio(
            colorby_field, .colorByNothingTitle,
            colourInput(paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                value=x[[.colorByDefaultColor]])
        ),
        .conditional_on_radio(
            colorby_field, .colorByColDataTitle,
            selectInput(paste0(plot_name, "_", .colorByColData), label=NULL,
                choices=covariates, selected=x[[.colorByColData]])
        ),
        .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
            selectizeInput(paste0(plot_name, "_", .colorByFeatName), label=NULL,
                choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(
                paste0(plot_name, "_", .colorByFeatNameAssay), label=NULL,
                choices=all_assays, selected=x[[.colorByFeatNameAssay]]),
            selectInput(
                paste0(plot_name, "_", .colorByRowTable), label=NULL, choices=select_info$row,
                selected=.choose_link(x[[.colorByRowTable]], select_info$row)),
            checkboxInput(
                paste0(plot_name, "_", .colorByFeatDynamic), label="Use dynamic feature selection for coloring",
                value=x[[.colorByFeatDynamic]])
        ),
        .conditional_on_radio(colorby_field, .colorBySampNameTitle,
            selectizeInput(paste0(plot_name, "_", .colorBySampName),
                label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
            selectInput(
                paste0(plot_name, "_", .colorByColTable), label=NULL, choices=select_info$column,
                selected=.choose_link(x[[.colorByColTable]], select_info$column)),
            colourInput(
                paste0(plot_name, "_", .colorBySampNameColor), label=NULL,
                value=x[[.colorBySampNameColor]]),
            checkboxInput(
                paste0(plot_name, "_", .colorBySampDynamic), label="Use dynamic sample selection for coloring",
                value=x[[.colorBySampDynamic]])
        )
    )
})

#' @export
setMethod(".defineVisualShapeInterface", "ColumnDotPlot", function(x, se) {
    discrete_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$discrete.colData.names

    plot_name <- .getEncodedName(x)
    shapeby_field <- paste0(plot_name, "_", .shapeByField)

    if (length(discrete_covariates)) {
        tagList(
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) .shapeByColDataTitle),
                selected=x[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, .shapeByColDataTitle,
                selectInput(
                    paste0(plot_name, "_", .shapeByColData), label=NULL,
                    choices=discrete_covariates, selected=x[[.shapeByColData]])
            )
        )
    } else {
        NULL
    }
})

#' @export
setMethod(".defineVisualSizeInterface", "ColumnDotPlot", function(x, se) {
    numeric_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$continuous.colData.names

    plot_name <- .getEncodedName(x)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)

    tagList(
        hr(),
        radioButtons(
            sizeby_field, label="Size by:", inline=TRUE,
            choices=c(.sizeByNothingTitle, if (length(numeric_covariates)) .sizeByColDataTitle),
            selected=x[[.sizeByField]]
        ),
        .conditional_on_radio(
            sizeby_field, .sizeByNothingTitle,
            numericInput(
                paste0(plot_name, "_", .plotPointSize), label="Point size:",
                min=0, value=x[[.plotPointSize]])
        ),
        .conditional_on_radio(
            sizeby_field, .sizeByColDataTitle,
            selectInput(paste0(plot_name, "_", .sizeByColData), label=NULL,
                choices=numeric_covariates, selected=x[[.sizeByColData]])
        )
    )
})

#' @export
setMethod(".defineVisualPointInterface", "ColumnDotPlot", function(x, se) {
    numeric_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$continuous.colData.names

    plot_name <- .getEncodedName(x)
    sizeby_field <- paste0(plot_name, "_", .shapeByField)

    tagList(
        hr(),
        .add_point_UI_elements(x),
        checkboxInput(
            inputId=paste0(plot_name, "_", .contourAdd),
            label="Add contour (scatter only)",
            value=FALSE),
        .conditional_on_check_solo(
            paste0(plot_name, "_", .contourAdd),
            on_select=TRUE,
            colourInput(
                paste0(plot_name, "_", .contourColor), label=NULL,
                value=x[[.contourColor]]))
    )
})

#' @export
setMethod(".defineVisualFacetInterface", "ColumnDotPlot", function(x, se) {
    discrete_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$discrete.colData.names

    if (length(discrete_covariates)) {
        tagList(
            hr(),
            .add_facet_UI_elements(x, discrete_covariates)
        )
    } else {
        NULL
    }

})

#' @export
setMethod(".hideInterface", "ColumnDotPlot", function(x, field) {
    if (field %in% c(.selectRowSource, .selectRowType, .selectRowSaved, .selectRowDynamic)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".createObservers", "ColumnDotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.colorByColData, .colorByFeatNameAssay,
            .shapeByColData, .sizeByColData, .colorBySampNameColor),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .create_dimname_propagation_observer(plot_name, choices=colnames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionDimension", "ColumnDotPlot", function(x) "column")

#' @export
setMethod(".singleSelectionDimension", "ColumnDotPlot", function(x) "sample")

###############################################################
# See ?.addDotPlotDataColor for documentation on these methods.

setMethod(".addDotPlotDataColor", "ColumnDotPlot", function(x, envir) {
    color_choice <- x[[.colorByField]]

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- x[[.colorByColData]]
        label <- covariate_name
        cmds <- sprintf("plot.data$ColorBy <- colData(se)[, %s];", deparse(covariate_name))

    } else if (color_choice == .colorByFeatNameTitle) {
        # Set the color to the selected gene
        chosen_gene <- x[[.colorByFeatName]]
        assay_choice <- x[[.colorByFeatNameAssay]]
        label <- sprintf("%s\n(%s)", chosen_gene, assay_choice)
        cmds <- sprintf("plot.data$ColorBy <- assay(se, %s, withDimnames=FALSE)[%s, ];",
            deparse(assay_choice), deparse(chosen_gene))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- x[[.colorBySampName]]
        label <- chosen_sample
        cmds <- sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
            deparse(chosen_sample))
    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ColorBy=label))
})

setMethod(".addDotPlotDataShape", "ColumnDotPlot", function(x, envir) {
    shape_choice <- x[[.shapeByField]]

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- x[[.shapeByColData]]
        label <- covariate_name
        cmds <- sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ShapeBy=label))
})

setMethod(".addDotPlotDataSize", "ColumnDotPlot", function(x, envir) {
    size_choice <- x[[.sizeByField]]

    if (size_choice == .sizeByColDataTitle) {
        covariate_name <- x[[.sizeByColData]]
        label <- covariate_name
        cmds <- sprintf("plot.data$SizeBy <- colData(se)[, %s];", deparse(covariate_name))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(SizeBy=label))
})

setMethod(".addDotPlotDataFacets", "ColumnDotPlot", function(x, envir) {
    facet_cmds <- NULL
    labels <- list()

    facet_row <- x[[.facetByRow]]
    if (facet_row!=.noSelection) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- colData(se)[, %s];", deparse(facet_row))
        labels$FacetRow <- facet_row
    }

    facet_column <- x[[.facetByColumn]]
    if (facet_column!=.noSelection) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- colData(se)[, %s];", deparse(facet_column))
        labels$FacetColumn <- facet_column
    }

    .textEval(facet_cmds, envir)

    list(commands=facet_cmds, labels=labels)
})

setMethod(".addDotPlotDataSelected", "ColumnDotPlot", function(x, envir) {
    if (!exists("col_selected", envir=envir, inherits=FALSE)) {
        return(NULL)
    }

    cmds <- c(
        header="# Receiving column point selection",
        SelectBy="plot.data$SelectBy <- rownames(plot.data) %in% unlist(col_selected);"
    )

    if (x[[.selectEffect]] == .selectRestrictTitle) {
        cmds["saved"] <- "plot.data.all <- plot.data;"
        cmds["subset"] <- "plot.data <- subset(plot.data, SelectBy);"
    }

    .textEval(cmds, envir)

    cmds
})

setMethod(".colorDotPlot", "ColumnDotPlot", function(x, colorby, x_aes="X", y_aes="Y") {
    color_choice <- x[[.colorByField]]

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- x[[.colorByColData]]
        .create_color_scale("colDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice == .colorByFeatNameTitle) {
        assay_choice <- x[[.colorByFeatNameAssay]]
        .create_color_scale("assayColorMap", deparse(assay_choice), colorby)

    } else if (color_choice == .colorBySampNameTitle) {
        col_choice <- x[[.colorBySampNameColor]]
        c(
            sprintf(
                "scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +",
                deparse(col_choice)
            ),
            sprintf(
                "geom_point(aes(x=%s, y=%s), data=subset(plot.data, ColorBy == 'TRUE'), col=%s, alpha=1%s) +",
                x_aes, y_aes, deparse(col_choice),
                ifelse(x[[.sizeByField]] == .sizeByNothingTitle,
                    paste0(", size=5*", x[[.plotPointSize]]),
                    ""
                )
            )
        )
    } else {
        .colorByNoneDotPlotScale(x)
    }
})
