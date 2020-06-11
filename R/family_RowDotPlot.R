#' The RowDotPlot virtual class
#'
#' The RowDotPlot is a virtual class where each row in the \linkS4class{SummarizedExperiment} is represented by no more than one point (i.e., a \dQuote{dot}) in a brushable \link{ggplot} plot.
#' It provides slots and methods to control various aesthetics of the dots and to store the brush or lasso selection.
#'
#' @section Slot overview:
#' The following slots control coloring of the points:
#' \itemize{
#' \item \code{ColorByRowData}, a string specifying the \code{\link{rowData}} field for controlling point color,
#' if \code{ColorBy="Row data"} (see the \linkS4class{Panel} class).
#' Defaults to the first field.
#' \item \code{ColorBySampleNameAssay}, a string specifying the assay of the SummarizedExperiment object containing values to use for coloring,
#' if \code{ColorBy="Sample name"}.
#' Defaults to the name of the first assay.
#' \item \code{ColorByFeatureNameColor}, a string specifying the color to use for coloring an individual sample on the plot,
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
#' In addition, this class inherits all slots from its parent \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDotPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"RowDotPlot"} entry containing \code{valid.rowData.names}, a character vector of valid column data names (i.e., containing atomic values); \code{discrete.rowData.names}, a character vector of names for discrete columns; and \code{continuous.rowData.names}, a character vector of names of continuous columns.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{ColorByFeatAssay} with the first valid assay name in \code{se}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineInterface}(x, se, select_info)} defines the user interface for manipulating all slots in the \linkS4class{RowDotPlot}.
#' It will also create a data parameter box that can respond to specialized \code{\link{.defineDataInterface}}.
#' This will \emph{override} the \linkS4class{Panel} method.
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"RowSelectionSource"}, \code{"RowSelectionType"} and \code{"RowSelectionSaved"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots in the \linkS4class{RowDotPlot}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"row"} to indicate that a row selection is being transmitted.
#' \item \code{\link{.singleSelectionDimension}(x)} returns \code{"feature"} to indicate that a feature identity is being transmitted.
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
#' The method for \code{\link{.generateDotPlotData}} should create a \code{plot.data} data.frame with one row per row in the \linkS4class{SummarizedExperiment} object.
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
#' .defineInterface,RowDotPlot-method
#' .createObservers,RowDotPlot-method
#' .hideInterface,RowDotPlot-method
#' .multiSelectionDimension,RowDotPlot-method
#' .singleSelectionDimension,RowDotPlot-method
#' .defineVisualColorInterface,RowDotPlot-method
#' .defineVisualShapeInterface,RowDotPlot-method
#' .defineVisualSizeInterface,RowDotPlot-method
#' .defineVisualFacetInterface,RowDotPlot-method
#' .defineVisualPointInterface,RowDotPlot-method
#' @name RowDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowDotPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .colorByRowData, NA_character_)
    args <- .emptyDefault(args, .colorBySampNameAssay, NA_character_)
    args <- .emptyDefault(args, .colorByFeatNameColor, iSEEOptions$get("selected.color"))

    args <- .emptyDefault(args, .shapeByRowData, NA_character_)

    args <- .emptyDefault(args, .sizeByRowData, NA_character_)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
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
    if (!is.null(.getCachedCommonInfo(se, "RowDotPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    df <- rowData(se)
    displayable <- .findAtomicFields(df)

    subdf <- df[,displayable,drop=FALSE]
    discrete <- .whichGroupable(subdf)
    continuous <- .whichNumeric(subdf)

    .setCachedCommonInfo(se, "RowDotPlot",
        valid.rowData.names=displayable,
        discrete.rowData.names=displayable[discrete],
        continuous.rowData.names=displayable[continuous])
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "RowDotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    rdp_cached <- .getCachedCommonInfo(se, "RowDotPlot")
    dp_cached <- .getCachedCommonInfo(se, "DotPlot")

    available <- rdp_cached$valid.rowData.names
    x <- .replace_na_with_first(x, .colorByRowData, available)

    assays <- dp_cached$valid.assay.names
    if (length(assays)) {
        assays <- c(intersect(iSEEOptions$get("assay"), assays), assays)
        x <- .replace_na_with_first(x, .colorBySampNameAssay, assays)
    } else {
        x[[.colorBySampNameAssay]] <- NA_character_
    }

    discrete <- rdp_cached$discrete.rowData.names
    x <- .replace_na_with_first(x, .shapeByRowData, discrete)

    continuous <- rdp_cached$continuous.rowData.names
    x <- .replace_na_with_first(x, .sizeByRowData, continuous)

    x
})

#' @export
setMethod(".defineInterface", "RowDotPlot", function(x, se, select_info) {
    list(
        .create_data_param_box(x, se, select_info),
        .create_visual_box(x, se, select_info$single),
        .create_dotplot_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

#' @export
setMethod(".defineVisualColorInterface", "RowDotPlot", function(x, se, select_info) {
    covariates <- .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(x)
    colorby_field <- paste0(plot_name, "_", .colorByField)

    tagList(
        hr(),
        radioButtons(
            colorby_field, label="Color by:", inline=TRUE,
            choices=.define_color_options_for_row_plots(se, covariates, all_assays),
            selected=x[[.colorByField]]
        ),
        .conditional_on_radio(
            colorby_field, .colorByNothingTitle,
            colourInput(
                paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                value=x[[.colorByDefaultColor]])
        ),
        .conditional_on_radio(
            colorby_field, .colorByRowDataTitle,
            selectInput(
                paste0(plot_name, "_", .colorByRowData), label=NULL,
                choices=covariates, selected=x[[.colorByRowData]])
        ),
        .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
            selectizeInput(paste0(plot_name, "_", .colorByFeatName),
                label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
            selectInput(
                paste0(plot_name, "_", .colorByRowTable), label=NULL, choices=select_info$row,
                selected=.choose_link(x[[.colorByRowTable]], select_info$row)),
            colourInput(paste0(plot_name, "_", .colorByFeatNameColor), label=NULL,
                value=x[[.colorByFeatNameColor]]),
            checkboxInput(
                paste0(plot_name, "_", .colorByFeatDynamic), label="Use dynamic feature selection",
                value=x[[.colorByFeatDynamic]])
        ),
        .conditional_on_radio(colorby_field, .colorBySampNameTitle,
            selectizeInput(paste0(plot_name, "_", .colorBySampName),
                label=NULL, choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(
                paste0(plot_name, "_", .colorBySampNameAssay), label=NULL,
                choices=all_assays, selected=x[[.colorBySampNameAssay]]),
            selectInput(
                paste0(plot_name, "_", .colorByColTable), label=NULL, choices=select_info$column,
                selected=.choose_link(x[[.colorByColTable]], select_info$column)),
            checkboxInput(
                paste0(plot_name, "_", .colorBySampDynamic), label="Use dynamic sample selection",
                value=x[[.colorBySampDynamic]])
        )
    )
})

#' @export
setMethod(".defineVisualShapeInterface", "RowDotPlot", function(x, se) {
    discrete_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$discrete.rowData.names

    plot_name <- .getEncodedName(x)
    shapeby_field <- paste0(plot_name, "_", .shapeByField)

    if (length(discrete_covariates)) {
        tagList(
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) .shapeByRowDataTitle),
                selected=x[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, .shapeByRowDataTitle,
                selectInput(
                    paste0(plot_name, "_", .shapeByRowData), label=NULL,
                    choices=discrete_covariates, selected=x[[.shapeByRowData]])
            )
        )
    } else {
        NULL
    }
})

#' @export
setMethod(".defineVisualSizeInterface", "RowDotPlot", function(x, se) {
    numeric_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$continuous.rowData.names

    plot_name <- .getEncodedName(x)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)

    tagList(
        hr(),
        radioButtons(
            sizeby_field, label="Size by:", inline=TRUE,
            choices=c(.sizeByNothingTitle, if (length(numeric_covariates)) .sizeByRowDataTitle),
            selected=x[[.sizeByField]]
        ),
        .conditional_on_radio(
            sizeby_field, .sizeByNothingTitle,
            numericInput(
                paste0(plot_name, "_", .plotPointSize), label="Point size:",
                min=0, value=x[[.plotPointSize]])
        ),
        .conditional_on_radio(
            sizeby_field, .sizeByRowDataTitle,
            selectInput(paste0(plot_name, "_", .sizeByRowData), label=NULL,
                        choices=numeric_covariates, selected=x[[.sizeByRowData]])
        )
    )
})

#' @export
setMethod(".defineVisualPointInterface", "RowDotPlot", function(x, se) {
    numeric_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$continuous.rowData.names

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
setMethod(".defineVisualFacetInterface", "RowDotPlot", function(x, se) {
    discrete_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$discrete.rowData.names

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
setMethod(".hideInterface", "RowDotPlot", function(x, field) {
    if (field %in% c(.selectColSource, .selectColType, .selectColSaved, .selectColDynamic)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".createObservers", "RowDotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.colorByRowData, .colorBySampNameAssay,
            .shapeByRowData, .sizeByRowData, .colorByFeatNameColor),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .create_dimname_propagation_observer(plot_name, choices=rownames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectRowSource, type_field=.selectRowType, saved_field=.selectRowSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionDimension", "RowDotPlot", function(x) "row")

#' @export
setMethod(".singleSelectionDimension", "RowDotPlot", function(x) "feature")

###############################################################
# See ?.addDotPlotDataColor for documentation on these methods.

setMethod(".addDotPlotDataColor", "RowDotPlot", function(x, envir) {
    color_choice <- x[[.colorByField]]

    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- x[[.colorByRowData]]
        label <- covariate_name
        cmds <- sprintf("plot.data$ColorBy <- rowData(se)[, %s];", deparse(covariate_name))

    } else if (color_choice == .colorByFeatNameTitle) {
        chosen_gene <- x[[.colorByFeatName]]
        label <- chosen_gene
        cmds <- sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
            deparse(chosen_gene))

    } else if (color_choice  == .colorBySampNameTitle) {
        chosen_sample <- x[[.colorBySampName]]
        assay_choice <- x[[.colorBySampNameAssay]]
        label <- sprintf("%s\n(%s)", chosen_sample, assay_choice)
        cmds <- sprintf("plot.data$ColorBy <- assay(se, %s, withDimnames <- FALSE)[, %s];",
            deparse(assay_choice), deparse(chosen_sample))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ColorBy=label))
})

setMethod(".addDotPlotDataShape", "RowDotPlot", function(x, envir) {
    shape_choice <- x[[.shapeByField]]

    if (shape_choice == .shapeByRowDataTitle) {
        covariate_name <- x[[.shapeByRowData]]
        label <- covariate_name
        cmds <- sprintf("plot.data$ShapeBy <- rowData(se)[, %s];", deparse(covariate_name))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ShapeBy=label))
})

setMethod(".addDotPlotDataSize", "RowDotPlot", function(x, envir) {
    size_choice <- x[[.sizeByField]]

    if (size_choice == .sizeByRowDataTitle) {
        covariate_name <- x[[.sizeByRowData]]
        label <- covariate_name
        cmds <- sprintf("plot.data$SizeBy <- rowData(se)[, %s];", deparse(covariate_name))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(SizeBy=label))
})

setMethod(".addDotPlotDataFacets", "RowDotPlot", function(x, envir) {
    facet_cmds <- NULL
    labels <- list()

    facet_row <- x[[.facetByRow]]
    if (facet_row!=.noSelection) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- rowData(se)[, %s];", deparse(facet_row))
        labels$FacetRow <- facet_row
    }

    facet_column <- x[[.facetByColumn]]
    if (facet_column!=.noSelection) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- rowData(se)[, %s];", deparse(facet_column))
        labels$FacetColumn <- facet_column
    }

    .textEval(facet_cmds, envir)

    list(commands=facet_cmds, labels=labels)
})

setMethod(".addDotPlotDataSelected", "RowDotPlot", function(x, envir) {
    if (!exists("row_selected", envir=envir, inherits=FALSE)) {
        return(NULL)
    }

    cmds <- c(
        header1="",
        header2="# Receiving row point selection",
        SelectBy="plot.data$SelectBy <- rownames(plot.data) %in% unlist(row_selected);"
    )

    if (x[[.selectEffect]] == .selectRestrictTitle) {
        cmds["saved"] <- "plot.data.all <- plot.data;"
        cmds["subset"] <- "plot.data <- subset(plot.data, SelectBy);"
    }
    cmds["footer"] <- ""

    .textEval(cmds, envir)

    cmds
})

#' @importFrom ggplot2 scale_color_manual geom_point
setMethod(".colorDotPlot", "RowDotPlot", function(x, colorby, x_aes="X", y_aes="Y") {
    color_choice <- x[[.colorByField]]

    # This slightly duplicates the work in .define_colorby_for_row_plot(),
    # but this is necessary to separate the function of data acquisition and plot generation.
    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- x[[.colorByRowData]]
        cmds <- .create_color_scale("rowDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice == .colorByFeatNameTitle) {
        col_choice <- x[[.colorByFeatNameColor]]
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

    } else if (color_choice == .colorBySampNameTitle) {
        assay_choice <- x[[.colorBySampNameAssay]]
        .create_color_scale("assayColorMap", deparse(assay_choice), colorby)
    } else {
        .colorByNoneDotPlotScale(x)
    }
})
