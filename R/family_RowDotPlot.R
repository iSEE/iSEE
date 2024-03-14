#' The RowDotPlot virtual class
#'
#' The RowDotPlot is a virtual class where each row in the \linkS4class{SummarizedExperiment} is represented by no more than one point (i.e., a \dQuote{dot}) in a brushable \link{ggplot} plot.
#' It provides slots and methods to extract \code{\link{rowData}} fields to control the per-point aesthetics on the plot.
#' This panel will transmit row identities in both its single and multiple selections, and it can receive multiple row selections but not multiple column selections.
#'
#' @section Slot overview:
#' The following slots control coloring of the points:
#' \itemize{
#' \item \code{ColorByRowData}, a string specifying the \code{\link{rowData}} field for controlling point color,
#' if \code{ColorBy="Row data"} (see the \linkS4class{Panel} class).
#' Defaults to the first valid field (see \code{.cacheCommonInfo} below).
#' \item \code{ColorBySampleNameAssay}, a string specifying the assay of the SummarizedExperiment object containing values to use for coloring,
#' if \code{ColorBy="Sample name"}.
#' Defaults to \code{"logcounts"} in \code{\link{getPanelDefault}}, falling back to the name of the first valid assay
#' (see \code{?"\link{.cacheCommonInfo,DotPlot-method}"} for the definition of validity).
#' \item \code{ColorByFeatureNameColor}, a string specifying the color to use for coloring an individual sample on the plot,
#' if \code{ColorBy="Feature name"}.
#' Defaults to \code{"red"} in \code{\link{getPanelDefault}}.
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
#' \item \code{TooltipRowData}, a character vector specifying \code{\link{rowData}} fields to show in the tooltip.
#' Defaults to `character(0)`, which displays only the `rownames` value of the data point.
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
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"RowSelectionSource"} and \code{"RowSelectionRestrict"}),
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
#' \item \code{\link{.multiSelectionRestricted}(x)} returns a logical scalar indicating whether \code{x} is restricting the plotted points to those that were selected in a transmitting panel.
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"row"} to indicate that a multiple row selection is being transmitted.
#' \item \code{\link{.multiSelectionInvalidated}(x)} returns \code{TRUE} if the faceting options use the multiple row selections,
#' such that the point coordinates/domain may change upon updates to upstream selections in transmitting panels.
#' \item \code{\link{.singleSelectionDimension}(x)} returns \code{"feature"} to indicate that a feature identity is being transmitted.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a tour relevant to subclasses,
#' mostly tuning the more generic descriptions from the same method of the parent \linkS4class{DotPlot}.
#' \item \code{\link{.getDotPlotColorHelp}(x, color_choices)} returns a data.frame containing the documentation for the \code{"ColorBy"} UI element,
#' specialized for row-based dot plots.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{DotPlot} and \linkS4class{Panel} are also available.
#'
#' @section Subclass expectations:
#' Subclasses are expected to implement methods for, at least:
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
#' .getTooltipUI,RowDotPlot-method
#' .hideInterface,RowDotPlot-method
#' .multiSelectionDimension,RowDotPlot-method
#' .multiSelectionResponsive,RowDotPlot-method
#' .multiSelectionRestricted,RowDotPlot-method
#' .multiSelectionInvalidated,RowDotPlot-method
#' .singleSelectionDimension,RowDotPlot-method
#' .definePanelTour,RowDotPlot-method
#' .getDotPlotColorHelp,RowDotPlot-method
#' [[,RowDotPlot-method
#' [[,RowDotPlot,ANY,ANY-method
#' [[<-,RowDotPlot-method
#' [[<-,RowDotPlot,ANY,ANY-method
#' updateObject,RowDotPlot-method
#' @name RowDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowDotPlot", function(.Object, ..., SelectionEffect=NULL, SelectionColor=NULL, FacetByRow=NULL, FacetByColumn=NULL) {
    args <- list(...)
    args <- .emptyDefault(args, .colorByRowData, NA_character_)
    args <- .emptyDefault(args, .colorBySampNameAssay, getPanelDefault("ColorByNameAssay"))
    args <- .emptyDefault(args, .colorByFeatNameColor, getPanelDefault("ColorByNameColor"))

    args <- .emptyDefault(args, .shapeByRowData, NA_character_)

    args <- .emptyDefault(args, .sizeByRowData, NA_character_)

    # Defensive measure to avoid problems with cyclic graphs
    # that the user doesn't have permissions to change!
    args <- .emptyDefault(args, .selectColDynamic, FALSE)

    args <- .emptyDefault(args, .facetRowByRowData, NA_character_)
    args <- .emptyDefault(args, .facetColumnByRowData, NA_character_)

    if (!is.null(FacetByRow)) {
        .Deprecated(msg="'FacetByRow=' is deprecated.\nUse 'FacetRowBy=\"Column data\"' and 'FacetRowByRowData=' instead.")
        if (FacetByRow!=.noSelection) {
            args[["FacetRowBy"]] <- "Row data"
            args[["FacetRowByRowData"]] <- FacetByRow
        }
    }

    if (!is.null(FacetByColumn)) {
        .Deprecated(msg="'FacetByColumn=' is deprecated.\nUse 'FacetColumnBy=\"Column data\"' and 'FacetColumnByRowData=' instead.")
        if (FacetByColumn!=.noSelection) {
            args[["FacetColumnBy"]] <- "Row data"
            args[["FacetColumnByRowData"]] <- FacetByColumn
        }
    }

    if (!is.null(SelectionEffect)) {
        .Deprecated(msg=sprintf("'SelectionEffect=' is deprecated.\nUse '%s=TRUE' instead.", .selectRowRestrict))
        args[[.selectRowRestrict]] <- TRUE
    }

    if (!is.null(SelectionColor)) {
        .Deprecated(msg="'SelectionColor=' is deprecated and will be ignored")
    }
    
    args <- .emptyDefault(args, .tooltipRowData, getPanelDefault(.tooltipRowData))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("RowDotPlot", function(object) {
    msg <- character(0)

    msg <- .singleStringError(msg, object,
        c(.colorByRowData, .colorBySampNameAssay, .colorByFeatNameColor, .facetRowByRowData, .facetColumnByRowData))

    msg <- .allowableChoiceError(msg, object, .colorByField,
          c(.colorByNothingTitle, .colorByRowDataTitle, .colorByFeatNameTitle, .colorBySampNameTitle, .colorByRowSelectionsTitle))

    msg <- .allowableChoiceError(msg, object, .shapeByField,
          c(.shapeByNothingTitle, .shapeByRowDataTitle))

    msg <- .allowableChoiceError(msg, object, .sizeByField,
          c(.sizeByNothingTitle, .sizeByRowDataTitle))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "RowDotPlot", function(x, i, j, ...) {
    if (i == "SelectionColor") {
        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.", cname, i))
        NA_character_
    } else if (i == "SelectionEffect") {
        x <- updateObject(x, check=FALSE)

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.\nUse <%s>[['%s']] and/or <%s>[['%s']] instead.",
            cname, i, cname, .selectRowRestrict, cname, .colorByField))

        if (slot(x, .selectRowRestrict)) {
            "Restrict" 
        } else if (slot(x, .colorByField) == .colorByRowSelectionsTitle) {
            "Color"
        } else {
            "Transparent"
        }
    } else {
        callNextMethod()
    }
})

#' @export
setReplaceMethod("[[", "RowDotPlot", function(x, i, j, ..., value) {
    if (i == "SelectionColor") {
        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.", cname, i))
        x 
    } else if (i == "SelectionEffect") {
        x <- updateObject(x, check=FALSE)

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.\nSet <%s>[['%s']] and/or <%s>[['%s']] instead.",
            cname, i, cname, .selectRowRestrict, cname, .colorByField))

        slot(x, .selectRowRestrict) <- (value=="Restrict")

        x
    } else {
        callNextMethod()
    }
})

###############################################################

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
    discrete <- .whichGroupable(subdf, max_levels = .get_factor_maxlevels())
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
    x <- .replaceMissingWithFirst(x, .colorByRowData, available)
    x <- .removeInvalidChoices(x, .tooltipRowData, available)

    assays <- dp_cached$valid.assay.names
    x <- .replaceMissingWithFirst(x, .colorBySampNameAssay, assays)

    discrete <- rdp_cached$discrete.rowData.names
    x <- .replaceMissingWithFirst(x, .shapeByRowData, discrete)
    x <- .replaceMissingWithFirst(x, .facetRowByRowData, discrete)
    x <- .replaceMissingWithFirst(x, .facetColumnByRowData, discrete)

    continuous <- rdp_cached$continuous.rowData.names
    x <- .replaceMissingWithFirst(x, .sizeByRowData, continuous)
    
    x <- .replaceMissingWithFirst(x, .plotCustomLabelsText, rownames(se)[1])

    x
})

#' @export
setMethod(".hideInterface", "RowDotPlot", function(x, field) {
    if (field %in% c(.selectColSource, .selectColRestrict, .selectColDynamic)) {
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

    .createProtectedParameterObservers(plot_name,
        fields=c(.facetRowByRowData, .facetColumnByRowData),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .create_dimname_propagation_observer(plot_name, choices=rownames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionDimension", "RowDotPlot", function(x) "row")

#' @export
setMethod(".multiSelectionRestricted", "RowDotPlot", function(x) {
    slot(x, .selectRowRestrict)
})

#' @export
setMethod(".multiSelectionInvalidated", "RowDotPlot", function(x) {
    slot(x, .facetRow) == .facetByRowSelectionsTitle || 
        slot(x, .facetColumn) == .facetByRowSelectionsTitle || 
        callNextMethod()
})

#' @export
setMethod(".multiSelectionResponsive", "RowDotPlot", function(x, dims = character(0)) {
    if ("row" %in% dims) {
        return(TRUE)
    }
    return(FALSE)
})

#' @export
setMethod(".singleSelectionDimension", "RowDotPlot", function(x) "feature")

###############################################################

setMethod(".getDiscreteMetadataChoices", "RowDotPlot", function(x, se) {
    .getCachedCommonInfo(se, "RowDotPlot")$discrete.rowData.names
})

setMethod(".getContinuousMetadataChoices", "RowDotPlot", function(x, se) {
    .getCachedCommonInfo(se, "RowDotPlot")$continuous.colData.names
})

setMethod(".getMetadataChoices", "RowDotPlot", function(x, se) {
    .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
})

setMethod(".defineDotPlotColorChoices", "RowDotPlot", function(x, se) {
    covariates <- .allowableColorByDataChoices(x, se)
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
    .define_color_options_for_row_plots(se, covariates, all_assays)
})

setMethod(".getDotPlotColorConstants", "RowDotPlot", function(x) {
    list(
        metadata=list(
            title=.colorByRowDataTitle,
            field=.colorByRowData
        ),
        name=list(
            title=.colorByFeatNameTitle,
            field=.colorByFeatName,
            table=.colorByRowTable,
            color=.colorByFeatNameColor,
            dynamic=.colorByFeatDynamic
        ),
        assay=list(
            title=.colorBySampNameTitle,
            field=.colorBySampName,
            assay=.colorBySampNameAssay,
            table=.colorByColTable,
            color=.colorBySampNameColor,
            dynamic=.colorBySampDynamic
        )
    )
})

setMethod(".getDotPlotSizeConstants", "RowDotPlot", function(x) {
    list(
        metadata=list(
            title=.sizeByRowDataTitle,
            field=.sizeByRowData
        )
    )
})

setMethod(".getDotPlotShapeConstants", "RowDotPlot", function(x) {
    list(
        metadata=list(
            title=.shapeByRowDataTitle,
            field=.shapeByRowData
        )
    )
})

setMethod(".getDotPlotMetadataCommand", "RowDotPlot", function(x) "rowData")

setMethod(".getDotPlotNamesCommand", "RowDotPlot", function(x) "rownames")

setMethod(".getDotPlotFacetConstants", "RowDotPlot", function(x) {
    list(
        metadata=list(
            title=.facetByRowDataTitle,
            row_field=.facetRowByRowData,
            column_field=.facetColumnByRowData
        ),
        selections=list(
            title=.facetByRowSelectionsTitle
        )
    )
})

###############################################################
# See ?.addDotPlotDataColor for documentation on these methods.

setMethod(".addDotPlotDataColor", "RowDotPlot", function(x, envir) {
    color_choice <- slot(x, .colorByField)

    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- slot(x, .colorByRowData)
        label <- covariate_name
        cmds <- sprintf("plot.data$ColorBy <- rowData(se)[, %s];", deparse(covariate_name))

    } else if (color_choice == .colorByFeatNameTitle) {
        chosen_gene <- slot(x, .colorByFeatName)
        label <- chosen_gene
        cmds <- sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
            deparse(chosen_gene))

    } else if (color_choice  == .colorBySampNameTitle) {
        chosen_sample <- slot(x, .colorBySampName)
        assay_choice <- slot(x, .colorBySampNameAssay)
        label <- sprintf("%s\n(%s)", chosen_sample, assay_choice)
        cmds <- sprintf("plot.data$ColorBy <- assay(se, %s)[, %s];",
            deparse(assay_choice), deparse(chosen_sample))

    } else if (color_choice == .colorByRowSelectionsTitle) {
        label <- "Row selection"
        if (exists("row_selected", envir=envir, inherits=FALSE)) {
            target <- "row_selected"
        } else {
            target <- "list()"
        }
        cmds <- sprintf(
            "plot.data$ColorBy <- iSEE::multiSelectionToFactor(%s, rownames(se));", 
            target
        )

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ColorBy=label))
})

setMethod(".addDotPlotDataShape", "RowDotPlot", function(x, envir) {
    shape_choice <- slot(x, .shapeByField)

    if (shape_choice == .shapeByRowDataTitle) {
        covariate_name <- slot(x, .shapeByRowData)
        label <- covariate_name
        cmds <- sprintf("plot.data$ShapeBy <- rowData(se)[, %s];", deparse(covariate_name))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ShapeBy=label))
})

setMethod(".addDotPlotDataSize", "RowDotPlot", function(x, envir) {
    size_choice <- slot(x, .sizeByField)

    if (size_choice == .sizeByRowDataTitle) {
        covariate_name <- slot(x, .sizeByRowData)
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

    params <- list(
        list(.facetRow, "FacetRow", .facetRowByRowData),
        list(.facetColumn, "FacetColumn", .facetColumnByRowData)
    )

    for (f in seq_len(2)) {
        current <- params[[f]]
        param_field <- current[[1]]
        pd_field <- current[[2]]
        facet_mode <- slot(x, param_field)

        if (facet_mode == .facetByRowDataTitle) {
            facet_data <- x[[current[[3]]]]
            facet_cmds[pd_field] <- sprintf("plot.data$%s <- rowData(se)[, %s];", pd_field, deparse(facet_data))
            labels[[pd_field]] <- facet_data

        } else if (facet_mode == .facetByRowSelectionsTitle) {
            if (exists("row_selected", envir=envir, inherits=FALSE)) {
                target <- "row_selected"
            } else {
                target <- "list()"
            }
            facet_cmds[pd_field] <- sprintf("plot.data$%s <- iSEE::multiSelectionToFactor(%s, rownames(se));", pd_field, target)
            labels[[pd_field]] <- "Row selection"
        }
    }

    .textEval(facet_cmds, envir)

    list(commands=facet_cmds, labels=labels)
})

setMethod(".addDotPlotDataSelected", "RowDotPlot", function(x, envir) {
    if (!exists("row_selected", envir=envir, inherits=FALSE)) {
        return(NULL)
    }

    cmds <- c(
        header="# Receiving row point selection",
        SelectBy="plot.data$SelectBy <- rownames(plot.data) %in% unlist(row_selected);"
    )

    if (slot(x, .selectRowRestrict)) {
        cmds["saved"] <- "plot.data.all <- plot.data;"
        cmds["subset"] <- "plot.data <- subset(plot.data, SelectBy);"
    }

    .textEval(cmds, envir)

    cmds
})

#' @importFrom ggplot2 scale_color_manual geom_point
setMethod(".colorDotPlot", "RowDotPlot", function(x, colorby, x_aes="X", y_aes="Y") {
    color_choice <- slot(x, .colorByField)

    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- slot(x, .colorByRowData)
        cmds <- .create_color_scale("rowDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice == .colorByFeatNameTitle) {
        col_choice <- slot(x, .colorByFeatNameColor)
        if (slot(x, .sizeByField) == .sizeByNothingTitle) {
            size_cmd <- paste0(", size=5*", slot(x, .plotPointSize)) 
        } else {
            size_cmd <- ""
        }
        c(
            sprintf(
                "scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +",
                deparse(col_choice)
            ),
            sprintf(
                "geom_point(aes(x=%s, y=%s), data=subset(plot.data, ColorBy == 'TRUE'), col=%s, alpha=1%s) +",
                x_aes, y_aes, deparse(col_choice), size_cmd
            )
        )

    } else if (color_choice == .colorBySampNameTitle) {
        assay_choice <- slot(x, .colorBySampNameAssay)
        .create_color_scale("assayColorMap", deparse(assay_choice), colorby)

    } else if (color_choice == .colorByRowSelectionsTitle) {
        sprintf("scale_color_manual(values=iSEE::rowSelectionColorMap(colormap, %s), drop=FALSE) +", 
            paste(deparse(levels(colorby)), collapse="")) 
        
    } else {
        .colorByNoneDotPlotScale(x)
    }
})

###############################################################
# Tooltip

setMethod(".getTooltipUI", "RowDotPlot", function(x, se, name) {
    if (length(x[[.tooltipRowData]]) > 0) {
        # as.data.frame sometimes needed before as.list to fix names of items in vector
        info <- as.list(as.data.frame(rowData(se)[name, x[[.tooltipRowData]], drop=FALSE]))
        ui <- .generate_tooltip_html(name, info)
        ui
    } else {
        name
    }
})

###############################################################################
# Documentation

setMethod(".getDotPlotColorHelp", "RowDotPlot", function(x, color_choices) {
    force(color_choices)
    function(plot_name) {
        start <- paste0("#", plot_name, "_", .colorByField)
        base <- "We can choose to color points by a constant (<em>None</em>) or various per-rowattributes. Try out some of the different choices here, and note how further options become available when each choice is selected."
        steps <- list(c(element=start, intro=base))

        if ("Row data" %in% color_choices) {
            steps <- c(steps, list(
                c(
                    element=start,
                    intro="For example, if we <strong>select <em>Row data</em></strong>..."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByRowData, " + .selectize-control"),
                    intro="... we can choose between different <code>rowData</code> fields that we might want to color by."
                )
            ))
        }

        if ("Sample name" %in% color_choices) {
            steps <- c(steps, list(
                c(
                    element=start,
                    intro="If we <strong>select <em>Sample name</em></strong>..."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorBySampName, " + .selectize-control"),
                    intro="... each point is colored according to the assay value of a sample of interest for the corresponding row."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorBySampNameAssay, " + .selectize-control"),
                    intro="We can change the choice of assay."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByColTable, " + .selectize-control"),
                    intro="And we can even synchronize the choice of sample to a selection in another panel. This assumes that our current application actually has another panel that allows us to select a single sample from our <code>SummarizedExperiment</code>."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorBySampDynamic),
                    intro="In fact, we don't even need to manually choose another panel - if dynamic sample selection is enabled, the plot will automatically respond to any single sample selection from any applicable panel in our application."
                )
            ))
        }

        if ("Feature name" %in% color_choices) {
            steps <- c(steps, list(
                c(
                    element=start,
                    intro="If we <strong>select <em>Feature name</em></strong>..."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByFeatName, " + .selectize-control"),
                    intro="... we can highlight a particular point based on the row name."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByFeatNameColor),
                    intro="We can fiddle with the choice of color for the highlighted point."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByRowTable, " + .selectize-control"),
                    intro="We can even synchronize the choice of sample to a selection in another panel. This assumes that our current application actually has another panel that we can use to select a single feature."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByFeatDynamic),
                    intro="In fact, we don't even need to manually choose another panel - if dynamic sample selection is enabled, the plot will automatically respond to any single feature selection from any applicable panel in our application."
                )
            ))
        }

        if ("Row selection" %in% color_choices) {
            steps <- c(steps, list(
                c(
                    element=start,
                    intro="If we <strong>select <em>Row selection</em></strong>, we will color the points according to the multiple column selection transmitted from another panel (see the Selection Parameters box). If a column is included in the active selection of the other panel, the corresponding point in this panel is assigned a certain color; if the column is in one of the saved selections, it gets another color; and if the column is not in any selection, it gets the default color (usually grey). Points that are present in multiple selections also get a different color."  
                )
            ))
        }

        data.frame(do.call(rbind, steps))
    }
})

###############################################################################
# Back compatibility

#' @export
setMethod("updateObject", "RowDotPlot", function(object, ..., verbose=FALSE) {
    if (!.is_latest_version(object)) {
        # nocov start

        # Do this before 'callNextMethod()', which fills in the Restrict.
        update.2.3 <- is(try(slot(object, .selectRowRestrict), silent=TRUE), "try-error")

        # NOTE: it is crucial that updateObject does not contain '[[' or '[[<-'
        # calls, lest we get sucked into infinite recursion with the calls to
        # 'updateObject' from '[['.
        object <- callNextMethod()

        if (update.2.3) {
            effect <- object@SelectionEffect
            slot(object, .selectRowRestrict) <- (effect=="Restrict")
        }

        # nocov end
    }

    object
})
