#' The ColumnDotPlot virtual class
#'
#' The ColumnDotPlot is a virtual class where each column in the \linkS4class{SummarizedExperiment} is represented by no more than one point (i.e., a \dQuote{dot}) in a brushable \link{ggplot} plot.
#' It provides slots and methods to extract \code{\link{colData}} fields to control the per-point aesthetics on the plot.
#' This panel will transmit column identities in both its single and multiple selections, and it can receive multiple column selections but not multiple row selections.
#'
#' @section Slot overview:
#' The following slots control coloring of the points:
#' \itemize{
#' \item \code{ColorByColumnData}, a string specifying the \code{\link{colData}} field for controlling point color,
#' if \code{ColorBy="Column data"} (see the \linkS4class{Panel} class).
#' Defaults to the first valid field (see \code{.cacheCommonInfo} below).
#' \item \code{ColorByFeatureNameAssay}, a string specifying the assay of the SummarizedExperiment object containing values to use for coloring,
#' if \code{ColorBy="Feature name"}.
#' Defaults to \code{"logcounts"} in \code{\link{getPanelDefault}}, falling back to the name of the first valid assay 
#' (see \code{?"\link{.cacheCommonInfo,DotPlot-method}"} for the definition of validity).
#' \item \code{ColorBySampleNameColor}, a string specifying the color to use for coloring an individual sample on the plot,
#' if \code{ColorBy="Sample name"}.
#' Defaults to \code{"red"} in \code{\link{getPanelDefault}}.
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
#' \item \code{TooltipColumnData}, a character vector specifying \code{\link{colData}} fields to show in the tooltip.
#' Defaults to `character(0)`, which displays only the `colnames` value of the data point.
#' }
#' 
#' In addition, this class inherits all slots from its parent \linkS4class{DotPlot} and \linkS4class{Panel} classes.
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
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"RowSelectionSource"} and \code{"RowSelectionRestrict"}),
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
#' \item \code{\link{.multiSelectionRestricted}(x)} returns a logical scalar indicating whether \code{x} is restricting the plotted points to those that were selected in a transmitting panel.
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"column"} to indicate that a multiple column selection is being transmitted.
#' \item \code{\link{.multiSelectionInvalidated}(x)} returns \code{TRUE} if the faceting options use multiple column selections,
#' such that the point coordinates/domain may change upon updates to upstream selections in transmitting panels.
#' \item \code{\link{.singleSelectionDimension}(x)} returns \code{"sample"} to indicate that a sample identity is being transmitted.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a tour relevant to subclasses,
#' mostly tuning the more generic descriptions from the same method of the parent \linkS4class{DotPlot}.
#' \item \code{\link{.getDotPlotColorHelp}(x, color_choices)} returns a data.frame containing the documentation for the \code{"ColorBy"} UI element,
#' specialized for column-based dot plots.
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
#' .getTooltipUI,ColumnDotPlot-method
#' .hideInterface,ColumnDotPlot-method
#' .multiSelectionDimension,ColumnDotPlot-method
#' .multiSelectionResponsive,ColumnDotPlot-method
#' .multiSelectionRestricted,ColumnDotPlot-method
#' .multiSelectionInvalidated,ColumnDotPlot-method
#' .singleSelectionDimension,ColumnDotPlot-method
#' .definePanelTour,ColumnDotPlot-method
#' .getDotPlotColorHelp,ColumnDotPlot-method
#' [[,ColumnDotPlot-method
#' [[,ColumnDotPlot,ANY,ANY-method
#' [[<-,ColumnDotPlot-method
#' [[<-,ColumnDotPlot,ANY,ANY-method
#' updateObject,ColumnDotPlot-method
#' @name ColumnDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnDotPlot", function(.Object, ..., SelectionEffect=NULL, SelectionColor=NULL, FacetByRow=NULL, FacetByColumn=NULL) {
    args <- list(...)
    args <- .emptyDefault(args, .colorByColData, NA_character_)
    args <- .emptyDefault(args, .colorByFeatNameAssay, getPanelDefault("ColorByNameAssay"))
    args <- .emptyDefault(args, .colorBySampNameColor, getPanelDefault("ColorByNameColor"))

    args <- .emptyDefault(args, .shapeByColData, NA_character_)

    args <- .emptyDefault(args, .sizeByColData, NA_character_)

    # Defensive measure to avoid problems with cyclic graphs 
    # that the user doesn't have permissions to change!
    args <- .emptyDefault(args, .selectRowDynamic, FALSE)

    args <- .emptyDefault(args, .facetRowByColData, NA_character_)
    args <- .emptyDefault(args, .facetColumnByColData, NA_character_)

    if (!is.null(FacetByRow)) {
        .Deprecated(msg=sprintf("'FacetByRow=' is deprecated.\nUse '%s=\"%s\"' and '%s=' instead.",
            .facetRow, .facetByColDataTitle, .facetRowByColData))
        if (FacetByRow!=.noSelection) {
            args[[.facetRow]] <- .facetByColDataTitle
            args[[.facetRowByColData]] <- FacetByRow
        }
    }

    if (!is.null(FacetByColumn)) {
        .Deprecated(msg=sprintf("'FacetByColumn=' is deprecated.\nUse '%s=\"%s\"' and '%s=' instead.",
            .facetColumn, .facetByColDataTitle, .facetColumnByColData))
        if (FacetByColumn!=.noSelection) {
            args[[.facetColumn]] <- .facetByColDataTitle
            args[[.facetColumnByColData]] <- FacetByColumn
        }
    }

    if (!is.null(SelectionEffect)) {
        .Deprecated(msg=sprintf("'SelectionEffect=' is deprecated.\nUse '%s=TRUE' instead.", .selectColumnRestrict))
        args[[.selectColumnRestrict]] <- TRUE
    }

    if (!is.null(SelectionColor)) {
        .Deprecated(msg="'SelectionColor=' is deprecated and will be ignored")
    }
    
    args <- .emptyDefault(args, .tooltipColData, getPanelDefault(.tooltipColData))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("ColumnDotPlot", function(object) {
    msg <- character(0)

    msg <- .singleStringError(msg, object,
        c(.colorByColData, .colorByFeatNameAssay, .colorBySampNameColor, .facetRowByColData, .facetColumnByColData))

    msg <- .allowableChoiceError(msg, object, .colorByField,
        c(.colorByNothingTitle, .colorByColDataTitle, .colorByFeatNameTitle, .colorBySampNameTitle, .colorByColSelectionsTitle))

    msg <- .allowableChoiceError(msg, object, .shapeByField,
          c(.shapeByNothingTitle, .shapeByColDataTitle))

    msg <- .allowableChoiceError(msg, object, .sizeByField,
          c(.sizeByNothingTitle, .sizeByColDataTitle))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "ColumnDotPlot", function(x, i, j, ...) {
    if (i == "SelectionColor") {
        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.", cname, i))
        NA_character_
    } else if (i == "SelectionEffect") {
        x <- updateObject(x, check=FALSE)

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.\nUse <%s>[['%s']] and/or <%s>[['%s']] instead.",
            cname, i, cname, .selectColumnRestrict, cname, .colorByField))

        if (slot(x, .selectColumnRestrict)) {
            "Restrict" 
        } else if (slot(x, .colorByField) == .colorByColSelectionsTitle) {
            "Color"
        } else {
            "Transparent"
        }
    } else {
        callNextMethod()

    }
})

#' @export
setReplaceMethod("[[", "ColumnDotPlot", function(x, i, j, ..., value) {
    if (i == "SelectionColor") {
        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.", cname, i))
        x 
    } else if (i == "SelectionEffect") {
        x <- updateObject(x, check=FALSE)

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.\nSet <%s>[['%s']] and/or <%s>[['%s']] instead.",
            cname, i, cname, .selectColumnRestrict, cname, .colorByField))

        slot(x, .selectColumnRestrict) <- (value=="Restrict")
        x
    } else {
        callNextMethod()
    }
})

###############################################################

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
    discrete <- .whichGroupable(subdf, max_levels = .get_factor_maxlevels())
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
    x <- .replaceMissingWithFirst(x, .colorByColData, available)
    x <- .removeInvalidChoices(x, .tooltipColData, available)

    assays <- dp_cached$valid.assay.names
    x <- .replaceMissingWithFirst(x, .colorByFeatNameAssay, assays)

    discrete <- cdp_cached$discrete.colData.names
    x <- .replaceMissingWithFirst(x, .shapeByColData, discrete)
    x <- .replaceMissingWithFirst(x, .facetRowByColData, discrete)
    x <- .replaceMissingWithFirst(x, .facetColumnByColData, discrete)

    continuous <- cdp_cached$continuous.colData.names
    x <- .replaceMissingWithFirst(x, .sizeByColData, continuous)
    
    x <- .replaceMissingWithFirst(x, .plotCustomLabelsText, colnames(se)[1])

    x
})

#' @export
setMethod(".hideInterface", "ColumnDotPlot", function(x, field) {
    if (field %in% c(.selectRowSource, .selectRowRestrict, .selectRowDynamic)) {
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

    .createProtectedParameterObservers(plot_name,
        fields=c(.facetRowByColData, .facetColumnByColData),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .create_dimname_propagation_observer(plot_name, choices=colnames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionDimension", "ColumnDotPlot", function(x) "column")

#' @export
setMethod(".multiSelectionRestricted", "ColumnDotPlot", function(x) {
    slot(x, .selectColumnRestrict)
})

#' @export
setMethod(".multiSelectionInvalidated", "ColumnDotPlot", function(x) {
    slot(x, .facetRow) == .facetByColSelectionsTitle || 
        slot(x, .facetColumn) == .facetByColSelectionsTitle || 
        callNextMethod()
})

#' @export
setMethod(".multiSelectionResponsive", "ColumnDotPlot", function(x, dims = character(0)) {
    if ("column" %in% dims) {
        return(TRUE)
    }
    return(FALSE)
})

#' @export
setMethod(".singleSelectionDimension", "ColumnDotPlot", function(x) "sample")

###############################################################

setMethod(".getDiscreteMetadataChoices", "ColumnDotPlot", function(x, se) {
    .getCachedCommonInfo(se, "ColumnDotPlot")$discrete.colData.names
})

setMethod(".getContinuousMetadataChoices", "ColumnDotPlot", function(x, se) {
    .getCachedCommonInfo(se, "ColumnDotPlot")$continuous.colData.names
})

setMethod(".getMetadataChoices", "ColumnDotPlot", function(x, se) {
    .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
})

setMethod(".defineDotPlotColorChoices", "ColumnDotPlot", function(x, se) {
    covariates <- .allowableColorByDataChoices(x, se)
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
    .define_color_options_for_column_plots(se, covariates, all_assays)
})

setMethod(".getDotPlotColorConstants", "ColumnDotPlot", function(x) {
    list(
        metadata=list(
            title=.colorByColDataTitle,
            field=.colorByColData
        ),
        name=list(
            title=.colorBySampNameTitle,
            field=.colorBySampName,
            table=.colorByColTable,
            color=.colorBySampNameColor,
            dynamic=.colorBySampDynamic
        ),
        assay=list(
            title=.colorByFeatNameTitle,
            field=.colorByFeatName,
            assay=.colorByFeatNameAssay,
            table=.colorByRowTable,
            color=.colorByFeatNameColor,
            dynamic=.colorByFeatDynamic
        )
    )
})

setMethod(".getDotPlotSizeConstants", "ColumnDotPlot", function(x) {
    list(
        metadata=list(
            title=.sizeByColDataTitle,
            field=.sizeByColData
        )
    )
})

setMethod(".getDotPlotShapeConstants", "ColumnDotPlot", function(x) {
    list(
        metadata=list(
            title=.shapeByColDataTitle,
            field=.shapeByColData
        )
    )
})

setMethod(".getDotPlotMetadataCommand", "ColumnDotPlot", function(x) "colData")

setMethod(".getDotPlotNamesCommand", "ColumnDotPlot", function(x) "colnames")

setMethod(".getDotPlotFacetConstants", "ColumnDotPlot", function(x) {
    list(
        metadata=list(
            title=.facetByColDataTitle,
            row_field=.facetRowByColData,
            column_field=.facetColumnByColData
        ),
        selections=list(
            title=.facetByColSelectionsTitle
        )
    )
})

###############################################################
# See ?.addDotPlotDataColor for documentation on these methods.

setMethod(".addDotPlotDataColor", "ColumnDotPlot", function(x, envir) {
    color_choice <- slot(x, .colorByField)

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- slot(x, .colorByColData)
        label <- covariate_name
        cmds <- sprintf("plot.data$ColorBy <- colData(se)[, %s];", deparse(covariate_name))

    } else if (color_choice == .colorByFeatNameTitle) {
        # Set the color to the selected gene
        chosen_gene <- slot(x, .colorByFeatName)
        assay_choice <- slot(x, .colorByFeatNameAssay)
        label <- sprintf("%s\n(%s)", chosen_gene, assay_choice)
        cmds <- sprintf("plot.data$ColorBy <- assay(se, %s)[%s, ];",
            deparse(assay_choice), deparse(chosen_gene))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- slot(x, .colorBySampName)
        label <- chosen_sample
        cmds <- sprintf("plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[%s, 'ColorBy'] <- TRUE;",
            deparse(chosen_sample))

    } else if (color_choice == .colorByColSelectionsTitle) {
        label <- "Column selection"
        if (exists("col_selected", envir=envir, inherits=FALSE)) {
            target <- "col_selected"
        } else {
            target <- "list()"
        }
        cmds <- sprintf(
            "plot.data$ColorBy <- iSEE::multiSelectionToFactor(%s, colnames(se));", 
            target
        )

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ColorBy=label))
})

setMethod(".addDotPlotDataShape", "ColumnDotPlot", function(x, envir) {
    shape_choice <- slot(x, .shapeByField)

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- slot(x, .shapeByColData)
        label <- covariate_name
        cmds <- sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))

    } else {
        return(NULL)
    }

    .textEval(cmds, envir)

    list(commands=cmds, labels=list(ShapeBy=label))
})

setMethod(".addDotPlotDataSize", "ColumnDotPlot", function(x, envir) {
    size_choice <- slot(x, .sizeByField)

    if (size_choice == .sizeByColDataTitle) {
        covariate_name <- slot(x, .sizeByColData)
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

    params <- list(
        list(.facetRow, "FacetRow", .facetRowByColData),
        list(.facetColumn, "FacetColumn", .facetColumnByColData)
    )

    for (f in seq_len(2)) {
        current <- params[[f]]
        param_field <- current[[1]]
        pd_field <- current[[2]]
        facet_mode <- slot(x, param_field)

        if (facet_mode == .facetByColDataTitle) {
            facet_data <- x[[current[[3]]]]
            facet_cmds[pd_field] <- sprintf("plot.data$%s <- colData(se)[, %s];", pd_field, deparse(facet_data))
            labels[[pd_field]] <- facet_data

        } else if (facet_mode == .facetByColSelectionsTitle) {
            if (exists("col_selected", envir=envir, inherits=FALSE)) {
                target <- "col_selected"
            } else {
                target <- "list()"
            }
            facet_cmds[pd_field] <- sprintf("plot.data$%s <- iSEE::multiSelectionToFactor(%s, colnames(se));", pd_field, target)
            labels[[pd_field]] <- "Column selection"
        }
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

    if (slot(x, .selectColumnRestrict)) {
        cmds["saved"] <- "plot.data.all <- plot.data;"
        cmds["subset"] <- "plot.data <- subset(plot.data, SelectBy);"
    }

    .textEval(cmds, envir)

    cmds
})

#' @importFrom ggplot2 scale_color_manual geom_point
setMethod(".colorDotPlot", "ColumnDotPlot", function(x, colorby, x_aes="X", y_aes="Y") {
    color_choice <- slot(x, .colorByField)

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- slot(x, .colorByColData)
        .create_color_scale("colDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice == .colorByFeatNameTitle) {
        assay_choice <- slot(x, .colorByFeatNameAssay)
        .create_color_scale("assayColorMap", deparse(assay_choice), colorby)

    } else if (color_choice == .colorBySampNameTitle) {
        col_choice <- slot(x, .colorBySampNameColor)
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

    } else if (color_choice == .colorByColSelectionsTitle) {
        sprintf("scale_color_manual(values=iSEE::columnSelectionColorMap(colormap, %s), drop=FALSE) +", 
            paste(deparse(levels(colorby)), collapse="")) 

    } else {
        .colorByNoneDotPlotScale(x)
    }
})

###############################################################
# Tooltip

setMethod(".getTooltipUI", "ColumnDotPlot", function(x, se, name) {
    if (length(x[[.tooltipColData]]) > 0) {
        # as.data.frame sometimes needed before as.list to fix names of items in vector
        info <- as.list(as.data.frame(colData(se)[name, x[[.tooltipColData]], drop=FALSE]))
        ui <- .generate_tooltip_html(name, info)
        ui
    } else {
        name
    }
})

###############################################################################
# Documentation

setMethod(".getDotPlotColorHelp", "ColumnDotPlot", function(x, color_choices) {
    force(color_choices)
    function(plot_name) {
        start <- paste0("#", plot_name, "_", .colorByField)
        base <- "We can choose to color points by a constant (<em>None</em>) or various per-column attributes. Try out some of the different choices here, and note how further options become available when each choice is selected."
        steps <- list(c(element=start, intro=base))

        if ("Column data" %in% color_choices) {
            steps <- c(steps, list(
                c(
                    element=start,
                    intro="For example, if we <strong>select <em>Column data</em></strong>..."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByColData, " + .selectize-control"),
                    intro="... we can choose between different <code>colData</code> fields that we might want to color by."
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
                    intro="... each point is colored according to the assay value of a feature of interest for the corresponding column."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByFeatNameAssay, " + .selectize-control"),
                    intro="We can change the choice of assay."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByRowTable, " + .selectize-control"),
                    intro="And we can even synchronize the choice of feature to a selection in another panel. This assumes that our current application actually has another panel that allows us to select a single feature from our <code>SummarizedExperiment</code>."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByFeatDynamic),
                    intro="In fact, we don't even need to manually choose another panel - if dynamic feature selection is enabled, the plot will automatically respond to any single feature selection from any applicable panel in our application."
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
                    intro="... we can highlight a particular point based on the column name."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorBySampNameColor),
                    intro="We can fiddle with the choice of color for the highlighted point."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorByColTable, " + .selectize-control"),
                    intro="We can even synchronize the choice of sample to a selection in another panel. This assumes that our current application actually has another panel that we can use to select a single sample."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colorBySampDynamic),
                    intro="In fact, we don't even need to manually choose another panel - if dynamic sample selection is enabled, the plot will automatically respond to any single sample selection from any applicable panel in our application."
                )
            ))
        }

        if ("Column selection" %in% color_choices) {
            steps <- c(steps, list(
                c(
                    element=start,
                    intro="If we <strong>select <em>Column selection</em></strong>, we will color the points according to the multiple column selection transmitted from another panel (see the Selection Parameters box). If a column is included in the active selection of the other panel, the corresponding point in this panel is assigned a certain color; if the column is in one of the saved selections, it gets another color; and if the column is not in any selection, it gets the default color (usually grey). Points that are present in multiple selections also get a different color."  
                )
            ))
        }

        data.frame(do.call(rbind, steps))
    }
})

###############################################################################
# Back compatibility

#' @export
setMethod("updateObject", "ColumnDotPlot", function(object, ..., verbose=FALSE) {
    if (!.is_latest_version(object)) {
        # nocov start

        # Do this before 'callNextMethod()', which fills in the Restrict.
        update.2.3 <- is(try(slot(object, .selectColumnRestrict), silent=TRUE), "try-error")

        # NOTE: it is crucial that updateObject does not contain '[[' or '[[<-'
        # calls, lest we get sucked into infinite recursion with the calls to
        # 'updateObject' from '[['.
        object <- callNextMethod()

        if (update.2.3) {
            effect <- object@SelectionEffect
            slot(object, .selectColumnRestrict) <- (effect=="Restrict")
        }

        # nocov end
    }

    object
})
