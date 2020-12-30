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
#' .hideInterface,ColumnDotPlot-method
#' .multiSelectionDimension,ColumnDotPlot-method
#' .multiSelectionRestricted,ColumnDotPlot-method
#' .multiSelectionInvalidated,ColumnDotPlot-method
#' .singleSelectionDimension,ColumnDotPlot-method
#' .definePanelTour,ColumnDotPlot-method
#' [[,ColumnDotPlot-method
#' [[,ColumnDotPlot,ANY,ANY-method
#' [[<-,ColumnDotPlot-method
#' [[<-,ColumnDotPlot,ANY,ANY-method
#' updateObject,ColumnDotPlot-method
#' @name ColumnDotPlot-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnDotPlot", function(.Object, ..., FacetByRow=NULL, FacetByColumn=NULL) {
    args <- list(...)
    args <- .emptyDefault(args, .colorByColData, NA_character_)
    args <- .emptyDefault(args, .colorByFeatNameAssay, NA_character_)
    args <- .emptyDefault(args, .colorBySampNameColor, iSEEOptions$get("selected.color"))

    args <- .emptyDefault(args, .shapeByColData, NA_character_)

    args <- .emptyDefault(args, .sizeByColData, NA_character_)

    # Defensive measure to avoid problems with cyclic graphs 
    # that the user doesn't have permissions to change!
    args <- .emptyDefault(args, .selectRowDynamic, FALSE)

    args <- .emptyDefault(args, .facetRowByColData, NA_character_)
    args <- .emptyDefault(args, .facetColumnByColData, NA_character_)

    # nocov start
    if (!is.null(FacetByRow)) {
        .Deprecated(msg="'FacetByRow=' is deprecated.\nUse 'FacetRowBy=\"Column data\"' and 'FacetRowByColData=' instead.")
        if (FacetByRow!=.noSelection) {
            args[["FacetRowBy"]] <- "Column data"
            args[["FacetRowByColData"]] <- FacetByRow
        }
    }

    if (!is.null(FacetByColumn)) {
        .Deprecated(msg="'FacetByColumn=' is deprecated.\nUse 'FacetColumnBy=\"Column data\"' and 'FacetColumnByColData=' instead.")
        if (FacetByColumn!=.noSelection) {
            args[["FacetColumnBy"]] <- "Column data"
            args[["FacetColumnByColData"]] <- FacetByColumn
        }
    }
    # nocov end

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
            cname, i, cname, .selectColRestrict, cname, .colorByField))

        if (x[[.selectColRestrict]]) {
            "Restrict" 
        } else if (x[[.colorByField]] == .colorByColSelectionsTitle) {
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
            cname, i, cname, .selectColRestrict, cname, .colorByField))

        x[[.selectColRestrict]] <- (value=="Restrict")
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
    x <- .replaceMissingWithFirst(x, .colorByColData, available)

    assays <- dp_cached$valid.assay.names
    assays <- c(intersect(iSEEOptions$get("assay"), assays), assays)
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
    x[[.selectColRestrict]]
})

#' @export
setMethod(".multiSelectionInvalidated", "ColumnDotPlot", function(x) {
    x[[.facetRow]] == .facetByColSelectionsTitle || x[[.facetColumn]] == .facetByColSelectionsTitle || callNextMethod()
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
    covariates <- .getMetadataChoices(x, se)
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
        cmds <- sprintf("plot.data$ColorBy <- assay(se, %s)[%s, ];",
            deparse(assay_choice), deparse(chosen_gene))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- x[[.colorBySampName]]
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

    params <- list(
        list(.facetRow, "FacetRow", .facetRowByColData),
        list(.facetColumn, "FacetColumn", .facetColumnByColData)
    )

    for (f in seq_len(2)) {
        current <- params[[f]]
        param_field <- current[[1]]
        pd_field <- current[[2]]
        facet_mode <- x[[param_field]]

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

    if (x[[.selectColRestrict]]) {
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

    } else if (color_choice == .colorByColSelectionsTitle) {
        sprintf("scale_color_manual(values=iSEE::columnSelectionColorMap(colormap, %s), drop=FALSE) +", 
            paste(deparse(levels(colorby)), collapse="")) 

    } else {
        .colorByNoneDotPlotScale(x)
    }
})

###############################################################

#' @export
setMethod(".definePanelTour", "ColumnDotPlot", function(x) {
    collated <- callNextMethod()
        
    collated$intro[collated$intro=="PLACEHOLDER_COLOR"] <- "We can choose to color by different per-column attributes - from the column metadata, across a specific feature of an assay, to identify a chosen sample, or based on a multiple column selection transmitted from another panel.<br/><br/><strong>Action:</strong> try out some of the different choices. Note how further options become available when each choice is selected."

    data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE)
})

#' @export
setMethod("updateObject", "ColumnDotPlot", function(object, ..., verbose=FALSE) {
    if (!.is_latest_version(object)) {
        # nocov start

        # Do this before 'callNextMethod()', which fills in the Restrict.
        update.2.3 <- is(try(slot(object, .selectColRestrict), silent=TRUE), "try-error")

        # NOTE: it is crucial that updateObject does not contain '[[' or '[[<-'
        # calls, lest we get sucked into infinite recursion with the calls to
        # 'updateObject' from '[['.
        object <- callNextMethod()

        if (update.2.3) {
            effect <- object@SelectionEffect
            slot(object, .selectColRestrict) <- (effect=="Restrict")
        }

        # nocov end
    }

    object
})
