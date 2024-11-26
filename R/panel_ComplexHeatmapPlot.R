#' The ComplexHeatmapPlot panel
#'
#' The ComplexHeatmapPlot is a panel class for creating a \linkS4class{Panel} that displays an assay of a \linkS4class{SummarizedExperiment} object as a \code{\link{Heatmap}} with features as rows and samples and columns, respectively.
#' It provides slots and methods for specifying the features of interest, which assay to display in the main heatmap, any transformations to perform on the data, and which metadata variables to display as row and column heatmap annotations.
#'
#' @section ComplexHeatmapPlot slot overview:
#' The following slots control the rows that are used:
#' \itemize{
#' \item \code{CustomRows}, a logical scalar indicating whether the custom list of rows should be used.
#' If \code{FALSE}, the incoming selection is used instead. Defaults to \code{TRUE}.
#' \item \code{CustomRowsText}, string containing newline-separated row names.
#' This specifies which rows of the \linkS4class{SummarizedExperiment} object are to be shown in the heatmap.
#' If \code{NA}, defaults to the first row name of the SummarizedExperiment.
#' }
#'
#' The following slots control the metadata variables that are used:
#' \itemize{
#' \item \code{ColumnData}, a character vector specifying columns of the \code{\link{colData}} to show as \code{\link{columnAnnotation}}.
#' Defaults to \code{character(0)}.
#' \item \code{RowData}, a character vector specifying columns of the \code{\link{rowData}} to show as \code{\link{rowAnnotation}}.
#' Defaults to \code{character(0)}.
#' \item \code{ShowColumnSelection}, a logical vector indicating whether the column selection should be shown as an extra annotation bar.
#' Defaults to \code{TRUE}.
#' \item \code{OrderColumnSelection}, a logical vector indicating whether the column selection should be used to order columns in the heatmap.
#' Defaults to \code{TRUE}.
#' }
#'
#' The following slots control the choice of assay values:
#' \itemize{
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to \code{"logcounts"} in \code{\link{getPanelDefault}},
#' falling back to the first valid assay name (see \code{.cacheCommonInfo} below).
#' }
#'
#' The following slots control the clustering of rows:
#' \itemize{
#' \item \code{ClusterRows}, a logical scalar indicating whether rows should be clustered by their assay values.
#' Defaults to \code{FALSE}.
#' \item \code{ClusterRowsDistance}, string specifying a distance measure to use.
#' This can be any one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"}, \code{"minkowski"}, \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' Defaults to \code{"spearman"}.
#' \item \code{ClusterRowsMethod}, string specifying a linkage method to use.
#' This can be any one of \code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"}, or \code{"centroid"}.
#' Defaults to \code{"ward.D2"}.
#' }
#'
#' The following control transformations applied to rows:
#' \itemize{
#' \item \code{AssayCenterRows} is a logical scalar indicating whether assay values should be centered for each row.
#' \item \code{AssayScaleRows} is a logical scalar indicating whether assay values should be scaled for each row.
#' This transformation is only applicable if \code{AssayCenterRows} is \code{TRUE}.
#' }
#'
#' The following slots control the color scale:
#' \itemize{
#' \item \code{CustomBounds} is logical scale indicating whether the color scale should be constrained by an upper and/or a lower bounds.
#' \item \code{LowerBound} is a numerical value setting the lower bound of the color scale;
#' or \code{NA} to disable the lower bound when \code{CustomBounds} is \code{TRUE}.
#' \item \code{UpperBound} is a numerical value setting the lower bound of the color scale;
#' or \code{NA} to disable the upper bound when \code{CustomBounds} is \code{TRUE}.
#' \item \code{DivergentColormap} is a character scalar indicating a 3-color divergent colormap to use when \code{AssayCenterRows} is \code{TRUE}.
#' }
#'
#' The following slots refer to general plotting parameters:
#' \itemize{
#' \item \code{ShowDimNames}, a character vector specifying the dimensions for which to display names.
#' This can contain zero or more of \code{"Rows"} and \code{"Columns"}.
#' Defaults to \code{"Rows"}.
#' \item \code{NamesRowFontSize}, a numerical value setting the font size of the row names.
#' \item \code{NamesColumnFontSize}, a numerical value setting the font size of the column names.
#' \item \code{LegendPosition}, string specifying the position of the legend on the plot.
#' Defaults to \code{"Bottom"} in \code{\link{getPanelDefault}} but can also be \code{"Right"}.
#' \item \code{LegendDirection}, string specifying the orientation of the legend on the plot for continuous covariates.
#' Defaults to \code{"Horizontal"} in \code{\link{getPanelDefault}} but can also be \code{"Vertical"}.
#' }
#'
#' The following slots control some aspects of the user interface:
#' \itemize{
#' \item \code{DataBoxOpen}, a logical scalar indicating whether the data parameter box should be open.
#' Defaults to \code{FALSE}.
#' \item \code{VisualBoxOpen}, a logical scalar indicating whether the visual parameter box should be open.
#' Defaults to \code{FALSE}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{ComplexHeatmapPlot(...)} creates an instance of a ComplexHeatmapPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ComplexHeatmapPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"ComplexHeatmapPlot"} entry containing
#' \code{valid.assay.names}, a character vector of valid (i.e., non-empty) assay names;
#' \code{discrete.assay.names}, a character vector of valid assay names with discrete atomic values;
#' \code{continuous.assay.names}, a character vector of valid assay names with continuous atomic values;
#' \code{valid.colData.names}, a character vector of names of columns in \code{colData} that are valid;
#' \code{discrete.colData.names}, a character vector of names for columns in \code{colData} with discrete atomic values;
#' \code{continuous.colData.names}, a character vector of names of columns in \code{colData} with continuous atomic values;
#' \code{valid.rowData.names}, a character vector of names of columns in \code{rowData} that are valid;
#' \code{discrete.rowData.names}, a character vector of names for columns in \code{rowData} with discrete atomic values;
#' \code{continuous.rowData.names}, a character vector of names of columns in \code{rowData} with continuous atomic values.
#' Valid assay names are defined as those that are non-empty, i.e., not \code{""};
#' valid columns in \code{colData} and \code{rowData} are defined as those that contain atomic values.
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces
#' any \code{NA} value in \code{"Assay"} with the first valid assay name;
#' and \code{NA} value in \code{"CustomRowsText"} with the first row name.
#' This will also call the equivalent \linkS4class{Panel} method for further refinements to \code{x}.
#' If no valid column metadata fields are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineInterface}(x, se, select_info)} defines the user interface for manipulating all slots described above and in the parent classes.
#' TODO
#' It will also create a data parameter box that can respond to specialized \code{\link{.defineDataInterface}},
#' and a visual parameter box and a selection parameter box both specific to the \code{ComplexHeatmapPlot} panel.
#' This will \emph{override} the \linkS4class{Panel} method.
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.defineOutput}(x)} returns a UI element for a brushable plot.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for the selection history (\code{"SelectionHistory"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For generating the output:
#' \itemize{
#' \item \code{\link{.generateOutput}(x, se, all_memory, all_contents)} returns a list containing \code{plot}, a \link{Heatmap} object;
#' \code{commands}, a list of character vector containing the R commands required to generate \code{contents} and \code{plot};
#' and \code{contents} and \code{varname}, both set to \code{NULL} as this is not a transmitting panel.
#' \item \code{\link{.exportOutput}(x, se, all_memory, all_contents)} will create a PDF file containing the current plot, and return a string containing the path to that PDF.
#' This assumes that the \code{plot} field returned by \code{\link{.generateOutput}} is a \link{Heatmap} object.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.renderOutput}(x, se, output, pObjects, rObjects)} will add a rendered plot element to \code{output}.
#' The reactive expression will add the contents of the plot to \code{pObjects$contents} and the relevant commands to \code{pObjects$commands}.
#' This will also call the equivalent \linkS4class{Panel} method to render the panel information text boxes.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Complex heatmap"}.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing a panel-specific tour.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso
#' \linkS4class{Panel}, for the immediate parent class.
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- ComplexHeatmapPlot()
#' x[["ShowDimNames"]]
#' x[["ShowDimNames"]] <- c("Rows", "Columns")
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' old_cd <- colData(sce)
#' colData(sce) <- NULL
#'
#' # Spits out a NULL and a warning if there is nothing to plot.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' colData(sce) <- old_cd
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases ComplexHeatmapPlot ComplexHeatmapPlot-class
#' .cacheCommonInfo,ComplexHeatmapPlot-method
#' .createObservers,ComplexHeatmapPlot-method
#' .defineDataInterface,ComplexHeatmapPlot-method
#' .defineInterface,ComplexHeatmapPlot-method
#' .defineOutput,ComplexHeatmapPlot-method
#' .defineInterface,ComplexHeatmapPlot-method
#' .fullName,ComplexHeatmapPlot-method
#' .generateOutput,ComplexHeatmapPlot-method
#' .hideInterface,ComplexHeatmapPlot-method
#' .multiSelectionResponsive,ComplexHeatmapPlot-method
#' .multiSelectionRestricted,ComplexHeatmapPlot-method
#' .panelColor,ComplexHeatmapPlot-method
#' .refineParameters,ComplexHeatmapPlot-method
#' .renderOutput,ComplexHeatmapPlot-method
#' .exportOutput,ComplexHeatmapPlot-method
#' initialize,ComplexHeatmapPlot-method
#' .definePanelTour,ComplexHeatmapPlot-method
#' [[,ComplexHeatmapPlot-method
#' [[,ComplexHeatmapPlot,ANY,ANY-method
#' [[<-,ComplexHeatmapPlot-method
#' [[<-,ComplexHeatmapPlot,ANY,ANY-method
#' updateObject,ComplexHeatmapPlot-method
#'
#' @name ComplexHeatmapPlot-class
NULL

#' @export
ComplexHeatmapPlot <- function(...) {
    new("ComplexHeatmapPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ComplexHeatmapPlot", function(.Object, ...) {
    args <- list(...)

    args <- .emptyDefault(args, .heatMapAssay, getPanelDefault(.heatMapAssay))
    args <- .emptyDefault(args, .heatMapCustomFeatNames, TRUE)
    args <- .emptyDefault(args, .heatMapFeatNameText, NA_character_)

    vals <- args[[.heatMapFeatNameText]]
    if (length(vals)!=1L) {
        args[[.heatMapFeatNameText]] <- paste(vals, collapse="\n")
    }

    args <- .emptyDefault(args, .heatMapClusterFeatures, FALSE)
    args <- .emptyDefault(args, .heatMapClusterDistanceFeatures, .clusterDistanceSpearman)
    args <- .emptyDefault(args, .heatMapClusterMethodFeatures, .clusterMethodWardD2)
    args <- .emptyDefault(args, .dataParamBoxOpen, FALSE)

    args <- .emptyDefault(args, .visualParamChoice, .visualParamChoiceMetadataTitle)
    args <- .emptyDefault(args, .heatMapColData, character(0))
    args <- .emptyDefault(args, .heatMapRowData, character(0))

    args <- .emptyDefault(args, .heatMapCustomAssayBounds, FALSE)
    args <- .emptyDefault(args, .assayLowerBound, NA_real_)
    args <- .emptyDefault(args, .assayUpperBound, NA_real_)
    args <- .emptyDefault(args, .assayCenterRows, FALSE)
    args <- .emptyDefault(args, .assayScaleRows, FALSE)
    args <- .emptyDefault(args, .heatMapCenteredColormap, .colormapPurpleBlackYellow)

    args <- .emptyDefault(args, .showDimnames, c(.showNamesRowTitle))

    args <- .emptyDefault(args, .namesRowFontSize, .plotFontSizeAxisTextDefault)
    args <- .emptyDefault(args, .namesColumnFontSize, .plotFontSizeAxisTextDefault)

    args <- .emptyDefault(args, .plotLegendPosition, getPanelDefault(.plotLegendPosition))
    args <- .emptyDefault(args, .plotLegendDirection, getPanelDefault(.plotLegendDirection))
    args <- .emptyDefault(args, .visualParamBoxOpen, FALSE)

    args <- .emptyDefault(args, .heatMapShowSelection, TRUE)
    args <- .emptyDefault(args, .heatMapOrderSelection, TRUE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("ComplexHeatmapPlot", function(object) {
    msg <- character(0)

    msg <- .singleStringError(msg, object, c(.heatMapAssay, .heatMapFeatNameText,
        .heatMapClusterDistanceFeatures, .heatMapClusterMethodFeatures,
        .heatMapCenteredColormap))

    msg <- .multipleChoiceError(msg, object, .visualParamChoice,
        c(.visualParamChoiceMetadataTitle, .visualParamChoiceTransformTitle, .visualParamChoiceColorTitle,
          .visualParamChoiceLabelsTitle, .visualParamChoiceLegendTitle))

    msg <- .multipleChoiceError(msg, object, .showDimnames,
        c(.showNamesRowTitle, .showNamesColumnTitle))

    msg <- .validNumberError(msg, object, .namesRowFontSize,
                             lower=0, upper=Inf)
    msg <- .validNumberError(msg, object, .namesColumnFontSize,
                             lower=0, upper=Inf)

    msg <- .allowableChoiceError(msg, object, .plotLegendPosition,
        c(.plotLegendRightTitle, .plotLegendBottomTitle))

    msg <- .allowableChoiceError(msg, object, .plotLegendDirection,
        c(.plotLegendHorizontalTitle, .plotLegendVerticalTitle))

    msg <- .validLogicalError(msg, object, c(
        .heatMapCustomFeatNames, .heatMapCustomFeatNames,
        .heatMapClusterFeatures, .dataParamBoxOpen,
        .heatMapCustomAssayBounds,
        .assayCenterRows, .assayScaleRows,
        .visualParamBoxOpen,
        .heatMapShowSelection, .heatMapOrderSelection))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "ComplexHeatmapPlot", function(x, i, j, ...) {
    if (i == "SelectionColor") {
        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.", cname, i))
        NA_character_
    } else if (i == "SelectionEffect") {
        x <- updateObject(x, check=FALSE)

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.\nUse <%s>[['%s']] and/or <%s>[['%s']] instead.",
            cname, i, cname, .selectColumnRestrict, cname, .heatMapShowSelection))

        if (slot(x, .selectColumnRestrict)) {
            "Restrict"
        } else if (slot(x, .heatMapShowSelection)) {
            "Color"
        } else {
            "Transparent"
        }
    } else {
        callNextMethod()
    }
})

#' @export
setReplaceMethod("[[", "ComplexHeatmapPlot", function(x, i, j, ..., value) {
    if (i %in% "SelectionColor") {
        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.", cname, i))
        x
    } else if (i %in% "SelectionEffect") {
        x <- updateObject(x, check=FALSE)

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.\nSet <%s>[['%s']] and/or <%s>[['%s']] instead.",
            cname, i, cname, .selectColumnRestrict, cname, .heatMapShowSelection))

        slot(x, .selectColumnRestrict) <- (value=="Restrict")
        slot(x, .heatMapShowSelection) <- (value!="Restrict")

        x
    } else {
        callNextMethod()
    }
})

###############################################################

#' @export
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment assayNames
setMethod(".cacheCommonInfo", "ComplexHeatmapPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "ComplexHeatmapPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    assays_continuous <- vapply(named_assays, .isAssayNumeric, logical(1), se=se)
    assays_discrete <- !assays_continuous

    df <- colData(se)
    coldata_displayable <- .findAtomicFields(df)
    subdf <- df[,coldata_displayable,drop=FALSE]
    coldata_discrete <- .whichGroupable(subdf, max_levels = .get_color_maxlevels())
    coldata_continuous <- union(.whichNumeric(subdf), setdiff(.whichGroupable(subdf, max_levels = Inf), coldata_discrete))

    df <- rowData(se)
    rowdata_displayable <- .findAtomicFields(df)
    subdf <- df[,rowdata_displayable,drop=FALSE]
    rowdata_discrete <- .whichGroupable(subdf, max_levels = .get_color_maxlevels())
    rowdata_continuous <- union(.whichNumeric(subdf), setdiff(.whichGroupable(subdf, max_levels = Inf), rowdata_discrete))

    .setCachedCommonInfo(se, "ComplexHeatmapPlot",
        valid.assay.names=named_assays,
        discrete.assay.names=named_assays[assays_discrete],
        continuous.assay.names=named_assays[assays_continuous],
        valid.colData.names=coldata_displayable,
        discrete.colData.names=coldata_displayable[coldata_discrete],
        continuous.colData.names=coldata_displayable[coldata_continuous],
        valid.rowData.names=rowdata_displayable,
        discrete.rowData.names=rowdata_displayable[rowdata_discrete],
        continuous.rowData.names=rowdata_displayable[rowdata_continuous])
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "ComplexHeatmapPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (nrow(se)==0L) {
        warning(sprintf("no rows available for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- .getCachedCommonInfo(se, "ComplexHeatmapPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, .heatMapAssay, all_assays)

    if (is.na(slot(x, .heatMapFeatNameText))) {
        slot(x, .heatMapFeatNameText) <- rownames(se)[1]
    }

    x
})

#' @export
setMethod(".panelColor", "ComplexHeatmapPlot", function(x) "#440154FF")

#' @export
setMethod(".fullName", "ComplexHeatmapPlot", function(x) "Complex heatmap")

#' @importFrom shiny plotOutput
#' @export
setMethod(".defineOutput", "ComplexHeatmapPlot", function(x) {
    plot_name <- .getEncodedName(x)
    addSpinner(
        plotOutput(plot_name, height=paste0(slot(x, .organizationHeight), "px")),
        color=.panelColor(x)
    )
})

#' @export
#' @importFrom shiny selectInput radioButtons checkboxInput actionButton
#' @importFrom shinyjs disabled
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "ComplexHeatmapPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .getCachedCommonInfo(se, "ComplexHeatmapPlot")$valid.assay.names

    assay_name <- slot(x, .heatMapAssay)
    assay_discrete <- assay_name %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.assay.names
    ABLEFUN <- if (assay_discrete) {
        disabled
    } else {
        identity
    }

    .addSpecificTour(class(x)[1], .heatMapAssay, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapAssay, " + .selectize-control"),
                    intro = "Here, we can select the name of the assay matrix to show.
The choices are extracted from the <code>assayNames</code> of a <code>SummarizedExperiment</code> object.
These matrices should be loaded into the object prior to calling <strong>iSEE</strong> - they are not computed on the fly."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapCustomFeatNames, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapCustomFeatNames),
                    intro = "Features displayed as rows in the heat map can be (i) manually specified by entering row names interactively in a modal, or (ii) use any multiple selection transmitted from another panel. This checkbox switches between these two options. <strong>Click on this checkbox to activate manual mode.</strong>"
                ),
                c(
                    element = paste0("#", plot_name, "_", .dimnamesModalOpen),
                    intro = "This brings up a modal that we can use to enter the names of features of interest. Each feature should be a row name in the original <code>SummarizedExperiment</code>, with one feature per line."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapClusterFeatures, function(plot_name) {
        data.frame(
            element = paste0("#", plot_name, "_", .heatMapClusterFeatures),
            intro = "Features displayed as rows in the heat map can be (i) clustered dynamically using a selection of distance metrics and clustering methods, or (ii) shown in the order they appear in <code>rownames</code>. The former choice is enabled by checking this box.<br/><br/>
The clustering itself is done using <code>hclust</code>, i.e., hierarchical clustering. This is simple and intuitive but not particularly efficient, so should only be used for small numbers of features.<br/><br/>
<strong>Click on this checkbox to cluster dynamically.</strong>"
        )
    })

    .addSpecificTour(class(x)[1], .heatMapClusterDistanceFeatures, function(plot_name) {
        data.frame(
            element = paste0("#", plot_name, "_", .heatMapClusterDistanceFeatures, " + .selectize-control"),
            intro = "Here we can choose from a variety of different metrics to compute distances between features based on their assay values in the heatmap. The resulting distance is then used in <code>hclust</code> to perform hierarchical clustering. Euclidean distances are probably most common; the Spearman distance is another popular choice that is more robust to outliers."
        )
    })

    .addSpecificTour(class(x)[1], .heatMapClusterMethodFeatures, function(plot_name) {
        data.frame(
            element = paste0("#", plot_name, "_", .heatMapClusterMethodFeatures, " + .selectize-control"),
            intro = "We can also choose from a variety of different clustering methods. Ward's method and complete linkage clustering are popular choices as they tend to yield more compact and interpretable clusters."
        )
    })

    list(
        .selectInput.iSEE(x, .heatMapAssay,
            label="Assay choice:",
            choices=all_assays,
            selected=slot(x, .heatMapAssay)),
        .checkboxInput.iSEE(x, .heatMapCustomFeatNames,
            label = "Use custom rows",
            value=slot(x, .heatMapCustomFeatNames),
            help = TRUE),
        .conditionalOnCheckSolo(
            .input_FUN(.heatMapCustomFeatNames),
            on_select=TRUE,
            actionButton(.input_FUN(.dimnamesModalOpen), label="Edit feature names")),
        ABLEFUN(
            .checkboxInput.iSEE(
                x, .heatMapClusterFeatures,
                label = "Cluster rows",
                value=slot(x, .heatMapClusterFeatures),
                help = TRUE)
            ),
        .conditionalOnCheckSolo(
            .input_FUN(.heatMapClusterFeatures),
            on_select=TRUE,
            ABLEFUN(
                .selectInput.iSEE(x, .heatMapClusterDistanceFeatures,
                    label="Clustering distance for rows",
                    choices=c(.clusterDistanceEuclidean, .clusterDistancePearson, .clusterDistanceSpearman,
                        .clusterDistanceManhattan, .clusterDistanceMaximum, .clusterDistanceCanberra,
                        .clusterDistanceBinary, .clusterDistanceMinkowski, .clusterDistanceKendall),
                    selected=slot(x, .heatMapClusterDistanceFeatures)
                )
            ),
            ABLEFUN(
                .selectInput.iSEE(x, .heatMapClusterMethodFeatures,
                    label="Clustering method for rows",
                    choices=c(.clusterMethodWardD, .clusterMethodWardD2, .clusterMethodSingle, .clusterMethodComplete,
                        "average (= UPGMA)"=.clusterMethodAverage,
                        "mcquitty (= WPGMA)"=.clusterMethodMcquitty,
                        "median (= WPGMC)"=.clusterMethodMedian,
                        "centroid (= UPGMC)"=.clusterMethodCentroid),
                    selected=slot(x, .heatMapClusterMethodFeatures)
                )
            )
        )
    )
})

#' @export
setMethod(".defineInterface", "ComplexHeatmapPlot", function(x, se, select_info) {
    out <- callNextMethod()
    list(
        out[1],
        .create_visual_box_for_complexheatmap(x, se),
        out[-1]
    )
})

#' @export
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom ggplot2 ggplot geom_text aes theme_void
#' @importFrom ComplexHeatmap Heatmap draw columnAnnotation rowAnnotation
#' @importFrom grid gpar
setMethod(".generateOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    # print(str(x))
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- .get_colormap(se)

    all_cmds <- list()
    heatmap_args <- character(0)

    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)
    all_cmds$assay <- .extractAssaySubmatrix(x, se, plot_env,
        use_custom_row_slot=.heatMapCustomFeatNames,
        custom_row_text_slot=.heatMapFeatNameText)

    # If there is a matrix to work with at all
    if (all(dim(plot_env[["plot.data"]]) > 0)) {

        # Applying tranfsormation
        trans_cmds <- .process_heatmap_assay_row_transformations(x, se, plot_env)
        if (length(trans_cmds)) {
            all_cmds$transform <- trans_cmds
        }

        # Compute the assay colormap after the transformations
        all_cmds$assay_colormap <- .process_heatmap_assay_colormap(x, se, plot_env)
        heatmap_args[["col"]] <- ".assay_colors"

        # Compute the annotations
        cmds <- .process_heatmap_column_annotations_colorscale(x, se, plot_env)
        if (length(cmds)) {
            all_cmds$column_annotations <- paste0(cmds, collapse = "\n")
            heatmap_args[["top_annotation"]] <- ".column_annot"
        }

        cmds <- .process_heatmap_row_annotations_colorscale(x, se, plot_env)
        if (length(cmds)) {
            all_cmds$row_annotations <- paste0(cmds, collapse = "\n")
            heatmap_args[["left_annotation"]] <- ".row_annot"
        }

        # Row clustering.
        if (.is_heatmap_continuous(x, se)) {
            heatmap_args[["cluster_rows"]] <- as.character(slot(x, .heatMapClusterFeatures))
            if (slot(x, .heatMapClusterFeatures)) {
                heatmap_args[["clustering_distance_rows"]] <- deparse(slot(x, .heatMapClusterDistanceFeatures))
                heatmap_args[["clustering_method_rows"]] <- deparse(slot(x, .heatMapClusterMethodFeatures))
            }
        }
    }

    # Column clustering is disabled (ordering by column metadata)
    heatmap_args[["cluster_columns"]] <- "FALSE"

    # Names
    heatmap_args[["name"]] <- deparse(.build_heatmap_assay_legend_title(x, !.is_heatmap_continuous(x, se)))
    heatmap_args[["show_row_names"]] <- as.character(.showNamesRowTitle %in% slot(x, .showDimnames))
    heatmap_args[["show_column_names"]] <- as.character(.showNamesColumnTitle %in% slot(x, .showDimnames))

    # Font sizes for names
    heatmap_args[["row_names_gp"]] <- sprintf('grid::gpar(fontsize=%s)', deparse(slot(x, .namesRowFontSize)))
    heatmap_args[["column_names_gp"]] <- sprintf('grid::gpar(fontsize=%s)', deparse(slot(x, .namesColumnFontSize)))

    # Legend parameters
    heatmap_args[['heatmap_legend_param']] <- sprintf('list(direction=%s)', deparse(tolower(slot(x, .plotLegendDirection))))

    # Heatmap
    heatmap_args <- sprintf("%s=%s", names(heatmap_args), heatmap_args)
    heatmap_args <- paste(heatmap_args, collapse=", ")
    heatmap_call <- sprintf("hm <- ComplexHeatmap::Heatmap(matrix=plot.data, %s)", heatmap_args)

    heat_cmd <- paste(strwrap(heatmap_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(heat_cmd, plot_env)
    all_cmds[["heatmap"]] <- heat_cmd

    # Add draw command after all evaluations (avoid drawing in the plotting device)
    heatmap_legend_side <- sprintf('heatmap_legend_side=%s', deparse(tolower(slot(x, .plotLegendPosition))))
    annotation_legend_side <- sprintf('annotation_legend_side=%s', deparse(tolower(slot(x, .plotLegendPosition))))
    all_cmds[["draw"]] <- sprintf("ComplexHeatmap::draw(hm, %s, %s)", heatmap_legend_side, annotation_legend_side)

    list(commands=all_cmds, plot=plot_out, varname=NULL, contents=NULL)
})

#' @export
#' @importFrom shiny renderPlot tagList
#' @importFrom ComplexHeatmap draw
setMethod(".renderOutput", "ComplexHeatmapPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

    # nocov start
    output[[plot_name]] <- renderPlot({
        p.out <- .retrieveOutput(plot_name, se, pObjects, rObjects)
        tmp.env <- new.env()
        tmp.env$hm <- p.out$plot
        eval(parse(text=p.out$commands[["draw"]]), envir=tmp.env)
    })
    # nocov end

    callNextMethod()
})

#' @export
#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    contents <- .generateOutput(x, se, all_memory=all_memory, all_contents=all_contents)
    newpath <- paste0(.getEncodedName(x), ".pdf")

    # These are reasonably satisfactory heuristics:
    # Width = Pixels -> Inches, Height = Bootstrap -> Inches.
    pdf(newpath, width=slot(x, .organizationHeight)/75, height=slot(x, .organizationWidth)*2)
    # print(contents$plot)
    draw(contents$plot)
    dev.off()

    newpath
})

#' @export
setMethod(".createObservers", "ComplexHeatmapPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    # Not much point distinguishing between protected and unprotected here,
    # as there aren't any selections transmitted from this panel anyway.
    .createProtectedParameterObservers(plot_name,
        fields=c(
            .heatMapClusterFeatures,
            .heatMapClusterDistanceFeatures,
            .heatMapClusterMethodFeatures,
            .heatMapCustomFeatNames,
            .heatMapOrderSelection
        ),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(
            .heatMapAssay,
            .heatMapCenteredColormap,
            .showDimnames,
            .namesRowFontSize,
            .namesColumnFontSize,
            .plotLegendPosition,
            .plotLegendDirection,
            .heatMapShowSelection
        ),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(
            .heatMapColData,
            .heatMapRowData
        ),
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL = FALSE)

    .create_heatmap_extra_observers(plot_name,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .createCustomDimnamesModalObservers(plot_name, .heatMapFeatNameText, .dimnamesModalOpen,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects, source_type="row")

    invisible(NULL)
})

#' @export
setMethod(".hideInterface", "ComplexHeatmapPlot", function(x, field) {
    if (field %in% c(.multiSelectHistory, .selectRowRestrict)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".multiSelectionRestricted", "ComplexHeatmapPlot", function(x) {
    ## .heatMapShowSelection is not technically restricted, but requires rerendering nonetheless
    !slot(x, .heatMapCustomFeatNames) || slot(x, .selectColumnRestrict) || slot(x, .heatMapShowSelection)
})

#' @export
setMethod(".multiSelectionResponsive", "ComplexHeatmapPlot", function(x, dim) {
    if (dim == "row") {
        if (slot(x, .selectRowRestrict) || !slot(x, .heatMapCustomFeatNames)) {
            return(TRUE)
        }
    } else {
        if (slot(x, .selectColumnRestrict) || slot(x, .heatMapShowSelection)) {
            return(TRUE)
        }
    }
    return(FALSE)
})

###############################################################

#' @export
setMethod(".definePanelTour", "ComplexHeatmapPlot", function(x) {
    rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">ComplexHeatmapPlot</font> panel contains a complex heatmap from the <i><a href='https://bioconductor.org/packages/ComplexHeatmap/'>ComplexHeatmap</a></i> package. This is quite conceptually different from the other panels as it shows assay data for multiple rows and columns at the same time. However, it is strictly an end-point panel, i.e., it cannot transmit to other panels.", .getPanelColor(x))),
        .addTourStep(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked to control the data on the heatmap.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .addTourStep(x, .visualParamBoxOpen, "The <i>Visual parameters</i> box shows the available visual parameters that can be tweaked in this heatmap.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        callNextMethod()
    )
})

#' @export
setMethod("updateObject", "ComplexHeatmapPlot", function(object, ..., verbose=FALSE) {
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
            slot(object, .heatMapShowSelection) <- (effect!="Restrict")
            slot(object, .heatMapOrderSelection) <- FALSE
        }
        # nocov end
    }

    object
})
