#' The ComplexHeatmapPlot panel
#'
#' The ComplexHeatmapPlot is a panel class for creating a \linkS4class{Panel} that displays an assay of a \linkS4class{SummarizedExperiment} object as a \code{\link{Heatmap}} with features as rows and samples and columns, respectively.
#' It provides slots and methods for specifying the features of interest, which assay to display in the main heatmap, any transformations to perform on the data, and which metadata variables to display as row and column heatmap annotations.
#'
#' @section ComplexHeatmapPlot slot overview:
#' The following slots control the assay that is used:
#' \itemize{
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,ComplexHeatmapPlot-method}"} for details).
#' \item \code{CustomRows}, a logical scalar indicating whether the custom list of features should be used.
#' If \code{FALSE}, the incoming selection is used instead. Defaults to \code{TRUE}.
#' \item \code{CustomRowsText}, string specifying a custom list of features to use, as newline-separated row names.
#' If \code{NA}, defaults to the first row name of the \linkS4class{SummarizedExperiment} object.
#' }
#'
#' The following slots control the metadata variables that are used:
#' \itemize{
#' \item \code{ColumnData}, a character vector specifying columns of the \code{\link{colData}} to show as \code{\link{columnAnnotation}}.
#' Defaults to \code{character(0)}.
#' \item \code{RowData}, a character vector specifying columns of the \code{\link{rowData}} to show as \code{\link{columnAnnotation}}.
#' Defaults to \code{character(0)}.
#' }
#'
#' The following slots control the clustering of features:
#' \itemize{
#' \item \code{ClusterRows}, a logical scalar indicating whether features should be clustered by assay data.
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
#' \item \code{LegendPosition}, string specifying the position of the legend on the plot.
#' Defaults to \code{"Bottom"} but can also be \code{"Right"}.
#' \item \code{LegendDirection}, string specifying the orientation of the legend on the plot for continuous covariates.
#' Defaults to \code{"Horizontal"} but can also be \code{"Vertical"}.
#' }
#'
#' The following slots control the effect of the transmitted selection from another panel:
#' \itemize{
#' \item \code{SelectionEffect}, a string specifying the selection effect.
#' This should be one of \code{"Color"} (the default), where all selected points change to the specified color;
#' \code{"Restrict"}, where all non-selected points are not plotted.
#' \item \code{SelectionColor}, a string specifying the color to use for selected points when \code{SelectionEffect="Color"}.
#' Defaults to \code{"red"}.
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
#' \code{valid.assay.names}, a character vector of valid assay names;
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
#' \item \code{\link{.generateOutput}(x, se, all_memory, all_contents)} returns a list containing \code{contents}, a data.frame with one row per point currently present in the plot;
#' \code{plot}, a \link{ggplot} object;
#' \code{commands}, a list of character vector containing the R commands required to generate \code{contents} and \code{plot};
#' and \code{varname}, a string containing the name of the variable in \code{commands} that was used to obtain \code{contents}.
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
#' .panelColor,ComplexHeatmapPlot-method
#' .refineParameters,ComplexHeatmapPlot-method
#' .renderOutput,ComplexHeatmapPlot-method
#' .exportOutput,ComplexHeatmapPlot-method
#' initialize,ComplexHeatmapPlot-method
#' .definePanelTour,ComplexHeatmapPlot-method
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

    args <- .emptyDefault(args, .heatMapAssay, NA_character_)
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

    args <- .emptyDefault(args, .plotLegendPosition, iSEEOptions$get("legend.position"))
    args <- .emptyDefault(args, .plotLegendDirection, iSEEOptions$get("legend.direction"))
    args <- .emptyDefault(args, .visualParamBoxOpen, FALSE)

    args <- .emptyDefault(args, .selectEffect, .selectColorTitle)
    args <- .emptyDefault(args, .selectColor, iSEEOptions$get("selected.color"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("ComplexHeatmapPlot", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object, c(.heatMapAssay, .heatMapFeatNameText,
        .heatMapClusterDistanceFeatures, .heatMapClusterMethodFeatures,
        .heatMapCenteredColormap,
        .selectEffect, .selectColor))

    msg <- .valid_string_error(msg, object, .selectColor)

    msg <- .multiple_choice_error(msg, object, .visualParamChoice,
        c(.visualParamChoiceMetadataTitle, .visualParamChoiceTransformTitle, .visualParamChoiceColorTitle,
          .visualParamChoiceLabelsTitle, .visualParamChoiceLegendTitle))

    msg <- .multiple_choice_error(msg, object, .showDimnames,
        c(.showNamesRowTitle, .showNamesColumnTitle))

    msg <- .allowable_choice_error(msg, object, .plotLegendPosition,
        c(.plotLegendRightTitle, .plotLegendBottomTitle))

    msg <- .allowable_choice_error(msg, object, .plotLegendDirection,
        c(.plotLegendHorizontalTitle, .plotLegendVerticalTitle))

    msg <- .valid_logical_error(msg, object, c(
        .heatMapCustomFeatNames, .heatMapCustomFeatNames,
        .heatMapClusterFeatures, .dataParamBoxOpen,
        .heatMapCustomAssayBounds,
        .assayCenterRows, .assayScaleRows,
        .visualParamBoxOpen))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment assayNames
setMethod(".cacheCommonInfo", "ComplexHeatmapPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "ComplexHeatmapPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]
    # matrix[0,0] preserves the storage mode, while avoiding out-of-bound errors
    assays_continuous <- vapply(named_assays, .is_assay_numeric, logical(1), se)
    assays_discrete <- !assays_continuous

    df <- colData(se)
    coldata_displayable <- .findAtomicFields(df)
    subdf <- df[,coldata_displayable,drop=FALSE]
    coldata_discrete <- .whichGroupable(subdf)
    coldata_continuous <- .whichNumeric(subdf)

    df <- rowData(se)
    rowdata_displayable <- .findAtomicFields(df)
    subdf <- df[,rowdata_displayable,drop=FALSE]
    rowdata_discrete <- .whichGroupable(subdf)
    rowdata_continuous <- .whichNumeric(subdf)

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

    all_assays <- c(intersect(iSEEOptions$get("assay"), all_assays), all_assays)
    x <- .replace_na_with_first(x, .heatMapAssay, all_assays)

    if (is.na(x[[.heatMapFeatNameText]])) {
        x[[.heatMapFeatNameText]] <- rownames(se)[1]
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
    plotOutput(plot_name, height=paste0(x[[.organizationHeight]], "px"))
})

#' @export
#' @importFrom shiny selectInput radioButtons checkboxInput actionButton
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "ComplexHeatmapPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .getCachedCommonInfo(se, "ComplexHeatmapPlot")$valid.assay.names

    assay_name <- x[[.heatMapAssay]]
    assay_discrete <- assay_name %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.assay.names
    ABLEFUN <- if (assay_discrete) {
        disabled
    } else {
        identity
    }

    list(
        selectInput(.input_FUN(.heatMapAssay), label="Assay choice",
            choices=all_assays, selected=x[[.heatMapAssay]]),
        checkboxInput(.input_FUN(.heatMapCustomFeatNames), label="Use custom rows",
            value=x[[.heatMapCustomFeatNames]]),
        .conditional_on_check_solo(
            .input_FUN(.heatMapCustomFeatNames),
            on_select=TRUE,
            actionButton(.input_FUN(.featureNamesEdit), label=.buttonEditFeatureNamesLabel)),
        ABLEFUN(checkboxInput(.input_FUN(.heatMapClusterFeatures), label="Cluster rows",
            value=x[[.heatMapClusterFeatures]])),
        .conditional_on_check_solo(
            .input_FUN(.heatMapClusterFeatures),
            on_select=TRUE,
            ABLEFUN(selectInput(.input_FUN(.heatMapClusterDistanceFeatures), label="Clustering distance for rows",
                choices=c(.clusterDistanceEuclidean, .clusterDistancePearson, .clusterDistanceSpearman,
                    .clusterDistanceManhattan, .clusterDistanceMaximum, .clusterDistanceCanberra,
                    .clusterDistanceBinary, .clusterDistanceMinkowski, .clusterDistanceKendall),
                selected=x[[.heatMapClusterDistanceFeatures]])),
            ABLEFUN(selectInput(.input_FUN(.heatMapClusterMethodFeatures), label="Clustering method for rows",
                choices=c(.clusterMethodWardD, .clusterMethodWardD2, .clusterMethodSingle, .clusterMethodComplete,
                    "average (= UPGMA)"=.clusterMethodAverage,
                    "mcquitty (= WPGMA)"=.clusterMethodMcquitty,
                    "median (= WPGMC)"=.clusterMethodMedian,
                    "centroid (= UPGMC)"=.clusterMethodCentroid),
                selected=x[[.heatMapClusterMethodFeatures]])))
    )
})

#' @export
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom ggplot2 ggplot geom_text aes theme_void
#' @importFrom ComplexHeatmap Heatmap draw columnAnnotation rowAnnotation
setMethod(".generateOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    # print(str(x))
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    heatmap_args <- character(0)

    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)
    all_cmds$assay <- .process_heatmap_assay_values(x, se, plot_env)

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
            heatmap_args[["cluster_rows"]] <- as.character(x[[.heatMapClusterFeatures]])
            if (x[[.heatMapClusterFeatures]]) {
                heatmap_args[["clustering_distance_rows"]] <- deparse(x[[.heatMapClusterDistanceFeatures]])
                heatmap_args[["clustering_method_rows"]] <- deparse(x[[.heatMapClusterMethodFeatures]])
            }
        }
    }

    # Column clustering is disabled (ordering by column metadata)
    heatmap_args[["cluster_columns"]] <- "FALSE"

    # Names
    heatmap_args[["name"]] <- deparse(.build_heatmap_assay_legend_title(x, !.is_heatmap_continuous(x, se)))
    heatmap_args[["show_row_names"]] <- as.character(.showNamesRowTitle %in% x[[.showDimnames]])
    heatmap_args[["show_column_names"]] <- as.character(.showNamesColumnTitle %in% x[[.showDimnames]])

    # Legend parameters
    heatmap_args[['heatmap_legend_param']] <- sprintf('list(direction=%s)', deparse(tolower(x[[.plotLegendDirection]])))

    # Heatmap
    heatmap_args <- sprintf("%s=%s", names(heatmap_args), heatmap_args)
    heatmap_args <- paste(heatmap_args, collapse=", ")
    heatmap_call <- sprintf("hm <- ComplexHeatmap::Heatmap(matrix=plot.data, %s)", heatmap_args)

    heat_cmd <- paste(strwrap(heatmap_call, width = 80, exdent = 4), collapse = "\n")
    plot_out <- .textEval(heat_cmd, plot_env)
    all_cmds[["heatmap"]] <- heat_cmd

    # Add draw command after all evaluations (avoid drawing in the plotting device)
    heatmap_legend_side <- sprintf('heatmap_legend_side=%s', deparse(tolower(x[[.plotLegendPosition]])))
    annotation_legend_side <- sprintf('annotation_legend_side=%s', deparse(tolower(x[[.plotLegendPosition]])))
    all_cmds[["draw"]] <- sprintf("ComplexHeatmap::draw(hm, %s, %s)", heatmap_legend_side, annotation_legend_side)

    list(commands=all_cmds, contents=plot_env$plot.data, plot=plot_out, varname="plot.data")
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
    pdf(newpath, width=x[[.organizationHeight]]/75, height=x[[.organizationWidth]]*2)
    # print(contents$plot)
    draw(contents$plot)
    dev.off()
    
    newpath
})

#' @export
setMethod(".defineInterface", "ComplexHeatmapPlot", function(x, se, select_info) {
    list(
        .create_data_param_box(x, se, select_info),
        .create_visual_box_for_complexheatmap(x, se),
        .create_heatmap_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

#' @export
setMethod(".createObservers", "ComplexHeatmapPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    # Not much point distinguishing between protected and unprotected here,
    # as there aren't any selections transmitted from this panel anyway.
    .createProtectedParameterObservers(plot_name,
        fields=c(.heatMapCustomFeatNames),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.heatMapClusterFeatures, .heatMapClusterDistanceFeatures, .heatMapClusterMethodFeatures,
            .heatMapColData, .heatMapRowData,
            .heatMapCenteredColormap,
            .selectEffect, .selectColor,
            .showDimnames,
            .plotLegendPosition, .plotLegendDirection),
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL = FALSE)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_heatmap_extra_observers(plot_name,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_modal_observers_for_dimnames(plot_name, .heatMapFeatNameText, .featureNamesEdit,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects, "row")

    invisible(NULL)
})

#' @export
setMethod(".hideInterface", "ComplexHeatmapPlot", function(x, field) {
    if (field %in% c(.multiSelectHistory)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".definePanelTour", "ComplexHeatmapPlot", function(x) {
    collated <- rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">ComplexHeatmap plot</font> panel contains... well, a complex heatmap, from the <i><a href='https://bioconductor.org/packages/ComplexHeatmap/'>ComplexHeatmap</a></i> package. This is quite conceptually different from the other panels as it shows assay data for multiple rows and columns at the same time. However, it is strictly an end-point panel, i.e., it cannot transmit to other panels.", .getPanelColor(x))),
        .add_tour_step(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked to control the data on the heatmap.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .add_tour_step(x, .featureNamesEdit, "The most relevant parameter is the choice of features to show as rows on the heatmap. This can be manually specified by entering row names of the <code>SummarizedExperiment</code> object into this modal..."),
        .add_tour_step(x, .heatMapCustomFeatNames, "Or it can be chained to a multiple row selection from another panel, if the <i>Custom rows</i> choice is unselected - see the <i>Selection parameters</i> later."),
        .add_tour_step(x, .heatMapClusterFeatures, "We can also choose whether to cluster the features for better visibility."),
       
        .add_tour_step(x, .visualParamBoxOpen, "The <i>Visual parameters</i> box shows the available visual parameters that can be tweaked in this heatmap.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .add_tour_step(x, .visualParamChoice, "A large number of options are available here, so not all of them are shown by default. We can check some of the boxes here to show or hide some classes of parameters.<br/><br/><strong>Action:</strong> check the <i>Transform</i> box to expose some transformation options."),
        .add_tour_step(x, .heatMapColData, "One key parameter is to select the column annotations to show as color bars on the top of the heatmap. This will also order the columns by the values of the selected annotations (in the specified order, if multiple annotations are specified). This is useful for providing some structure to the heatmap.",
            element=paste0("#", .getEncodedName(x), "_", .heatMapColData, " + .selectize-control")),
        .add_tour_step(x, .assayCenterRows, "Another useful setting is to center the heatmap by row so that the colors represent deviations from the average. This better handles differences in assay values "),

        callNextMethod(),
        .add_tour_step(x, .selectEffect, "Here, we can choose the effect of the multiple column selection that was transmitted from the chosen source panel - should the unselected columns be colored with a separate annotation bar? Or should the heatmap be explicitly restricted to only the selected columns?")
    )

    collated[which(collated$intro=="PLACEHOLDER_ROW_SELECT"), "intro"] <- "We can choose the \"source\" panel from which to receive a multiple row selection, which is used to control the features on the heatmap when <i>Custom rows</i> checkbox is unselected. In other words, if we selected some rows of the <code>SummarizedExperiment</code> object in the chosen source panel, those rows would make up the rows of the heatmap."

    collated[which(collated$intro=="PLACEHOLDER_COLUMN_SELECT"), "intro"] <- "We can choose the \"source\" panel from which to receive a multiple column selection. That is to say, if we selected some columns of the <code>SummarizedExperiment</code> object in the chosen source panel, that selection would manifest in the appearance of the heatmap."

    collated
})
