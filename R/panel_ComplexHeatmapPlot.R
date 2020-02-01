#' The ComplexHeatmapPlot panel
#'
#' The ComplexHeatmapPlot is a panel class for creating a \linkS4class{Panel} that displays an assay of a \linkS4class{SummarizedExperiment} object as a \code{\link{Heatmap}} with features as rows and samples and columns, respectively.
#' It provides slots and methods for specifying which assay to display in the main heatmap, and which metadata variables to display as row and column heatmap annotations.
#'
#' @section Slot overview:
#' The following slots control the assay that is used:
#' \itemize{
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,ComplexHeatmapPlot-method}"} for details).
#' \item \code{CustomFeatName}, a logical scalar indicating whether the custom list of features should be used.
#' If \code{FALSE}, the incoming selection is used instead. Defaults to \code{TRUE}.
#' \item \code{FeatNameText}, string speciyfing a custom list of features to use, as newline-separated row names.
#' If \code{NA}, defaults to the first row name of the SummarizedExperiment object.
#' }
#'
#' The following slots control the metadata variables that are used:
#' \itemize{
#' \item \code{ColData}, a character vector specifying columns of the \code{\link{colData}} to show as \code{\link{columnAnnotation}}.
#' Defaults to \code{character(0)}.
#' \item \code{RowData}, a character vector specifying columns of the \code{\link{rowData}} to show as \code{\link{columnAnnotation}}.
#' Defaults to \code{character(0)}.
#' }
#'
#' The following slots control the clustering of features:
#' \itemize{
#' \item \code{ClusterFeatures}, a logical scalar indicating whether features should be clustered by assay data.
#' Defaults to \code{FALSE}.
#' \item \code{ClusterDistanceFeatures}, string specifying a distance measure to use.
#' This can be any one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"}, \code{"minkowski"}, \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' Defaults to \code{"spearman"}.
#' \item \code{ClusterMethodFeatures}, string specifying a distance measure to use.
#' This can be any one of \code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"}, or \code{"centroid"}.
#' Defaults to \code{"ward.D2"}.
#' }
#'
#' The following slots refer to general plotting parameters:
#' \itemize{
#' \item \code{ShowDimNames}, a character vector specifying the dimensions for which to display names.
#' This can contain zero or more of \code{"Features"} and \code{"Samples"}.
#' Defaults to \code{"Features"}.
#' \item \code{LegendPosition}, string specifying the position of the legend on the plot.
#' Defaults to \code{"Bottom"} but can also be \code{"Right"}.
#' \item \code{LegendDirection}, string specifying the orientation of the legend on the plot for continuous covariates.
#' Defaults to \code{"Horizontal"} but can also be \code{"Vertical"}.
#' }
#'
#' The following slots control the effect of the transmitted selection from another panel:
#' \itemize{
#' \item \code{SelectEffect}, a string specifying the selection effect.
#' This should be one of \code{"Color"} (the default), here all selected points change to the specified color;
#' \code{"Restrict"}, where all non-selected points are not plotted.
#' \item \code{SelectColor}, a string specifying the color to use for selected points when \code{SelectEffect="Color"}.
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
#' @section Contract description:
#' The ComplexHeatmapPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ComplexHeatmapPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after replacing any \code{NA} value in \code{"Assay"} with the first valid assay name; and any \code{NA} values in \code{FeatNameText} with the first row name.
#' This will also call the equivalent \linkS4class{Panel} method for further refinements to \code{x}.
#' If no valid column metadata fields are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.fullName}(x)} will return the full name of the panel class.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{Panel} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Complex heatmap"}.
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
#' x[["ShowDimNames"]] <- c("Features", "Samples")
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
#' initialize,ComplexHeatmapPlot-method
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

    args <- .empty_default(args, .heatMapAssay, NA_character_)
    args <- .empty_default(args, .heatMapCustomFeatNames, TRUE)
    args <- .empty_default(args, .heatMapFeatNameText, NA_character_)
    if (!is.na(vals <- args[[.heatMapFeatNameText]])) {
        args[[.heatMapFeatNameText]] <- paste(vals, collapse="\n")
    }

    args <- .empty_default(args, .heatMapClusterFeatures, FALSE)
    args <- .empty_default(args, .heatMapClusterDistanceFeatures, .clusterDistanceSpearman)
    args <- .empty_default(args, .heatMapClusterMethodFeatures, .clusterMethodWardD2)
    args <- .empty_default(args, .dataParamBoxOpen, FALSE)

    args <- .empty_default(args, .heatMapColData, character(0))
    args <- .empty_default(args, .heatMapRowData, character(0))

    args <- .empty_default(args, .heatMapCustomAssayBounds, FALSE)
    args <- .empty_default(args, .assayLowerBound, NA_real_)
    args <- .empty_default(args, .assayUpperBound, NA_real_)
    args <- .empty_default(args, .assayCenterRowsTitle, FALSE)
    args <- .empty_default(args, .assayScaleRowsTitle, FALSE)
    args <- .empty_default(args, .heatMapDivergentColormap, .colormapPurpleBlackYellow)

    args <- .empty_default(args, .showDimnames, c(.showNamesRowTitle))

    args <- .empty_default(args, .plotLegendPosition, .plotLegendBottomTitle)
    args <- .empty_default(args, .plotLegendDirection, .plotLegendHorizontalTitle)
    args <- .empty_default(args, .visualParamBoxOpen, FALSE)

    args <- .empty_default(args, .selectEffect, .selectColorTitle)
    args <- .empty_default(args, .selectColor, "red")

    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("ComplexHeatmapPlot", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object, c(.heatMapAssay, .heatMapFeatNameText,
        .heatMapClusterDistanceFeatures, .heatMapClusterMethodFeatures,
        .heatMapDivergentColormap,
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
        .assayCenterRowsTitle, .assayScaleRowsTitle,
        .visualParamBoxOpen))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "ComplexHeatmapPlot", function(x, se) {
    if (!is.null(.get_common_info(se, "ComplexHeatmapPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]
    # matrix[0,0] preserves the storage mode, while avoiding out-of-bound errors
    assays_continuous <- vapply(named_assays, function(name){is.numeric(assay(se, name)[0, 0])}, logical(1))
    assays_discrete <- !assays_continuous

    df <- colData(se)
    coldata_displayable <- .find_atomic_fields(df)
    subdf <- df[,coldata_displayable,drop=FALSE]
    coldata_discrete <- .which_groupable(subdf)
    coldata_continuous <- .which_numeric(subdf)

    df <- rowData(se)
    rowdata_displayable <- .find_atomic_fields(df)
    subdf <- df[,rowdata_displayable,drop=FALSE]
    rowdata_discrete <- .which_groupable(subdf)
    rowdata_continuous <- .which_numeric(subdf)

    .set_common_info(se, "ComplexHeatmapPlot",
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

    all_assays <- .get_common_info(se, "ComplexHeatmapPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replace_na_with_first(x, .heatMapAssay, all_assays)

    if (is.na(x[[.heatMapFeatNameText]])) {
        x[[.heatMapFeatNameText]] <- rownames(se)[1]
    }

    x
})

#' @export
setMethod(".panelColor", "ComplexHeatmapPlot", function(x) "#ABCDEF")

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

    all_assays <- .get_common_info(se, "ComplexHeatmapPlot")$valid.assay.names

    list(
        selectInput(.input_FUN(.heatMapAssay), label="Assay choice",
            choices=all_assays, selected=x[[.heatMapAssay]]),
        checkboxInput(.input_FUN(.heatMapCustomFeatNames), label="Use custom feature names",
            value=x[[.heatMapCustomFeatNames]]),
        .conditional_on_check_solo(
            .input_FUN(.heatMapCustomFeatNames),
            on_select=TRUE,
            actionButton(.input_FUN(.featureNamesEdit), label=.buttonEditFeatureNamesLabel)),
        checkboxInput(.input_FUN(.heatMapClusterFeatures), label="Cluster features",
            value=x[[.heatMapClusterFeatures]]),
        .conditional_on_check_solo(
            .input_FUN(.heatMapClusterFeatures),
            on_select=TRUE,
            selectInput(.input_FUN(.heatMapClusterDistanceFeatures), label="Clustering distance for features",
                choices=c(.clusterDistanceEuclidean, .clusterDistancePearson, .clusterDistanceSpearman,
                    .clusterDistanceManhattan, .clusterDistanceMaximum, .clusterDistanceCanberra,
                    .clusterDistanceBinary, .clusterDistanceMinkowski, .clusterDistanceKendall),
                selected=x[[.heatMapClusterDistanceFeatures]]),
            selectInput(.input_FUN(.heatMapClusterMethodFeatures), label="Clustering method for features",
                choices=c(.clusterMethodWardD, .clusterMethodWardD2, .clusterMethodSingle, .clusterMethodComplete,
                    "average (= UPGMA)"=.clusterMethodAverage,
                    "mcquitty (= WPGMA)"=.clusterMethodMcquitty,
                    "median (= WPGMC)"=.clusterMethodMedian,
                    "centroid (= UPGMC)"=.clusterMethodCentroid),
                selected=x[[.heatMapClusterMethodFeatures]]))
    )
})

.process_heatmap_assay_colormap <- function(x, se, plot_env) {
    assay_name <- x[[.heatMapAssay]]

    cmds <- c()

    if (assay_name %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.assay.names) {
        is_centered <- x[[.assayCenterRowsTitle]]
        if (is_centered) {
            choice_colors <- x[[.heatMapDivergentColormap]]
            choice_colors <- strsplit(choice_colors, split = " < ", fixed = TRUE)[[1]]
            cmds <- c(cmds, sprintf(".col_colors <- %s", deparse(choice_colors)))
        } else {
            cmds <- c(cmds, sprintf(".col_colors <- assayColorMap(colormap, %s, discrete=FALSE)(21L)", deparse(assay_name)))
        }
        .text_eval(cmds, plot_env)
        cmds <- c(cmds, .process_heatmap_continuous_assay_colorscale(x, plot_env, is_centered))
        cmds <- c(cmds, "heatmap_col <- .col_FUN")
    } else if (assay_name %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.assay.names) {
        cmds <- c(cmds, ".col_values <- as.vector(plot.data)")
        cmds <- c(cmds, '.col_values <- setdiff(.col_values, NA)')
        cmds <- c(cmds, sprintf(".col_colors <- colDataColorMap(colormap, %s, discrete=TRUE)(%s)",
            deparse(assay_name), 'length(unique(.col_values))'))
        cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
        cmds <- c(cmds, "heatmap_col <- .col_colors")
    }

    cmds
}

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation
.process_heatmap_column_annotations <- function(x, se, plot_env) {
    cmds <- c()
    if (length(x[[.heatMapColData]]) || x[[.selectEffect]] == .selectColorTitle) {
        cmds <- c(cmds, "# Keep all samples to compute the full range of continuous annotations")
        cmds <- c(cmds, sprintf("column_data <- colData(se)[, %s, drop=FALSE]", deparse(x[[.heatMapColData]])))
        # Process selected points
        if (x[[.selectEffect]] == .selectColorTitle) {
            cmds <- c(cmds, 'column_data[["Selected points"]] <- rep(FALSE, nrow(column_data))')
            if (exists("col_selected", envir=plot_env, inherits=FALSE)){
                cmds <- c(cmds, 'column_data[unlist(col_selected), "Selected points"] <- TRUE')
            }
        }
        .text_eval(cmds, plot_env)
        # Collect color maps
        cmds <- c(cmds, "", "column_col <- list()", "")
        for (annot in x[[.heatMapColData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(plot_env, annot, "column_data"))
            # Need to store and evaluate this command, without re-evaluating all the previous ones (TODO: use a command store)
            cmd_get_value <- sprintf(".col_values <- column_data[[%s]]", deparse(annot))
            cmds <- c(cmds, cmd_get_value)
            .text_eval(cmd_get_value, plot_env)
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.colData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- colDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot)))
                cmds <- c(cmds, .process_heatmap_continuous_annotation_colorscale(x, plot_env))
                cmds <- c(cmds, sprintf("column_col[[%s]] <- .col_FUN", deparse(annot)))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.colData.names) {
                cmds <- c(cmds, ".col_values <- setdiff(.col_values, NA)")
                cmds <- c(cmds, sprintf(".col_colors <- colDataColorMap(colormap, %s, discrete=TRUE)(%s)",
                    deparse(annot), 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf("column_col[[%s]] <- .col_colors", deparse(annot)))
            }
            cmds <- c(cmds, "")
        }
        # Add color map for selected points
        if (x[[.selectEffect]] == .selectColorTitle) {
            cmds <- c(cmds,
                sprintf('column_col[["Selected points"]] <- c("TRUE"=%s, "FALSE"="white")', deparse(x[[.selectColor]])),
                "")
        }
        cmds <- c(cmds, 'column_data <- column_data[.heatmap.columns, , drop=FALSE]')
        cmds <- c(cmds, 'column_data <- as.data.frame(column_data, optional=TRUE)') # preserve colnames
        if (length(x[[.heatMapColData]])) {
            cmds <- c(cmds, sprintf(".column_annot_order <- with(column_data, order(%s))",
                paste0(x[[.heatMapColData]], collapse=", ")))
            cmds <- c(cmds, "column_data <- column_data[.column_annot_order, , drop=FALSE]")
        }
        cmds <- c(cmds, sprintf("column_annot <- columnAnnotation(df=column_data, col=column_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))))
    }
    cmds
}

.process_heatmap_row_annotations <- function(x, se, plot_env) {
    cmds <- c()
    if (length(x[[.heatMapRowData]])) {
        cmds <- c(cmds, "# Keep all features to compute the full range of continuous annotations")
        cmds <- c(cmds, sprintf("row_data <- rowData(se)[, %s, drop=FALSE]", deparse(x[[.heatMapRowData]])))
        .text_eval(cmds, plot_env)
        # column color maps
        cmds <- c(cmds, "", "row_col <- list()", "")
        for (annot in x[[.heatMapRowData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(plot_env, annot, "row_data"))
            # Need to store and evaluate this command, without re-evaluating all the previous ones (TODO: use a command store)
            cmd_get_value <- sprintf('.col_values <- row_data[[%s]]', deparse(annot))
            cmds <- c(cmds, cmd_get_value)
            .text_eval(cmd_get_value, plot_env)
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.rowData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot)))
                cmds <- c(cmds, .process_heatmap_continuous_annotation_colorscale(x, plot_env))
                cmds <- c(cmds, sprintf('row_col[[%s]] <- .col_FUN', deparse(annot)))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.rowData.names) {
                cmds <- c(cmds, sprintf('.col_values <- setdiff(.col_values, NA)', annot))
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, %s, discrete=TRUE)(%s)',
                    deparse(annot), 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('row_col[[%s]] <- .col_colors', deparse(annot)))
            }
            cmds <- c(cmds, "")
        }
        cmds <- c(cmds, 'row_data <- row_data[.heatmap.rows, , drop=FALSE]')
        cmds <- c(cmds, 'row_data <- as.data.frame(row_data, optional=TRUE)') # preserve colnames
        cmds <- c(cmds, sprintf("row_annot <- rowAnnotation(df=row_data, col=row_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))))
    }
    cmds
}

#' Range utilities
#'
#' \code{.clean_colorscale_range} adds 1 to the upper bound of 0-length ranges.
#' It also subtracts 1 from the lower bound if the range is meant to be centered.
#'
#' @param col_range A numeric vector of length 2.
#'
#' @return The input given range with boundaries edited
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_range
.clean_colorscale_range <- function(range, centered) {
    if (identical(range[1], range[2])) {
        # "colorRamp2" does not like all-identical breaks
        range[2] <- range[2] + 1
        if (centered) {
            range[1] <- range[1] - 1
        }
    }
    range
}

#' Process continuous colorscales
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#' @param envir An environment containing \code{plot.data}.
#'
#' @return A character vector of commands that define \code{.col_FUN}, a function which accepts a vector of numeric values and returns interpolated colors.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso \code{\link{colorRamp2}}
#'
#' @rdname INTERNAL_process_heatmap_continuous_colorscale
.process_heatmap_continuous_annotation_colorscale <- function(x, envir) {
    cmds <- c()

    col_values <- as.vector(envir$plot.data)
    col_range <- range(col_values, na.rm = TRUE)

    col_range <- .clean_colorscale_range(col_range, centered = FALSE)

    return(sprintf(
        '.col_FUN <- colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = .col_colors)',
        col_range[1], col_range[2]))
}

#' @param centered A logical scalar that indicates whether \code{plot.data} rows are centered.
#'
#' @rdname INTERNAL_process_heatmap_continuous_colorscale
.process_heatmap_continuous_assay_colorscale <- function(x, envir, centered=FALSE) {
    cmds <- c()

    if (x[[.heatMapCustomAssayBounds]]) {
        lower_bound <- x[[.assayLowerBound]]
        upper_bound <- x[[.assayUpperBound]]

        if (any(is.na(c(lower_bound, upper_bound)))) {
            col_range <- range(envir$plot.data, na.rm = TRUE)
            if (is.na(lower_bound)) {
                lower_bound <- col_range[1]
            }
            if (is.na(upper_bound)) {
                upper_bound <- col_range[2]
            }
        }
        col_range <- c(lower_bound, upper_bound)
    } else {
        col_range <- range(envir$plot.data, na.rm = TRUE)
    }

    col_range <- .clean_colorscale_range(col_range, centered)

    if (centered) {
        cmds <- c(cmds, sprintf(
            '.col_FUN <- colorRamp2(breaks = c(%s, 0, %s), colors = .col_colors)',
            col_range[1], col_range[2]))
    } else {
        cmds <- c(cmds, sprintf(
            ".col_FUN <- colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = .col_colors)",
            col_range[1], col_range[2]))
    }

    return(cmds)
}

.process_heatmap_assay_row_transformations <- function(x) {
    cmds <- c()

    if (x[[.assayCenterRowsTitle]]) {
        cmds <- c(cmds, "plot.data <- plot.data - rowMeans(plot.data)")
        if (x[[.assayScaleRowsTitle]]) {
            cmds <- c(cmds, "plot.data <- plot.data / rowSds(plot.data)")
        }
    }

    return(cmds)
}

.convert_text_to_names <- function(txt)
# Remove comment and whitespace.
{
    rn <- strsplit(txt, split="\n")[[1]]
    rn <- sub("#.*", "", rn)
    rn <- sub("^ +", "", rn)
    sub(" +$", "", rn)
}

#' @export
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom ggplot2 ggplot geom_text aes theme_void
#' @importFrom ComplexHeatmap Heatmap draw columnAnnotation rowAnnotation
setMethod(".generateOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    print(str(x))
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    heatmap_args <- ""

    # Feature names default to custom selection if no multiple selection is available.
    if (x[[.heatMapCustomFeatNames]] || is.null(plot_env$row_selected)) {
        rn <- .convert_text_to_names(x[[.heatMapFeatNameText]])
        rn <- intersect(rn, rownames(se))
        all_cmds[["rows"]] <- sprintf(".heatmap.rows <- %s;", .deparse_for_viewing(rn))
    } else {
        all_cmds[["rows"]] <- ".heatmap.rows <- intersect(rownames(se), unlist(row_selected));"
    }

    # TODO: implement visual effects for other forms of selection.
    if (!is.null(plot_env$col_selected) && x[[.selectEffect]]==.selectRestrictTitle) {
        all_cmds[["columns"]] <- ".heatmap.columns <- intersect(colnames(se), unlist(col_selected));"
    } else {
        # includes color effect
        all_cmds[["columns"]] <- ".heatmap.columns <- colnames(se);"
    }

    assay_name <- x[[.heatMapAssay]]
    all_cmds[["data"]] <- sprintf(
        'plot.data <- assay(se, %s)[.heatmap.rows, .heatmap.columns, drop=FALSE]',
        deparse(assay_name))
    .text_eval(all_cmds, plot_env) # TODO: use a command store to avoid re-evaluating those commands below

    # If there is a matrix to work with at all
    if (all(dim(plot_env[["plot.data"]]) > 0)) {
        # Assay matrix
        cmds <- .process_heatmap_assay_row_transformations(x)
        .text_eval(cmds, plot_env) # TODO: use a command store to avoid re-evaluating those commands below
        all_cmds[["assay_transforms"]] <- paste0(cmds, collapse = "\n")

        # Compute the assay colormap after the transformations
        cmds <- .process_heatmap_assay_colormap(x, se, plot_env)
        all_cmds[["assay_colormap"]] <- paste0(cmds, collapse = "\n")
        heatmap_args <- paste0(heatmap_args, ", col=heatmap_col")

        # Side annotations
        cmds <- .process_heatmap_column_annotations(x, se, plot_env)
        if (length(cmds)) {
            all_cmds[["column_annotations"]] <- paste0(cmds, collapse = "\n")
            heatmap_args <- paste0(heatmap_args, ", top_annotation=column_annot")
        }
        cmds <- .process_heatmap_row_annotations(x, se, plot_env)
        if (length(cmds)) {
            all_cmds[["row_annotations"]] <- paste0(cmds, collapse = "\n")
            heatmap_args <- paste0(heatmap_args, ", left_annotation=row_annot")
        }

        # Row clustering
        heatmap_args <- paste0(heatmap_args, ", ",
            sprintf("cluster_rows=%s", x[[.heatMapClusterFeatures]]))
        # Row clustering options
        if (x[[.heatMapClusterFeatures]]) {
            heatmap_args <- paste0(heatmap_args, ", ",
                sprintf("clustering_distance_rows=%s", deparse(x[[.heatMapClusterDistanceFeatures]])))
            heatmap_args <- paste0(heatmap_args, ", ",
                sprintf("clustering_method_rows=%s", deparse(x[[.heatMapClusterMethodFeatures]])))
        }

        .text_eval(all_cmds[["column_annotations"]], plot_env) # TODO, use command store
        if (exists(".column_annot_order", plot_env, inherits = FALSE)) {
            all_cmds[["order_columns"]] <- c("plot.data <- plot.data[, .column_annot_order, drop=FALSE]")
        }

    }

    # Column clustering is disabled (ordering by column metadata)
    heatmap_args <- paste0(heatmap_args, ", cluster_columns=FALSE")

    # Names
    heatmap_args <- paste0(heatmap_args, ", ", sprintf('name=%s', deparse(assay_name)))
    heatmap_args <- paste0(heatmap_args, ", ",
        sprintf("show_row_names=%s", .showNamesRowTitle %in% x[[.showDimnames]]))
    heatmap_args <- paste0(heatmap_args, ", ",
        sprintf("show_column_names=%s", .showNamesColumnTitle %in% x[[.showDimnames]]))

    # Legend parameters
    heatmap_args <- paste0(heatmap_args, ", ",
        sprintf('heatmap_legend_param=list(direction=%s)', deparse(tolower(x[[.plotLegendDirection]]))))

    # Cleanup
    heatmap_args <- paste0(strwrap(heatmap_args, width = 80, exdent = 4), collapse = "\n")

    # Heatmap
    all_cmds[["heatmap"]] <- sprintf("hm <- Heatmap(matrix = plot.data%s)", heatmap_args)

    print(all_cmds)
    plot_out <- .text_eval(all_cmds, plot_env) # TODO, use command store

    panel_data <- plot_env$plot.data

    # Add draw command after all evaluations (avoid drawing in the plotting device)
    draw_args_list <- list(
        heatmap_legend_side=tolower(x[[.plotLegendPosition]]),
        annotation_legend_side=tolower(x[[.plotLegendPosition]])
    )
    heatmap_legend_side <- sprintf('heatmap_legend_side = %s', deparse(tolower(x[[.plotLegendPosition]])))
    annotation_legend_side <- sprintf('annotation_legend_side = %s', deparse(tolower(x[[.plotLegendPosition]])))
    draw_args <- paste("", heatmap_legend_side, annotation_legend_side, sep = ", ")
    all_cmds[["draw"]] <- sprintf("draw(hm%s)", draw_args)

    list(commands=all_cmds, contents=panel_data, plot=plot_out, draw_args_list=draw_args_list)
})

#' @export
setMethod(".renderOutput", "ComplexHeatmapPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)

    .create_plot_output(plot_name, se=se, output=output, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".defineInterface", "ComplexHeatmapPlot", function(x, se, select_info) {
    list(
        .create_data_param_box(x, se, select_info),
        .create_visual_box_for_complexheatmap(x, se),
        .create_heatmap_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

.create_visual_box_for_complexheatmap <- function(x, se) {
    plot_name <- .getEncodedName(x)

    all_coldata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.colData.names
    all_rowdata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.rowData.names

    assay_range <- range(assay(se, x[[.heatMapAssay]]), na.rm = TRUE)

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    pchoice_field <- .input_FUN(.visualParamChoice)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=x[[.visualParamChoice]],
            choices=c(.visualParamChoiceMetadataTitle, .visualParamChoiceTransformTitle, .visualParamChoiceColorTitle, .visualParamChoiceLabelsTitle, .visualParamChoiceLegendTitle)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceMetadataTitle,
            hr(),
            selectizeInput(.input_FUN(.heatMapColData), label="Column annotations:",
                selected=x[[.heatMapColData]], choices=all_coldata, multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop'))),
            selectizeInput(.input_FUN(.heatMapRowData), label="Row annotations:",
                selected=x[[.heatMapRowData]], choices=all_rowdata, multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop')))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceTransformTitle,
            hr(),
            strong("Row transformations:"),
            checkboxInput(.input_FUN(.assayCenterRowsTitle), "Center", value=x[[.assayCenterRowsTitle]]),
            .conditional_on_check_solo(.input_FUN(.assayCenterRowsTitle), on_select = TRUE,
                checkboxInput(.input_FUN(.assayScaleRowsTitle), "Scale", value=x[[.assayCenterRowsTitle]]),
                selectizeInput(.input_FUN(.heatMapDivergentColormap), label="Divergent assay colormap:",
                    selected=x[[.heatMapDivergentColormap]],
                    choices=c(.colormapPurpleBlackYellow, .colormapBlueWhiteOrange, .colormapBlueWhiteRed, .colormapGreenWhiteRed))
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            checkboxInput(.input_FUN(.heatMapCustomAssayBounds), "Use custom colorscale bounds",
                value = x[[.heatMapCustomAssayBounds]]),
            .conditional_on_check_solo(.input_FUN(.heatMapCustomAssayBounds), on_select = TRUE,
                numericInput(.input_FUN(.assayLowerBound), "Lower bound",
                    value=x[[.assayLowerBound]], min = -Inf, max = Inf),
                numericInput(.input_FUN(.assayUpperBound), "Upper bound",
                    value=x[[.assayUpperBound]], min = -Inf, max = Inf))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceLabelsTitle,
            hr(),
            checkboxGroupInput(
                inputId=.input_FUN(.showDimnames), label="Show names:", inline=TRUE,
                selected=x[[.showDimnames]],
                choices=c(.showNamesRowTitle, .showNamesColumnTitle))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceLegendTitle,
            hr(),
            radioButtons(.input_FUN(.plotLegendPosition), label="Legend position:", inline=TRUE,
                choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
                selected=x[[.plotLegendPosition]]),
            radioButtons(.input_FUN(.plotLegendDirection), label="Legend direction:", inline=TRUE,
                choices=c(.plotLegendHorizontalTitle, .plotLegendVerticalTitle),
                selected=x[[.plotLegendDirection]])
        )
    )
}

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
            .heatMapCustomAssayBounds,
            .assayCenterRowsTitle, .assayScaleRowsTitle, .heatMapDivergentColormap,
            .selectEffect, .selectColor,
            .showDimnames,
            .plotLegendPosition, .plotLegendDirection),
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL = FALSE)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_heatmap_extra_observers(plot_name,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_heatmap_modal_observers(plot_name,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    invisible(NULL)
})


.create_heatmap_extra_observers <- function(plot_name, se, input, session, pObjects, rObjects) {

    .input_FUN <- function(field) paste0(plot_name, "_", field)
    # nocov start
    observeEvent(input[[.input_FUN(.heatMapAssay)]], {
        # .createProtectedParameterObservers with a twist
        matched_input <- as(input[[.input_FUN(.heatMapAssay)]], typeof(pObjects$memory[[plot_name]][[.heatMapAssay]]))
        if (identical(matched_input, pObjects$memory[[plot_name]][[.heatMapAssay]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.heatMapAssay]] <- matched_input

        # Twist: update the value and limits of lower/upper bounds based on the new data
        plot_range <- range(assay(se, input[[.input_FUN(.heatMapAssay)]]), na.rm = TRUE)
        updateNumericInput(session, .input_FUN(.assayLowerBound), value = plot_range[1])
        updateNumericInput(session, .input_FUN(.assayUpperBound), value = plot_range[2])

        .requestCleanUpdate(plot_name, pObjects, rObjects)
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    # nocov end
    # nocov start
    observeEvent(input[[.input_FUN(.assayLowerBound)]], {

        cur_value <- input[[.input_FUN(.assayLowerBound)]]

        if (is.null(cur_value)) {
            return(NULL)
        }

        if (is.na(cur_value)) {
            pObjects$memory[[plot_name]][[.assayLowerBound]] <- NA_real_
            .requestUpdate(plot_name, rObjects)
            return(NULL)
        }

        pObjects$memory[[plot_name]][[.assayLowerBound]] <- cur_value

        # The upper bound cannot be lower than the lower bound
        upper_bound <- input[[.input_FUN(.assayUpperBound)]]
        if (!is.null(upper_bound) && !is.na(upper_bound) && cur_value > upper_bound) {
            # set identical values; 0-length range is handled later
            pObjects$memory[[plot_name]][[.assayUpperBound]] <- cur_value
            updateNumericInput(session, .input_FUN(.assayUpperBound), value = cur_value)
        }

        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    # nocov end
    # nocov start
    observeEvent(input[[.input_FUN(.assayUpperBound)]], {

        cur_value <- input[[.input_FUN(.assayUpperBound)]]

        if (is.null(cur_value)) {
            return(NULL)
        }

        if (is.na(cur_value)) {
            pObjects$memory[[plot_name]][[.assayUpperBound]] <- NA_real_
            .requestUpdate(plot_name, rObjects)
            return(NULL)
        }

        pObjects$memory[[plot_name]][[.assayUpperBound]] <- cur_value

        # The lower bound cannot be higher than the upper bound
        lower_bound <- input[[.input_FUN(.assayLowerBound)]]
        if (!is.null(lower_bound) && !is.na(lower_bound) && cur_value < lower_bound) {
            # set identical values; 0-length range is handled later
            pObjects$memory[[plot_name]][[.assayLowerBound]] <- cur_value
            updateNumericInput(session, .input_FUN(.assayLowerBound), value = cur_value)
        }

        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    # nocov end
    invisible(NULL)
}

#' @importFrom shiny modalDialog fluidRow column h4 actionButton br
#' @importFrom shinyAce aceEditor updateAceEditor
.create_heatmap_modal_observers <- function(plot_name, se, input, session, pObjects, rObjects) {
    apply_field <- "INTERNAL_ApplyFeatNameChanges"
    order_field <- "INTERNAL_OrderFeatNames"
    import_field <- "INTERNAL_ImportFeatNames"
    validate_field <- "INTERNAL_ValidateFeatNames"
    clear_field <- "INTERNAL_ClearFeatNames"

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    # nocov start
    observeEvent(input[[.input_FUN(.featureNamesEdit)]], {
        instance <- pObjects$memory[[plot_name]]

        modal_ui <- modalDialog(
            title=paste("Custom feature names for", .getFullName(instance)),
            size="l", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            fluidRow(
                column(width = 8,
                    aceEditor(.input_FUN(.heatMapFeatNameText),
                        mode="text",
                        theme="xcode",
                        autoComplete="disabled",
                        value=instance[[.heatMapFeatNameText]],
                        height="500px")
                ),
                column(width = 4,
                    actionButton(.input_FUN(clear_field), "Clear editor"), br(), br(),
                    actionButton(.input_FUN(import_field), "Import selection"), br(), br(),
                    actionButton(.input_FUN(order_field), "Order alphabetically"), br(), br(),
                    actionButton(.input_FUN(validate_field), "Validate names"), br(), br(),
                    actionButton(.input_FUN(apply_field), label="Apply", style=.actionbutton_biocstyle)
                )
            )
        )

        showModal(modal_ui)
    }, ignoreInit=TRUE)
    # nocov end

    # The button that imports incoming selection into the aceEditor
    # nocov start
    observeEvent(input[[.input_FUN(import_field)]], {
        instance <- pObjects$memory[[plot_name]]

        # Compute names for the incoming selection, if any
        plot_env <- new.env()
        select_cmds <- .processMultiSelections(pObjects$memory[[plot_name]], pObjects$memory, pObjects$contents, plot_env)
        if (exists("row_selected", envir=plot_env, inherits=FALSE)){
            incoming_names <- unique(unlist(get("row_selected", envir=plot_env)))
        } else {
            incoming_names <- NULL
        }

        editor_text <- input[[.input_FUN(.heatMapFeatNameText)]]
        if (!is.null(incoming_names)) {
            editor_names <- strsplit(gsub("\n$", "", editor_text), split="\n")[[1]]
            editor_names <- union(editor_names, incoming_names)
            editor_text <- paste0(editor_names, collapse = "\n")
        }

        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = editor_text)
    })
    # nocov end

    # Button to clear the editor
    # nocov start
    observeEvent(input[[.input_FUN(clear_field)]], {
        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = "")
    })
    # nocov end

    # Button to comment out invalid names
    # nocov start
    observeEvent(input[[.input_FUN(validate_field)]], {
        instance <- pObjects$memory[[plot_name]]

        editor_text <- input[[.input_FUN(.heatMapFeatNameText)]]
        editor_lines <- strsplit(editor_text, split="\n")[[1]]
        invalid_idx <- !editor_lines %in% rownames(se) & !grepl("^[ ]*#", editor_lines)
        editor_lines[invalid_idx] <- paste0("# ", editor_lines[invalid_idx])
        editor_text <- paste0(editor_lines, collapse = "\n")
        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = editor_text)
    })
    # nocov end

    # Button to order names alphabetically
    # nocov start
    observeEvent(input[[.input_FUN(order_field)]], {
        instance <- pObjects$memory[[plot_name]]

        editor_text <- input[[.input_FUN(.heatMapFeatNameText)]]
        editor_lines <- strsplit(editor_text, split="\n")[[1]]
        editor_lines <- sort(editor_lines)
        editor_text <- paste0(editor_lines, collapse = "\n")
        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = editor_text)
    })
    # nocov end

    # The button that actually updates the FeatNameText field.
    # nocov start
    observeEvent(input[[.input_FUN(apply_field)]], {
        pObjects$memory[[plot_name]][[.heatMapFeatNameText]] <- input[[.input_FUN(.heatMapFeatNameText)]]
        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
    })
    # nocov end
}

#' @export
setMethod(".hideInterface", "ComplexHeatmapPlot", function(x, field) {
    if (field %in% c(.multiSelectHistory)) {
        TRUE
    } else {
        callNextMethod()
    }
})
