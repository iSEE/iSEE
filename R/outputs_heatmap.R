#' Extract assay submatrix
#'
#' Extract an assay submatrix based on the multiple row/column selection and any custom specifications from \code{\link{.createCustomDimnamesModalObservers}}.
#'
#' @param x A \linkS4class{Panel} instance that uses the row selection modal.
#' @param se The current \linkS4class{SummarizedExperiment} object.
#' @param envir The evaluation environment.
#' This assumes that \code{\link{.processMultiSelections}} has already been run.
#' @param use_custom_row_slot String specifying the name of the slot indicating whether to use custom rows.
#' @param custom_row_text_slot String specifying the name of the slot holding the custom row names.
#' This is expected to be of the same format as described in \code{?\link{.createCustomDimnamesModalObservers}}.
#'
#' @return
#' A character vector of commands to set up the assay submatrix.
#' The submatrix itself is generated within \code{envir} as the \code{plot.data} variable.
#'
#' @details
#' This is designed to extract a matrix of assay values for a subset of rows/columns of interest, most typically for a \linkS4class{ComplexHeatmapPlot}.
#' It assumes that the class of \code{x} contains a slot indicating whether custom rows should be used, plus a slot to hold the selected custom row names (usually from a modal, see \code{\link{.createCustomDimnamesModalObservers}}).
#'
#' If a multiple row selection is present in \code{envir} and custom rows are \emph{not} to be used, that selection is used to define the rows of the submatrix.
#' All columns are returned in the submatrix unless a multiple column selection is present in \code{envir} and the \code{SelectEffect} in \code{x} is \dQuote{Restrict}, in which case only the selected columns are returned.
#' 
#' @author
#' Kevin Rue-Albrecht
#'
#' @export
#' @rdname extractAssaySubmatrix
#' @importFrom SummarizedExperiment assay
.extractAssaySubmatrix <- function(x, se, envir, use_custom_row_slot, custom_row_text_slot) {
    all_cmds <- character(0)

    # Feature names default to custom selection if no multiple selection is available.
    if (x[[use_custom_row_slot]] || is.null(envir$row_selected)) {
        rn <- .convert_text_to_names(x[[custom_row_text_slot]])
        rn <- intersect(rn, rownames(se))
        all_cmds[["rows"]] <- sprintf(".chosen.rows <- %s;", .deparse_for_viewing(rn))
    } else {
        all_cmds[["rows"]] <- ".chosen.rows <- intersect(rownames(se), unlist(row_selected));"
    }

    if (!is.null(envir$col_selected) && x[[.selectEffect]]==.selectRestrictTitle) {
        # TODO: implement visual effects for other forms of selection.
        all_cmds[["columns"]] <- ".chosen.columns <- intersect(colnames(se), unlist(col_selected));"
    } else {
        # includes color effect
        all_cmds[["columns"]] <- ".chosen.columns <- colnames(se);"
    }

    all_cmds[["data"]] <- paste(
        sprintf(
            'plot.data <- assay(se, %s)[.chosen.rows, .chosen.columns, drop=FALSE]',
            deparse(x[[.heatMapAssay]])
        ),
        'plot.data <- as.matrix(plot.data);',
        sep='\n'
    )

    .textEval(all_cmds, envir)
    all_cmds
}

.is_heatmap_continuous <- function(x, se) {
    x[[.heatMapAssay]] %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$continuous.assay.names
}

#' Process heatmap colorscales
#'
#' These functions construct and evaluate commands to generate various colorscales for a \code{Complex\link{Heatmap}}.
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#' @param envir An environment containing \code{plot.data}.
#'
#' @return
#' \code{.process_heatmap_assay_colormap} creates \code{.assay_colors} in \code{envir}
#' and returns a character vector of commands used to do so.
#' This is either a named set of color for discrete assays
#' or a \code{\link{colorRamp2}} function that interpolates colors for continuous assays.
#'
#' \code{.process_heatmap_column_annotations_colorscale} defines \code{.column_annot}, a \code{\link{columnAnnotation}} object,
#' and returns a character vector of commands used to do so.
#' If no column metadata was selected, this function is a no-op.
#'
#' \code{.process_heatmap_row_annotations_colorscale} defines \code{row_annot}, a \code{\link{rowAnnotation}} object,
#' and returns a character vector of commands used to do so.
#' If no row metadata was selected, this function is a no-op.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_process_heatmap_colormap
.process_heatmap_assay_colormap <- function(x, se, envir) {
    assay_name <- x[[.heatMapAssay]]
    cmds <- character(0)

    if (.is_heatmap_continuous(x, se)) {
        if (x[[.assayCenterRows]]) {
            choice_colors <- x[[.heatMapCenteredColormap]]
            choice_colors <- strsplit(choice_colors, split = " < ", fixed = TRUE)[[1]]
            cmds <- c(cmds, sprintf(".assay_colors <- %s", deparse(choice_colors)))
        } else {
            cmds <- c(cmds, sprintf(".assay_colors <- assayColorMap(colormap, %s, discrete=FALSE)(21L)", deparse(assay_name)))
        }

        if (x[[.heatMapCustomAssayBounds]]) {
            lower_bound <- x[[.assayLowerBound]]
            if (is.na(lower_bound)) {
                lower_bound <- min(envir$plot.data, na.rm=TRUE)
            }
            upper_bound <- x[[.assayUpperBound]]
            if (is.na(upper_bound)) {
                upper_bound <- max(envir$plot.data, na.rm=TRUE)
            }
            col_range <- c(lower_bound, upper_bound)
        } else {
            col_range <- range(envir$plot.data, na.rm = TRUE)
        }

        col_range <- .safe_nonzero_range(col_range, x[[.assayCenterRows]])
        if (x[[.assayCenterRows]]) {
            fmt <- '.assay_colors <- circlize::colorRamp2(breaks = c(%s, 0, %s), colors = .assay_colors)'
        } else {
            fmt <- ".assay_colors <- circlize::colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = .assay_colors)"
        }
        col_cmd <- sprintf(fmt, col_range[1], col_range[2])
        cmds <- c(cmds, col_cmd)

    } else if (assay_name %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.assay.names) {
        cmds <- c(cmds, '.assay_values <- unique(as.vector(plot.data))')
        cmds <- c(cmds, '.assay_values <- setdiff(.assay_values, NA)')
        cmds <- c(cmds, sprintf(".assay_colors <- colDataColorMap(colormap, %s, discrete=TRUE)(%s)",
            deparse(assay_name), 'length(.assay_values)'))
        cmds <- c(cmds, 'names(.assay_colors) <- .assay_values')
    }

    .textEval(cmds, envir)
    cmds
}

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation
#' @importFrom SummarizedExperiment colData
#' @rdname INTERNAL_process_heatmap_colormap
.process_heatmap_column_annotations_colorscale <- function(x, se, envir) {
    has_incoming_color <- x[[.selectEffect]]==.selectColorTitle && exists("col_selected", envir, inherits = FALSE)
    if (length(x[[.heatMapColData]])==0 && !has_incoming_color) {
        return(NULL)
    }

    cmds <- "# Keep all samples to compute the full range of continuous annotations"
    cmds <- c(cmds, sprintf(".column_data <- colData(se)[, %s, drop=FALSE]", .deparse_for_viewing(x[[.heatMapColData]])))

    # Process selected points
    if (has_incoming_color) {
        cmds <- c(cmds, '.column_data[["Selected points"]] <- logical(nrow(.column_data))')
        cmds <- c(cmds, '.column_data[unlist(col_selected), "Selected points"] <- TRUE')
    }
    .textEval(cmds, envir)

    # Collect color maps
    init_cmd <- ".column_col <- list()"
    .textEval(init_cmd, envir)
    cmds <- c(cmds, "", init_cmd, "")

    for (annot in x[[.heatMapColData]]) {
        cmds <- c(cmds, .coerce_dataframe_columns(envir, annot, ".column_data"))

        cmd_get_value <- sprintf(".color_values <- .column_data[[%s]]", deparse(annot))
        .textEval(cmd_get_value, envir)
        cmds <- c(cmds, cmd_get_value)

        if (annot %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$continuous.colData.names) {
            colcmds <- sprintf('.col_colors <- colDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot))
            colcmds <- c(colcmds,
                sprintf(
                    ".column_col[[%s]] <- %s",
                    deparse(annot),
                    .define_continuous_annotation_colorscale(envir$.color_values, ".col_colors")
                )
            )
        } else if (annot %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.colData.names) {
            colcmds <- ".color_values <- setdiff(unique(.color_values), NA)"
            colcmds <- c(colcmds, sprintf(".col_colors <- colDataColorMap(colormap, %s, discrete=TRUE)(%s)",
                deparse(annot), 'length(.color_values)'))
            colcmds <- c(colcmds, 'names(.col_colors) <- unique(.color_values)')
            colcmds <- c(colcmds, sprintf(".column_col[[%s]] <- .col_colors", deparse(annot)))
        }

        .textEval(colcmds, envir)
        cmds <- c(cmds, colcmds, "")
    }

    # Add color map for selected points
    additional <- character(0)

    if (has_incoming_color) {
        additional <- c(additional, sprintf('.column_col[["Selected points"]] <- c("TRUE"=%s, "FALSE"="white")',
            deparse(x[[.selectColor]])), "")
    }

    additional <- c(additional, '.column_data <- .column_data[.heatmap.columns, , drop=FALSE]')
    additional <- c(additional, '.column_data <- as.data.frame(.column_data, optional=TRUE)') # preserve colnames

    if (length(x[[.heatMapColData]])) {
        # Reordering by the column annotations.
        additional <- c(additional, sprintf(".column_annot_order <- order(%s)",
            paste(sprintf(".column_data[[%s]]", vapply(x[[.heatMapColData]], deparse, "")), collapse=", ")))
        additional <- c(additional, ".column_data <- .column_data[.column_annot_order, , drop=FALSE]")
        additional <- c(additional, "plot.data <- plot.data[, .column_annot_order, drop=FALSE]")
    }

    additional <- c(additional,
        sprintf(
            ".column_annot <- ComplexHeatmap::columnAnnotation(df=.column_data, col=.column_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))
        )
    )

    .textEval(additional, envir)
    c(cmds, additional)
}

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation
#' @importFrom SummarizedExperiment rowData
#' @rdname INTERNAL_process_heatmap_colormap
.process_heatmap_row_annotations_colorscale <- function(x, se, envir) {
    if (length(x[[.heatMapRowData]])==0) {
        return(NULL)
    }

    cmds <- "# Keep all features to compute the full range of continuous annotations"
    cmds <- c(cmds, sprintf(".row_data <- rowData(se)[, %s, drop=FALSE]", .deparse_for_viewing(x[[.heatMapRowData]])))
    .textEval(cmds, envir)

    # column color maps
    init_cmd <- ".row_col <- list()"
    .textEval(init_cmd, envir)
    cmds <- c(cmds, "", init_cmd, "")

    for (annot in x[[.heatMapRowData]]) {
        cmds <- c(cmds, .coerce_dataframe_columns(envir, annot, ".row_data"))

        cmd_get_value <- sprintf('.color_values <- .row_data[[%s]]', deparse(annot))
        .textEval(cmd_get_value, envir)
        cmds <- c(cmds, cmd_get_value)

        if (annot %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$continuous.rowData.names) {
            rowcmds <- sprintf('.row_colors <- rowDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot))
            rowcmds <- c(rowcmds,
                sprintf(
                    ".row_col[[%s]] <- %s",
                    deparse(annot),
                    .define_continuous_annotation_colorscale(envir$.color_values, ".row_colors")
                )
            )
        } else if (annot %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.rowData.names) {
            rowcmds <- sprintf('.color_values <- setdiff(unique(.color_values), NA)', annot)
            rowcmds <- c(rowcmds, sprintf('.row_colors <- rowDataColorMap(colormap, %s, discrete=TRUE)(%s)',
                deparse(annot), 'length(.color_values)'))
            rowcmds <- c(rowcmds, 'names(.row_colors) <- .color_values')
            rowcmds <- c(rowcmds, sprintf('.row_col[[%s]] <- .row_colors', deparse(annot)))
        }

        .textEval(rowcmds, envir)
        cmds <- c(cmds, rowcmds, "")
    }

    additional <- '.row_data <- .row_data[.heatmap.rows, , drop=FALSE]'
    additional <- c(additional, '.row_data <- as.data.frame(.row_data, optional=TRUE)') # preserve colnames
    additional <- c(additional,
        sprintf(
            ".row_annot <- ComplexHeatmap::rowAnnotation(df=.row_data, col=.row_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))
        )
    )

    .textEval(additional, envir)
    c(cmds, additional)
}

#' Define continuous annotation colorscale
#'
#' Construct an R expression that takes a vector of numeric values and converts them to interpolated colors.
#'
#' @param val A numeric vector from which the range of possible values is defined.
#' @param map A string containing the variable name for the vector of colors to interpolate across.
#'
#' @return A string containing an R expression that uses \code{\link{colorRamp2}} to interpolate the colors.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_define_continuous_annotation
#' @importFrom circlize colorRamp2
.define_continuous_annotation_colorscale <- function(val, map) {
    col_range <- range(val, na.rm = TRUE)
    col_range <- .safe_nonzero_range(col_range, centered = FALSE)
    sprintf(
        'circlize::colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = %s)',
        col_range[1], col_range[2], map
    )
}

#' Process transfomations applied to rows of a heatmap matrix
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#' @param se The current \linkS4class{SummarizedExperiment} object.
#' @param envir The evaluation environment.
#'
#' @return A character vector of commands that apply transformations to \code{plot.data},
#' or \code{NULL} if no transformations are to be performed.
#'
#' @details
#' Note that we are not using \code{rowSds} so as to avoid unnecessary dependencies.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_process_heatmap_assay_row_transformations
.process_heatmap_assay_row_transformations <- function(x, se, envir) {
    cmds <- NULL

    if (.is_heatmap_continuous(x, se)) {
        if (x[[.assayCenterRows]]) {
            cmds <- "plot.data <- plot.data - rowMeans(plot.data)"
            if (x[[.assayScaleRows]]) {
                cmds <- c(cmds, "plot.data <- plot.data / apply(plot.data, 1, sd)")
            }
            .textEval(cmds, envir)
        }
    }

    cmds
}

#' Build the main heatmap legend title
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#' @param discrete A logical scalar indicating whether the selected assay is discrete.
#'
#' @return A character value to use as the title of the heatmap legend.
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_build_heatmap_assay_name
.build_heatmap_assay_legend_title <- function(x, discrete) {
    assay_name <- x[[.heatMapAssay]]

    if (discrete) {
        return(assay_name)
    }

    if (x[[.assayCenterRows]]) {
        transform_labels <- c("centered"=TRUE, "scaled"=x[[.assayScaleRows]])
        transform_str <- sprintf("(%s)", paste0(names(which(transform_labels)), collapse = ", "))
    } else {
        transform_str <- NULL
    }

    legend_sep <- ifelse(x[[.plotLegendDirection]] == "Vertical", "\n", " ")
    paste0(c(assay_name, transform_str), collapse = legend_sep)
}
