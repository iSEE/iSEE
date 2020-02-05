#' Process heatmap colorscales
#'
#' These functions generate sets of commands ultimately evaluated  in the panel environment to generate various colorscale for the \code{\link{ComplexHeatmap}} plot.
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#' @param envir An environment containing \code{plot.data}.
#'
#' @return
#' \code{.process_heatmap_assay_colormap} defines \code{heatmap_col}, which is either a named set of color for discrete assays, or a \code{\link{colorRamp2}} function that interpolates colors for continuous assays.
#'
#' \code{.process_heatmap_column_annotations_colorscale} defines \code{column_annot}, a \code{\link{columnAnnotation}}, if any column metadata is selected. Otherwise returns an empty vector.
#'
#' \code{.process_heatmap_row_annotations_colorscale} defines \code{row_annot}, a \code{\link{rowAnnotation}}., if any row metadata is selected. Otherwise returns an empty vector.
#'
#' \code{.process_heatmap_continuous_annotation_colorscale} defines \code{.col_FUN}, a function which accepts a vector of numeric values and returns interpolated colors.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_assay_colormap
.process_heatmap_assay_colormap <- function(x, se, envir) {
    assay_name <- x[[.heatMapAssay]]

    cmds <- c()

    if (assay_name %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.assay.names) {
        is_centered <- x[[.assayCenterRows]]
        if (is_centered) {
            choice_colors <- x[[.heatMapCenteredColormap]]
            choice_colors <- strsplit(choice_colors, split = " < ", fixed = TRUE)[[1]]
            cmds <- c(cmds, sprintf(".col_colors <- %s", deparse(choice_colors)))
        } else {
            cmds <- c(cmds, sprintf(".col_colors <- assayColorMap(colormap, %s, discrete=FALSE)(21L)", deparse(assay_name)))
        }
        .text_eval(cmds, envir)
        cmds <- c(cmds, .process_heatmap_continuous_assay_colorscale(x, envir, is_centered))
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
#'
#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_column_annotations_colorscale
.process_heatmap_column_annotations_colorscale <- function(x, se, envir) {
    cmds <- c()
    has_incoming_color <- x[[.selectEffect]] == .selectColorTitle && exists("col_selected", envir, inherits = FALSE)
    if (length(x[[.heatMapColData]]) || has_incoming_color) {
        cmds <- c(cmds, "# Keep all samples to compute the full range of continuous annotations")
        cmds <- c(cmds, sprintf("column_data <- colData(se)[, %s, drop=FALSE]", .deparse_for_viewing(x[[.heatMapColData]])))
        # Process selected points
        if (has_incoming_color) {
            cmds <- c(cmds, 'column_data[["Selected points"]] <- rep(FALSE, nrow(column_data))')
            cmds <- c(cmds, 'column_data[unlist(col_selected), "Selected points"] <- TRUE')
        }
        .text_eval(cmds, envir)
        # Collect color maps
        cmds <- c(cmds, "", "column_col <- list()", "")
        for (annot in x[[.heatMapColData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(envir, annot, "column_data"))
            # Need to store and evaluate this command, without re-evaluating all the previous ones (TODO: use a command store)
            cmd_get_value <- sprintf(".col_values <- column_data[[%s]]", deparse(annot))
            cmds <- c(cmds, cmd_get_value)
            .text_eval(cmd_get_value, envir)
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.colData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- colDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot)))
                cmds <- c(cmds, .process_heatmap_continuous_annotation_colorscale(x, envir))
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
            cmds <- c(cmds, sprintf('column_col[["Selected points"]] <- c("TRUE"=%s, "FALSE"="white")', deparse(x[[.selectColor]])), "")
        }
        cmds <- c(cmds, 'column_data <- column_data[.heatmap.columns, , drop=FALSE]')
        cmds <- c(cmds, 'column_data <- as.data.frame(column_data, optional=TRUE)') # preserve colnames
        if (length(x[[.heatMapColData]])) {
            cmds <- c(cmds, sprintf(".column_annot_order <- with(column_data, order(%s))", paste0(x[[.heatMapColData]], collapse=", ")))
            cmds <- c(cmds, "column_data <- column_data[.column_annot_order, , drop=FALSE]")
        }
        cmds <- c(cmds, sprintf("column_annot <- columnAnnotation(df=column_data, col=column_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))))
    }
    cmds
}

#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_row_annotations_colorscale
.process_heatmap_row_annotations_colorscale <- function(x, se, envir) {
    cmds <- c()
    if (length(x[[.heatMapRowData]])) {
        cmds <- c(cmds, "# Keep all features to compute the full range of continuous annotations")
        cmds <- c(cmds, sprintf("row_data <- rowData(se)[, %s, drop=FALSE]", .deparse_for_viewing(x[[.heatMapRowData]])))
        .text_eval(cmds, envir)
        # column color maps
        cmds <- c(cmds, "", "row_col <- list()", "")
        for (annot in x[[.heatMapRowData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(envir, annot, "row_data"))
            # Need to store and evaluate this command, without re-evaluating all the previous ones (TODO: use a command store)
            cmd_get_value <- sprintf('.col_values <- row_data[[%s]]', deparse(annot))
            cmds <- c(cmds, cmd_get_value)
            .text_eval(cmd_get_value, envir)
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.rowData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot)))
                cmds <- c(cmds, .process_heatmap_continuous_annotation_colorscale(x, envir))
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

#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_continuous_annotation_colorscale
.process_heatmap_continuous_annotation_colorscale <- function(x, envir) {
    cmds <- c()

    col_values <- as.vector(envir$.col_values)
    col_range <- range(col_values, na.rm = TRUE)

    col_range <- .safe_nonzero_range(col_range, centered = FALSE)

    return(sprintf(
        '.col_FUN <- colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = .col_colors)',
        col_range[1], col_range[2]))
}

#' @param centered A logical scalar that indicates whether \code{plot.data} rows are centered.
#'
#' @rdname INTERNAL_process_heatmap_colormap
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

    col_range <- .safe_nonzero_range(col_range, centered)

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

#' Process transfomations applied to rows of a heatmap matrix
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#'
#' @return A character vector of commands that apply transformations to \code{plot.data}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_process_heatmap_assay_row_transformations
.process_heatmap_assay_row_transformations <- function(x) {
    cmds <- c()

    if (x[[.assayCenterRows]]) {
        cmds <- c(cmds, "plot.data <- plot.data - rowMeans(plot.data)")
        if (x[[.assayScaleRows]]) {
            cmds <- c(cmds, "plot.data <- plot.data / rowSds(plot.data)")
        }
    }

    return(cmds)
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
    title <- paste0(c(assay_name, transform_str), collapse = legend_sep)

    return(title)
}
