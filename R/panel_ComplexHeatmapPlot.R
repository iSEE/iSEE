
#' @export
ComplexHeatmapPlot <- function(...) {
    new("ComplexHeatmapPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ComplexHeatmapPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .heatMapAssay, NA_character_)
    args <- .empty_default(args, .heatMapColData, character(0))
    args <- .empty_default(args, .heatMapRowData, character(0))

    args <- .empty_default(args, .visualParamBoxOpen, FALSE)

    args <- .empty_default(args, .showDimnames, c(.showNamesRowTitle, .showNamesColumnTitle))
    args <- .empty_default(args, .plotLegendPosition, .plotLegendBottomTitle)
    args <- .empty_default(args, .plotLegendDirection, .plotLegendHorizontalTitle)

    do.call(callNextMethod, c(list(.Object), args))
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

    x
})

#' @export
setMethod(".panelColor", "ComplexHeatmapPlot", function(x) "#ABCDEF")

#' @export
setMethod(".fullName", "ComplexHeatmapPlot", function(x) "Complex heatmap")

#' @export
setMethod(".defineOutput", "ComplexHeatmapPlot", function(x) {
    plot_name <- .getEncodedName(x)
    plotOutput(plot_name, height=paste0(x[[.organizationHeight]], "px"))
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "ComplexHeatmapPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .get_common_info(se, "ComplexHeatmapPlot")$valid.assay.names

    list(
        selectInput(paste0(.getEncodedName(x), "_", .heatMapAssay), label=NULL,
            choices=all_assays, selected=x[[.heatMapAssay]])
    )
})

.process_heatmap_column_annotations <- function(x, se, plot_env) {
    cmds <- c()
    if (length(x[[.heatMapColData]])) {
        cmds <- c(cmds, sprintf(".column_annot <- colData(se)[, %s, drop=FALSE]", deparse(x[[.heatMapColData]])))
        .text_eval(cmds, plot_env)
        # column color maps
        cmds <- c(cmds, "column_col <- list()", "")
        for (annot in x[[.heatMapColData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(plot_env, annot, ".column_annot"))
            cmds <- c(cmds, sprintf('.col_values <- .column_annot[["%s"]]', annot))
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.colData.names) {
                cmds <- c(cmds, sprintf('col_colors <- colDataColorMap(ecm, "%s", discrete=FALSE)(21)', annot))
                cmds <- c(cmds, 'col_FUN <- colorRamp2(
    breaks = seq(min(.col_values, na.rm = TRUE), max(.col_values, na.rm = TRUE), length.out = 21L),
    colors = col_colors)')
                cmds <- c(cmds, sprintf('column_col[["%s"]] <- col_FUN', annot))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.colData.names) {
                cmds <- c(cmds, sprintf('.col_values <- setdiff(.col_values, NA)', annot))
                cmds <- c(cmds, sprintf('col_colors <- colDataColorMap(ecm, "%s", discrete=TRUE)(%s)', annot, 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(col_colors))) { names(col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('column_col[["%s"]] <- col_colors', annot))
            }
            cmds <- c(cmds, "")
        }
        cmds <- c(cmds, sprintf("column_annot <- columnAnnotation(df=.column_annot[unique(unlist(col_selected)), , drop=FALSE], col=column_col)",
            deparse(x[[.heatMapColData]])))
    }
    cmds
}

.process_heatmap_row_annotations <- function(x, se, plot_env) {
    cmds <- c()
    if (length(x[[.heatMapRowData]])) {
        cmds <- c(cmds, sprintf(".row_annot <- rowData(se)[, %s, drop=FALSE]", deparse(x[[.heatMapRowData]])))
        .text_eval(cmds, plot_env)
        # column color maps
        cmds <- c(cmds, "row_col <- list()", "")
        for (annot in x[[.heatMapRowData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(plot_env, annot, ".row_annot"))
            cmds <- c(cmds, sprintf('.col_values <- .row_annot[["%s"]]', annot))
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.rowData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(ecm, "%s", discrete=FALSE)(21)', annot))
                cmds <- c(cmds, '.col_FUN <- colorRamp2(
    breaks = seq(min(.col_values, na.rm = TRUE), max(.col_values, na.rm = TRUE), length.out = 21L),
    colors = .col_colors)')
                cmds <- c(cmds, sprintf('row_col[["%s"]] <- .col_FUN', annot))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.rowData.names) {
                cmds <- c(cmds, sprintf('.col_values <- setdiff(.col_values, NA)', annot))
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(ecm, "%s", discrete=TRUE)(%s)', annot, 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('row_col[["%s"]] <- .col_colors', annot))
            }
            cmds <- c(cmds, "")
        }
        cmds <- c(cmds, sprintf("row_annot <- rowAnnotation(df=.row_annot[unique(unlist(row_selected)), , drop=FALSE], col=row_col)",
            deparse(x[[.heatMapRowData]])))
    }
    cmds
}

#' @export
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom ggplot2 ggplot geom_text aes theme_void
#' @importFrom ComplexHeatmap Heatmap draw columnAnnotation rowAnnotation
setMethod(".generateOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    print(x)
    plot_env <- new.env()
    all_cmds <- list()

    # Doing this first so that .generateDotPlotData can respond to the selection.
    select_cmds <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    if (!exists("col_selected", plot_env, inherits = FALSE) || !exists("row_selected", plot_env, inherits = FALSE)) {
        msg <- "This panel requires both row and column incoming selection."
        all_cmds[["ggplot"]] <- "ggplot() +"
        all_cmds[["geom_text"]] <- sprintf('geom_text(aes(x, y, label=Label), data.frame(x=0, y=0, Label="%s")) +', msg)
        all_cmds[["theme"]] <- "theme_void()"
    } else {
        all_cmds[["select"]] <- select_cmds

        # Incoming selections
        assay_slice <- sprintf("[%s, %s, drop=FALSE]",
                "unique(unlist(row_selected))",
                "unique(unlist(col_selected))")
        # plot.data
        all_cmds[["data"]] <- sprintf('plot.data <- assay(se, "%s")%s', x[[.heatMapAssay]], assay_slice)
        .text_eval(all_cmds, plot_env)
        # column annotation data
        cmds <- .process_heatmap_column_annotations(x, se, plot_env)
        if (length(cmds)) {
            all_cmds[["coldata"]] <- paste0(cmds, collapse = "\n")
            top_annotation <- "\n\ttop_annotation=column_annot"
        } else {
            top_annotation <- NULL
        }
        # row annotation data
        cmds <- .process_heatmap_row_annotations(x, se, plot_env)
        if (length(cmds)) {
            all_cmds[["rowdata"]] <- paste0(cmds, collapse = "\n")
            left_annotation <- "\n\tleft_annotation=row_annot"
        } else {
            left_annotation <- NULL
        }
        # Names
        assay_name <- head(assayNames(se), 1)
        assay_name <- ifelse(is.null(assay_name), "assay", assay_name)
        assay_name <- ifelse(assay_name == "", "assay", assay_name)
        heatmap_name <- sprintf('name="%s"', assay_name)
        show_row_names <- sprintf("show_row_names=%s", .showNamesRowTitle %in% x[[.showDimnames]])
        show_column_names <- sprintf("show_column_names=%s", .showNamesColumnTitle %in% x[[.showDimnames]])
        # Clustering (TODO)
        cluster_rows <- sprintf("\n\tcluster_rows=%s", "TRUE")
        cluster_columns <- sprintf("cluster_columns=%s", "TRUE")
        # Legend
        heatmap_legend_param <- sprintf('\n\theatmap_legend_param=list(direction = "%s")', tolower(x[[.plotLegendDirection]]))
        # Combine options
        heatmap_args <- paste(
            "", heatmap_name, cluster_rows, cluster_columns, show_row_names, show_column_names,
            top_annotation, left_annotation,
            heatmap_legend_param,
            sep = ", ")
        # Heatmap
        all_cmds[["heatmap"]] <- sprintf("hm <- Heatmap(matrix = plot.data%s)", heatmap_args)
        # draw
        heatmap_legend_side <- sprintf('heatmap_legend_side = "%s"', tolower(x[[.plotLegendPosition]]))
        annotation_legend_side <- sprintf('annotation_legend_side = "%s"', tolower(x[[.plotLegendPosition]]))
        draw_args <- paste(
            "", heatmap_legend_side, annotation_legend_side,
            sep = ", ")
        all_cmds[["draw"]] <- sprintf("draw(hm%s)", draw_args)
    }
    print(all_cmds)

    plot_out <- .text_eval(all_cmds, plot_env)

    panel_data <- plot_env$plot.data

    list(commands=all_cmds, contents=panel_data, plot=plot_out)
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
        .create_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

.create_visual_box_for_complexheatmap <- function(x, se) {
    plot_name <- .getEncodedName(x)

    all_coldata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.colData.names
    all_rowdata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.rowData.names

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
        selectizeInput(paste0(plot_name, "_", .heatMapColData), label="Column annotations:",
            selected=x[[.heatMapColData]], choices=all_coldata, multiple=TRUE),
        selectizeInput(paste0(plot_name, "_", .heatMapRowData), label="Row annotations:",
            selected=x[[.heatMapRowData]], choices=all_rowdata, multiple=TRUE),
        checkboxGroupInput(
            inputId=paste0(plot_name, "_", .showDimnames), label="Show names:", inline=TRUE,
            selected=x[[.showDimnames]],
            choices=c(.showNamesRowTitle, .showNamesColumnTitle)),
        radioButtons( # Copied from ".add_other_UI_elements()", to avoid "Font size"
            paste0(plot_name, "_", .plotLegendPosition), label="Legend position:", inline=TRUE,
            choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
            selected=x[[.plotLegendPosition]]),
        radioButtons(
            paste0(plot_name, "_", .plotLegendDirection), label="Legend direction:", inline=TRUE,
            choices=c(.plotLegendHorizontalTitle, .plotLegendVerticalTitle),
            selected=x[[.plotLegendDirection]])
    )
}

#' @export
setMethod(".createObservers", "ComplexHeatmapPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.heatMapAssay),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.heatMapColData, .heatMapRowData,
            .selectColor,
            .showDimnames,
            .plotLegendPosition, .plotLegendDirection),
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL = FALSE)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".hideInterface", "ComplexHeatmapPlot", function(x, field) {
    if (field %in% c(.multiSelectHistory)) {
        TRUE
    } else {
        callNextMethod()
    }
})
