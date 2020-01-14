
#' @export
ComplexHeatmapPlot <- function(...) {
    new("ComplexHeatmapPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ComplexHeatmapPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .heatMapAssay, NA_character_)
    args <- .empty_default(args, .heatMapRownames, character(0))
    args <- .empty_default(args, .heatMapColnames, character(0))
    args <- .empty_default(args, .heatMapColData, character(0))
    args <- .empty_default(args, .heatMapRowData, character(0))

    args <- .empty_default(args, .heatMapCustomFeatNames, TRUE)
    args <- .empty_default(args, .heatMapFeatNameText, NA_character_)
    if (!is.na(vals <- args[[.heatMapFeatNameText]])) {
        args[[.heatMapFeatNameText]] <- paste(vals, collapse="\n")
    }

    args <- .empty_default(args, .heatMapClusterFeatures, FALSE)
    args <- .empty_default(args, .heatMapClusterDistanceFeatures, .clusterDistanceSpearman)
    args <- .empty_default(args, .heatMapClusterMethodFeatures, .clusterMethodWardD2)

    args <- .empty_default(args, .visualParamBoxOpen, FALSE)

    args <- .empty_default(args, .selectEffect, .selectColorTitle)
    args <- .empty_default(args, .selectColor, "red")
    args <- .empty_default(args, .selectTransAlpha, 0.1)

    args <- .empty_default(args, .showDimnames, c(.showNamesRowTitle))
    args <- .empty_default(args, .plotLegendPosition, .plotLegendBottomTitle)
    args <- .empty_default(args, .plotLegendDirection, .plotLegendHorizontalTitle)

    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("ComplexHeatmapPlot", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object, c(.heatMapFeatNameText, .selectEffect))

    msg <- .valid_string_error(msg, object, .selectColor)

    msg <- .valid_number_error(msg, object, .selectTransAlpha, lower=0, upper=1)

    msg <- .valid_logical_error(msg, object, .heatMapCustomFeatNames)

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

    if (is.na(x[[.heatMapFeatNameText]])) {
        x[[.heatMapFeatNameText]] <- rownames(se)[1]
    }

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
            actionButton(.input_FUN(.rownamesEdit), label=.buttonEditRownamesLabel)),
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

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation
#' @importFrom dplyr arrange
.process_heatmap_column_annotations <- function(x, se, plot_env) {
    cmds <- c()
    if (length(x[[.heatMapColData]]) || x[[.selectEffect]] == .selectColorTitle) {
        cmds <- c(cmds, sprintf(".column_annot <- colData(se)[.heatmap.columns, %s, drop=FALSE]", deparse(x[[.heatMapColData]])))
        # Process selected points
        if (x[[.selectEffect]] == .selectColorTitle) {
            cmds <- c(cmds, '.column_annot[["Selected points"]] <- rep(FALSE, nrow(.column_annot))')
            if (exists("col_selected", envir=plot_env, inherits=FALSE)){
                cmds <- c(cmds, '.column_annot[unlist(col_selected), "Selected points"] <- TRUE')
            }
        }
        .text_eval(cmds, plot_env)
        # Collect color maps
        cmds <- c(cmds, "column_col <- list()", "")
        for (annot in x[[.heatMapColData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(plot_env, annot, ".column_annot"))
            cmds <- c(cmds, sprintf('.col_values <- .column_annot[["%s"]]', annot))
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.colData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- colDataColorMap(colormap, "%s", discrete=FALSE)(21)', annot))
                cmds <- c(cmds, '.col_FUN <- colorRamp2(
    breaks = seq(min(.col_values, na.rm = TRUE), max(.col_values, na.rm = TRUE), length.out = 21L),
    colors = .col_colors)')
                cmds <- c(cmds, sprintf('column_col[["%s"]] <- .col_FUN', annot))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.colData.names) {
                cmds <- c(cmds, '.col_values <- setdiff(.col_values, NA)')
                cmds <- c(cmds, sprintf('.col_colors <- colDataColorMap(colormap, "%s", discrete=TRUE)(%s)', annot, 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('column_col[["%s"]] <- .col_colors', annot))
            }
            cmds <- c(cmds, "")
        }
        # Add color map for selected points
        if (x[[.selectEffect]] == .selectColorTitle) {
            cmds <- c(cmds,
                sprintf('column_col[["Selected points"]] <- c("TRUE"=%s, "FALSE"="white")', deparse(x[[.selectColor]])),
                "")
        }
        cmds <- c(cmds, '.column_annot <- as.data.frame(.column_annot, optional=TRUE)') # preserve colnames
        cmds <- c(cmds, sprintf(".column_annot <- dplyr::arrange(.column_annot, %s);",
            paste0(x[[.heatMapColData]], collapse=", ")))
        cmds <- c(cmds, "column_annot <- columnAnnotation(df=.column_annot, col=column_col)")
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
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, "%s", discrete=FALSE)(21)', annot))
                cmds <- c(cmds, '.col_FUN <- colorRamp2(
    breaks = seq(min(.col_values, na.rm = TRUE), max(.col_values, na.rm = TRUE), length.out = 21L),
    colors = .col_colors)')
                cmds <- c(cmds, sprintf('row_col[["%s"]] <- .col_FUN', annot))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.rowData.names) {
                cmds <- c(cmds, sprintf('.col_values <- setdiff(.col_values, NA)', annot))
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, "%s", discrete=TRUE)(%s)', annot, 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('row_col[["%s"]] <- .col_colors', annot))
            }
            cmds <- c(cmds, "")
        }
        cmds <- c(cmds, '.row_annot <- as.data.frame(.row_annot, optional=TRUE)') # preserve colnames
        cmds <- c(cmds, "row_annot <- rowAnnotation(df=.row_annot[.heatmap.rows, , drop=FALSE], col=row_col)")
    }
    cmds
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
    print(x)
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)

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
        'plot.data <- assay(se, "%s")[.heatmap.rows, .heatmap.columns, drop=FALSE]',
        assay_name)
    .text_eval(all_cmds, plot_env)

    # column annotation data
    cmds <- .process_heatmap_column_annotations(x, se, plot_env)
    if (length(cmds)) {
        all_cmds[["coldata"]] <- paste0(cmds, collapse = "\n")
        top_annotation <- "top_annotation=column_annot"
    } else {
        top_annotation <- ""
    }
    # row annotation data
    cmds <- .process_heatmap_row_annotations(x, se, plot_env)
    if (length(cmds)) {
        all_cmds[["rowdata"]] <- paste0(cmds, collapse = "\n")
        left_annotation <- "left_annotation=row_annot"
    } else {
        left_annotation <- ""
    }
    # Names
    assay_name <- ifelse(is.null(assay_name), "assay", assay_name)
    assay_name <- ifelse(assay_name == "", "assay", assay_name)
    heatmap_name <- sprintf('name="%s"', assay_name)
    show_row_names <- sprintf("show_row_names=%s", .showNamesRowTitle %in% x[[.showDimnames]])
    show_column_names <- sprintf("show_column_names=%s", .showNamesColumnTitle %in% x[[.showDimnames]])
    # Clustering
    cluster_rows <- sprintf("cluster_rows=%s", x[[.heatMapClusterFeatures]])
    cluster_columns <- "cluster_columns=FALSE"
    # Clustering options
    if (x[[.heatMapClusterFeatures]]) {
        clustering_distance_rows <- sprintf("clustering_distance_rows=%s", deparse(x[[.heatMapClusterDistanceFeatures]]))
        clustering_method_rows <- sprintf("clustering_method_rows=%s", deparse(x[[.heatMapClusterMethodFeatures]]))
    } else {
        clustering_distance_rows <- clustering_method_rows <- ""
    }
    # Legend
    heatmap_legend_param <- sprintf('heatmap_legend_param=list(direction="%s")', tolower(x[[.plotLegendDirection]]))
    # Combine options
    heatmap_args <- ""
    new_arg_names <- c(heatmap_name, cluster_rows, clustering_distance_rows,
        clustering_method_rows, cluster_columns, show_row_names, show_column_names,
        top_annotation, left_annotation, heatmap_legend_param)
    for (new_arg in new_arg_names) {
        if (!identical(new_arg, "")) {
            heatmap_args <- paste0(heatmap_args, paste0(", ", new_arg))
        }
    }
    heatmap_args <- paste0(strwrap(heatmap_args, width = 80, exdent = 4), collapse = "\n")
    # Heatmap
    all_cmds[["heatmap"]] <- sprintf("hm <- Heatmap(matrix = plot.data%s)", heatmap_args)
    # draw
    draw_args_list <- list(
        heatmap_legend_side=tolower(x[[.plotLegendPosition]]),
        annotation_legend_side=tolower(x[[.plotLegendPosition]])
    )
    print(all_cmds)
    plot_out <- .text_eval(all_cmds, plot_env)

    panel_data <- plot_env$plot.data
    
    # Add draw command after all evaluations (avoid drawing in the plotting device)
    heatmap_legend_side <- sprintf('heatmap_legend_side = "%s"', tolower(x[[.plotLegendPosition]]))
    annotation_legend_side <- sprintf('annotation_legend_side = "%s"', tolower(x[[.plotLegendPosition]]))
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
        radioButtons(
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

    # Not much point distinguishing between protected and unprotected here,
    # as there aren't any selections transmitted from this panel anyway.
    .createProtectedParameterObservers(plot_name,
        fields=c(.heatMapAssay, .heatMapCustomFeatNames, .heatMapFeatNameText),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.heatMapColData, .heatMapRowData,
            .heatMapClusterFeatures, .heatMapClusterDistanceFeatures, .heatMapClusterMethodFeatures,
            .selectEffect, .selectColor,
            .showDimnames,
            .plotLegendPosition, .plotLegendDirection),
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL = FALSE)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_heatmap_modal_observers(plot_name,
        se,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

})

#' @importFrom shiny modalDialog fluidRow column h4 actionButton br
#' @importFrom shinyAce aceEditor
.create_heatmap_modal_observers <- function(plot_name, se, input, session, pObjects, rObjects) {
    apply_field <- "INTERNAL_ApplyFeatNameChanges"
    order_field <- "INTERNAL_OrderFeatNames"
    import_field <- "INTERNAL_ImportFeatNames"
    validate_field <- "INTERNAL_ValidateFeatNames"

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    observeEvent(input[[paste0(plot_name, "_", .rownamesEdit)]], { # TODO: rename as .heatMapFeatNameEdit
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
                    actionButton(.input_FUN(validate_field), "Validate names"), br(), br(),
                    actionButton(.input_FUN(order_field), "Organize by order"), br(), br(),
                    actionButton(.input_FUN(import_field), "Import selection"), br(), br(),
                    actionButton(.input_FUN(apply_field), label="Apply", style=.actionbutton_biocstyle)
                )
            )
        )

        showModal(modal_ui)
    }, ignoreInit=TRUE)

    # The button that imports incoming selection into the aceEditor
    observeEvent(input[[.input_FUN(import_field)]], {
        instance <- pObjects$memory[[plot_name]]

        # Compute names for the incoming selection, if any
        plot_env <- new.env()
        select_cmds <- .processMultiSelections(pObjects$memory[[plot_name]], pObjects$memory, pObjects$contents, plot_env)
        print(select_cmds)
        print(ls(plot_env))
        if (exists("row_selected", envir=plot_env, inherits=FALSE)){
            incoming_names <- unique(unlist(get("row_selected", envir=plot_env)))
        } else {
            incoming_names <- NULL
        }
        print(incoming_names)

        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText),
            value = paste0(instance[[.heatMapFeatNameText]], "\n", paste0(incoming_names, collapse = "\n")))
    })

    # The button that actually updates the FeatNameText field.
    observeEvent(input[[paste0(plot_name, "_", apply_field)]], {
        pObjects$memory[[plot_name]][[.heatMapFeatNameText]] <- input[[paste0(plot_name, "_", .heatMapFeatNameText)]]
        .requestCleanUpdate(plot_name, pObjects, rObjects)
    })
}

#' @export
setMethod(".hideInterface", "ComplexHeatmapPlot", function(x, field) {
    if (field %in% c(.multiSelectHistory)) {
        TRUE
    } else {
        callNextMethod()
    }
})
