
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
    args <- .empty_default(args, .heatMapCustomSampNames, FALSE)

    # Doing the courtesy of packing these into a single string.
    args <- .empty_default(args, .heatMapFeatNameText, NA_character_)
    if (!is.na(vals <- args[[.heatMapFeatNameText]])) {
        args[[.heatMapFeatNameText]] <- paste(vals, collapse="\n")
    }

    args <- .empty_default(args, .heatMapSampNameText, NA_character_)
    if (!is.na(vals <- args[[.heatMapSampNameText]])) {
        args[[.heatMapSampNameText]] <- paste(vals, collapse="\n")
    }

    args <- .empty_default(args, .visualParamBoxOpen, FALSE)

    args <- .empty_default(args, .showDimnames, c(.showNamesRowTitle, .showNamesColumnTitle))
    args <- .empty_default(args, .plotLegendPosition, .plotLegendBottomTitle)
    args <- .empty_default(args, .plotLegendDirection, .plotLegendHorizontalTitle)

    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("ComplexHeatmapPlot", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object, c(.heatMapFeatNameText, .heatMapSampNameText))

    msg <- .valid_logical_error(msg, object, c(.heatMapCustomFeatNames, .heatMapCustomSampNames))

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

    if (is.na(x[[.heatMapSampNameText]])) {
        x[[.heatMapSampNameText]] <- paste(colnames(se), collapse="\n")
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
        selectInput(.input_FUN(.heatMapAssay), label=NULL,
            choices=all_assays, selected=x[[.heatMapAssay]]),
        checkboxInput(.input_FUN(.heatMapCustomFeatNames), label="Use custom feature names",
            value=x[[.heatMapCustomFeatNames]]),
        actionButton(.input_FUN(.rownamesEdit), label=.buttonEditRownamesLabel),
        checkboxInput(.input_FUN(.heatMapCustomSampNames), label="Use custom sample names",
            value=x[[.heatMapCustomSampNames]]),
        actionButton(.input_FUN(.colnamesEdit), label=.buttonEditColnamesLabel)
    )
})

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation
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
                cmds <- c(cmds, sprintf('col_colors <- colDataColorMap(colormap, "%s", discrete=FALSE)(21)', annot))
                cmds <- c(cmds, 'col_FUN <- colorRamp2(
    breaks = seq(min(.col_values, na.rm = TRUE), max(.col_values, na.rm = TRUE), length.out = 21L),
    colors = col_colors)')
                cmds <- c(cmds, sprintf('column_col[["%s"]] <- col_FUN', annot))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.colData.names) {
                cmds <- c(cmds, sprintf('.col_values <- setdiff(.col_values, NA)', annot))
                cmds <- c(cmds, sprintf('col_colors <- colDataColorMap(colormap, "%s", discrete=TRUE)(%s)', annot, 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(col_colors))) { names(col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('column_col[["%s"]] <- col_colors', annot))
            }
            cmds <- c(cmds, "")
        }
        cmds <- c(cmds, "column_annot <- columnAnnotation(df=.column_annot[heatmap_columns, , drop=FALSE], col=column_col)")
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
        cmds <- c(cmds, "row_annot <- rowAnnotation(df=.row_annot[heatmap_rows, , drop=FALSE], col=row_col)")
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
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()

    # plot.data
    assay_name <- x[[.heatMapAssay]]
    all_cmds[["rows"]] <- sprintf("heatmap_rows <- %s", paste0(deparse(x[[.heatMapRownames]]), collapse = "\n"))
    all_cmds[["columns"]] <- sprintf("heatmap_columns <- %s", paste0(deparse(x[[.heatMapColnames]]), collapse = "\n"))
    assay_slice <- "[heatmap_rows, heatmap_columns, drop=FALSE]"
    all_cmds[["data"]] <- sprintf('plot.data <- assay(se, "%s")%s', assay_name, assay_slice)
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
        fields=c(.heatMapAssay, .heatMapCustomSampNames, .heatMapCustomFeatNames),
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

    .create_heatmap_modal_observers(plot_name,
        se,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

})

#' @importFrom shiny modalDialog fluidRow column h4 actionButton br
#' @importFrom shinyAce aceEditor
.create_heatmap_dimnames_modal_dialog <- function(x,
    text_field, 
    title, 
    validate_field,
    organize_cluster_field,
    organize_order_field,
    import_field,
    apply_field)
{
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) paste0(plot_name, "_", field)

    modalDialog(
        title=paste(title, "for", .getFullName(x)),
        size="l", fade=TRUE,
        footer=NULL, easyClose=TRUE,
        fluidRow(
            column(width = 8,
                aceEditor(.input_FUN(text_field),
                    mode="text",
                    theme="xcode",
                    autoComplete="disabled",
                    value=x[[text_field]],
                    height="500px")
            ),
            column(width = 4,
                actionButton(.input_FUN(validate_field), "Validate names"), br(), br(),
                actionButton(.input_FUN(organize_cluster_field), "Organize by cluster"), br(), br(),
                actionButton(.input_FUN(organize_order_field), "Organize by order"), br(), br(),
                actionButton(.input_FUN(import_field), "Import selection"), br(), br(),
                actionButton(.input_FUN(apply_field), label="Apply", style=.actionbutton_biocstyle)
            ),
        ),
        fluidRow(
            column(width = 12,
            )
        )
    )
}

.create_heatmap_modal_observers <- function(plot_name, se,
    input, session, pObjects, rObjects)
{
    for (dim in c("row", "column")) {
        local({
            if (dim=="row") {
                text_field0 <- .heatMapFeatNameText 
                edit_field0 <- .rownamesEdit # TODO: name as .heatMapFeatNameEdit
                shorttype <- "Feat"
                longtype <- "Feature"
            } else {
                text_field0 <- .heatMapSampNameText 
                edit_field0 <- .colnamesEdit # TODO: rename as .heatMapSampNameEdit
                shorttype <- "Samp"
                longtype <- "Sample"
            }

            apply_field0 <- sprintf("INTERNAL_Apply%sNameChanges", shorttype)
            cluster_field0 <- sprintf("INTERNAL_Cluster%sNames", shorttype)
            order_field0 <- sprintf("INTERNAL_Order%sNames", shorttype)
            import_field0 <- sprintf("INTERNAL_Import%sNames", shorttype)
            validate_field0 <- sprintf("INTERNAL_Validate%sNames", shorttype)

            observeEvent(input[[paste0(plot_name, "_", edit_field0)]], {
                instance <- pObjects$memory[[plot_name]]

                modal_ui <- .create_heatmap_dimnames_modal_dialog(instance,
                    text_field=text_field0,
                    title=paste(longtype, "names"),
                    validate_field=validate_field0,
                    organize_cluster_field=cluster_field0,
                    organize_order_field=order_field0,
                    import_field=import_field0,
                    apply_field=apply_field0)

                showModal(modal_ui)
            }, ignoreInit=TRUE)

            text_field <- paste0(plot_name, "_", text_field0)
            observeEvent(input[[paste0(plot_name, "_", apply_field0)]], {
                pObjects$memory[[plot_name]][[text_field]] <- input[[text_field]]
                .requestCleanUpdate(plot_name, pObjects, rObjects)
            })
        })
    }
}

#' @export
setMethod(".hideInterface", "ComplexHeatmapPlot", function(x, field) {
    if (field %in% c(.multiSelectHistory)) {
        TRUE
    } else {
        callNextMethod()
    }
})
