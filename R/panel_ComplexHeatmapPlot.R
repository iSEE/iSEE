
#' @export
ComplexHeatmapPlot <- function(...) {
    new("ComplexHeatmapPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ComplexHeatmapPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .heatMapAssay, NA_character_)

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
    .set_common_info(se, "ComplexHeatmapPlot",
        valid.assay.names=named_assays)
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
        .create_assay_selectize_ui(x, .heatMapAssay, all_assays)
    )
})

#' @export
#' @importFrom SummarizedExperiment assay
#' @importFrom ggplot2 ggplot geom_text aes theme_void
#' @importFrom ComplexHeatmap Heatmap draw
setMethod(".generateOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    print(x)
    plot_env <- new.env()
    all_cmds <- list()

    # Doing this first so that .generateDotPlotData can respond to the selection.
    all_cmds[["select"]] <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    if (!exists("col_selected", plot_env, inherits = FALSE) || !exists("row_selected", plot_env, inherits = FALSE)) {
        msg <- "This panel requires both row and column incoming selection."
        all_cmds[["ggplot"]] <- "ggplot() +"
        all_cmds[["geom_text"]] <- sprintf('geom_text(aes(x, y, label=Label), data.frame(x=0, y=0, Label="%s")) +', msg)
        all_cmds[["theme"]] <- "theme_void()"
    } else {
        # Incoming selections
        assay_slice <- sprintf("[%s, %s, drop=FALSE]",
                "unique(unlist(row_selected))",
                "unique(unlist(col_selected))")
        # plot.data
        all_cmds[["data"]] <- sprintf('plot.data <- assay(se, "%s")%s', x[[.heatMapAssay]], assay_slice)
        # Names
        assay_name <- head(assayNames(se), 1)
        assay_name <- ifelse(is.null(assay_name), "assay", assay_name)
        assay_name <- ifelse(assay_name == "", "assay", assay_name)
        heatmap_name <- sprintf('name="%s"', assay_name)
        show_row_names <- sprintf("show_row_names=%s", .showNamesRowTitle %in% x[[.showDimnames]])
        show_column_names <- sprintf("show_column_names=%s", .showNamesColumnTitle %in% x[[.showDimnames]])
        # Clustering
        cluster_rows <- sprintf("\ncluster_rows=%s", "TRUE")
        cluster_columns <- sprintf("cluster_columns=%s", "TRUE")
        # Legend
        heatmap_legend_param <- sprintf('\nheatmap_legend_param=list(direction = "%s")', tolower(x[[.plotLegendDirection]]))
        # Combine options
        heatmap_args <- paste(
            "", heatmap_name, cluster_rows, cluster_columns, show_row_names, show_column_names,
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
        .create_visual_box_for_complexheatmap(x),
        .create_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

.create_visual_box_for_complexheatmap <- function(x) {
    plot_name <- .getEncodedName(x)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
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
        fields=c(.selectColor,
            .showDimnames,
            .plotLegendPosition, .plotLegendDirection),
        input=input, pObjects=pObjects, rObjects=rObjects)

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
