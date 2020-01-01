
#' @export
ComplexHeatmapPlot <- function(...) {
    new("ComplexHeatmapPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ComplexHeatmapPlot", function(.Object, ...) {
    args <- list(...)

    args <- .empty_default(args, .selectEffect, .selectRestrictTitle)
    args <- .empty_default(args, .selectColor, "red")

    do.call(callNextMethod, c(list(.Object), args))
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
#' @importFrom ComplexHeatmap Heatmap
setMethod(".generateOutput", "ComplexHeatmapPlot", function(x, se, all_memory, all_contents) {
    print(x)
    plot_env <- new.env()
    all_cmds <- list()

    # Doing this first so that .generateDotPlotData can respond to the selection.
    all_cmds[["select"]] <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    if (!exists("col_selected", plot_env) || !exists("row_selected", plot_env)) {
        msg <- "This panel requires both row and column incoming selection."
        all_cmds[["ggplot"]] <- "ggplot() +"
        all_cmds[["geom_text"]] <- sprintf('geom_text(aes(x, y, label=Label), data.frame(x=0, y=0, Label="%s")) +', msg)
        all_cmds[["theme"]] <- "theme_void()"
    } else {
        # Incoming selections
        if (.multiSelectionRestricted(x)) {
            assay_slice <- sprintf("[%s, %s, drop=FALSE]",
                "unique(unlist(row_selected))",
                "unique(unlist(col_selected))")
        } else { # color
            assay_slice <- ""
        }
        all_cmds[["data"]] <- sprintf("plot.data <- assay(sce)%s", assay_slice)
        # Names
        assay_name <- head(assayNames(se), 1)
        assay_name <- ifelse(is.null(assay_name), "assay", assay_name)
        assay_name <- ifelse(assay_name == "", "assay", assay_name)
        heatmap_name <- sprintf('name="%s"', assay_name)
        show_row_names <- sprintf("show_row_names=%s", "TRUE")
        show_column_names <- sprintf("show_column_names=%s", "TRUE")
        # Clustering
        cluster_rows <- sprintf("cluster_rows=%s", "TRUE")
        cluster_columns <- sprintf("cluster_columns=%s", "TRUE")
        # Combine options
        extra_args <- paste(
            "", heatmap_name, cluster_rows, cluster_columns, show_row_names, show_column_names,
            sep = ", ")
        cat(extra_args)
        all_cmds[["heatmap"]] <- sprintf("Heatmap(matrix = plot.data%s)", extra_args)
    }

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
        .create_heatmap_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

.create_heatmap_selection_param_box <- function(x, row_selectable, col_selectable) {
    plot_name <- .getEncodedName(x)
    select_effect <- paste0(plot_name, "_", .selectEffect)

    .create_selection_param_box(x, row_selectable, col_selectable,
        .radioButtonsHidden(x, field=.selectEffect,
            label="Selection effect:", inline=TRUE,
            choices=c(.selectRestrictTitle, .selectColorTitle),
            selected=x[[.selectEffect]]),

        .conditional_on_radio(
            select_effect, .selectColorTitle,
            colourInput(
                paste0(plot_name, "_", .selectColor), label=NULL,
                value=x[[.selectColor]])
        )
    )
}

#' @export
setMethod(".createObservers", "ComplexHeatmapPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .create_multi_selection_effect_observer(plot_name,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.selectColor),
        input=input, pObjects=pObjects, rObjects=rObjects)
})
