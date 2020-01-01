
#' @export
ComplexHeatmapPlot <- function(...) {
    new("ComplexHeatmapPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ComplexHeatmapPlot", function(.Object, ...) {
    args <- list(...)

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

    all_cmds[["data"]] <- 'plot.data <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))'
    all_cmds[["heatmap"]] <- "Heatmap(plot.data)"

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
})
