#' @export
setClass("ComplexHeatmapPlot", contains="Panel")

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
setMethod(".renderOutput", "ComplexHeatmapPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    output[[plot_name]] <- renderPlot({
        force(rObjects[[plot_name]])

        p.out <- .make_complexheatmap()

        pObjects$commands[[plot_name]] <- p.out$cmd_list

        p.out$plot
    })
})

#' Makes a complex heatmap
#'
#' Make a heatmap with features on the Y axis and samples on the X axis.
#'
#' @importFrom ComplexHeatmap Heatmap
#' @rdname INTERNAL_make_complexheatmap
.make_complexheatmap <- function() {
    eval_env <- new.env()
    plot_cmds <- list()

    plot_cmds[["data"]] <- 'm <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))'
    plot_cmds[["heatmap"]] <- "Heatmap(m)"

    plot_out <- .text_eval(plot_cmds, eval_env)
    list(plot=plot_out, cmd_list=plot_cmds)
}
