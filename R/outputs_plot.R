#' Create the plot output
#'
#' Create a reactive expression to render the plot output.
#' This is used inside \code{\link{.renderOutput,DotPlot-method}} to satisfy the requirements of that generic.
#' It calls \code{\link{.generateOutput}} to do the heavy lifting of creating a panel-specific plot.
#'
#' @param plot_name String containing the current name of the plot panel.
#' @param se The \linkS4class{SingleCellExperiment} object to be visualized.
#' @param output The Shiny output object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' A reactive element to render the plot is added to \code{output}.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_plot_output
#' @importFrom shiny renderPlot renderUI tagList br
.create_plot_output <- function(plot_name, se, output, pObjects, rObjects) {
    force(se)

    output[[plot_name]] <- renderPlot({
        p.out <- .retrieveOutput(plot_name, se, pObjects, rObjects)
        pObjects$varname[[plot_name]] <- "plot.data"
        p.out$plot
    })

    invisible(NULL)
}

#' Define the plot UI
#'
#' Define the \code{\link{plotOutput}} object for a given panel containing a single plot.
#'
#' @param plot_name String containing the name of the plot.
#' @param height Integer scalar specifying the height of the plot in pixels.
#' @param brush_direction String specifying the direction of brushing, i.e., \code{"x"}, \code{"y"} or \code{"xy"}.
#' @param brush_fill String containing a color to use for the fill of the brush.
#' @param brush_stroke String containing a color to use for the stroke of the brush.
#'
#' @return The output of \code{\link{plotOutput}} with relevant parametrization.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_define_plot_ui
#' @importFrom shinyWidgets addSpinner
#' @importFrom shiny brushOpts
.define_plot_ui <- function(plot_name, height, brush_direction, brush_fill, brush_stroke) {
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    brush.opts <- brushOpts(.input_FUN(.brushField), resetOnNew=TRUE, delay=2000,
        direction=brush_direction, fill=brush_fill, stroke=brush_stroke,
        opacity=.brushFillOpacity)

    dblclick <- .input_FUN(.zoomClick)
    clickopt <- .input_FUN(.lassoClick)
    panel_height <- paste0(height, "px")

    addSpinner(
        plotOutput(plot_name, brush=brush.opts, dblclick=dblclick, click=clickopt, height=panel_height),
        color=brush_fill
    )
}

#' Add extra aesthetic columns
#'
#' This is a simple utility that is broken out of \code{\link{.generateOutput,DotPlot-method}}.
#' It purely serves to run all of the non-exposed generics that modify \code{plot.data}
#' (e.g., \code{\link{.addDotPlotDataColor}}) and accumulate the resulting commands and labels.
#'
#' @param x An instance of a \linkS4class{DotPlot} class.
#' @param envir An environment containing \code{plot.data}.
#'
#' @return A list containing \code{commands}, a list of character vectors with R commands;
#' and \code{labels}, a list of strings containing various labels to use in legends.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_add_extra_aesthetic_columns
.add_extra_aesthetic_columns <- function(x, envir) {
    collected <- list()
    labels <- list()

    # Add commands coercing X and Y to appropriate type
    collected$coerce <- .coerce_dataframe_columns(envir, c("X", "Y"), "plot.data")

    # Add commands adding optional columns to plot.data
    out_color <- .addDotPlotDataColor(x, envir)
    collected$color <- out_color$commands
    labels <- c(labels, out_color$labels)
    if (!is.null(envir$plot.data$ColorBy)) {
        collected$color <- c(collected$color, .coerce_dataframe_columns(envir, "ColorBy", "plot.data"))
    }

    out_shape <- .addDotPlotDataShape(x, envir)
    collected$shape <- out_shape$commands
    labels <- c(labels, out_shape$labels)

    out_size <- .addDotPlotDataSize(x, envir)
    collected$size <- out_size$commands
    labels <- c(labels, out_size$labels)

    out_facets <- .addDotPlotDataFacets(x, envir)
    collected$facets <- out_facets$commands
    labels <- c(labels, out_facets$labels)

    # Removing NAs in axes aesthetics as they mess up .process_selectby_choice.
    clean_select_fields <- c("X", "Y", names(collected$facets))
    clean_expression <- paste(sprintf("!is.na(%s)", clean_select_fields), collapse=" & ")
    collected$na.rm <- sprintf("plot.data <- subset(plot.data, %s);", clean_expression)

    collected$select.effect <- .addDotPlotDataSelected(x, envir)

    list(commands=collected, labels=labels)
}

#' Coerce \code{plot.data} columns
#'
#' Coerce key columns of \code{plot.data} to factors if they are categorical,
#' otherwise make them numeric by any means possible.
#' This ensures that downstream code only has to deal with factors or numbers.
#'
#' @param envir An environment containing the \code{df} data.frame.
#' @param fields A character vector of column names to coerce.
#' @param df Name of the data.frame in \code{envir}.
#'
#' @return
#' The specified \code{plot.data} columns are coerced to factors or numeric values.
#' A character vector is returned containing the commands to do so.
#'
#' @seealso
#' \code{\link{.coerce_type}}, which generates the commands to do the coercion.
#' @author Aaron Lun, Kevin Rue-Albrecht
#' @rdname INTERNAL.coerce_dataframe_columns
.coerce_dataframe_columns <- function(envir, fields, df="plot.data") {
    coerce_cmds <- NULL
    for (f in fields) {
        v <- get(df, envir = envir)[[f]]
        coerce_cmds <- c(coerce_cmds, .coerce_type(v, f, as_numeric=!.is_groupable(v), df=df))
    }
    .text_eval(coerce_cmds, envir)
    coerce_cmds
}
