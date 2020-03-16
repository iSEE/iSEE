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
#' @importFrom shiny brushOpts plotOutput
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

    list(commands=collected, labels=labels)
}

#' Add a \code{SelectBy} column
#'
#' This is another simple utility to just add a \code{SelectBy} column to \code{plot.data},
#' indicating whether each row was included in a multiple selection transmitted from another panel.
#' If \code{x} restricts its multiple selections, \code{plot.data} will be subsetted to only those rows with \code{SelectBy=TRUE}.
#'
#' @param x An instance of a \linkS4class{DotPlot} class.
#' @param envir An environment containing \code{plot.data}.
#'
#' @return A list of character vectors containing commands necessary to add \code{SelectBy} and,
#' if necessary, perform the subsetting.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_add_selectby_column
.add_selectby_column <- function(x, envir) {
    collected <- list()
    plot.data <- envir$plot.data

    # Removing NAs in axes aesthetics as they mess up .process_selectby_choice.
    clean_select_fields <- c("X", "Y", intersect(c("FacetRow", "FacetColumn"), colnames(plot.data)))
    clean_affected <- vapply(clean_select_fields, function(i) any(is.na(plot.data[[i]])), TRUE)

    if (any(clean_affected)) {
        clean_expression <- paste(sprintf("!is.na(%s)", clean_select_fields[clean_affected]), collapse=" & ")
        collected$na.rm <- sprintf("plot.data <- subset(plot.data, %s);", clean_expression)
        .textEval(collected$na.rm, envir)
    }

    collected$select.effect <- .addDotPlotDataSelected(x, envir)
    collected
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
#' @rdname INTERNAL_coerce_dataframe_columns
.coerce_dataframe_columns <- function(envir, fields, df="plot.data") {
    coerce_cmds <- NULL
    for (f in fields) {
        v <- get(df, envir = envir)[[f]]
        coerce_cmds <- c(coerce_cmds, .coerce_type(v, f, as_numeric=!.is_groupable(v), df=df))
    }
    .textEval(coerce_cmds, envir)
    coerce_cmds
}
