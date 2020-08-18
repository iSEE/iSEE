#' Create hover observers
#'
#' Create observers to insert or remove the feature/sample name box when hovering over a point in a \linkS4class{DotPlot}.
#'
#' @param plot_name String containing the name of the plot panel containing the brush.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#'
#' @details
#' This function is highly sensitive to the structure of the \code{input$hover} object returned by Shiny.
#' It needs this information to determine exactly (in terms of pixels) where the cursor location is,
#' in order to insert an information UI element at the same location.
#'
#' In addition, we edit the \code{input$hover} object to mimic a scatter plot for violin or square plots.
#' This is necessary to ensure that the info box is intuitively placed at the location of the jittered point,
#' rather than at its true location (i.e., at the center of the violin or square).
#' 
#' In my opinion, we are \emph{too} sensitive to the object's internals, but there's not a lot of choice here.
#'
#' @return
#' An observer is created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#' 
#' @rdname INTERNAL_create_dotplot_hover_observer
#' @importFrom shiny observeEvent insertUI removeUI div
.create_hover_observer <- function(plot_name, input, pObjects, session) {
    hover_field <- paste0(plot_name, "_", .hoverTooltip)
    hover_info <- paste0(plot_name, "_", .hoverInfo)

    # nocov start
    observeEvent(input[[hover_field]], { 
        if (!pObjects$memory[[plot_name]][[.plotHoverInfo]]) {
            return(NULL)
        }
        hover <- input[[hover_field]]
        removeUI(paste0("#", hover_info))

        if (!is.null(hover)) {
            # NOTE: a bit of a hack here to account for violin X/Y.
            # We should probably figure out a better way to pass this 
            # information about the 'real'point coordinates, e.g., by
            # tucking something into the data.frame's attributes.
            df <- pObjects$contents[[plot_name]]
            if (!is.null(df$jitteredX)) {
                df$X <- df$jitteredX
                hover$mapping$group <- NULL
                hover$domain$discrete_limits <- NULL
            }
            if (!is.null(df$jitteredY)) {
                df$Y <- df$jitteredY
                hover$mapping$group <- NULL
                hover$domain$discrete_limits <- NULL
            }
            point <- nearPoints(df, hover, threshold = 5, maxpoints = 1)

            if (nrow(point)!=0) {
                bg <- .panelColor(pObjects$memory[[plot_name]])
                rgb <- .lighten_color_for_fill(bg, as.vector=TRUE)
                rgb <- as.integer(rgb)

                # z-index ensures that the tooltip will be on top. I don't
                # really know why we use coords_css, but it seems to work.
                style <- paste0("position:absolute; z-index:100; padding: 2px; background-color:",
                    sprintf("rgba(%i, %i, %i, 1); ", rgb[1], rgb[2], rgb[3]),
                    "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y + 2, "px;")
                insertUI(paste0("#", plot_name), where="beforeEnd", div(id=hover_info, style=style, rownames(point)))
            }
        }
    }, ignoreNULL=FALSE)
    # nocov end
}

