# Credits: Adapted from knitr
# https://github.com/yihui/knitr/blob/670a530b53d6cc5002797d54034ec8b07b74702c/R/defaults.R
new_defaults = function(value = list()) {
    defaults = value

    get = function(name, default = FALSE, drop = TRUE) {
        if (default) defaults = value  # this is only a local version
        if (missing(name)) defaults else {
            if (drop && length(name) == 1) defaults[[name]] else {
                setNames(defaults[name], name)
            }
        }
    }
    resolve = function(...) {
        dots = list(...)
        if (length(dots) == 0) return()
        if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
            if (length(dots <- dots[[1]]) == 0) return()
        dots
    }
    set = function(...) {
        dots = resolve(...)
        if (length(dots)) defaults <<- merge(dots)
        invisible(NULL)
    }
    merge = function(values) merge_list(defaults, values)
    restore = function(target = value) defaults <<- target

    list(
        get = get, set = set,
        merge = merge, restore = restore
    )
}

#' Default panel options
#'
#' Set global options using \code{iSEEOptions$set()}, so that all relevant panels will use these default values during initialization.
#'
#' See \code{str(iSEEOptions$get())} for a list of default panel options.
#'
#' Note that \code{iSEEOptions$restore()} can be used to reset the global options to the package default values.
#'
#' @section Available options:
#'
#' \describe{
#' \item{\code{point.color}}{Default color of data points in \code{DotPlot} panels (character).}
#' \item{\code{point.size}}{Default size of data points in \code{DotPlot} panels (numeric).}
#' \item{\code{point.alpha}}{Default alpha level controlling transparency of data points in \code{DotPlot} panels (numeric).}
#' \item{\code{downsample}}{Enable visual downsampling in \code{DotPlot} panels (logical).}
#' \item{\code{downsample.resolution}}{Resolution of the visual downsampling, if active (numeric).}
#' \item{\code{selected.color}}{Color of selected data points in \code{DotPlot} panels (character).}
#' \item{\code{selected.alpha}}{Alpha level controlling transparency of data points \emph{not} selected in \code{DotPlot} panels (numeric).}
#' \item{\code{selection.dynamic.single}}{Toggle dynamic single selections for all panels (logical).}
#' \item{\code{selection.dynamic.multiple}}{Toggle dynamic multiple selections for all panels (logical).}
#' \item{\code{contour.color}}{Color of the 2d density estimation contour in \code{DotPlot} panels (character).}
#' \item{\code{font.size}}{Global multiplier controlling the magnification of plot title and text elements in \code{DotPlot} panels (numeric).}
#' \item{\code{legend.position}}{Position of the legend in \code{DotPlot} and \code{ComplexHeatmapPlot} panels (one of \code{"Bottom"}, \code{"Right"}).}
#' \item{\code{legend.direction}}{Position of the legend in \code{DotPlot} and \code{ComplexHeatmapPlot} panels (one of \code{"Horizontal"}, \code{"Vertical"}).}
#' \item{\code{panel.width}}{Default panel grid width (must be between 1 and 12).}
#' \item{\code{panel.height}}{Default panel height (in pixels).}
#' \item{\code{panel.color}}{Named character vector of colors.
#' The names of the vector should be set to the name of class to be overridden; if a class is not named here, its default color is used.
#' It is highly recommended to define colors as hex color codes (e.g., \code{"#1e90ff"}), for full compatibility with both HTML elements and R plots.}
#' \item{\code{assay}}{Character vector of assay names to use if available, in order of preference.}
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @examples iSEEOptions$get('downsample'); iSEEOptions$get('selected.color')
iSEEOptions <- new_defaults(list(

    point.color = "black",
    point.size = 1,
    point.alpha = 1,

    downsample = FALSE,
    downsample.resolution = 200,

    selected.color = "red",
    selected.alpha = 0.1,

    selection.dynamic.single = FALSE,
    selection.dynamic.multiple = FALSE,

    contour.color = "blue",

    font.size = 1,
    legend.point.size = 1,
    legend.position = .plotLegendBottomTitle,
    legend.direction = .plotLegendHorizontalTitle,

    panel.width = 4L,
    panel.height = 500L,
    panel.color = c(),

    assay = c("logcounts", "normcounts")
))

# merge elements of y into x with the same names
# Credits: knitr
merge_list = function(x, y) {
    x[names(y)] = y
    x
}
