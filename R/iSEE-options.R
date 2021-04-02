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
        if (names(dots) %in% .translation.panel_defaults) {
            .Deprecated(old="iSEEOptions$set", new="panelDefaults")
        }
        if (names(dots) %in% .translation.app_defaults) {
            .Deprecated(old="iSEEOptions$set", new="registerAppOptions")
        }
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

.translation.panel_defaults <- c(
    ColorByDefaultColor = "point.color",
    PointSize = "point.size",
    PointAlpha = "point.alpha",

    Downsample = "downsample",
    DownsampleResolution = "downsample.resolution",

    ColorByNameColor = "selected.color",
    SelectionAlpha = "selected.alpha",

    SingleSelectionDynamicSource = "selection.dynamic.single",
    MultipleSelectionDynamicSource = "selection.dynamic.multiple",

    ContourColor = "contour.color",

    FontSize = "font.size",
    LegendPointSize = "legend.point.size",
    LegendPosition = "legend.position",
    LegendDirection = "legend.direction",

    PanelWidth = "panel.width",
    PanelHeight = "panel.height",

    Assay = "assay"
)

.translation.app_defaults <- c(
    "panel.color",
    "color.maxlevels",
    "factor.maxlevels",
    "RowTable.extra.info",
    "ColumnTable.extra.info"
)

#' Global \pkg{iSEE} options
#'
#' Get or set global values that are used by relevant panels during construction and application initialization.
#' This has been deprecated in favor of \code{\link{panelDefaults}} (for options that apply during \linkS4class{Panel} construction)
#' and \code{\link{registerAppOptions}} (for options that apply during application runtime).
#'
#' @section Commands:
#' \code{str(iSEEOptions$get())} will show the default values for all options.
#'
#' \code{iSEEOptions$set(name=value)} will set the \code{name}d option to \code{value}.
#' 
#' \code{iSEEOptions$restore()} will reset the global options to the package default values.
#'
#' @section Available options:
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
#' \item{\code{color.maxlevels}}{Maximum number of levels for a categorical variable used for coloring.
#' Variables with more levels are coerced to numeric to avoid problems with an overly-large legend.
#' Defaults to 24.}
#' \item{\code{factor.maxlevels}}{Maximum number of levels for a categorical variable to be used anywhere in the app.
#' Variables with more levels are coerced to numeric to avoid rendering delays.
#' Defaults to 100.}
#' \item{\code{assay}}{Character vector of assay names to use if available, in order of preference.}
#' \item{\code{RowTable.select.details}}{A function that takes a string containing the name of a feature (i.e., the current selection in the \linkS4class{RowTable}) and returns a HTML element with more details.} 
#' \item{\code{ColumnTable.select.details}}{A function that takes a string containing the name of a sample (i.e., the current selection in the \linkS4class{ColumnTable}) and returns a HTML element with more details.}
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @examples iSEEOptions$get('downsample'); iSEEOptions$get('selected.color')
iSEEOptions <- new_defaults({
    x <- c(unname(.translation.panel_defaults), .translation.app_defaults)
    y <- vector("list", length(x))
    names(y) <- x
    y
})

# merge elements of y into x with the same names
# Credits: knitr
merge_list = function(x, y) {
    x[names(y)] = y
    x
}
