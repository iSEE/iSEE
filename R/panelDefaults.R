panel.default.env <- new.env()
panel.default.env$options <- list(
    ColorByDefaultColor = "black",
    PointSize = 1,
    PointAlpha = 1,

    Downsample = FALSE,
    DownsampleResolution = 200,

    ColorByNameColor = "red",
    ColorByNameAssay = "logcounts",
    SelectionAlpha = 0.1,

    SingleSelectionDynamicSource = FALSE,
    MultipleSelectionDynamicSource = FALSE,

    ContourColor = "blue",

    FontSize = 1,
    LegendPointSize = 1,
    LegendPosition = .plotLegendBottomTitle,
    LegendDirection = .plotLegendHorizontalTitle,

    PanelWidth = 4L,
    PanelHeight = 500L,

    Assay = "logcounts"
)

#' Panel defaults
#'
#' Get or set default parameters that are used by certain \linkS4class{Panel} during their construction.
#' This allows users to easily change the defaults for multiple panels simultaneously without having to manually specify them in the constructor.
#'
#' @param ... Named options to set.
#' Alternatively a single named list containing the options to set.
#' @param name String containing the name of the option to retrieve.
#' Alternatively \code{NULL}, in which case the current values of all options are returned as a named list.
#' @param error Logical scalar indicating whether an error should be raised if \code{name} cannot be found.
#'
#' @return
#' \code{panelDefaults} will return a named list of the values of all options. 
#' If \code{...} is non-empty, \code{panelDefaults} will modify the global options that are used during the constructors for the relevant \linkS4class{Panel} classes.
#' (Note that the return value still contains the values \emph{before} the modification is applied.)
#'
#' \code{getPanelDefault} will return the current value of the requested option.
#' If \code{error=TRUE} and \code{name} is not present, an error is raised; otherwise \code{NULL} is returned.
#'
#' @details
#' All options set by \code{panelDefaults} will only affect \linkS4class{Panel} construction and have no effect on the behavior of Panels that are already constructed.
#' Most options are named after the affected slot in the relevant Panel subclass.
#'
#' For general \linkS4class{Panel}s:
#' \itemize{
#' \item \code{PanelWidth}, defaults to 4. 
#' \item \code{PanelHeight}, defaults to 500.
#' }
#'
#' For \linkS4class{DotPlot}s:
#' \itemize{
#' \item \code{ColorByDefaultColor}, defaults to \code{"black"}.
#' \item \code{PointSize}, defaults to 1.
#' \item \code{PointAlpha}, defaults to 1.
#' \item \code{Downsample}, defaults to \code{FALSE}.
#' \item \code{DownsampleResolution}, defaults to 200.
#' \item \code{SelectionAlpha}, defaults to 0.1.
#' \item \code{ContourColor}, defaults to \code{"blue"}.
#' \item \code{FontSize}, defaults to 1.
#' \item \code{LegendPointSize}, defaults to 1.
#' }
#' 
#' For \linkS4class{RowDotPlot}s:
#' \itemize{
#' \item \code{TooltipRowData}, defaults to \code{character(0)}.
#' }
#' 
#' For \linkS4class{ColumnDotPlot}s:
#' \itemize{
#' \item \code{TooltipColumnData}, defaults to \code{character(0)}.
#' }
#'
#' For \linkS4class{ComplexHeatmapPlot}s:
#' \itemize{
#' \item \code{LegendDirection}, defaults to \code{"Horizontal"}.
#' }
#'
#' A few options affect multiple subclasses that independently define the same slot:
#' \itemize{
#' \item \code{LegendPosition}, defaults to \code{"Bottom"}.
#' Affects \linkS4class{DotPlot}s and \linkS4class{ComplexHeatmapPlot}s.
#' \item \code{Assay}, defaults to \code{"logcounts"}.
#' Affects \linkS4class{FeatureAssayPlot}s, \linkS4class{SampleAssayPlot}s and \linkS4class{ComplexHeatmapPlot}s.
#' }
#'
#' A few options are not named after any particular slot as they affect different slots in different subclasses:
#' \itemize{
#' \item \code{ColorByNameColor}, defaults to \code{"red"}.
#' This affects \code{ColorByFeatureNameColor} in \linkS4class{RowDotPlot}s and \code{ColorBySampleNameColor} in \linkS4class{ColumnDotPlot}s.
#' \item \code{ColorByNameAssay}, defaults to \code{"logcounts"}.
#' This affects \code{ColorByFeatureNameAssay} in \linkS4class{RowDotPlot}s and \code{ColorBySampleNameAssay} in \linkS4class{ColumnDotPlot}s.
#' \item \code{SingleSelectionDynamicSource}, defaults to \code{FALSE}.
#' This affects \code{ColorByFeatureDynamicSource}, \code{ColorBySampleDynamicSource}, \code{XAxisFeatureDynamicSource}, \code{YAxisFeatureDynamicSource},
#' \code{XAxisSampleDynamicSource} and \code{YAxisSampleDynamicSource} in the relevant panels.
#' \item \code{MultipleSelectionDynamicSource}, defaults to \code{FALSE}.
#' This affects \code{RowSelectionDynamicSource} and \code{ColumnSelectionDynamicSource}.
#' }
#'
#' @section For developers:
#' Developers of Panel subclasses may add more options to this list, typically by calling \code{panelDefaults} in the \code{.onLoad} expressions of the package containing the subclass.
#' We recommend prefixing any options with the name of the package in the form of \code{<PACKAGE>_<OPTION>},
#' so as to avoid conflicts with other options (in the base classes, or in other downstream packages) that have the same name.
#' Any options added in this manner should correspond to parameters that are already present as slots in the panel class.
#' If this is not the case, consider using \code{\link{registerAppOptions}} instead.
#'
#' @author Kevin Rue-Albrecht
#'
#' @examples
#' old <- panelDefaults(Assay="WHEE")
#' getPanelDefault("Assay")
#'
#' old <- panelDefaults(Assay="FOO", PointSize=5)
#' getPanelDefault("Assay")
#' getPanelDefault("PointSize")
#'
#' # We can also list out all options:
#' panelDefaults()
#' 
#' # Restoring the previous defaults.
#' panelDefaults(old)
#' getPanelDefault("Assay")
#' getPanelDefault("PointSize")
#'
#' @export
panelDefaults <- function(...) {
    current <- list(...)
    previous <- panel.default.env$options

    if (.is_options_list(current)) {
        latest <- current[[1]]
    } else {
        latest <- previous
        latest[names(current)] <- current
    }

    panel.default.env$options <- latest
    invisible(previous)
}

.is_options_list <- function(x) length(x)==1L && is.null(names(x)) && is.list(x[[1]])

#' @export
#' @rdname panelDefaults
getPanelDefault <- function(name, error=TRUE) {
    if (error && !name %in% names(panel.default.env$options)) {
        stop("could not find '", name, "'")
    }

    # For back-compatibility, for the time being.
    back.comp <- iSEEOptions$get(.translation.panel_defaults[name])
    if (!is.null(back.comp)) {
        return(back.comp)
    }

    panel.default.env$options[[name]]
}
