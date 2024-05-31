#' Constants
#'
#' Constant values used throughout iSEE panels and extensions.
#' 
#' @section Panel slot names (Deprecated):
#' \describe{
#' \item{\code{.dataParamBoxOpen}}{Name of slot that indicates whether the 'Data parameter' box is open.}
#' \item{\code{.multiSelectHistory}}{Name of slot that stores the list of saved selections.}
#' \item{\code{.organizationHeight}}{Name of slot that stores the panel height.}
#' \item{\code{.organizationWidth}}{Name of slot that stores the panel width.}
#' }
#' 
#' @section Multiple selection parameters (Deprecated):
#' \describe{
#' \item{\code{.noSelection}}{Value displayed in the absence of selection.}
#' }
#'
#' @author Kevin Rue-Albrecht
#' 
#' @name constants
#' @docType data
#' @keywords data
#' @aliases .dataParamBoxOpen
#' .multiSelectHistory
#' .noSelection
#' .organizationHeight
#' .organizationWidth
NULL

#' iSEE constants
#' 
#' \pkg{iSEE} and extension packages use constants to represent UI choices that are not data-driven.
#' 
#' This class allows each package to store all its constants in a single object that can be exported and queried for values.
#' In particular, the \code{$} operator provides a mechanism to emit deprecation warnings and redirect renamed constants.
#'
#' @slot info Read-only data.frame of information about the constants. 
#'
#' @return An object of class \code{iSEEconstants}.
#' @export
#'
#' @examples
#' constants <- new("iSEEconstants")
#' constants$key <- "value"
setClass("iSEEconstants", slots=c(info = "data.frame"))

setMethod(
  "initialize", "iSEEconstants", function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@info <- data.frame(
      row.names = character(0),
      value = character(0),
      package = character(0)
    )
    .Object
})

setMethod("show", "iSEEconstants", function(object) {
  cat("class:", class(object), "\n")
  cat("count:", nrow(object@info), "\n")
  print(head(object@info, 5))
  if (nrow(object@info) > 5) {
    cat("... and", nrow(object@info) - 5, "more rows.\n")
  }
})

setMethod("$<-", "iSEEconstants", function(x, name, value) {
  sourcePackage <- packageName()
  if (is.null(sourcePackage)) {
    stop("Only extension packages can edit this object.")
  }
  if (name %in% rownames(x@info)) {
    stop("Name already in use: ", sQuote(name))
  }
  x@info <- rbind(x@info, data.frame(
    row.names = name,
    "value" = value,
    "package" = sourcePackage)
  )
  invisible(x)
})

setMethod("$", "iSEEconstants", function(x, name) {
  x@info[name, "value"]
})

.DollarNames.iSEEconstants <- function(x, pattern = "") {
  grep(pattern, rownames(x@info), value=TRUE)
}

#' @export
constants <- new("iSEEconstants")

# Point colouring parameters. ----

.colorByNothingTitle <- "None"
constants$colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
constants$colorByColDataTitle <- "Column data"
.colorByRowDataTitle <- "Row data"
constants$colorByRowDataTitle <- "Row data"
.colorByFeatNameTitle <- "Feature name"
constants$colorByFeatNameTitle <- "Feature name"
.colorBySampNameTitle <- "Sample name"
constants$colorBySampNameTitle <- "Sample name"
.colorByColSelectionsTitle <- "Column selection"
constants$colorByColSelectionsTitle <- "Column selection"
.colorByRowSelectionsTitle <- "Row selection"
constants$colorByRowSelectionsTitle <- "Row selection"

# Point shaping parameters. ----

.shapeByNothingTitle <- "None"
constants$shapeByNothingTitle <- "None"
.shapeByColDataTitle <- "Column data"
constants$shapeByColDataTitle <- "Column data"
.shapeByRowDataTitle <- "Row data"
constants$shapeByRowDataTitle <- "Row data"

# Point sizing parameters. ----

.sizeByNothingTitle <- "None"
constants$sizeByNothingTitle <- "None"
.sizeByColDataTitle <- "Column data"
constants$sizeByColDataTitle <- "Column data"
.sizeByRowDataTitle <- "Row data"
constants$sizeByRowDataTitle <- "Row data"

# Faceting parameters. ----

.facetByNothingTitle <- "None"
constants$facetByNothingTitle <- "None"
.facetByRowDataTitle <- "Row data"
constants$facetByRowDataTitle <- "Row data"
.facetByColDataTitle <- "Column data"
constants$facetByColDataTitle <- "Column data"
.facetByRowSelectionsTitle <- "Row selection"
constants$facetByRowSelectionsTitle <- "Row selection"
.facetByColSelectionsTitle <- "Column selection"
constants$facetByColSelectionsTitle <- "Column selection"

# Multiple selection parameters. ---

.multiSelectSave <- "INTERNAL_MultiSelectSave"
constants$multiSelectSave <- "INTERNAL_MultiSelectSave"
.multiSelectDelete <- "INTERNAL_MultiSelectDelete"
constants$multiSelectDelete <- "INTERNAL_MultiSelectDelete"


#' @export
.noSelection <- "---" # imported by e.g. iSEEu::AggregatedDotPlot
constants$noSelection <- "---"
.customSelection <- "Custom ..."
constants$customSelection <- "Custom ..."

# Zooming parameters. ----

.zoomClick <- "INTERNAL_ZoomClick"
constants$zoomClick <- "INTERNAL_ZoomClick"

# Lasso parameters. ----

.lassoClick <- "INTERNAL_LassoClick"
constants$lassoClick <- "INTERNAL_LassoClick"

# Brush parameters. ----

.brushField <- "Brush"
constants$brushField <- "Brush"

# Button parameters ----

.buttonUpToDateLabel <- "Up to date"
constants$buttonUpToDateLabel <- "Up to date"
.buttonEmptyHistoryLabel <- "No history"
constants$buttonEmptyHistoryLabel <- "No history"
.buttonDeleteLabel <- "Delete"
constants$buttonDeleteLabel <- "Delete"
.buttonNoSelectionLabel <- "No selection"
constants$buttonNoSelectionLabel <- "No selection"
.buttonSaveLabel <- "Save"
constants$buttonSaveLabel <- "Save"

.dimnamesModalOpen <- "INTERNAL_DimNamesEdit"

# Other plot parameters. ----

.visualParamChoiceColorTitle <- "Color"
.visualParamChoiceShapeTitle <- "Shape"
.visualParamChoiceSizeTitle <- "Size"
.visualParamChoicePointTitle <- "Point"
.visualParamChoiceFacetTitle <- "Facet"
.visualParamChoiceTextTitle <- "Text"
.visualParamChoiceOtherTitle <- "Other"
.visualParamChoiceMetadataTitle <- "Annotations"
.visualParamChoiceLabelsTitle <- "Labels"
.visualParamChoiceTransformTitle <- "Transform"
.visualParamChoiceLegendTitle <- "Legends"

.showNamesRowTitle <- "Rows"
.showNamesColumnTitle <- "Columns"

.plotLegendRightTitle <- "Right"
.plotLegendBottomTitle <- "Bottom"

.plotLegendHorizontalTitle <- "Horizontal"
.plotLegendVerticalTitle <- "Vertical"

.plotFontSizeAxisTextDefault <- 10
.plotFontSizeAxisTitleDefault <- 12
.plotFontSizeLegendTextDefault <- 9
.plotFontSizeLegendTitleDefault <- 11
.plotFontSizeTitleDefault <- 12

.hoverTooltip <- "INTERNAL_hover_event"
.hoverInfo <- "INTERNAL_hover_info"

# Table parameters. ----

.int_statTableSelected <- "_rows_selected"
.int_statTableSearch <- "_search"
.int_statTableColSearch <- "_search_columns"

.tableExtraInfo <- "INTERNAL_extra_info"

# Tour parameters. ---

.panelHelpTour <- "INTERNAL_help"

# Reactive flags. ---

.flagOutputUpdate <- "INTERNAL_output_update"
.flagSingleSelect <- "INTERNAL_single_select"
.flagMultiSelect <- "INTERNAL_multi_select"
.flagRelinkedSelect <- "INTERNAL_relinked_select"

.panelMultiSelectInfo <- "INTERNAL_PanelMultiSelectInfo"
.panelSelectLinkInfo <- "INTERNAL_PanelSelectLinkInfo"

.flagTableUpdate <- "INTERNAL_table_update"

# Voice parameters ----

.voiceActivePanel <- "voiceActivePanel"

.voiceShowActivePanelInput = "voiceShowActivePanel"

.voiceCreatePanelInput <- "voiceCreatePanel"
.voiceRemovePanelInput <- "voiceRemovePanel"

.voiceControlPanelInput <- "voiceControlPanel"
.voiceColorUsingInput <- "voiceColorUsing"
.voiceColorByInput <- "voiceColorBy"
.voiceReceiveFromInput <- "voiceReceiveFrom"
.voiceSendToInput <- "voiceSendTo"

# Clustering parameters ----

.clusterDistanceEuclidean <- "euclidean"
.clusterDistanceMaximum <- "maximum"
.clusterDistanceManhattan <- "manhattan"
.clusterDistanceCanberra <- "canberra"
.clusterDistanceBinary <- "binary"
.clusterDistanceMinkowski <- "minkowski"
.clusterDistancePearson <- "pearson"
.clusterDistanceSpearman <- "spearman"
.clusterDistanceKendall <- "kendall"

.clusterMethodWardD <- "ward.D"
.clusterMethodWardD2 <- "ward.D2"
.clusterMethodSingle <- "single"
.clusterMethodComplete <- "complete"
.clusterMethodAverage <- "average"
.clusterMethodMcquitty <- "mcquitty"
.clusterMethodMedian <- "median"
.clusterMethodCentroid <- "centroid"

# .heatMapCenteredColormap colormaps ----

.colormapPurpleBlackYellow <- "purple < black < yellow"
.colormapBlueWhiteOrange <- "blue < white < orange"
.colormapBlueWhiteRed <- "blue < white < red"
.colormapGreenWhiteRed <- "green < black < red"

# Versioning information ---

#' @importFrom utils packageVersion
.latest_version <- list(iSEE=packageVersion("iSEE"))
