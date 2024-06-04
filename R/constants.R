#' Constants
#'
#' Constant values used throughout iSEE panels and extensions.
#' The value of each constant can be accessed using the \code{$} operator and the corresponding key (listed below).
#' 
#' @section Point colouring parameters:
#' \describe{
#' \item{\code{colorByNothingTitle}}{Radio button choice for coloring data points by a fixed color.}
#' \item{\code{colorByColDataTitle}}{Radio button choice for coloring data points by column zwdata.}
#' \item{\code{colorByRowDataTitle}}{Radio button choice for coloring data points by row data.}
#' \item{\code{colorByFeatNameTitle}}{Radio button choice for highlighting a selected feature or coloring data points by assay data for that feature.}
#' \item{\code{colorBySampNameTitle}}{Radio button choice for highlighting a selected sample or coloring data points by assay data for that sample.}
#' \item{\code{colorByColSelectionsTitle}}{Radio button choice for coloring data points by an incoming column selection.}
#' \item{\code{colorByRowSelectionsTitle}}{Radio button choice for coloring data points by an incoming row selection.}
#' }
#' 
#' @section Point shaping parameters: 
#' \describe{
#' \item{\code{shapeByNothingTitle}}{Radio button choice for shaping all data points as points.}
#' \item{\code{shapeByColDataTitle}}{Radio button choice for shaping data points by column data.}
#' \item{\code{shapeByRowDataTitle}}{Radio button choice for shaping data points by row data.}
#' }
#' 
#' @section Point sizing parameters: 
#' \describe{
#' \item{\code{sizeByNothingTitle}}{Radio button choice for sizing all data points as points.}
#' \item{\code{sizeByColDataTitle}}{Radio button choice for sizing data points by column data.}
#' \item{\code{sizeByRowDataTitle}}{Radio button choice for sizing data points by row data.}
#' }
#' 
#' @section Faceting parameters:
#' \describe{
#' \item{\code{facetByNothingTitle}}{Radio button choice for disabling faceting.}
#' \item{\code{facetByRowDataTitle}}{Radio button choice for faceting by row data.}
#' \item{\code{facetByColDataTitle}}{Radio button choice for faceting by column data.}
#' \item{\code{facetByRowSelectionsTitle}}{Radio button choice for faceting by an incoming row selection.}
#' \item{\code{facetByColSelectionsTitle}}{Radio button choice for faceting by an incoming column selection.}
#' }
#' 
#' @section Panel slot names (Deprecated):
#' TODO: Move this section to another man page when another class is implemented to store slot names and types (and default value?).
#' \describe{
#' \item{\code{.dataParamBoxOpen}}{Name of slot that indicates whether the 'Data parameter' box is open.}
#' \item{\code{.multiSelectHistory}}{Name of slot that stores the list of saved selections.}
#' \item{\code{.organizationHeight}}{Name of slot that stores the panel height.}
#' \item{\code{.organizationWidth}}{Name of slot that stores the panel width.}
#' }
#' 
#' @section Multiple selection parameters (Deprecated):
#' \describe{
#' \item{\code{noSelection}}{Value displayed in the absence of selection.}
#' }
#' 
#' @section TODO: 
#' Move the following constants to the appropriate section
#' 
#' \describe{
#' \item{\code{multiSelectSave}}{}
#' \item{\code{multiSelectDelete}}{}
#' \item{\code{noSelection}}{}
#' \item{\code{customSelection}}{}
#' \item{\code{zoomClick}}{}
#' \item{\code{lassoClick}}{}
#' \item{\code{brushField}}{}
#' \item{\code{buttonUpToDateLabel}}{}
#' \item{\code{buttonEmptyHistoryLabel}}{}
#' \item{\code{buttonDeleteLabel}}{}
#' \item{\code{buttonNoSelectionLabel}}{}
#' \item{\code{buttonSaveLabel}}{}
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
#' 
#' @examples
#' iSEE::constants
#' iSEE::constants$colorByColDataTitle
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
    cat("... and", nrow(object@info) - 5, "more constants.\n")
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
constants$visualParamChoiceColorTitle <- "Color"
.visualParamChoiceShapeTitle <- "Shape"
constants$visualParamChoiceShapeTitle <- "Shape"
.visualParamChoiceSizeTitle <- "Size"
constants$visualParamChoiceSizeTitle <- "Size"
.visualParamChoicePointTitle <- "Point"
constants$visualParamChoicePointTitle <- "Point"
.visualParamChoiceFacetTitle <- "Facet"
constants$visualParamChoiceFacetTitle <- "Facet"
.visualParamChoiceTextTitle <- "Text"
constants$visualParamChoiceTextTitle <- "Text"
.visualParamChoiceOtherTitle <- "Other"
constants$visualParamChoiceOtherTitle <- "Other"
.visualParamChoiceMetadataTitle <- "Annotations"
constants$visualParamChoiceMetadataTitle <- "Annotations"
.visualParamChoiceLabelsTitle <- "Labels"
constants$visualParamChoiceLabelsTitle <- "Labels"
.visualParamChoiceTransformTitle <- "Transform"
constants$visualParamChoiceTransformTitle <- "Transform"
.visualParamChoiceLegendTitle <- "Legends"
constants$visualParamChoiceLegendTitle <- "Legends"

.showNamesRowTitle <- "Rows"
constants$showNamesRowTitle <- "Rows"
.showNamesColumnTitle <- "Columns"
constants$showNamesColumnTitle <- "Columns"

.plotLegendRightTitle <- "Right"
constants$plotLegendRightTitle <- "Right"
.plotLegendBottomTitle <- "Bottom"
constants$plotLegendBottomTitle <- "Bottom"

.plotLegendHorizontalTitle <- "Horizontal"
constants$plotLegendHorizontalTitle <- "Horizontal"
.plotLegendVerticalTitle <- "Vertical"
constants$plotLegendVerticalTitle <- "Vertical"

.plotFontSizeAxisTextDefault <- 10
constants$plotFontSizeAxisTextDefault <- 10
.plotFontSizeAxisTitleDefault <- 12
constants$plotFontSizeAxisTitleDefault <- 12
.plotFontSizeLegendTextDefault <- 9
constants$plotFontSizeLegendTextDefault <- 9
.plotFontSizeLegendTitleDefault <- 11
constants$plotFontSizeLegendTitleDefault <- 11
.plotFontSizeTitleDefault <- 12
constants$plotFontSizeTitleDefault <- 12

.hoverTooltip <- "INTERNAL_hover_event"
constants$hoverTooltip <- "INTERNAL_hover_event"
.hoverInfo <- "INTERNAL_hover_info"
constants$hoverInfo <- "INTERNAL_hover_info"

# Table parameters. ----

.int_statTableSelected <- "_rows_selected"
constants$int_statTableSelected <- "_rows_selected"
.int_statTableSearch <- "_search"
constants$int_statTableSearch <- "_search"
.int_statTableColSearch <- "_search_columns"
constants$int_statTableColSearch <- "_search_columns"

.tableExtraInfo <- "INTERNAL_extra_info"
constants$tableExtraInfo <- "INTERNAL_extra_info"

# Tour parameters. ---

.panelHelpTour <- "INTERNAL_help"
constants$panelHelpTour <- "INTERNAL_help"

# Reactive flags. ---

.flagOutputUpdate <- "INTERNAL_output_update"
constants$flagOutputUpdate <- "INTERNAL_output_update"
.flagSingleSelect <- "INTERNAL_single_select"
constants$flagSingleSelect <- "INTERNAL_single_select"
.flagMultiSelect <- "INTERNAL_multi_select"
constants$flagMultiSelect <- "INTERNAL_multi_select"
.flagRelinkedSelect <- "INTERNAL_relinked_select"
constants$flagRelinkedSelect <- "INTERNAL_relinked_select"

.panelMultiSelectInfo <- "INTERNAL_PanelMultiSelectInfo"
constants$panelMultiSelectInfo <- "INTERNAL_PanelMultiSelectInfo"
.panelSelectLinkInfo <- "INTERNAL_PanelSelectLinkInfo"
constants$panelSelectLinkInfo <- "INTERNAL_PanelSelectLinkInfo"

.flagTableUpdate <- "INTERNAL_table_update"
constants$flagTableUpdate <- "INTERNAL_table_update"

# Voice parameters ----

.voiceActivePanel <- "voiceActivePanel"
constants$voiceActivePanel <- "voiceActivePanel"

.voiceShowActivePanelInput = "voiceShowActivePanel"
constants$voiceShowActivePanelInput = "voiceShowActivePanel"

.voiceCreatePanelInput <- "voiceCreatePanel"
constants$voiceCreatePanelInput <- "voiceCreatePanel"
.voiceRemovePanelInput <- "voiceRemovePanel"
constants$voiceRemovePanelInput <- "voiceRemovePanel"

.voiceControlPanelInput <- "voiceControlPanel"
constants$voiceControlPanelInput <- "voiceControlPanel"
.voiceColorUsingInput <- "voiceColorUsing"
constants$voiceColorUsingInput <- "voiceColorUsing"
.voiceColorByInput <- "voiceColorBy"
constants$voiceColorByInput <- "voiceColorBy"
.voiceReceiveFromInput <- "voiceReceiveFrom"
constants$voiceReceiveFromInput <- "voiceReceiveFrom"
.voiceSendToInput <- "voiceSendTo"
constants$voiceSendToInput <- "voiceSendTo"

# Clustering parameters ----

.clusterDistanceEuclidean <- "euclidean"
constants$clusterDistanceEuclidean <- "euclidean"
.clusterDistanceMaximum <- "maximum"
constants$clusterDistanceMaximum <- "maximum"
.clusterDistanceManhattan <- "manhattan"
constants$clusterDistanceManhattan <- "manhattan"
.clusterDistanceCanberra <- "canberra"
constants$clusterDistanceCanberra <- "canberra"
.clusterDistanceBinary <- "binary"
constants$clusterDistanceBinary <- "binary"
.clusterDistanceMinkowski <- "minkowski"
constants$clusterDistanceMinkowski <- "minkowski"
.clusterDistancePearson <- "pearson"
constants$clusterDistancePearson <- "pearson"
.clusterDistanceSpearman <- "spearman"
constants$clusterDistanceSpearman <- "spearman"
.clusterDistanceKendall <- "kendall"
constants$clusterDistanceKendall <- "kendall"

.clusterMethodWardD <- "ward.D"
constants$clusterMethodWardD <- "ward.D"
.clusterMethodWardD2 <- "ward.D2"
constants$clusterMethodWardD2 <- "ward.D2"
.clusterMethodSingle <- "single"
constants$clusterMethodSingle <- "single"
.clusterMethodComplete <- "complete"
constants$clusterMethodComplete <- "complete"
.clusterMethodAverage <- "average"
constants$clusterMethodAverage <- "average"
.clusterMethodMcquitty <- "mcquitty"
constants$clusterMethodMcquitty <- "mcquitty"
.clusterMethodMedian <- "median"
constants$clusterMethodMedian <- "median"
.clusterMethodCentroid <- "centroid"
constants$clusterMethodCentroid <- "centroid"

# .heatMapCenteredColormap colormaps ----

.colormapPurpleBlackYellow <- "purple < black < yellow"
constants$colormapPurpleBlackYellow <- "purple < black < yellow"
.colormapBlueWhiteOrange <- "blue < white < orange"
constants$colormapBlueWhiteOrange <- "blue < white < orange"
.colormapBlueWhiteRed <- "blue < white < red"
constants$colormapBlueWhiteRed <- "blue < white < red"
.colormapGreenWhiteRed <- "green < black < red"
constants$colormapGreenWhiteRed <- "green < black < red"

# Versioning information ---

#' @importFrom utils packageVersion
.latest_version <- list(iSEE=packageVersion("iSEE"))
