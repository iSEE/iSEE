
.validParameterChoices <- list()

# Reduced dimension plotting parameters. ----
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

# Feature assay plotting parameters. ----
.featAssayXAxisNothingTitle <- "None"
.featAssayXAxisColDataTitle <- "Column data"
.featAssayXAxisFeatNameTitle <- "Feature name"

.featAssayAssay <- "Assay"
.featAssayXAxis <- "XAxis"
.featAssayXAxisColData <- "XAxisColData"
.featAssayXAxisRowTable <- "XAxisRowTable"
.featAssayXAxisFeatName <- "XAxisFeatName"
.featAssayYAxisRowTable <- "YAxisRowTable"
.featAssayYAxisFeatName <- "YAxisFeatName"

# Column data plotting parameters. ----
.colDataXAxisNothingTitle <- "None"
.colDataXAxisColDataTitle <- "Column data"

.colDataYAxis <- "YAxis"
.colDataXAxis <- "XAxis"
.colDataXAxisColData <- "XAxisColData"

# Row data plotting parameters. ----
.rowDataXAxisNothingTitle <- "None"
.rowDataXAxisRowDataTitle <- "Row data"

.rowDataYAxis <- "YAxis"
.rowDataXAxis <- "XAxis"
.rowDataXAxisRowData <- "XAxisRowData"

# Sample assay plotting parameters. ----
.sampAssayXAxisNothingTitle <- "None"
.sampAssayXAxisRowDataTitle <- "Row data"
.sampAssayXAxisSampNameTitle <- "Sample name"

.sampAssayAssay <- "Assay"
.sampAssayXAxis <- "XAxis"
.sampAssayXAxisRowData <- "XAxisRowData"
.sampAssayXAxisColTable <- "XAxisColTable"
.sampAssayXAxisSampName <- "XAxisSampName"
.sampAssayYAxisColTable <- "YAxisColTable"
.sampAssayYAxisSampName <- "YAxisSampName"

# Custom plotting parameters. ----
.customFun <- "Function"
.customArgs <- "Arguments"
.customVisibleArgs <- "VisibleArgs"
.customSubmit <- "Submit"
.customColSource <- "ColumnSource"
.customRowSource <- "RowSource"

# Heatmap modal ----
.heatMapFeaturesTextInput <- "FeatTextInput"
.heatMapFeaturesTextSubmit <- "FeatTextSubmit"
.heatMapFeaturesFileInput <- "FeatFileInput"

.heatMapModalSummary <- "ModalSummary"
.heatMapModalTable <- "ModalTable"

# Heatmap plotting parameters. ----
.heatMapAssay <- "Assay"

.heatMapFeatName <- "FeatName"
.heatMapFeatNameBoxOpen <- "FeatNameBoxOpen"
.heatMapImportFeatures <- "Import"
.heatMapClearFeatures <- "Clear"
.heatMapImportSource <- "FeatNameSource"
.heatMapCluster <- "Clustered"

.heatMapColData <- "ColData"
.heatMapColDataBoxOpen <- "ColDataBoxOpen"
.heatMapLegend <- "Legend"

.heatMapColorBoxOpen <- "ColorBoxOpen"
.heatMapCenterScale <- "CenterScale"
.heatMapCenterTitle <- "Centered"
.heatMapScaleTitle <- "Scaled"

.heatMapLower <- "Lower"
.heatMapUpper <- "Upper"
.heatMapCenteredColors <- "ColorScale"

.heatMapRelHeightColorBar <- 0.1
.heatMapRelHeightHeatmap <- 1
.heatMapRelHeightAnnot <- 0.1

# Faceting parameters ----

# Logical fields whether to facet
.facetByRow <- "FacetByRow"
.facetByColumn <- "FacetByColumn"

# Fields holding the covariate to facet by
.facetRowsByRowData <- "RowFacetByRowData"
.facetColumnsByRowData <- "ColumnFacetByRowData"
.facetRowsByColData <- "RowFacetByColData"
.facetColumnsByColData <- "ColumnFacetByColData"

# Point colouring parameters. ----
.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByRowDataTitle <- "Row data"
.colorByFeatNameTitle <- "Feature name"
.colorBySampNameTitle <- "Sample name"

.colorByField <- "ColorBy"
.colorByDefaultColor <- "ColorByDefaultColor"
.colorByColData <- "ColorByColData"
.colorByRowData <- "ColorByRowData"

.validParameterChoices[[.colorByField]] <-
    c(.colorByNothingTitle, .colorByColDataTitle, .colorByRowDataTitle, .colorByFeatNameTitle, .colorBySampNameTitle)

.colorByFeatName <- "ColorByFeatName"
.colorByRowTable <- "ColorByRowTable"
.colorByFeatNameAssay <- "ColorByFeatNameAssay"
.colorByFeatNameColor <- "ColorByFeatNameColor"

.colorBySampName <- "ColorBySampName"
.colorByColTable <- "ColorByColTable"
.colorBySampNameAssay <- "ColorBySampNameAssay"
.colorBySampNameColor <- "ColorBySampNameColor"

# Point shaping parameters. ----

.shapeByNothingTitle <- "None"
.shapeByColDataTitle <- "Column data"
.shapeByRowDataTitle <- "Row data"

.shapeByField <- "ShapeBy"
.shapeByColData <- "ShapeByColData"
.shapeByRowData <- "ShapeByRowData"

# Point sizing parameters. ----

.sizeByNothingTitle <- "None"
.sizeByColDataTitle <- "Column data"
.sizeByRowDataTitle <- "Row data"

.sizeByField <- "SizeBy"
.sizeByColData <- "SizeByColData"
.sizeByRowData <- "SizeByRowData"

# Point selection parameters. ----
.selectParamBoxOpen <- "SelectBoxOpen"

.selectByPlot <- "SelectByPlot"
.selectEffect <- "SelectEffect"
.selectRestrictTitle <- "Restrict"
.selectColorTitle <- "Color"
.selectTransTitle <- "Transparent"

.selectColor <- "SelectColor"
.selectTransAlpha <- "SelectAlpha"

# Multiple selection parameters. ---
.multiSelectHistory <- "MultiSelectHistory"
.multiSelectSave <- "MultiSelectSave"
.multiSelectDelete <- "MultiSelectDelete"

.selectMultiType <- "SelectMultiType"
.selectMultiActiveTitle <- "Active"
.selectMultiUnionTitle <- "Union"
.selectMultiSavedTitle <- "Saved"

.selectMultiSaved <- "SelectMultiSaved"

.noSelection <- "---"
.customSelection <- "Custom ..."

# Zooming parameters. ----
.zoomData <- "ZoomData"
.zoomClick <- "ZoomClick"

# Lasso parameters. ----
.lassoClick <- "LassoClick"
.lassoData <- "LassoData"

# Brush parameters. ----
.brushField <- "Brush"
.brushData <- "BrushData"

# Button parameters ----
.buttonUpToDateLabel <- "Up to date"
.buttonUpdateLabel <- "Update"
.buttonEmptyHistoryLabel <- "No history"
.buttonDeleteLabel <- "Delete"
.buttonNoSelectionLabel <- "No selection"
.buttonSaveLabel <- "Save"

# Other plot parameters. ----
.dataParamBoxOpen <- "DataBoxOpen"
.visualParamBoxOpen <- "VisualBoxOpen"
.visualParamChoice <- "VisualChoices"

.visualParamChoiceColorTitle <- "Color"
.visualParamChoiceShapeTitle <- "Shape"
.visualParamChoicePointTitle <- "Points"
.visualParamChoiceFacetTitle <- "Facets"
.visualParamChoiceOtherTitle <- "Other"

.contourAddTitle <- "ContourAdd"
.contourColor <- "ContourColor"

.plotPointSize <- "PointSize"
.plotPointAlpha <- "PointAlpha"
.plotPointDownsample <- "Downsample"
.plotPointSampleRes <- "SampleRes"

.plotFontSize <- "FontSize"
.plotLegendPosition <- "LegendPosition"
.plotLegendRightTitle <- "Right"
.plotLegendBottomTitle <- "Bottom"

.plotFontSizeAxisTextDefault <- 10
.plotFontSizeAxisTitleDefault <- 12
.plotFontSizeLegendTextDefault <- 9
.plotFontSizeLegendTitleDefault <- 11
.plotFontSizeTitleDefault <- 12

# INTERNAL: Memory parameters ----

.allCoordinatesNames <- c("X", "Y", "FacetRow", "FacetColumn")

# Table parameters. ----
.statTableSelected <- "Selected"
.statTableSelected <- "Selected"
.statTableSearch <- "Search"
.statTableColSearch <- "SearchColumns"
.int_statTableSelected <- "_rows_selected"
.int_statTableSearch <- "_search"
.int_statTableColSearch <- "_search_columns"

.rowStatSelected <- "Selected"
.rowStatSearch <- "Search"
.rowStatColSearch <- "SearchColumns"
.int_rowStatSelected <- "_rows_selected"
.int_rowStatSearch <- "_search"
.int_rowStatColSearch <- "_search_columns"

.customStatSearch <- .rowStatSearch
.int_customStatSearch <- .int_rowStatSearch

# Panel organization parameters. ----
.organizationNew <- "MakeNew"
.organizationUp <- "ShiftUp"
.organizationDown <- "ShiftDown"
.organizationDiscard <- "Discard"
.organizationModify <- "Modify"
.organizationWidth <- "PanelWidth"
.organizationHeight <- "PanelHeight"

.panelGeneralInfo <- "PanelGeneralInfo"
.panelLinkInfo <- "PanelLinkInfo"

# Voice parameters ----

.voiceActivePanel <- "voiceActivePanel"

.voiceShowActivePanelInput = "voiceShowActivePanel"

.voiceShowPanelInput <- "voiceShowPanel"
.voiceHidePanelInput <- "voiceHidePanel"

.voiceControlPanelInput <- "voiceControlPanel"
.voiceColorUsingInput <- "voiceColorUsing"
.voiceColorByInput <- "voiceColorBy"
.voiceReceiveFromInput <- "voiceReceiveFrom"
.voiceSendToInput <- "voiceSendTo"

# Encoding and decoding names for user/shiny ----

#' Available panel types
#'
#' @description
#' \code{panelTypes} is the character vector of available panel types.
#' Names indicate the encoded panel name.
#'
#' \code{panelCodes} is the complementary of \code{panelTypes}.
#' Values are the encoded names of each panel type, and names indicate their full name.
#'
#' @details
#' Decoded names are used to define the panels visible at initialization.
#' Refer to the documentation of the \code{iSEE} function for more information.
#'
#' Encoded names are used to name the individual HTML elements in the UI.
#' Those names may be used to anchor tour steps at HTML elements using the \code{rintrojs} package.
#'
#' @format
#' Named character vectors.
#'
#' @export
#' @rdname availablePanelTypes
#' @aliases availablePanelTypes
#' @author Kevin Rue-Albrecht
#'
#' @seealso \code{\link{iSEE}}
#'
#' @examples
#' panelTypes
#' panelCodes
panelTypes <- c(redDimPlot="Reduced dimension plot",
                 colDataPlot="Column data plot",
                 featAssayPlot="Feature assay plot",
                 rowStatTable="Row statistics table",
                 rowDataPlot="Row data plot",
                 sampAssayPlot="Sample assay plot",
                 colStatTable="Column statistics table",
                 customDataPlot="Custom data plot",
                 customStatTable="Custom statistics table",
                 heatMapPlot="Heat map")

#' @export
#' @rdname availablePanelTypes
panelCodes <- names(panelTypes)
names(panelCodes) <- panelTypes

# Panel groupings for convenience.
# Refer to dynamicUI.R for the minimum expected structure for each group.
row_point_plot_types <- c("rowDataPlot", "sampAssayPlot")
col_point_plot_types <- c("redDimPlot", "colDataPlot", "featAssayPlot")
point_plot_types <- c(col_point_plot_types, row_point_plot_types)
linked_table_types <- c("rowStatTable", "colStatTable")
custom_panel_types <- c("customDataPlot", "customStatTable")
all_panel_types <- c(point_plot_types, linked_table_types, custom_panel_types, "heatMapPlot")

#' Decode the panel name
#'
#' Translate a panel name from the internal encoding to a user-visible encoding.
#'
#' @param mode Character vector specifying the types of panel, using the internal encoding.
#' @param id Integer vector specifying the panel IDs of the given type.
#'
#' @return A character vector of decoded panel names.
#'
#' @details
#' This function takes an encoded \code{mode} such as \code{"redDimPlot"} and an ID like \code{1}.
#' and returns the decoded panel name \code{"Reduced dimension plot 1"} for presentation in the UI.
#' The input \code{mode} and \code{ID} should be parallel to each other.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_decode_panel_name
#' @seealso
#' \code{\link{.encode_panel_name}},
#' \code{\link{.split_encoded}}
.decode_panel_name <- function(mode, id) {
    paste(panelTypes[mode], id)
}

#' Encode the panel name
#'
#' Convert a decoded panel name to the internal encoding.
#'
#' @param names Character vector of decoded panel names.
#'
#' @return
#' For \code{.encode_panel_name}, a list is returned containing \code{Type}, a character vector of panel types in encoded format;
#' and \code{ID}, an integer vector of panel IDs.
#'
#' For \code{.decoded2encoded}, a character vector is returned containing the encoded panel names.
#'
#' @details
#' The \code{.encode_panel_name} function takes a decoded name like \code{"Reduced dimension plot 1"} and converts it to the encoded type (\code{"redDimPlot"}) and ID (\code{1}).
#' This yields two vectors that are parallel to \code{names}.
#' Invalid types or IDs will raise an error.
#'
#' The \code{.decoded2encoded} function goes one step further and concatenates the type and ID to yield the full encoded name \code{"redDimPlot1"}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_encode_panel_name
#' @seealso
#' \code{\link{.decode_panel_name}}
.encode_panel_name <- function(names) {
    decSplit <- .split_decoded(names)
    id <- decSplit$ID
    encType <- panelCodes[decSplit$Type]

    failed <- is.na(encType) | is.na(id)
    if (any(failed)) {
        stop(sprintf("'%s' is not a legal panel name", names[failed][1]))
    }
    return(list(Type=encType, ID=id))
}

#' @rdname INTERNAL_encode_panel_name
.decoded2encoded <- function(names) {
    x <- .encode_panel_name(names)
    sprintf("%s%i", x$Type, x$ID)
}

#' @rdname INTERNAL_encode_panel_name
.split_decoded <- function(names) {
    id <- as.integer(gsub(".* ([0-9]+)$", "\\1", names))
    raw.str <- gsub(" [0-9]+$", "", names)
    return(list(Type=raw.str, ID=id))
}

#' Split an encoded name
#'
#' Splits an encoded panel name into its panel type and ID.
#'
#' @param names Character vector of encoded panel names.
#'
#' @details
#' This is a convenient function to split an encoded name into its constituents, e.g., for referencing to elements in memory.
#'
#' @return
#' A list containing \code{Type}, a character vector of panel types in encoded format;
#' and \code{ID}, an integer vector of panel IDs.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_split_encoded
#' @seealso
#' \code{\link{.encode_panel_name}},
#' \code{\link{.decode_panel_name}}
.split_encoded <- function(names) {
    id <- as.integer(gsub(".*([0-9]+)$", "\\1", names))
    Type <- gsub("[0-9]+$", "", names)
    return(list(Type=Type, ID=id))
}
