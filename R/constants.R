# Reduced dimension plotting parameters. ----
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

# Feature expression plotting parameters. ----
.featExprXAxisNothingTitle <- "None"
.featExprXAxisColDataTitle <- "Column data"
.featExprXAxisFeatNameTitle <- "Feature name"

.featExprAssay <- "Assay"
.featExprXAxis <- "XAxis"
.featExprXAxisColData <- "XAxisColData"
.featExprXAxisRowTable <- "XAxisRowTable"
.featExprXAxisFeatName <- "XAxisFeatName"
.featExprYAxisRowTable <- "YAxisRowTable"
.featExprYAxisFeatName <- "YAxisFeatName"

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

# Heatmap plotting parameters. ----
.heatMapAssay <- "Assay"

.heatMapFeatName <- "FeatName"
.heatMapFeatNameBoxOpen <- "FeatNameBoxOpen"
.heatMapImportFeatures <- "Import"
.heatMapSetFeatures <- "Set"
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

# Point colouring parameters. ----
.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByRowDataTitle <- "Row data"
.colorByFeatNameTitle <- "Feature name"

.colorByField <- "ColorBy"
.colorByDefaultColor <- "ColorByDefaultColor"
.colorByColData <- "ColorByColData"
.colorByRowData <- "ColorByRowData"

.colorByFeatName <- "ColorByFeatName"
.colorByRowTable <- "ColorByRowTable"
.colorByFeatNameAssay <- "ColorByFeatNameAssay"
.colorByFeatNameColor <- "ColorByFeatNameColor"

# Point selection parameters. ----
.selectParamBoxOpen <- "SelectBoxOpen"

.selectByPlot <- "SelectByPlot"
.selectEffect <- "SelectEffect"
.selectRestrictTitle <- "Restrict"
.selectColorTitle <- "Color"
.selectTransTitle <- "Transparent"

.selectColor <- "SelectColor"
.selectTransAlpha <- "SelectAlpha"

.noSelection <- "---"

# Zooming parameters. ----
.zoomData <- "ZoomData"
.zoomClick <- "ZoomClick"

# Lasso parameters. ----
.lassoClick <- "LassoClick"
.lassoData <- "LassoData"

# Brush parameters. ----
.brushField <- "Brush"
.brushData <- "BrushData"


# Other plot parameters. ----
.dataParamBoxOpen <- "DataBoxOpen"
.visualParamBoxOpen <- "VisualBoxOpen"
.visualParamChoice <- "VisualChoices"

.visualParamChoiceColorTitle <- "Color"
.visualParamChoicePointTitle <- "Points"
.visualParamChoiceOtherTitle <- "Other"

.plotPointSize <- "PointSize"
.plotPointAlpha <- "PointAlpha"
.plotFontSize <- "FontSize"
.plotLegendPosition <- "LegendPosition"
.plotLegendRightTitle <- "Right"
.plotLegendBottomTitle <- "Bottom"

.plotFontSizeAxisTextDefault <- 10
.plotFontSizeAxisTitleDefault <- 12

# Row statistic table parameters. ----
.rowStatSelected <- "Selected"
.rowStatSearch <- "Search"
.rowStatColSearch <- "SearchColumns"
.int_rowStatSelected <- "_rows_selected"
.int_rowStatSearch <- "_search"
.int_rowStatColSearch <- "_search_columns"

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

# Encoding and decoding names for user/shiny ----
translation <- c(redDimPlot="Reduced dimension plot",
                 colDataPlot="Column data plot",
                 featExprPlot="Feature expression plot",
                 rowStatTable="Row statistics table",
                 rowDataPlot="Row data plot",
                 heatMapPlot="Heat map")
rev.translation <- names(translation)
names(rev.translation) <- translation

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
    paste(translation[mode], id)
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
    id <- as.integer(gsub(".* ([0-9]+)$", "\\1", names))
    raw.str <- rev.translation[gsub(" [0-9]+$", "", names)]
    failed <- is.na(raw.str) | is.na(id)
    if (any(failed)) {
        stop(sprintf("'%s' is not a legal panel name", names[failed][1]))
    }
    return(list(Type=raw.str, ID=id))
}

#' @rdname INTERNAL_encode_panel_name
.decoded2encoded <- function(names) {
    x <- .encode_panel_name(names)
    sprintf("%s%i", x$Type, x$ID)
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

