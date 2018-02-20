# Reduced dimension plotting parameters. ----
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

# Feature expression plotting parameters. ----
.featExprXAxisNothingTitle <- "None"
.featExprXAxisColDataTitle <- "Column data"
.featExprXAxisRowTableTitle <- "Row table"
.featExprXAxisFeatNameTitle <- "Feature name"

.featExprYAxisRowTableTitle <- "Row table"
.featExprYAxisFeatNameTitle <- "Feature name"

.featExprAssay <- "Assay"
.featExprXAxis <- "XAxis"
.featExprXAxisColData <- "XAxisColData"
.featExprXAxisRowTable <- "XAxisRowTable"
.featExprXAxisFeatName <- "XAxisFeatName"
.featExprYAxisRowTable <- "YAxisRowTable"
.featExprYAxisFeatName <- "YAxisFeatName"
.featExprYAxis <- "YAxis"

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
.heatMapFeatNamePanelOpen <- "FeatNamePanelOpen"
.heatMapImport <- "Import"
.heatMapImportSource <- "FeatNameSource"
.heatMapCluster <- "Clustered"

.heatMapColData <- "ColData"
.heatMapColDataPanelOpen <- "ColDataPanelOpen"
.heatMapLegend <- "Legend"

.heatMapColorPanelOpen <- "ColorPanelOpen"
.heatMapCentering <- "Centering"
.heatMapScaling <- "Scaling"
.heatMapYesTitle <- "On"
.heatMapNoTitle <- "Off"

.heatMapLower <- "Lower"
.heatMapUpper <- "Upper"
.heatMapCenteredColors <- "ColorScale"

# Plot colouring parameters. ----
.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByRowDataTitle <- "Row data"
.colorByRowTableTitle <- "Row table"
.colorByFeatNameTitle <- "Feature name"

.colorParamPanelOpen <- "ColorPanelOpen"

.colorByField <- "ColorBy"
.colorByColData <- "ColorByColData"
.colorByRowData <- "ColorByRowData"

.colorByRowTable <- "ColorByRowTable"
.colorByRowTableAssay <- "ColorByRowTableAssay"
.colorByRowTableColor <- "ColorByRowTableColor"

.colorByFeatName <- "ColorByFeatName"
.colorByFeatNameAssay <- "ColorByFeatNameAssay"
.colorByFeatNameColor <- "ColorByFeatNameColor"

# Plot brushing parameters. ----
.brushParamPanelOpen <- "BrushPanelOpen"

.brushField <- "Brush"
.brushByPlot <- "BrushByPlot"
.brushData <- "BrushData"

.brushEffect <- "BrushEffect"
.brushRestrictTitle <- "Restrict"
.brushColorTitle <- "Color"
.brushTransTitle <- "Transparent"

.brushColor <- "BrushColor"
.brushTransAlpha <- "BrushAlpha"

.noSelection <- "---"

# Zooming parameters. ----
.zoomData <- "ZoomData"
.zoomClick <- "ZoomClick"

# Lasso parameters. ----
.lassoClick <- "LassoClick"
.lassoData <- "LassoData"

# Plot parameters. ----
.plotParamPanelOpen <- "PlotPanelOpen"
.plotParamPanelName <- "ParamPanel"

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
#' @param ID Integer vector specifying the panel IDs of the given type.
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
.decode_panel_name <- function(mode, ID) {
    paste(translation[mode], ID)
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
#' This will \emph{not} raise errors upon encountering an empty string, which are simply returned without modification.
#' Such behaviour is useful when dealing with empty selections for table choices.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_encode_panel_name
#' @seealso
#' \code{\link{.decode_panel_name}}
.encode_panel_name <- function(names) {
    ID <- as.integer(gsub(".* ([0-9]+)$", "\\1", names))
    raw.str <- rev.translation[gsub(" [0-9]+$", "", names)]
    failed <- is.na(raw.str) | is.na(ID)
    if (any(failed)) {
        stop(sprintf("'%s' is not a legal panel name", names[failed][1]))
    }
    return(list(Type=raw.str, ID=ID))
}

#' @rdname INTERNAL_encode_panel_name
.decoded2encoded <- function(names) {
    keep <- names!=""
    x <- .encode_panel_name(names[keep])
    names[keep] <- sprintf("%s%i", x$Type, x$ID)
    names
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
  ID <- as.integer(gsub(".*([0-9]+)$", "\\1", names))
  Type <- gsub("[0-9]+$", "", names)
  return(list(Type=Type, ID=ID))
}

