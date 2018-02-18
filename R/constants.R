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

.decode_panel_name <- function(mode, ID) {
    paste(translation[mode], ID)
}

.encode_panel_name <- function(names) {
    ID <- as.integer(gsub(".* ([0-9]+)$", "\\1", names))
    raw.str <- rev.translation[gsub(" [0-9]+$", "", names)]
    failed <- is.na(raw.str) | is.na(ID)
    if (any(failed)) {
        stop(sprintf("'%s' is not a legal panel name", names[failed][1]))
    }
    return(list(Type=raw.str, ID=ID))
}

.decoded2encoded <- function(names) {
    keep <- names!=""
    x <- .encode_panel_name(names[keep])
    names[keep] <- sprintf("%s%i", x$Type, x$ID)
    names
}

.split_encoded <- function(names) {
  ID <- as.integer(gsub(".*([0-9]+)$", "\\1", names))
  Type <- gsub("[0-9]+$", "", names)
  return(list(Type=Type, ID=ID))
}

