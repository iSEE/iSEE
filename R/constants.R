# Reduced dimension plotting parameters. ----
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

# Gene expression plotting parameters. ----
.geneExprXAxisNothingTitle <- "None"
.geneExprXAxisColDataTitle <- "Column data"
.geneExprXAxisGeneTableTitle <- "Gene table"
.geneExprXAxisGeneTextTitle <- "Gene text"

.geneExprYAxisGeneTableTitle <- "Gene table"
.geneExprYAxisGeneTextTitle <- "Gene text"

.geneExprAssay <- "Assay"
.geneExprXAxis <- "XAxis"
.geneExprXAxisColData <- "XAxisColData"
.geneExprXAxisGeneTable <- "XAxisGeneTable"
.geneExprXAxisGeneText <- "XAxisGeneText"
.geneExprYAxisGeneTable <- "YAxisGeneTable"
.geneExprYAxisGeneText <- "YAxisGeneText"
.geneExprYAxis <- "YAxis"

# Column data plotting parameters. ----
.colDataXAxisNothingTitle <- "None"
.colDataXAxisColDataTitle <- "Column data"

.colDataYAxis <- "YAxis"
.colDataXAxis <- "XAxis"
.colDataXAxisColData <- "XAxisColData"

# Plot colouring parameters. ----
.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByGeneTableTitle <- "Gene table"
.colorByGeneTextTitle <- "Gene text"

.colorParamPanelOpen <- "ColorPanelOpen"

.colorByField <- "ColorBy"
.colorByColData <- "ColorByColData"
.colorByGeneTable <- "ColorByGeneTable"
.colorByGeneText <- "ColorByGeneText"
.colorByGeneTableAssay <- "ColorByGeneTableAssay"
.colorByGeneTextAssay <- "ColorByGeneTextAssay"

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

# Zooming parameters. ----
.zoomData <- "ZoomData"
.zoomClick <- "ZoomClick"

# Plot parameters. ----
.plotParamPanelOpen <- "PlotPanelOpen"
.plotParamPanelName <- "ParamPanel"

# Gene statistic table parameters. ----
.geneStatSelected <- "Selected"
.geneStatSearch <- "Search"
.geneStatColSearch <- "SearchColumns"
.int_geneStatSelected <- "_rows_selected"
.int_geneStatSearch <- "_search"
.int_geneStatColSearch <- "_search_columns"

# Panel organization parameters. ----
.organizationNew <- "MakeNew"
.organizationUp <- "ShiftUp"
.organizationDown <- "ShiftDown"
.organizationDiscard <- "Discard"
.organizationModify <- "Modify"
.organizationWidth <- "PanelWidth"
.organizationHeight <- "PanelHeight"

# Encoding and decoding names for user/shiny ----
translation <- c(redDim="Reduced dimension plot",
                 colData="Column data plot",
                 geneExpr="Gene expression plot",
                 geneStat="Gene statistics table")
rev.translation <- names(translation)
names(rev.translation) <- translation

.decode_panel_name <- function(mode, ID) {
    paste(translation[mode], ID)
}

.encode_panel_name <- function(names) {
    ID <- as.integer(gsub(".* ", "", names))
    raw.str <- rev.translation[gsub(" [0-9]+", "", names)]
    failed <- is.na(raw.str) | is.na(ID)
    if (any(failed)) {
        stop(sprintf("'%s' is not a legal panel name", names[failed][1]))
    }
    return(list(Type=raw.str, ID=ID))
}

.decoded2encoded <- function(names) {
    keep <- names!=""
    x <- .encode_panel_name(names[keep])
    Mode <- ifelse(x$Type=="geneStat", "Table", "Plot")
    names[keep] <- sprintf("%s%s%i", x$Type, Mode, x$ID)
    names
}

.split_encoded <- function(names) {
  if (length(names)==0) {
    return(list(Type=character(0), ID=integer(0)))
  }
  sp <- strsplit(names, "Plot|Table")
  sp <- do.call(rbind, sp)
  return(list(Type=sp[,1], ID=as.integer(sp[,2])))
}

.plothexcode_redDim <- "#3C8DBC"
.plothexcode_colData <- "#F39D12"
.plothexcode_geneExpr <- "#03A659"
.plothexcode_geneTable <- "#DD4B39"
