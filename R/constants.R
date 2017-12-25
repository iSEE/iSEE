# Reduced dimension plotting parameters.
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

.inputRedDim <- function(field, i) paste0("redDim", field, i)
.redDimPlot <- function(i) paste0("redDimPlot", i)

# Gene expression plotting parameters.
.geneExprXAxisNothingTitle <- "None"
.geneExprXAxisColDataTitle <- "Column data"
.geneExprXAxisGeneExprsTitle <- "Gene expression"

.geneExprID <- "GeneTable"
.geneExprAssay <- "Assay"
.geneExprXAxis <- "XAxis"
.geneExprXAxisColData <- "XAxisColData"
.geneExprXAxisGeneExprs <- "XAxisGeneExprs"

.inputGeneExpr <- function(field, i) paste0("geneExpr", field, i)
.geneExprPlot <- function(i) paste0("geneExprPlot", i)

# Gene expression plotting parameters.
.colDataXAxisNothingTitle <- "None"
.colDataXAxisColDataTitle <- "Column data"

.colDataYAxis <- "YAxis"
.colDataXAxis <- "XAxis"
.colDataXAxisColData <- "XAxisColData"

.inputColData <- function(field, i) paste0("colData", field, i)
.colDataPlot <- function(i) paste0("colDataPlot", i)

# Plot colouring parameters.
.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByGeneExprsTitle <- "Gene expression"

.colorParamPanelTitle <- "Coloring parameters"
.colorParamPanelOpen <- "ColorPanelOpen"

.colorByField <- "ColorBy"
.colorByColData <- "ColorByColData"
.colorByGeneExprs <- "ColorByGeneTable"
.colorByGeneExprsAssay <- "ColorByGeneAssay"

# Plot brushing parameters.
.brushParamPanelTitle <- "Brushing parameters"
.brushParamPanelOpen <- "BrushPanelOpen"

.brushField <- "Brush"
.brushActive <- "BrushOn"
.brushByPlot <- "BrushByPlot"

# Other parameter panel constants.
.plotParamPanelTitle <- "Plotting parameters"
.plotParamPanelOpen <- "PlotPanelOpen"
.plotParamPanelName <- "ParamPanel"

# Panel organization parameters.
.organizationNew <- "MakeNew"
.organizationUp <- "ShiftUp"
.organizationDown <- "ShiftDown"
.organizationDiscard <- "Discard"
.organizationWidth <- "PanelWidth"

# Encoding and decoding names for user/shiny
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


