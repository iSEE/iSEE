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

.geneExprID <- "Gene"
.geneExprAssay <- "Assay"
.geneExprXAxis <- "XAxis"
.geneExprXAxisColData <- "XAxisColData"
.geneExprXAxisGeneExprs <- "XAxisGeneExprs"

.inputGeneExpr <- function(field, i) paste0("geneExpr", field, i)
.geneExprPlot <- function(i) paste0("geneExprPlot", i)

# Gene expression plotting parameters.
.phenoDataXAxisNothingTitle <- "None"
.phenoDataXAxisColDataTitle <- "Column data"

.phenoDataYAxisColData <- "YAxisColData"
.phenoDataXAxis <- "XAxis"
.phenoDataXAxisColData <- "XAxisColData"

.inputPhenoData <- function(field, i) paste0("phenoData", field, i)
.phenoDataPlot <- function(i) paste0("phenoDataPlot", i)

# General plot parameters.

.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByGeneExprsTitle <- "Gene expression"

.generalPlotParamPanelTitle <- "Advanced plot parameters"

.generalColorBy <- "ColorBy"
.generalColorByColData <- "ColorByColData"
.generalColorByGeneExprs <- "ColorByGeneTable"
.generalColorByGeneExprsAssay <- "ColorByGeneAssay"
.generalPlotPanel <- "OpenPlotPanel"

.brushParamPanelTitle <- "Brushing parameters"

.brushField <- "Brush"
.brushByPlot <- "BrushByPlot"

.organizationNew <- "MakeNew"
.organizationUp <- "ShiftUp"
.organizationDown <- "ShiftDown"
.organizationDiscard <- "Discard"
.organizationWidth <- "PanelWidth"

# Encoding and decoding names for user/shiny

translation <- c(redDim="Reduced dimension plot",
                 phenoData="Column data plot",
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
    return(list(Type=raw.str, ID=ID))
}


