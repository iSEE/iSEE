.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByGeneExprsTitle <- "Gene expression"

# Reduced dimension plotting parameters.
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

.inputRedDim <- function(field, i) paste0("redDim", field, i)
.redDimPlot <- function(i) paste0("redDimPlot", i)
.redDimDiscard <- function(i) paste0("redDimDiscard", i)

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
.geneExprDiscard <- function(i) paste0("geneExprDiscard", i)

# Gene expression plotting parameters.
.phenoDataXAxisNothingTitle <- "None"
.phenoDataXAxisColDataTitle <- "Column data"

.phenoDataYAxisColData <- "YAxisColData"
.phenoDataXAxis <- "XAxis"
.phenoDataXAxisColData <- "XAxisColData"

.inputPhenoData <- function(field, i) paste0("phenoData", field, i)
.phenoDataPlot <- function(i) paste0("phenoDataPlot", i)
.phenoDataDiscard <- function(i) paste0("phenoDataDiscard", i)

# General plot parameters.

.generalPlotParamPanelTitle <- "Advanced plot parameters"

.generalColorBy <- "ColorBy"
.generalColorByColData <- "ColorByColData"
.generalColorByGeneExprs <- "ColorByGeneTable"
.generalColorByGeneExprsAssay <- "ColorByGeneAssay"
.generalPlotPanel <- "OpenPlotPanel"


