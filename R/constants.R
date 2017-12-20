.colorByNothingTitle <- "None"
.colorByColDataTitle <- "Column data"
.colorByGeneExprsTitle <- "Gene expression"

# Reduced dimension plotting parameters.
.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"
.redDimPlotPanel <- "OpenPlotPanel"
.redDimColorBy <- "ColorBy"
.redDimColorByColData <- "ColorByColData"
.redDimColorByGeneExprs <- "ColorByGeneExprs"
.redDimColorByGeneExprsAssay <- "ColorByGeneExprsAssay"

.inputRedDim <- function(field, i) paste0("redDim", field, i)
.redDimPlot <- function(i) paste0("redDimPlot", i)
.redDimDiscard <- function(i) paste0("redDimDiscard", i)

.redDimPlotParamPanelTitle <- "Advanced plot parameters"

# Gene expression plotting parameters.
.geneExprXAxisNothingTitle <- "None"
.geneExprXAxisColDataTitle <- "Column data"
.geneExprXAxisGeneExprsTitle <- "Gene expression"

.geneExprID <- "Gene"
.geneExprAssay <- "Assay"
.geneExprXAxis <- "XAxis"
.geneExprXAxisColData <- "XAxisColData"
.geneExprXAxisGeneExprs <- "XAxisGeneExprs"

.geneExprColorBy <- "ColorBy"
.geneExprColorByColData <- "ColorByColData"
.geneExprColorByGeneExprs <- "ColorByGeneExprs"
.geneExprPlotPanel <- "OpenPlotPanel"

.inputGeneExpr <- function(field, i) paste0("geneExpr", field, i)
.geneExprPlot <- function(i) paste0("geneExprPlot", i)
.geneExprDiscard <- function(i) paste0("geneExprDiscard", i)

.geneExprPlotParamPanelTitle <- "Advanced plot parameters"

# Gene expression plotting parameters.
.phenoDataXAxisNothingTitle <- "None"
.phenoDataXAxisColDataTitle <- "Column data"

.phenoDataYAxisColData <- "YAxisColData"
.phenoDataXAxis <- "XAxis"
.phenoDataXAxisColData <- "XAxisColData"

.phenoDataColorBy <- "ColorBy"
.phenoDataColorByColData <- "ColorByColData"
.phenoDataColorByGeneExprs <- "ColorByGeneExprs"
.phenoDataColorByGeneExprsAssay <- "ColorByGeneExprsAssay"
.phenoDataPlotPanel <- "OpenPlotPanel"

.inputPhenoData <- function(field, i) paste0("phenoData", field, i)
.phenoDataPlot <- function(i) paste0("phenoDataPlot", i)
.phenoDataDiscard <- function(i) paste0("phenoDataDiscard", i)

.phenoDataPlotParamPanelTitle <- "Advanced plot parameters"
