#' @rdname tourTCGA 
#' @export
tourTCGA_preprocess <- function(){
  stopifnot(
    require(ExperimentHub),
    require(SingleCellExperiment),
    requireNamespace("irlba"),
    requireNamespace("Rtsne"),
    requireNamespace("scater"),
    require(shiny)
  )
  # Vignette demo
  ehub <- ExperimentHub()
  eh1 <- ehub[["EH1"]] # an ExpressionSet
  se1 <- as(eh1, "SummarizedExperiment")
  sce <- as(se1, "SingleCellExperiment")
  sce <- scater::runPCA(sce, exprs_values = "exprs")
  irlba_out <- irlba::irlba(assay(sce, "exprs"))
  tsne_out <- Rtsne::Rtsne(
    irlba_out$v, pca = FALSE, perplexity = 50, verbose = TRUE)
  reducedDim(sce, "TSNE") <- tsne_out$Y
  # library size and CPM
  colData(sce)[,"lib_size"] <- colSums(assay(sce, "exprs"))
  assay(sce, "log2CPM") <-
    log2(1 + t(t(assay(sce, "exprs")) / colData(sce)[,"lib_size"]))
  return(sce)
}

#' Tour of The Cancer Genome Atlas RNA-sequencing data set
#'
#' @name tourTCGA
#' @param sce \code{SingleCellExperiment} object obtained from
#' \code{tourTCGA_preprocess}
#'
#' @return An instance of Shiny app that runs the tour.
#' @export
#'
#' @examples
#' sce_tour <- tourTCGA_preprocess()
#' app <- tourTCGA(sce_tour)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234)
#' }
tourTCGA <- function(sce){
  stopifnot(
    require(shiny)
  )
  # Panel 1: colData (phenotype selection)
  # Y = CancerType
  # X = Gender
  # select breast cancer female patients
  
  cd <- colDataPlotDefaults(sce, 2)
  # data
  cd$DataBoxOpen <- TRUE
  cd$YAxis <- "CancerType"
  cd$XAxis <- "Column data"
  cd$XAxisColData <- "gender"
  # visual
  cd$VisualBoxOpen <- TRUE
  cd$ColorBy <- "Column data"
  cd$ColorByColData <- "CancerType"
  cd$VisualChoices[[1]] <- c("Color", "Points", "Other")
  cd$PointAlpha <- 0.25
  cd$Downsample <- TRUE
  cd$SampleRes <- 100
  cd$LegendPosition <- "Right"
  # point selection
  cd$BrushData <- list(
    # (female BRCA patients)
    list(
      xmin = 0.45, xmax = 1.55, ymin = 1.45, ymax = 2.55, mapping = list(x = "X", y = "Y"), 
      domain = list(left = 0.4, right = 2.6, bottom = 0.4, top = 20.6), range = list(
          left = 55.8972468964041, right = 253.520547945205, bottom = 432.984589041096, 
          top = 24.0290131340161), log = list(x = NULL, y = NULL), direction = "xy", 
      brushId = "colDataPlot1_Brush", outputId = "colDataPlot1"),
    # (male BRCA patients)
    list(
      xmin = 0.45, xmax = 1.55, ymin = 1.45, ymax = 2.55, mapping = list(x = "X", y = "Y"), 
      domain = list(left = 0.4, right = 2.6, bottom = 0.4, top = 20.6), range = list(
          left = 55.8972468964041, right = 253.520547945205, bottom = 432.984589041096, 
          top = 24.0290131340161), log = list(x = NULL, y = NULL), direction = "xy", 
      brushId = "colDataPlot2_Brush", outputId = "colDataPlot2")
  )
  
  # Panel 2: reduce dimensions (overview)
  # selection: tSNE
  # color: colData > CancerType
  # downsample for speed (tSNE): 100
  # panel width: 5
  # legend position: right
  rd <- redDimPlotDefaults(sce, 2)
  # data
  rd$Type <- "TSNE"
  # visual
  rd$VisualBoxOpen <- TRUE
  rd$VisualChoices[[1]] <- c("Color", "Points", "Other")
  rd$ColorBy <- "Column data"
  rd$ColorByColData <- "CancerType"
  rd$Downsample <- TRUE
  rd$SampleRes <- 100
  rd$LegendPosition <- "Right"
  # select
  rd$SelectBoxOpen <- TRUE
  rd$SelectByPlot <- c("Column data plot 1", "Column data plot 2")
  
  # Panel 3: feature expression (analysis)
  fe <- featExprPlotDefaults(sce, 2)
  # data
  fe$DataBoxOpen <- TRUE
  fe$Assay <- 2
  fe$XAxis <- "Column data"
  fe$XAxisColData <- "CancerType"
  fe$YAxisFeatName <- match("ERBB2", rownames(sce))
  # visual
  fe$VisualBoxOpen <- TRUE
  fe$VisualChoices[[1]] <- c("Points")
  fe$Downsample <- TRUE
  fe$SampleRes <- 100
  # select
  fe$SelectBoxOpen <- TRUE
  fe$SelectByPlot <- c("Column data plot 1", "Column data plot 2")
  fe$SelectEffect <- "Color"
  
  # "monocytes_count" # could be interesting
  
  initialPanels = DataFrame(
      Name = c(
        "Column data plot 1",
        "Reduced dimension plot 1",
        "Feature expression plot 1"
      ),
      Width = c(
        3, # colData
        4, # reducedDim
        5 # featExprs
      )
    )
  
  tour <- read.delim(
    system.file("extdata", "demo_TCGA.txt", package = "iSEE"),
    sep=";", stringsAsFactors = FALSE,row.names = NULL)
  
  app <- iSEE(
    sce, tour = tour, 
    redDimArgs = rd, colDataArgs = cd, featExprArgs = fe,
    rowStatArgs = NULL, rowDataArgs = NULL, heatMapArgs = NULL,
    redDimMax = 1, colDataMax = 1, featExprMax = 1,
    rowStatMax = 1, rowDataMax = 1, heatMapMax = 1,
    initialPanels = initialPanels,
    appTitle = "TCGA RNA-seq tour")
  
  return(app)
}
