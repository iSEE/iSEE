.onLoad <- function(...) {
    # Create link to javascript files.
    shiny::addResourcePath("iSEE", system.file("www", package="iSEE"))
}

#############################################
# Functions for iSEE, moved to a separate file for clarity.

.update_list_element <- function(memory, ID, field, value) {
    out <- memory[[field]]
    out[ID] <- list(value)
    memory[[field]] <- out
    return(memory)
}

.check_plot_feasibility <- function(se) {
  return(list(redDim=! (length(reducedDims(se))==0L || ncol(se)==0L),
              colData=! (ncol(colData(se))==0L || ncol(se)==0L),
              geneExpr=! (nrow(se)==0L || ncol(se)==0L || length(assayNames(se))==0L),
              geneStat=! (nrow(se)==0L)
  ))
}

.increment_counter <- function(counter, max=10000L) {
  counter <- counter + 1L
  if (counter >= max) {
    counter <- 0L
  }
  return(counter)
}

.setup_memory <- function(se, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
                          redDimMax, colDataMax, geneExprMax, geneStatMax) {
  # Defining the maximum number of panels.
  reddim_max_plots <- max(nrow(redDimArgs), redDimMax)
  coldata_max_plots <- max(nrow(colDataArgs), colDataMax)
  geneexpr_max_plots <- max(nrow(geneExprArgs), geneExprMax)
  genestat_max_tabs <- max(nrow(geneStatArgs), geneStatMax)

  feasibility <- .check_plot_feasibility(se)
  if (!feasibility$redDim) { 
    reddim_max_plots <- 0L
    redDimArgs <- NULL
  } 
  if (!feasibility$colData) {
    coldata_max_plots <- 0L
    colDataArgs <- NULL
  }
  if (!feasibility$geneExpr) {
    geneexpr_max_plots <- 0L
    geneExprArgs <- NULL
  }
  if (!feasibility$geneStat) {
    genestat_max_tabs <- 0L
    geneStatArgs <- NULL
  }

  # Setting up parameters for each panel.
  memory <- list()
  if (is.null(redDimArgs)) {
    memory$redDim <- redDimPlotDefaults(se, reddim_max_plots)
  } else {
    memory$redDim <- redDimPlotDefaults(se, reddim_max_plots)
    memory$redDim <- .override_defaults(memory$redDim, redDimArgs)
  }
  rownames(memory$redDim) <- sprintf("redDimPlot%i", seq_len(reddim_max_plots))

  if (is.null(geneExprArgs)) {
    memory$geneExpr <- geneExprPlotDefaults(se, geneexpr_max_plots)
  } else {
    memory$geneExpr <- geneExprPlotDefaults(se, geneexpr_max_plots)
    memory$geneExpr <- .override_defaults(memory$geneExpr, geneExprArgs)
  }
  rownames(memory$geneExpr) <- sprintf("geneExprPlot%i", seq_len(geneexpr_max_plots))

  if (is.null(colDataArgs)) {
    memory$colData <- colDataPlotDefaults(se, coldata_max_plots)
  } else {
    memory$colData <- colDataPlotDefaults(se, coldata_max_plots)
    memory$colData <- .override_defaults(memory$colData, colDataArgs)
  }
  rownames(memory$colData) <- sprintf("colDataPlot%i", seq_len(coldata_max_plots))

  if (is.null(geneStatArgs)) {
    memory$geneStat <- geneStatTableDefaults(se, genestat_max_tabs)
  } else {
    memory$geneStat <- geneStatTableDefaults(se, genestat_max_tabs)
    memory$geneStat <- .override_defaults(memory$geneStat, geneStatArgs, can_brush=FALSE)
  }
  rownames(memory$geneStat) <- sprintf("geneStatTable%i", seq_len(genestat_max_tabs))

  return(memory)
}

#############################################
# 

iSEE_footer <- function(){
  tags$div(
    class = "panel-footer",
    style = "text-align:center",
    tags$div(
      class = "foot-inner",
      list(
        # hr(),
        "iSEE is a project developed by
        Aaron Lun (CRUK Cambridge Institute, University of Cambridge),
        Charlotte Soneson (University of Zurich and SIB Swiss Institute of Bioinformatics),
        Kevin Rue-Albrecht (",
        tags$a(
            href="https://www.kennedy.ox.ac.uk",
            "Kennedy Institute of Rheumatology, University of Oxford"
        ),
        "), and Federico Marini in the Bioinformatics division of the ",
        tags$a(href="http://www.unimedizin-mainz.de/imbei","IMBEI"),
        "- Institute for Medical Biostatistics, Epidemiology and Informatics",br(),
        "License: ",tags$a(href="https://opensource.org/licenses/MIT","MIT"), br(),

        "Development of the iSEE package is on ",
        tags$a(href="https://github.com/csoneson/iSEE", "GitHub")
      )
    )
  )
}
