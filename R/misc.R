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
                          redDimMax, colDataMax, geneExprMax, geneStatMax) 
# This function sets up the memory for the current session, taking in any
# specifications from the user regarding the defaults and max number of panels.
{
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

width_limits <- c(2L, 12L)
height_limits <- c(400L, 1000L)

.setup_initial <- function(initialPanels, memory) 
# This function sets up the initial active panels.
{
  if (is.null(initialPanels)) {
    initialPanels <- data.frame(Name=c("Reduced dimension plot 1", "Column data plot 1", 
                                       "Gene expression plot 1", "Gene statistics table 1"),
                                Width=4, Height=500L, stringsAsFactors=FALSE)
  } 

  if (is.null(initialPanels$Name)) {
    stop("need 'Name' field in 'initialPanels'")
  }

  if (is.null(initialPanels$Width)) {
    initialPanels$Width <- 4L
  } else {
    initialPanels$Width <- pmax(width_limits[1], pmin(width_limits[2], as.integer(initialPanels$Width)))
  }

  if (is.null(initialPanels$Height)) {
    initialPanels$Height <- 500L
  } else {
    initialPanels$Height <- pmax(height_limits[1], pmin(height_limits[2], as.integer(initialPanels$Height)))
  }

  encoded <- .encode_panel_name(initialPanels$Name)
  max_each <- unlist(lapply(memory, nrow))
  illegal <- max_each[encoded$Type] < encoded$ID
  if (any(illegal)) {
    badpanel <- which(illegal)[1]
    message(sprintf("'%s' in 'initialPanels' is not available (maximum ID is %i)",
                    initialPanels$Name[badpanel], max_each[encoded$Type[badpanel]]))
  }

  data.frame(Type=encoded$Type, ID=encoded$ID,
             Width=initialPanels$Width,
             Height=initialPanels$Height,
             stringsAsFactors=FALSE)[!illegal,,drop=FALSE]
}

.sanitize_memory <- function(active_panels, memory) 
# This function ensures that the memory is valid
# with respect to the starting panels, i.e., no brushing
# or table links to panels that are not active.
{
    is_tab <- active_panels$Type=="geneStat"
    brushable <- active_panels[!is_tab,]
    brush_names <- .decode_panel_name(brushable$Type, brushable$ID)
    linkable <- active_panels[is_tab,]
    link_names <- .decode_panel_name(linkable$Type, linkable$ID)

    for (mode in c("redDim", "colData", "geneExpr")) {
        bb <- memory[[mode]][,.brushByPlot]
        bad <- bb %in% brush_names
        if (any(bad)) { 
            memory[[mode]][,.brushByPlot][bb] <- ""
        }

        cb <- memory[[mode]][,.colorByGeneTable]
        bad <- cb %in% link_names 
        if (any(bad)) { 
            memory[[mode]][,.colorByGeneTable][cb] <- ""
        }
    }

    for (field in c(.geneExprXAxisGeneTable, .geneExprYAxisGeneTable)) {
        bb <- memory$geneExpr[,field]
        bad <- bb %in% link_names 
        if (any(bad)) { 
            memory$geneExpr[,field][bb] <- ""
        }
    }
    return(memory)
}

.setup_table_observer <- function(mode, i, input, pObjects, by_field, tab_title, tab_field, param='color') 
# Convenience function to update table links and memory.    
{
    choice <- input[[paste0(mode, by_field, i)]]
    tab <- input[[paste0(mode, tab_field, i)]]
    reset <- FALSE

    if (!is.null(choice) && !is.null(tab)) { 
        # Editing the table_links, if we're switching to/from the table choice. 
        old <- pObjects$memory[[mode]][i, tab_field]
        plot_name <- paste0(mode, "Plot", i)
        if (choice==tab_title) {
            pObjects$table_links <- .modify_table_links(pObjects$table_links, plot_name, tab, old, mode=param)
        } else {
            pObjects$table_links <- .modify_table_links(pObjects$table_links, plot_name, "", old, mode=param)
        }

        # Triggering replotting, but only if both of the input values are initialized.
        # We don't have an 'ignoreInit' that we can rely on here.
        reset <- TRUE
    }
    
    # Updating stored parameters. These should persist due to environment's pass-by-reference.
    if (!is.null(choice)) { 
        pObjects$memory[[mode]][i, by_field] <- choice
    }
    if (!is.null(tab)) { 
        pObjects$memory[[mode]][i, tab_field] <- tab 
    }
    return(reset)
}

.identical_brushes <- function(old_brush, new_brush)
# Check whether the brush coordinates have actually changed. 
{
    old_null <- is.null(old_brush) 
    new_null <- is.null(new_brush)
    if (old_null || new_null) {
        return(old_null==new_null)
    }

    xspan <- old_brush$xmax - old_brush$xmin
    tol <- xspan * 1e-6
    if (abs(old_brush$xmin - new_brush$xmin) > tol 
        || abs(old_brush$xmax - new_brush$xmax) > tol) {
      return(FALSE)        
    }

    yspan <- old_brush$ymax - old_brush$ymin
    tol <- yspan * 1e-6
    if (abs(old_brush$ymin - new_brush$ymin) > tol 
        || abs(old_brush$ymax - new_brush$ymax) > tol) {
      return(FALSE)        
    }

    return(TRUE)
}

#############################################
# Information constants.

iSEE_info <- function() {
    tagList(
    HTML(sprintf("iSEE is a project developed by 
Aaron Lun (%s),
Charlotte Soneson (%s),
Kevin Rue-Albrecht (%s),
and Federico Marini (%s).", 
a(href="http://www.cambridgecancer.org.uk/", "CRUK Cambridge Institute, University of Cambridge"),
a(href="https://www.sib.swiss/", "University of Zurich and SIB Swiss Institute of Bioinformatics"),
a(href="https://www.kennedy.ox.ac.uk", "Kennedy Institute of Rheumatology, University of Oxford"),
a(href="http://www.unimedizin-mainz.de/imbei","Institute for Medical Biostatistics, Epidemiology and Informatics"))),
    br(), br(),
    HTML(sprintf("The iSEE package is being developed on %s under the %s license.",
         a(href="https://github.com/csoneson/iSEE", "GitHub"),
         a(href="https://opensource.org/licenses/MIT","MIT")))
    )
}
