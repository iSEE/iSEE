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
  return(list(redDimPlot=! (length(reducedDims(se))==0L || ncol(se)==0L),
              colDataPlot=! (ncol(colData(se))==0L || ncol(se)==0L),
              featExprPlot=! (nrow(se)==0L || ncol(se)==0L || length(assayNames(se))==0L),
              rowStatTable=! (nrow(se)==0L),
              rowDataPlot=! (ncol(rowData(se))==0L || nrow(se)==0L),
              heatMapPlot=! (nrow(se)==0L || ncol(se)==0L || length(assayNames(se))==0L)
  ))
}

.increment_counter <- function(counter, max=10000L) {
  counter <- counter + 1L
  if (counter >= max) {
    counter <- 0L
  }
  return(counter)
}

.setup_memory <- function(se, redDimArgs, colDataArgs, featExprArgs, rowStatArgs, rowDataArgs, heatMapArgs,
                          redDimMax, colDataMax, featExprMax, rowStatMax, rowDataMax, heatMapMax) 
# This function sets up the memory for the current session, taking in any
# specifications from the user regarding the defaults and max number of panels.
{
  # Defining the maximum number of panels.
  reddim_max_plots <- max(nrow(redDimArgs), redDimMax)
  coldata_max_plots <- max(nrow(colDataArgs), colDataMax)
  geneexpr_max_plots <- max(nrow(featExprArgs), featExprMax)
  genestat_max_tabs <- max(nrow(rowStatArgs), rowStatMax)
  rowdata_max_plots <- max(nrow(rowDataArgs), rowDataMax)
  heatmap_max_plots <- max(nrow(heatMapArgs), heatMapMax)

  feasibility <- .check_plot_feasibility(se)
  if (!feasibility$redDimPlot) { 
    reddim_max_plots <- 0L
    redDimArgs <- NULL
  } 
  if (!feasibility$colDataPlot) {
    coldata_max_plots <- 0L
    colDataArgs <- NULL
  }
  if (!feasibility$featExprPlot) {
    geneexpr_max_plots <- 0L
    featExprArgs <- NULL
  }
  if (!feasibility$rowStatTable) {
    genestat_max_tabs <- 0L
    rowStatArgs <- NULL
  }
  if (!feasibility$rowDataPlot) {
    rowdata_max_plots <- 0L
    rowDataArgs <- NULL
  }
  if (!feasibility$heatMapPlot) {
    heatmap_max_plots <- 0L
    heatMapArgs <- NULL
  }

  # Setting up parameters for each panel.
  memory <- list()
  
  memory$redDimPlot <- redDimPlotDefaults(se, reddim_max_plots)
  if (!is.null(redDimArgs)) {
    memory$redDimPlot <- .override_defaults(memory$redDimPlot, redDimArgs)
  }
  rownames(memory$redDimPlot) <- sprintf("redDimPlot%i", seq_len(reddim_max_plots))

  memory$featExprPlot <- featExprPlotDefaults(se, geneexpr_max_plots)
  if (!is.null(featExprArgs)) {
    memory$featExprPlot <- .override_defaults(memory$featExprPlot, featExprArgs)
  }
  rownames(memory$featExprPlot) <- sprintf("featExprPlot%i", seq_len(geneexpr_max_plots))

  memory$colDataPlot <- colDataPlotDefaults(se, coldata_max_plots)
  if (!is.null(colDataArgs)) {
    memory$colDataPlot <- .override_defaults(memory$colDataPlot, colDataArgs)
  }
  rownames(memory$colDataPlot) <- sprintf("colDataPlot%i", seq_len(coldata_max_plots))

  memory$rowStatTable <- rowStatTableDefaults(se, genestat_max_tabs)
  if (!is.null(rowStatArgs)) {
    memory$rowStatTable <- .override_defaults(memory$rowStatTable, rowStatArgs)
  }
  rownames(memory$rowStatTable) <- sprintf("rowStatTable%i", seq_len(genestat_max_tabs))

  memory$rowDataPlot <- rowDataPlotDefaults(se, rowdata_max_plots)
  if (!is.null(rowDataArgs)) {
    memory$rowDataPlot <- .override_defaults(memory$rowDataPlot, rowDataArgs)
  }
  rownames(memory$rowDataPlot) <- sprintf("rowDataPlot%i", seq_len(rowdata_max_plots))

  memory$heatMapPlot <- heatMapPlotDefaults(se, heatmap_max_plots)
  if (!is.null(heatMapArgs)) {
    memory$heatMapPlot <- .override_defaults(memory$heatMapPlot, heatMapArgs)
  }
  rownames(memory$heatMapPlot) <- sprintf("heatMapPlot%i", seq_len(heatmap_max_plots))
  
  return(memory)
}

width_limits <- c(2L, 12L)
height_limits <- c(400L, 1000L)

.setup_initial <- function(initialPanels, memory) 
# This function sets up the initial active panels.
{
  if (is.null(initialPanels)) {
    initialPanels <- data.frame(Name=c("Reduced dimension plot 1", "Column data plot 1", 
                                       "Feature expression plot 1", "Row statistics table 1",
                                       "Row data plot 1", "Heat map 1"),
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
    link_sources <- .define_link_sources(active_panels)
    active_tab <- link_sources$tab
    row_brushable <- link_sources$row
    col_brushable <-  link_sources$col
    all_active <- paste0(active_panels$Type, active_panels$ID)

    # Checking for brushing/linking of column-based plots.
    for (mode in c("redDimPlot", "colDataPlot", "featExprPlot")) {
        cur_memory <- memory[[mode]]
        self_active <- rownames(cur_memory)

        bb <- cur_memory[,.brushByPlot]
        bad <- !bb %in% col_brushable | !self_active %in% all_active
        if (any(bad)) { 
            memory[[mode]][,.brushByPlot][bad] <- ""
        }

        cb <- cur_memory[,.colorByRowTable]
        bad <- !cb %in% active_tab | !self_active %in% all_active
        if (any(bad)) { 
            memory[[mode]][,.colorByRowTable][bad] <- ""
        }
    }

    # Checking for brushing/linking of row data plots.
    cur_memory <- memory$rowDataPlot
    self_active <- rownames(cur_memory)

    bb <- cur_memory[,.brushByPlot]
    bad <- !bb %in% row_brushable | !self_active %in% all_active
    if (any(bad)) { 
        memory$rowDataPlot[,.brushByPlot][bad] <- ""
    }

    cb <- cur_memory[,.colorByRowTable]
    bad <- !cb %in% active_tab | !self_active %in% all_active
    if (any(bad)) { 
        memory$rowDataPlot[,.colorByRowTable][bad] <- ""
    }

    # Checking for linking of x/y-axes of feature expression plots.
    feat_active <- rownames(memory$featExprPlot)
    for (field in c(.featExprXAxisRowTable, .featExprYAxisRowTable)) {
        bb <- memory$featExprPlot[,field]

        bad <- !bb %in% active_tab | !feat_active %in% all_active
        if (any(bad)) { 
            memory$featExprPlot[,field][bad] <- ""
        }
    }
    return(memory)
}

.define_plot_links <- function(panel, memory, graph) 
# This creates a description of all of the incoming/outgoing
# relationships between a plot panel and the other plots/tables.
{
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID,]
    output <- list()

    # Checking brush status.
    brush_in <- param_choices[[.brushByPlot]]
    if (brush_in!="") {
        output <- c(output, list("Receiving brush from", em(strong(brush_in)), br()))
    }

    # Checking colour status.
    if (param_choices[[.colorByField]]==.colorByRowTableTitle) {
        output <- c(output, list("Receiving color from", em(strong(param_choices[[.colorByRowTable]])), br()))
    }

    # Checking input/output for feature expression plots.
    if (enc$Type=="featExprPlot") {
        if (param_choices[[.featExprYAxis]]==.featExprYAxisRowTableTitle) {
            output <- c(output, list("Receiving y-axis from", em(strong(param_choices[[.featExprYAxisRowTable]])), br()))
        }
        if (param_choices[[.featExprXAxis]]==.featExprXAxisRowTableTitle) {
            output <- c(output, list("Receiving x-axis from", em(strong(param_choices[[.featExprXAxisRowTable]])), br()))
        }
    }

    # Defining immediate children.
    children <- names(adjacent_vertices(graph, panel)[[1]])
    child_enc <- .split_encoded(children)
    child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)
    for (child in child_names) {
        output <- c(output, list("Transmitting brush to", em(strong(child)), br()))
    }

    do.call(tagList, output)
}

.define_table_links <- function(panel, memory, table_links) 
# This creates a description of all of the incoming/outgoing
# relationships between a table panel and the other plots/tables.
{
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID,]
    output <- list()

    # Checking brush status.
    brush_in <- param_choices[[.brushByPlot]]
    if (brush_in!="") {
        output <- c(output, list("Receiving brush from", em(strong(brush_in)), br()))
    }

    # Checking where it broadcasts to plots. 
    current <- table_links[[panel]]
    for (trans in list(c("yaxis", "y-axis"),
                       c("xaxis", "x-axis"),
                       c("color", "color"))) {

        children <- current[[trans[1]]]
        child_enc <- .split_encoded(children)
        child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)

        for (child in child_names) {
            output <- c(output, list(paste("Transmitting", trans[2], "to"), em(strong(child)), br()))
        }
    }  

    do.call(tagList, output)
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
