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
    all_args <- list(redDimPlot=redDimArgs, colDataPlot=colDataArgs, featExprPlot=featExprArgs,
                     rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs, heatMapPlot=heatMapArgs)
    all_maxes <- list(redDimPlot=redDimMax, colDataPlot=colDataMax, featExprPlot=featExprMax,
                      rowStatTable=rowStatMax, rowDataPlot=rowDataMax, heatMapPlot=heatMapMax)
    feasibility <- .check_plot_feasibility(se)

    for (x in names(all_args)) { 
        if (!feasibility[[x]]) {
            all_args[x] <- list(NULL)
            all_maxes[[x]] <- 0L
        } else {
            all_maxes[[x]] <- max(all_maxes[[x]], nrow(all_args[[x]]))
        }
    }
  
    # Coercing string arguments that should be integers.
    all_args$redDimPlot <- .name2index(all_args$redDimPlot, .redDimType, reducedDimNames(se))
    
    all_args$featExprPlot <- .name2index(all_args$featExprPlot, c(.featExprXAxisFeatName, .featExprYAxisFeatName), rownames(se))
    all_args$featExprPlot <- .name2index(all_args$featExprPlot, .featExprAssay, assayNames(se))
    
    all_args$rowStatTable <- .name2index(all_args$rowStatTable, .rowStatSelected, rownames(se))
    
    all_args$heatMapPlot <- .name2index(all_args$heatMapPlot, .heatMapFeatName, rownames(se))
    all_args$heatMapPlot <- .name2index(all_args$heatMapPlot, .heatMapAssay, assayNames(se))

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot")) {
        all_args[[mode]] <- .name2index(all_args[[mode]], .colorByFeatName, rownames(se))  
        all_args[[mode]] <- .name2index(all_args[[mode]], c(.colorByFeatNameAssay, .colorByRowTableAssay), assayNames(se))  
    }
    all_args$rowDataPlot <- .name2index(all_args$rowDataPlot, .colorByFeatName, rownames(se))  

    # Setting up parameters for each panel. 
    memory <- list()
    for (mode in names(all_maxes)) { 
        DEFFUN <- switch(mode,
                         redDimPlot=redDimPlotDefaults,
                         featExprPlot=featExprPlotDefaults,
                         colDataPlot=colDataPlotDefaults,
                         rowDataPlot=rowDataPlotDefaults,
                         rowStatTable=rowStatTableDefaults,
                         heatMapPlot=heatMapPlotDefaults)

        cur_max <- all_maxes[[mode]]
        cur_args <- all_args[[mode]]
        tmp <- DEFFUN(se, cur_max)
        if (!is.null(cur_args)) {
            tmp <- .override_defaults(tmp, cur_args)
        }
        rownames(tmp) <- sprintf("%s%i", mode, seq_len(cur_max))
        memory[[mode]] <- tmp
    }

    return(memory)
}

.name2index <- function(df, fields, choices) 
# This converts default arguments specified as strings into the relevant integer indices.
# The idea is to allow users to flexibly specify the input choices; while integers are
# safer when names are not unique or absent, strings are easier to work with.
{
    for (f in intersect(fields, colnames(df))) {
        vals <- df[,f]
        if (is.character(vals)) { 
            m <- match(vals, choices)
            m[is.na(m)] <- 1L
            df[,f] <- m 
        } else if (!is.integer(vals)) {
            df[,f] <- as.integer(vals) 
        }
    }
    return(df)
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
            memory[[mode]][,.brushByPlot][bad] <- .noSelection
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
        memory$rowDataPlot[,.brushByPlot][bad] <- .noSelection
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

.regenerate_unselected_plot <- function(mode, i, pObjects, rObjects, input, session) 
# This is a convenience function whenever a plot needs to be regenerated
# without any selections (i.e., cleared brush and lasso waypoints). It 
# relies on the fact that pObjects, rObjects and session are passed by reference.
{
    plot_name <- paste0(mode, i)
    brush_id <- paste0(plot_name, "_", .brushField)

    if (!is.null(isolate(input[[brush_id]]))) {
        # This will trigger replotting via the brush observer above.
        # It will also implicitly wipe the lasso data, so there's no need to do that manually.
        session$resetBrush(brush_id)
        pObjects$force_rerender[plot_name] <- TRUE
    } else {
        # Manually triggering replotting.
        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

        # Destroying any lasso waypoints as well.
        pObjects$memory[[mode]] <- .update_list_element(pObjects$memory[[mode]], i, .lassoData, NULL)
    }
    return(NULL)
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
    if (brush_in!=.noSelection) {
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
    if (brush_in!=.noSelection) {
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
