#' iSEE: interactive SummarizedExperiment/SingleCellExperiment Explorer
#'
#' Interactive and reproducible visualization of data contained in a
#' SummarizedExperiment/SingleCellExperiment, using a Shiny interface.
#'
#' @param se An object that is coercible to \linkS4class{SingleCellExperiment}.
#' @param redDimArgs A DataFrame similar to that produced by
#' \code{\link{redDimPlotDefaults}}, specifying initial parameters
#' for the plots.
#' @param colDataArgs A DataFrame similar to that produced by
#' \code{\link{colDataPlotDefaults}}, specifying initial parameters
#' for the plots.
#' @param featExprArgs A DataFrame similar to that produced by
#' \code{\link{featExprPlotDefaults}}, specifying initial parameters
#' for the plots.
#' @param rowStatArgs A DataFrame similar to that produced by
#' \code{\link{rowStatTableDefaults}}, specifying initial parameters
#' for the plots.
#' @param rowDataArgs A DataFrame similar to that produced by
#' \code{\link{rowDataPlotDefaults}}, specifying initial parameters
#' for the plots.
#' @param heatMapArgs A DataFrame similar to that produced by
#' \code{\link{heatMapPlotDefaults}}, specifying initial parameters
#' for the plots.
#' @param redDimMax An integer scalar specifying the maximum number of
#' reduced dimension plots in the interface.
#' @param colDataMax An integer scalar specifying the maximum number of
#' column data plots in the interface.
#' @param featExprMax An integer scalar specifying the maximum number of
#' feature expression plots in the interface.
#' @param rowStatMax An integer scalar specifying the maximum number of
#' row statistics tables in the interface.
#' @param rowDataMax An integer scalar specifying the maximum number of
#' row data plots in the interface.
#' @param heatMapMax An integer scalar specifying the maximum number of
#' heatmaps in the interface.
#' @param initialPanels A DataFrame specifying which panels should be
#' created at initialization. 
#' This should contain a \code{Name} character field and a \code{Width}
#' integer field, see Details.
#' @param annotFun A function, constructed in the form of
#' \code{\link{annotateEntrez}} or \code{\link{annotateEnsembl}}.
#' This function is built in a way to generate itself 
#' a function supposed to accept two parameters,
#' \code{se} and \code{row_index},
#' to have a unified yet flexible interface to generate additional information
#' to display for the selected genes of interest. 
#' @param colormap An \linkS4class{ExperimentColorMap} object that defines
#' custom color maps to apply to individual \code{assays}, \code{colData},
#' and \code{rowData} covariates.
#' @param run_local A logical indicating whether the app is to be run locally
#' or remotely on a server, which determines how documentation will be
#' accessed.
#'
#' @details Users can pass default parameters via DataFrame objects in
#' \code{redDimArgs} and \code{featExprArgs}. Each object can contain
#' some or all of the expected fields (see \code{\link{redDimPlotDefaults}}).
#' Any missing fields will be filled in with the defaults.
#'
#' The number of maximum plots for each type of plot is set to the larger
#' of \code{*Max} and \code{nrow(*Args)}. Users can specify any number of
#' maximum plots, though increasing the number will increase the time
#' required to render the interface.
#'
#' The \code{initialPanels} argument specifies the panels to be created
#' upon initializing the interface. This should be a DataFrame containing
#' a \code{Name} field specifying the identity of the panel, e.g.,
#' \code{"Reduced dimension plot 1"}, \code{"Row statistics table 2"}.
#' The trailing number should not be greater than the number of
#' maximum plots of that type. The \code{Width} field may also be specified
#' describing the width of the panel from 2 to 12 (values will be coerced
#' inside this range).
#'
#' @return A Shiny App is launched for interactive data exploration of the
#' \code{\link{SummarizedExperiment}}/\code{\link{SingleCellExperiment}} 
#' object
#'
#' @export
#'
#' @examples
#' library(scRNAseq)
#' data(allen)
#' class(allen)
#'
#' # Example data ----
#'
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#'
#' sce <- runPCA(sce)
#' sce <- runTSNE(sce)
#' rowData(sce)$ave_count <- rowMeans(counts(sce))
#' rowData(sce)$n_cells <- rowSums(counts(sce)>0)
#' sce
#'
#' # launch the app itself ----
#'
#' app <- iSEE(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234)
#' }
iSEE <- function(
  se,
  redDimArgs=NULL,
  colDataArgs=NULL,
  featExprArgs=NULL,
  rowStatArgs=NULL,
  rowDataArgs=NULL,
  heatMapArgs=NULL,
  redDimMax=5,
  colDataMax=5,
  featExprMax=5,
  rowStatMax=5,
  rowDataMax=5,
  heatMapMax=5,
  initialPanels=NULL,
  annotFun = NULL,
  colormap=ExperimentColorMap(),
  run_local=TRUE
) {
  # Save the original name of the input object for the command to rename it
  # in the tracker
  se_name <- deparse(substitute(se))
  ecm_name <- deparse(substitute(colormap))
  se_out <- .sanitize_SE_input(se)
  se <- se_out$object
  se_cmds <- se_out$cmds

  # Throw an error if the colormap supplied is not compatible with the object
  isColorMapCompatible(colormap, se, error = TRUE)

  # Setting up inputs for DT::datatable something to play with.
  gene_data <- as.data.frame(rowData(se))
  rownames(gene_data) <- rownames(se)
  if (ncol(gene_data)==0L) {
    gene_data$Present <- !logical(nrow(gene_data))
  }
  tab_select_col <- .safe_field_name("Selected", colnames(gene_data))

  # Defining the maximum number of plots.
  memory <- .setup_memory(se,
        redDimArgs,colDataArgs,featExprArgs,rowStatArgs,rowDataArgs,heatMapArgs,
        redDimMax, colDataMax, featExprMax, rowStatMax, rowDataMax, heatMapMax)

  # Defining the initial elements to be plotted.
  active_panels <- .setup_initial(initialPanels, memory)
  memory <- .sanitize_memory(active_panels, memory)
  
  #######################################################################
  ## UI definition. ----
  #######################################################################

  iSEE_ui <- dashboardPage(
    dashboardHeader(
      title = paste0(
        "iSEE - interactive SingleCell/Summarized Experiment Explorer v",
        packageVersion("iSEE")),
      titleWidth = 750,

      dropdownMenu(type = "tasks",
                   icon = icon("wrench fa-1g"),
                   badgeStatus = NULL,
                   headerText = "iSEE diagnostics",
                   notificationItem(
                     text = actionButton(
                       'open_linkgraph', label="Examine panel chart",
                       icon = icon("chain"),
                       style=.actionbutton_biocstyle
                     ),
                     icon = icon(""), status = "primary"
                   ),
                   notificationItem(
                     text = actionButton(
                       'getcode_all', label="Extract the R code",
                       icon = icon("magic"),
                       style=.actionbutton_biocstyle
                     ),
                     icon = icon(""), status = "primary"
                   )
      ), # end of dropdownMenu

      dropdownMenu(type = "tasks",
                   icon = icon("question-circle fa-1g"),
                   badgeStatus = NULL,
                   headerText = "Documentation",
                   notificationItem(
                     text = actionButton(
                       "tour_firststeps", "Click me for a quick tour",
                       icon("hand-o-right"),
                       style=.actionbutton_biocstyle),
                     icon = icon(""), # tricking it to not have additional icon
                     status = "primary"),
                   notificationItem(
                     text = actionButton(
                       'open_vignette', label="Open the vignette",
                       icon = icon("book"),
                       style=.actionbutton_biocstyle,
                       onclick = ifelse(
                         run_local, "",
                         "window.open('http://google.com', '_blank')")), # to be replaced with vignette url
                     icon = icon(""), status = "primary"
                   )
        ),

        dropdownMenu(type = "tasks",
                    icon = icon("info fa-1g"),
                    badgeStatus = NULL,
                    headerText = "Additional information",
                    notificationItem(
                     text = actionButton(
                       'session_info', label="About this session",
                       icon = icon("window-maximize"),
                       style=.actionbutton_biocstyle
                     ),
                     icon = icon(""), status = "primary"
                   ),
                   notificationItem(
                     text = actionButton('iSEE_info', label="About iSEE",
                                         icon = icon("heart"),
                                         style=.actionbutton_biocstyle
                                         ),
                     icon = icon(""), status = "primary"
                   )
      ) # end of dropdownMenu
    ), # end of dashboardHeader

    dashboardSidebar(
      selectizeInput(
        "newPanelChoice", label="Choose panel type:",
        selected=rev.translation[1], choices=rev.translation),
      actionButton("newPanelAdd", "Add new panel"), 
      hr(),
      uiOutput("panelOrganization")
    ), # end of dashboardSidebar

    dashboardBody(
      useShinyjs(),
      introjsUI(), # must be included in UI

      # for error message handling
      tags$head(
        tags$style(HTML(paste("
.shiny-output-error-validation {
        font-size: 15px;
        color: forestgreen;
        text-align: center;
}
", .define_box_statuses))
                )
      ),

      uiOutput("allPanels")
    ), # end of dashboardBody
    skin = "blue"
  ) # end of dashboardPage

  #######################################################################
  ## Server definition. ----
  #######################################################################

  #nocov start
  iSEE_server <- function(input, output, session) {
    all_names <- list()
    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot", "rowStatTable", "heatMapPlot")) {
        max_plots <- nrow(memory[[mode]])
        all_names[[mode]] <- sprintf("%s%i", mode, seq_len(max_plots))
    }
    all_names <- unlist(all_names)
    empty_list <- vector("list", length(all_names))
    names(empty_list) <- all_names

    # Storage for persistent non-reactive objects.
    pObjects <- new.env()
    pObjects$memory <- memory

    pObjects$coordinates <- empty_list
    pObjects$commands <- empty_list

    pObjects$selection_links <- .spawn_selection_chart(memory)
    pObjects$table_links <- .spawn_table_links(memory)

    pObjects$cached_plots <- empty_list

    # Storage for all the reactive objects
    rObjects <- reactiveValues(
        active_panels = active_panels,
        rerendered = 1L
    )

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot", "rowStatTable", "heatMapPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            rObjects[[paste0(mode, id)]] <- 1L
            rObjects[[paste0(mode, id, "_", .panelLinkInfo)]] <- 1L
            rObjects[[paste0(mode, id, "_", .panelGeneralInfo)]] <- 1L
        }
    }

    mode <- "heatMapPlot"
    max_plots <- nrow(pObjects$memory[[mode]])
    for (id in seq_len(max_plots)) {
        rObjects[[paste0(mode, id, "_", .heatMapLegend)]] <- 1L
    }

    # Help and documentation-related observers.
    intro_firststeps <- read.delim(
      system.file("extdata", "intro_firststeps.txt",package = "iSEE"),
      sep=";", stringsAsFactors = FALSE,row.names = NULL)

    observeEvent(input$tour_firststeps, {
      introjs(session,
              options = list(steps= intro_firststeps)
      )
    })

    observeEvent(input$getcode_all, {
      showModal(modalDialog(
        title = "My code", size = "l",fade = TRUE,
        footer = NULL, easyClose = TRUE,
        p("You can click anywhere in the code editor and select all the code using",
          "a keyboard shortcut that depends on your operating system (e.g. Ctrl/Cmd + A",
          "followed by Ctrl/Cmd + C).",
          "This will copy the selected parts to the clipboard."),
        aceEditor("acereport_r", mode="r",theme = "solarized_light",autoComplete = "live",
                  value = paste0(.track_it_all(rObjects$active_panels, pObjects, se_name, ecm_name,se_cmds), collapse="\n"),
                  height="600px")
        ))
    })

    observeEvent(input$session_info, {
      showModal(
        modalDialog(
          title = "Session information", size = "l",fade = TRUE,
          footer = NULL, easyClose = TRUE,
          tagList(
            renderPrint({
              sessionInfo()
            })
          )
        )
      )
    })

    observeEvent(input$iSEE_info, {
      showModal(
        modalDialog(
          title = "About iSEE", size = "m", fade = TRUE,
          footer = NULL, easyClose = TRUE,
          tagList(
             iSEE_info, br(), br(),
             HTML("If you use this package, please use the following citation information:"),
             renderPrint({
                 citation("iSEE")
             })
          )
        )
      )
    })

    observeEvent(input$open_linkgraph, {
      showModal(
        modalDialog(
          title = "Graph of inter-panel links", size = "l",
          fade = TRUE, footer = NULL, easyClose = TRUE,
          renderPlot({
            .snapshot_graph_linkedpanels(rObjects$active_panels, pObjects)
          })
        )
      )
    })

    if (run_local) {
      observeEvent(input$open_vignette, {
        path <- system.file("doc","iSEE_vignette.html", package="iSEE")
        if (path=="") {
          showNotification("vignette has not been built on this system", type="error")
        } else {
          browseURL(path)
        }
      })
    }

    #######################################################################
    # Multipanel UI generation section. ----
    # This is adapted from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    #######################################################################

    output$allPanels <- renderUI({
        .panel_generation(rObjects$active_panels, pObjects$memory, se)
    })

    output$panelOrganization <- renderUI({
        rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
        .panel_organization(rObjects$active_panels)
    })

    # Panel addition.
    observeEvent(input$newPanelAdd, {
        mode <- input$newPanelChoice                 
        all_active <- rObjects$active_panels
        all.memory <- pObjects$memory[[mode]]
        first.missing <- setdiff(seq_len(nrow(all.memory)), all_active$ID[all_active$Type==mode])

        if (length(first.missing)) {
            rObjects$active_panels <- rbind(all_active, DataFrame(Type=mode, ID=first.missing[1], Width=4L, Height=500L))
        } else {
            showNotification(sprintf("maximum number of plots reached for mode '%s'", mode), type="error")
        }
    })

    # Note: we need "local" so that each item gets its own number. Without it, the value
    # of 'id' in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowStatTable", "rowDataPlot", "heatMapPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                prefix <- paste0(mode0, id0, "_")
                panel_name <- paste0(mode0, id0)
                max_plots0 <- max_plots

                # Panel removal.
                observeEvent(input[[paste0(prefix, .organizationDiscard)]], {
                    all_active <- rObjects$active_panels
                    current_type <- all_active$Type==mode0

                    # Destroying links for point selection or tables.
                    if (mode0=="heatMapPlot") {
                        .destroy_selection_panel(pObjects, panel_name)
                    } else if (mode0=="rowStatTable") {
                        .destroy_table(pObjects, panel_name)
                    } else {
                        .destroy_selection_panel(pObjects, panel_name)
                        .delete_table_links(mode0, id0, pObjects)
                    }

                    # Triggering re-rendering of the UI via change to active_panels.
                    index <- which(current_type & all_active$ID==id0)
                    rObjects$active_panels <- rObjects$active_panels[-index,]
               }, ignoreInit=TRUE)

                # Panel shifting, up and down.
                observeEvent(input[[paste0(prefix, .organizationUp)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    if (index!=1L) {
                        reindex <- seq_len(nrow(all_active))
                        reindex[index] <- reindex[index]-1L
                        reindex[index-1L] <- reindex[index-1L]+1L
                        rObjects$active_panels <- all_active[reindex,]
                    }
                }, ignoreInit=TRUE)

                observeEvent(input[[paste0(prefix, .organizationDown)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    if (index!=nrow(all_active)) {
                        reindex <- seq_len(nrow(all_active))
                        reindex[index] <- reindex[index]+1L
                        reindex[index+1L] <- reindex[index+1L]-1L
                        rObjects$active_panels <- all_active[reindex,]
                    }
                }, ignoreInit=TRUE)

                # Panel modification options.
                observeEvent(input[[paste0(prefix, .organizationModify)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    cur_width <- all_active$Width[index]
                    cur_height <- all_active$Height[index]

                    showModal(modalDialog(
                        sliderInput(paste0(prefix, .organizationWidth), label="Width",
                                    min=width_limits[1], max=width_limits[2], value=cur_width, step=1),
                        sliderInput(paste0(prefix, .organizationHeight), label="Height",
                                    min=height_limits[1], max=height_limits[2], value=cur_height, step=50),
                        title=paste(.decode_panel_name(mode0, id0), "panel parameters"),
                        easyClose=TRUE, size="m", footer=NULL
                        )
                    )
                })

                width_name <- paste0(prefix, .organizationWidth)
                observeEvent(input[[width_name]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    cur.width <- all_active$Width[index]
                    new.width <- input[[width_name]]
                    if (!isTRUE(all.equal(new.width, cur.width))) {
                        rObjects$active_panels$Width[index] <- new.width
                    }
                }, ignoreInit=TRUE)

                height_name <- paste0(prefix, .organizationHeight)
                observeEvent(input[[height_name]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    cur.height <- all_active$Height[index]
                    new.height <- input[[height_name]]
                    if (!isTRUE(all.equal(new.height, cur.height))) {
                        rObjects$active_panels$Height[index] <- new.height
                    }
                }, ignoreInit=TRUE)
            })
        }
    }

    #######################################################################
    # Parameter panel observers.
    #######################################################################

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            for (panel in c(.dataParamBoxOpen, .visualParamBoxOpen, .selectParamBoxOpen)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    panel0 <- panel

                    open_field <- paste0(mode0, id0, "_", panel0)
                    observeEvent(input[[open_field]], {
                        pObjects$memory[[mode0]][[panel0]][id0] <- input[[open_field]]
                    })
                })
            }
        }
    }

    # Panel opening/closing observers for heat map plots.
    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_plots)) {
        for (panel in c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen, .selectParamBoxOpen)) {
            local({
                mode0 <- "heatMapPlot"
                id0 <- id
                panel0 <- panel

                open_field <- paste0(mode0, id0, "_", panel0)
                observeEvent(input[[open_field]], {
                    pObjects$memory[[mode0]][[panel0]][id0] <- input[[open_field]]
                })
            })
        }
    }

    # Same for the tables.
    max_tabs <- nrow(pObjects$memory$rowStatTable)
    for (id in seq_len(max_tabs)) {
        local({
            mode0 <- "rowStatTable"
            id0 <- id
            tab_name <- paste0(mode0, id0)
            prefix <- paste0(tab_name, "_")

            select_open_field <- paste0(prefix, .selectParamBoxOpen)
            observeEvent(input[[select_open_field]], {
                pObjects$memory[[mode0]][[.selectParamBoxOpen]][id0] <- input[[select_open_field]]
            })
        })
    }

    #######################################################################
    # Point selection observers.
    #######################################################################

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                prefix <- paste0(plot_name, "_")

                ###############

                # Selection choice observer.
                select_plot_field <- paste0(prefix, .selectByPlot)
                observeEvent(input[[select_plot_field]], {
                    old_transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    new_transmitter <- input[[select_plot_field]]
                    
                    # Determining whether the new and old transmitting plot have selections.
                    old_out <- .transmitted_selection(old_transmitter, pObjects$memory)
                    old_select <- old_out$selected
                    old_encoded <- old_out$encoded
                    new_out <- .transmitted_selection(new_transmitter, pObjects$memory)
                    new_select <- new_out$selected
                    new_encoded <- new_out$encoded
                    
                    # Trying to update the graph, but breaking if it's not a DAG.
                    # We also break if users try to self-select in restrict mode.
                    # In both cases, we just reset back to the choice they had before.
                    tmp <- .choose_new_selection_source(pObjects$selection_links, plot_name, new_encoded, old_encoded)
                    
                    daggy <- is_dag(simplify(tmp, remove.loops=TRUE))
                    self_restrict <- new_encoded==plot_name &&
                        new_encoded!=.noSelection &&
                        pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle
                    
                    if (!daggy || self_restrict) {
                        if (!daggy) {
                            showNotification("point selection relationships cannot be cyclic", type="error")
                        } else if (self_restrict){
                            showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                        }
                        pObjects$memory[[mode0]][id0, .selectByPlot] <- old_transmitter
                        updateSelectInput(session, select_plot_field, selected=old_transmitter)
                        return(NULL)
                    }
                    
                    pObjects$selection_links <- tmp
                    pObjects$memory[[mode0]][id0, .selectByPlot] <- new_transmitter
                    
                    # Update the elements reporting the links between plots.
                    for (relinked in setdiff(c(old_encoded, new_encoded, plot_name), .noSelection)) {
                        relink_field <- paste0(relinked, "_", .panelLinkInfo)
                        rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                    }
                    
                    # Not replotting if there were no selection in either the new or old transmitters.
                    if (!old_select && !new_select){
                        return(NULL)
                    }
                    
                    # Triggering self update of the plot.
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    
                    # Triggering replotting of children, if the current panel is set to restrict;
                    # and we have a selection, so that there was already some selection in the children.
                    if (pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle
                        && .any_point_selection(mode0, id0, pObjects$memory)) {
                        children <- .get_selection_dependents(pObjects$selection_links, plot_name, pObjects$memory)
                        for (child_plot in children) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                        }
                    }
                }, ignoreInit=TRUE)

                ###############

                # Selection effect observer.
                select_effect_field <- paste0(prefix, .selectEffect)
                observeEvent(input[[select_effect_field]], {
                    cur_effect <- input[[select_effect_field]]
                    old_effect <- pObjects$memory[[mode0]][id0, .selectEffect]
                    
                    # Storing the new choice into memory, unless self-selecting to restrict.
                    # In which case, we trigger an error and reset to the previous choice.
                    if (cur_effect == .selectRestrictTitle
                        && pObjects$memory[[mode0]][id0, .selectByPlot]==.decode_panel_name(mode0, id0)) {
                        showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                        updateRadioButtons(session, select_effect_field, selected=old_effect)
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][id0, .selectEffect] <- cur_effect
                    
                    # Avoiding replotting if there was no transmitting selection.
                    transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    if (transmitter==.noSelection) {
                        return(NULL)
                    }
                    enc <- .encode_panel_name(transmitter)
                    if (!.any_point_selection(enc$Type, enc$ID, pObjects$memory)) {
                        return(NULL)
                    }
                    
                    # Triggering self update.
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    
                    # Triggering replotting of children, if we are set to or from restrict;
                    # and we have a selection, so there was already some selecting in the children.
                    if ((cur_effect==.selectRestrictTitle || old_effect==.selectRestrictTitle)
                        && .any_point_selection(mode0, id0, pObjects$memory)) {
                        children <- .get_selection_dependents(pObjects$selection_links, plot_name, pObjects$memory)
                        for (child_plot in children) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                        }
                    }
                }, ignoreInit=TRUE)
                
                ###############

                # Shiny brush structure observers. ----
                brush_id <- paste0(prefix, .brushField)
                observeEvent(input[[brush_id]], {
                    cur_brush <- input[[brush_id]]
                    old_brush <- pObjects$memory[[mode0]][,.brushData][[id0]]
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, cur_brush)
                    
                    # If the Shiny brushes have the same coordinates, we don't bother replotting.
                    replot <- !.identical_brushes(cur_brush, old_brush)
                    
                    # Destroying lasso points upon Shiny brush (replotting if existing lasso was not NULL).
                    replot <- replot || !is.null(pObjects$memory[[mode0]][,.lassoData][[id0]])
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, NULL)
                    if (!replot) {
                        return(NULL)
                    }
                    
                    # Trigger replotting of self, to draw a more persistent selection Shiny brush visual
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    
                    # Trigger replotting of all dependent plots that receive this Shiny brush
                    children <- .get_selection_dependents(pObjects$selection_links, plot_name, pObjects$memory)
                    for (child_plot in children) {
                        rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                    }
                }, ignoreInit=TRUE, ignoreNULL=FALSE)
            })
        }
    }

    # Linked selection choice observers for the tables. ----
    max_tabs <- nrow(pObjects$memory$rowStatTable)
    for (id in seq_len(max_tabs)) {
        local({
            mode0 <- "rowStatTable"
            id0 <- id
            tab_name <- paste0(mode0, id0)
            prefix <- paste0(tab_name, "_")

            select_plot_field <- paste0(prefix, .selectByPlot)
            observeEvent(input[[select_plot_field]], {
                old_transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                new_transmitter <- input[[select_plot_field]]

                # Determining whether the new and old transmitting plot have selections. 
                old_out <- .transmitted_selection(old_transmitter, pObjects$memory)
                old_select <- old_out$selected
                old_encoded <- old_out$encoded
                new_out <- .transmitted_selection(new_transmitter, pObjects$memory)
                new_select <- new_out$selected
                new_encoded <- new_out$encoded

                # Updating the graph (no need for DAG protection here, as tables do not transmit selections).
                pObjects$selection_links <- .choose_new_selection_source(pObjects$selection_links, tab_name, new_encoded, old_encoded)
                pObjects$memory[[mode0]][id0, .selectByPlot] <- new_transmitter

                # Update the elements reporting the links between plots.
                for (relinked in c(old_encoded, new_encoded, tab_name)) {
                    if (relinked==.noSelection) { next }
                    relink_field <- paste0(relinked, "_", .panelLinkInfo)
                    rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                }

                # Not re-rendering if there were no selections in either the new or old transmitters.
                if (!old_select && !new_select){
                    return(NULL)
                }

                # Triggering update of the table.
                rObjects[[tab_name]] <- .increment_counter(isolate(rObjects[[tab_name]]))
            }, ignoreInit=TRUE)
        })
    }

    # Linked selection choice observers for the heatmaps.
    max_tabs <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_tabs)) {
        local({
            mode0 <- "heatMapPlot"
            id0 <- id
            plot_name <- paste0(mode0, id0)
            prefix <- paste0(plot_name, "_")
 
            # Selection choice observer.
            select_plot_field <- paste0(prefix, .selectByPlot)
            observeEvent(input[[select_plot_field]], {
                old_transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                new_transmitter <- input[[select_plot_field]]
                
                # Determining whether the new and old transmitting plot have selections. 
                old_out <- .transmitted_selection(old_transmitter, pObjects$memory)
                old_select <- old_out$selected
                old_encoded <- old_out$encoded
                new_out <- .transmitted_selection(new_transmitter, pObjects$memory)
                new_select <- new_out$selected
                new_encoded <- new_out$encoded
                
                # Updating the graph (no need to wrorry about DAGs here, as heatmaps do not transmit).
                pObjects$selection_links <- .choose_new_selection_source(pObjects$selection_links, plot_name, new_encoded, old_encoded)
                pObjects$memory[[mode0]][id0, .selectByPlot] <- new_transmitter
                
                # Update the elements reporting the links between plots.
                for (relinked in c(old_encoded, new_encoded, plot_name)) {
                    if (relinked==.noSelection) { next }
                    relink_field <- paste0(relinked, "_", .panelLinkInfo)
                    rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                }
                
                # Not replotting if there were no selections in either the new or old transmitters.
                if (!old_select && !new_select){
                    return(NULL)
                }
                
                # Triggering self update of the plot.
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            }, ignoreInit=TRUE)

            ###############

            # Selection effect observer.
            select_effect_field <- paste0(prefix, .selectEffect)
            observeEvent(input[[select_effect_field]], {
                cur_effect <- input[[select_effect_field]]
                old_effect <- pObjects$memory[[mode0]][id0, .selectEffect]
                pObjects$memory[[mode0]][id0, .selectEffect] <- cur_effect

                # Avoiding replotting if there was no transmitting selection.
                transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                if (transmitter==.noSelection) {
                    return(NULL)
                }
                enc <- .encode_panel_name(transmitter)
                if (!.any_point_selection(enc$Type, enc$ID, pObjects$memory)) {
                    return(NULL)
                }

                # Triggering self update.
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            }, ignoreInit=TRUE)
        })
    }

    #######################################################################
    # Click observers.
    #######################################################################

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                prefix <- paste0(plot_name, "_")

                click_field <- paste0(prefix, .lassoClick)
                observeEvent(input[[click_field]], {
                    cur_click <- input[[click_field]]
                    previous <- pObjects$memory[[mode0]][,.lassoData][[id0]]
                    bump_children <- FALSE

                    # Don't add to waypoints if a Shiny brush exists in memory (as they are mutually exclusive).
                    if (!is.null(pObjects$memory[[mode0]][,.brushData][[id0]]) ||
                        !is.null(input[[paste0(prefix, .brushField)]])) {
                        return(NULL)
                    }

                    # Closing the lasso if you click close to the starting point.
                    xrange <- cur_click$domain$right - cur_click$domain$left
                    yrange <- cur_click$domain$top - cur_click$domain$bottom
                    if (!is.null(previous)
                        && abs(cur_click$x - previous[1,1]) < xrange/100
                        && abs(cur_click$y - previous[1,2]) < yrange/100) {
                        updated <- rbind(previous, previous[1,])
                        attr(updated, "closed") <- TRUE
                        bump_children <- TRUE

                        # Checking out whether coordinates are flipped.
                        attr(updated, "flipped") <- (cur_click$mapping$x=="Y" && cur_click$mapping$y=="X")

                    } else {
                        is_closed <- attr(previous, "closed")
                        if (!is.null(is_closed) && is_closed) {
                            previous <- NULL
                            bump_children <- TRUE
                        }
                        updated <- rbind(previous, c(cur_click$x, cur_click$y))
                        attr(updated, "closed") <- FALSE
                    }

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, updated)

                    # Trigger replotting of self, to draw the lasso waypoints.
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                    # Trigger replotting of child panels that receive point selection information.
                    if (bump_children) {
                        children <- .get_selection_dependents(pObjects$selection_links, plot_name, pObjects$memory)
                        for (child_plot in children) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                        }
                    }
                })
            })
        }
    }

    #######################################################################
    # Zoom observers.
    #######################################################################

    # The interpretation of the double-click is somewhat complicated.
    # - If you double-click on a Shiny brush, you zoom it while wiping the brush.
    # - If you double-click outside a brush, you wipe the brush (this is done automatically).
    #   If an open lasso is present, it is deleted.
    #   If there was no open lasso, you zoom out.

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                prefix <- paste0(plot_name, "_")

                brush_id <- paste0(prefix, .brushField)
                zoom_click_field <- paste0(prefix, .zoomClick)

                observeEvent(input[[zoom_click_field]], {
                    brush <- input[[brush_id]]

                    if (!is.null(brush)) {
                        new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
                        session$resetBrush(brush_id) # This should auto-trigger replotting above.
                    } else {
                        # brush is already NULL at this point, so there is no need to reset it.
                        # However, we do need to manually trigger replotting. We don't move this outside the
                        # "else", to avoid two reactive updates of unknown priorities.
                        new_coords <- pObjects$memory[[mode0]][,.zoomData][[id0]]

                        lasso_data <- pObjects$memory[[mode0]][,.lassoData][[id0]]
                        if (!is.null(lasso_data)) {
                            # We wipe out any lasso waypoints if they are present, and trigger replotting with the same scope.
                            pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, NULL)
                            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                            
                        } else {
                            if (!is.null(new_coords)) {
                                # If there are already no lasso waypoints, we zoom out.
                                new_coords <- NULL
                                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                            }
                        }
                    }

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .zoomData, new_coords)
                })
            })
        }
    }

    # Zoom observers for the heatmaps.
    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_plots)) {
        local({
            mode0 <- "heatMapPlot"
            id0 <- id
            plot_name <- paste0(mode0, id0)
            prefix <- paste0(plot_name, "_")

            brush_id <- paste0(prefix, .brushField)
            zoom_click_field <- paste0(prefix, .zoomClick)

            observeEvent(input[[zoom_click_field]], {
                brush <- input[[brush_id]]
                if (!is.null(brush)) {
                    new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
                    session$resetBrush(brush_id) # This does NOT trigger replotting, as there is no brush observer for the heatmap.
                    if (is.null(pObjects$memory$heatMapPlot[id0,][[.zoomData]][[1]])) { # if we haven't already zoomed in
                        inp_rows <- seq_along(pObjects$memory$heatMapPlot[id0,][[.heatMapFeatName]][[1]])
                    } else {
                        inp_rows <- pObjects$memory$heatMapPlot[id0,][[.zoomData]][[1]]
                    }

                    # Update data and force replotting.
                    # Is the heatmap receiving a color brush (in that case the number of annotations should be increased by 1)
                    is_receiving_color_selection <- pObjects$memory$heatMapPlot[id0,][[.selectByPlot]]!=.noSelection && 
                        pObjects$memory$heatMapPlot[id0,][[.selectEffect]]==.selectColorTitle && 
                        .transmitted_selection(pObjects$memory$heatMapPlot[id0, .selectByPlot], pObjects$memory)$select

                    n.annot <- length(pObjects$memory$heatMapPlot[,.heatMapColData][[id0]]) + is_receiving_color_selection
                    ymin <- .transform_global_to_local_y(new_coords["ymin"], n.genes=length(inp_rows), n.annot=n.annot)
                    ymax <- .transform_global_to_local_y(new_coords["ymax"], n.genes=length(inp_rows), n.annot=n.annot)
                    new_rows <- inp_rows[ymin:ymax]
                    
                } else {
                    new_rows <- NULL # Zoom out.
                }

                pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .zoomData, new_rows)
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            }, ignoreInit=TRUE)
        })
    }

    #######################################################################
    # Selectize updators. ----
    #######################################################################

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    max_plots <- nrow(pObjects$memory$featExprPlot)
    for (id in seq_len(max_plots)) {
        for (axis in c("xaxis", "yaxis")) {
            if (axis=="xaxis") {
                axis_name_choice <- .featExprYAxisFeatName
            } else {
                axis_name_choice <- .featExprXAxisFeatName
            }

            local({
                id0 <- id
                mode0 <- "featExprPlot"
                field0 <- axis_name_choice
                cur_field <- paste0(mode0, id0, "_", field0)

                observe({
                    force(rObjects$rerendered)

                    updateSelectizeInput(session, cur_field, label = NULL, choices = feature_choices, server = TRUE,
                                         selected = pObjects$memory[[mode0]][id0, field0][[1]])
                }, priority=-1) # Lower priority so that it executes AFTER the UI rerender.
            })
        }
    }

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                id0 <- id
                mode0 <- mode
                field0 <- .colorByFeatName
                cur_field <- paste0(mode0, id0, "_", field0)

                observe({
                    force(rObjects$rerendered)
                    updateSelectizeInput(session, cur_field, label = NULL, choices = feature_choices, server = TRUE,
                                         selected = pObjects$memory[[mode0]][id0, field0][[1]])
                }, priority=-1) # Lower priority so that it executes AFTER the UI rerender.
            })
        }
    }

    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_plots)) {
        local({
            id0 <- id
            mode0 <- "heatMapPlot"

            observe({
                force(rObjects$rerendered)
                updateSelectizeInput(session, paste0(mode0, id0, "_", .heatMapFeatName), choices = feature_choices,
                                     server = TRUE, selected = pObjects$memory[[mode0]][id0, .heatMapFeatName][[1]])
            }, priority=-1) # Lower priority so that it executes AFTER the UI rerender.
        })
    }

    #######################################################################
    # Dot-related plot creation section. ----
    #######################################################################

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])

        # Defining mode-specific plotting functions.
        FUN <- switch(mode,
                      redDimPlot=.make_redDimPlot,
                      featExprPlot=.make_featExprPlot,
                      colDataPlot=.make_colDataPlot,
                      rowDataPlot=.make_rowDataPlot)

        # Defining fundamental parameters that destroy brushes/lassos upon being changed.
        protected <- switch(mode,
                            redDimPlot=c(.redDimType, .redDimXAxis, .redDimYAxis),
                            colDataPlot=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
                            featExprPlot=c(.featExprAssay, .featExprXAxisColData),
                            rowDataPlot=c(.rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData))

        # Defining non-fundamental parameters that do not destroy brushes/lassos.
        if (mode=="rowDataPlot") {
            nonfundamental <- c(.colorByRowData, .colorByFeatNameColor)
        } else {
            nonfundamental <- c(.colorByColData, .colorByFeatNameAssay)
        }
        nonfundamental <- c(nonfundamental, .selectColor, .selectTransAlpha, .plotPointSize,
                            .plotPointAlpha, .plotFontSize, .plotLegendPosition, .colorByDefaultColor)

        for (id in seq_len(max_plots)) {
            # Observers for the non-fundamental parameter options.
            for (field in nonfundamental) {
                local({
                    id0 <- id
                    mode0 <- mode
                    field0 <- field
                    plot_name <- paste0(mode0, id0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[field0]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }, ignoreInit=TRUE)
                })
            }

            # Observers for non-fundamental list parameters.
            for (field in c(.visualParamChoice)) {
                local({
                    id0 <- id
                    mode0 <- mode
                    field0 <- field
                    plot_name <- paste0(mode0, id0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        existing <- pObjects$memory[[mode0]][,field0][[id0]]
                        incoming <- as(input[[cur_field]], typeof(existing))
                        if (identical(incoming, existing)) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, field0, incoming)
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }, ignoreInit=TRUE, ignoreNULL=FALSE)
                })
            }

            # Observers for the fundamental plot parameters.
            for (field in protected) {
                local({
                    id0 <- id
                    mode0 <- mode
                    field0 <- field
                    plot_name <- paste0(mode0, id0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[field0]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                        .regenerate_unselected_plot(mode0, id0, pObjects, rObjects, input, session)
                     }, ignoreInit=TRUE, priority=-2) # executes AFTER the update selectize.
                })
            }

            local({
                id0 <- id
                mode0 <- mode
                FUN0 <- FUN
                plot_name <- paste0(mode0, id0)

                # Observers for the linked color by feature name. This also updates the table_links information.
                observe({
                    replot <- .setup_table_observer(mode0, id0, pObjects, rObjects, input, session, 
                                                    by_field = .colorByField, title = .colorByFeatNameTitle, 
                                                    feat_field = .colorByFeatName, tab_field = .colorByRowTable, 
                                                    feat_choices = feature_choices, param='color')
                    if (replot) {
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }
                })

                # Defining the rendered plot, and saving the coordinates.
                gen_field <- paste0(plot_name, "_", .panelGeneralInfo)
                output[[plot_name]] <- renderPlot({
                    force(rObjects[[plot_name]])
                    rObjects[[gen_field]] <- .increment_counter(isolate(rObjects[[gen_field]]))
                    p.out <- FUN0(id0, pObjects$memory, pObjects$coordinates, se, colormap)
                    pObjects$commands[[plot_name]] <- p.out$cmd_list
                    pObjects$coordinates[[plot_name]] <- p.out$xy[,c("X", "Y")]
                    p.out$plot
                })

                # Describing some general panel information.
                dec_name <- .decode_panel_name(mode0, id0)
                output[[gen_field]] <- renderUI({
                    force(rObjects[[gen_field]])
                    selected <- .get_selected_points(rownames(pObjects$coordinates[[plot_name]]), dec_name, 
                                                     pObjects$memory, pObjects$coordinates)
                    if (is.null(selected)) { 
                        return(NULL)
                    }
                    n_selected <- sum(selected)
                    n_total <- length(selected)
                    HTML(sprintf("%i of %i points selected (%.1f%%)",
                                 n_selected, n_total, 100*n_selected/n_total))
                })

                # Describing the links between panels.
                link_field <- paste0(plot_name, "_", .panelLinkInfo)
                output[[link_field]] <- renderUI({
                    force(rObjects[[link_field]])
                    .define_plot_links(plot_name, pObjects$memory, pObjects$selection_links)
                })
            })
        }
    }

    # Feature expression plots need some careful handling, as we need to update the
    # table links and destroy a brush/lasso whenever an x/y-axis-specifying parameter changes.
    max_plots <- nrow(pObjects$memory$featExprPlot)
    for (id in seq_len(max_plots)) {
        for (axis in c("xaxis", "yaxis")) {
            if (axis=="yaxis") {
                axis_choice <- NA
                axis_tab_title <- NA
                axis_tab_choice <- .featExprYAxisRowTable
                axis_feat <- .featExprYAxisFeatName
            } else {
                axis_choice <- .featExprXAxis
                axis_tab_title <- .featExprXAxisFeatNameTitle
                axis_tab_choice <- .featExprXAxisRowTable
                axis_feat <- .featExprXAxisFeatName
            }

            local({
                id0 <- id
                mode0 <- "featExprPlot"
                plot_name <- paste0(mode0, id0)

                axis0 <- axis
                axis_choice0 <- axis_choice
                axis_tab_title0 <- axis_tab_title
                axis_tab_choice0 <- axis_tab_choice
                axis_feat0 <- axis_feat

                observe({
                    replot <- .setup_table_observer(mode0, id0, pObjects, rObjects, input, session, 
                                                    by_field = axis_choice0, title = axis_tab_title0, 
                                                    feat_field = axis_feat0, tab_field = axis_tab_choice0, 
                                                    feat_choices = feature_choices, param=axis0)
                    if (replot) {
                        .regenerate_unselected_plot(mode0, id0, pObjects, rObjects, input, session)
                    }
                })
            })
        }
    }

    #######################################################################
    # Row table section. ----
    #######################################################################

    # Load the gene level data
    for (id in seq_len(nrow(memory$rowStatTable))) {
      local({
        id0 <- id
        panel_name <- paste0("rowStatTable", id0)

        output[[panel_name]] <- renderDataTable({
            force(rObjects$active_panels) # to trigger recreation when the number of plots is changed.
            force(rObjects[[panel_name]])

            chosen <- pObjects$memory$rowStatTable[id0, .rowStatSelected]
            search <- pObjects$memory$rowStatTable[id0, .rowStatSearch]
            search_col <- pObjects$memory$rowStatTable[,.rowStatColSearch][[id0]]
            search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

            # Adding a "Selected" field to the plotting data, which responds to point selection input.
            # Note that this AUTOMATICALLY updates search_col upon re-rendering via the observer below.
            # The code below keeps search_col valid for the number of columns (i.e., with or without selection).
            selected <- .get_selected_points(rownames(gene_data), pObjects$memory$rowStatTable[id0,.selectByPlot], 
                                             pObjects$memory, pObjects$coordinates)
            tmp_gene_data <- gene_data
            if (!is.null(selected)) { 
                tmp_gene_data[[tab_select_col]] <- selected
                if (length(search_col)!=ncol(tmp_gene_data)) {
                    search_col <- c(search_col, list(list(search="true")))
                } else {
                    search_col[[ncol(tmp_gene_data)]]$search <- "true"
                }
            } else {
                search_col <- search_col[seq_len(ncol(tmp_gene_data))]
            }

            datatable(tmp_gene_data, filter="top", rownames=TRUE,
                      options=list(search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                                   searchCols=c(list(NULL), search_col), # row names are the first column!
                                   scrollX=TRUE),
                      selection=list(mode="single", selected=chosen))
        })

        # Updating memory for new selection parameters (no need for underscore
        # in 'select_field' definition, as this is already in the '.int' constant).
        select_field <- paste0(panel_name, .int_rowStatSelected)
        observe({
            chosen <- input[[select_field]]
            if (length(chosen)==0L) {
                return(NULL)
            }
            pObjects$memory$rowStatTable[id0, .rowStatSelected] <- chosen

            col_kids <- pObjects$table_links[[id0]][["color"]]
            x_kids <- pObjects$table_links[[id0]][["xaxis"]]
            y_kids <- pObjects$table_links[[id0]][["yaxis"]]

            # Triggering the replotting of all color children that are NOT xy children.
            # This is done indirectly, by triggering the observer for the color parameters upon updateSelectizeInput.
            col_kids <- sprintf("%s_%s", setdiff(col_kids, c(x_kids, y_kids)), .colorByFeatName)
            for (kid in col_kids) {
                updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=feature_choices)
            }

            # Triggering the replotting and brush/lasso clearing of all x/y-axis children.
            # There is a possibility that this would cause double-rendering as they trigger different observers.
            # But this would imply that you're plotting the same gene against itself, which would be stupid.
            x_kids <- sprintf("%s_%s", x_kids, .featExprXAxisFeatName)
            for (kid in x_kids) {
                updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=feature_choices)
            }
            y_kids <- sprintf("%s_%s", y_kids, .featExprYAxisFeatName)
            for (kid in y_kids) {
                updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=feature_choices)
            }
        })

        # Updating memory for new selection parameters.
        search_field <- paste0(panel_name, .int_rowStatSearch)
        observe({
            search <- input[[search_field]]
            if (length(search)) {
                pObjects$memory$rowStatTable[id0, .rowStatSearch] <- search
            }
        })

        colsearch_field <- paste0(panel_name, .int_rowStatColSearch)
        observe({
            search <- input[[colsearch_field]]
            if (length(search)) {
                pObjects$memory$rowStatTable <- .update_list_element(
                    pObjects$memory$rowStatTable, id0, .rowStatColSearch, search)
            }
        })

        # Updating the annotation box.
        anno_field <- paste0(panel_name, "_annotation")
        output[[anno_field]] <- renderUI({
          if(is.null(annotFun)) return()
          chosen <- input[[select_field]]
          annotFun(se,chosen)
          
        })

        # Describing the links between panels.
        link_field <- paste0(panel_name, "_", .panelLinkInfo)
        output[[link_field]] <- renderUI({
            force(rObjects[[link_field]])
            .define_table_links(panel_name, pObjects$memory, pObjects$table_links)
        })
      })
    }

    #######################################################################
    # Heat map section. ----
    #######################################################################

    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_plots)) {
        local({
            mode0 <- "heatMapPlot"
            id0 <- id
            plot_name <- paste0(mode0, id0)

            # Triggering an update of the selected elements.
            import_button <- paste0(plot_name, "_", .heatMapImport)
            observeEvent(input[[import_button]], {
                origin <- pObjects$memory[[mode0]][id0, .heatMapImportSource]
                if (origin==.noSelection) { 
                    return(NULL)
                }
                enc <- .encode_panel_name(origin)

                incoming <- NULL
                if (enc$Type=="rowStatTable") {
                    incoming <- input[[paste0(enc$Type, enc$ID, "_rows_all")]]
                } else {
                    incoming <- which(.get_selected_points(rownames(gene_data), origin, pObjects$memory, pObjects$coordinates))
                }

                limit <- 100
                if (length(incoming) > limit) {
                    showNotification(sprintf("only the first %i features used", limit), type="warning")
                    incoming <- utils::head(incoming, limit)
                }

                combined <- union(pObjects$memory[[mode0]][id0, .heatMapFeatName][[1]], incoming)
                updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices = feature_choices,
                                     server = TRUE, selected = combined)
            }, ignoreInit=TRUE)

            # Updating the import source, but this does NOT trigger replotting, as we need to press the button.
            field0 <- .heatMapImportSource
            cur_field <- paste0(plot_name, "_", field0)
            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                if (identical(input[[cur_field]], pObjects$memory[[mode0]][id0, field0])) {
                    return(NULL)
                }
                pObjects$memory[[mode0]][[field0]][id0] <- matched_input
            }, ignoreInit=TRUE)

            # Defining the rendered plot, and saving the coordinates.
            # Also triggering an update to the accompanying legend plot.
            legend_field <- paste0(plot_name, "_", .heatMapLegend)
            output[[plot_name]] <- renderPlot({
                force(rObjects[[plot_name]])
                rObjects[[legend_field]] <- .increment_counter(isolate(rObjects[[legend_field]]))

                p.out <- .make_heatMapPlot(id0, pObjects$memory, pObjects$coordinates, se, colormap)
                pObjects$commands[[plot_name]] <- p.out$cmd_list
                pObjects$coordinates[[plot_name]] <- p.out$xy # Caching the expression matrix.
                pObjects$cached_plots[[plot_name]] <- p.out$legends # Caching the legend plot for downstream use.
                p.out$plot
            })

            # Defining the legend.
            output[[legend_field]] <- renderPlot({
                force(rObjects[[legend_field]])
                gg <- pObjects$cached_plots[[plot_name]]
                cowplot::plot_grid(plotlist = gg, ncol=1)
            })

            # Defining link information.
            link_field <- paste0(plot_name, "_", .panelLinkInfo)
            output[[link_field]] <- renderUI({
                force(rObjects[[link_field]])
                select_in <- pObjects$memory$heatMapPlot[[id0, .selectByPlot]]
                if (select_in==.noSelection) {
                    return(NULL)
                } 
                tagList("Receiving selection from", em(strong(select_in)), br())
            })

            # Triggering an update of the selected order.
            cluster_button <- paste0(plot_name, "_", .heatMapCluster)
            observeEvent(input[[cluster_button]], {
                emat <- pObjects$coordinates[[plot_name]]
                new_order <- match(.cluster_genes(emat), names(feature_choices))
                updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices = feature_choices,
                                     server = TRUE, selected = new_order)
            })
        })

        # Saving list-based values.
        for (field in c(.heatMapColData, .heatMapFeatName, .heatMapCenterScale)) {
            local({
                id0 <- id
                mode0 <- "heatMapPlot"
                field0 <- field
                plot_name <- paste0(mode0, id0)
                cur_field <- paste0(plot_name, "_", field0)

                observeEvent(input[[cur_field]], {
                    existing <- pObjects$memory[[mode0]][,field0][[id0]]
                    incoming <- as(input[[cur_field]], typeof(existing))
                    if (identical(incoming, existing)) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, field0, incoming)
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                }, ignoreInit=TRUE, ignoreNULL=(field0==.heatMapFeatName))

                # ignoreNULL necessary for FeatName where updateSelectize generates a temporary NULL;
                # this would trigger re-rendering of the plot upon re-rendering of the UI.
            })
        }

        # Saving other bits and pieces.
        for (field in c(.heatMapAssay, .heatMapLower, .heatMapUpper, .heatMapCenteredColors)) {
            local({
                id0 <- id
                mode0 <- "heatMapPlot"
                field0 <- field
                plot_name <- paste0(mode0, id0)
                cur_field <- paste0(plot_name, "_", field0)

                observeEvent(input[[cur_field]], {
                    matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                    if (identical(input[[cur_field]], pObjects$memory[[mode0]][id0, field0])) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                }, ignoreInit=TRUE)
            })
        }
    }
  } # end of iSEE_server
  #nocov end

  #######################################################################
  # Launching the app.
  #######################################################################

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}
