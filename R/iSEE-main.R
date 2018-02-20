## Not run:
#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' Interactive visualization of single-cell data using a Shiny interface.
#'
#' @param se An object that coercible to \code{\linkS4class{SingleCellExperiment}}.
#' @param redDimArgs A DataFrame similar to that produced by
#' \code{\link{redDimPlotDefaults}}, specifying initial parameters for the plots.
#' @param colDataArgs A DataFrame similar to that produced by \code{\link{colDataPlotDefaults}}, specifying initial parameters for the plots.
#' @param featExprArgs A DataFrame similar to that produced by \code{\link{featExprPlotDefaults}}, specifying initial parameters for the plots.
#' @param rowStatArgs A DataFrame similar to that produced by \code{\link{rowStatTableDefaults}}, specifying initial parameters for the plots.
#' @param rowDataArgs A DataFrame similar to that produced by \code{\link{rowDataPlotDefaults}}, specifying initial parameters for the plots.
#' @param heatMapArgs A DataFrame similar to that produced by \code{\link{heatMapPlotDefaults}}, specifying initial parameters for the plots.
#' @param redDimMax An integer scalar specifying the maximum number of reduced dimension plots in the interface.
#' @param colDataMax An integer scalar specifying the maximum number of column data plots in the interface.
#' @param featExprMax An integer scalar specifying the maximum number of feature expression plots in the interface.
#' @param rowStatMax An integer scalar specifying the maximum number of row statistics tables in the interface.
#' @param rowDataMax An integer scalar specifying the maximum number of row data plots in the interface.
#' @param heatMapMax An integer scalar specifying the maximum number of heatmaps in the interface.
#' @param initialPanels A DataFrame specifying which panels should be created at initialization. 
#' This should contain a \code{Name} character field and a \code{Width} integer field, see Details.
#' @param annot.orgdb An \code{org.*.db} annotation object from which Entrez identifiers can be retrieved.
#' @param annot.keytype A string specifying the keytype to use to query \code{annot.orgdb}.
#' @param annot.keyfield A string specifying the field of \code{rowData(se)} containing the keys of type \code{annot.keytype}. 
#' If \code{NULL}, the row names of \code{se} are used as the keys.
#' @param colormap An \linkS4class{ExperimentColorMap} object that defines custom color maps to apply to individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#' @param run_local A logical indicating whether the app is to be run locally or remotely on a server, which determines how documentation will be accessed.
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
#' If \code{annot.orgdb} is specified, gene information will be retrieved
#' upon selection of particular genes in the data table. No retrieval is
#' performed if \code{annot.orgdb=NULL}.
#'
#' @return A Shiny App is launched for interactive data exploration of the
#' \code{\link{SingleCellExperiment}} / \code{\link{SummarizedExperiment}}
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
#' sce <- runPCA(sce)
#' sce <- runTSNE(sce)
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
  annot.orgdb=NULL,
  annot.keytype="ENTREZID",
  annot.keyfield=NULL,
  colormap=ExperimentColorMap(),
  run_local=TRUE
) {
  # Save the original name of the input object for the command to rename it
  # in the tracker
  se_name <- deparse(substitute(se))
  ecm_name <- deparse(substitute(colormap))

  if (!is(se, "SingleCellExperiment")) {
    se <- as(se, "SummarizedExperiment") # supports ExpressionSet objects
    se <- as(se, "SingleCellExperiment")
    se_name <- sprintf('as(as(%s, "SummarizedExperiment"), "SingleCellExperiment")', se_name)
  }

  # Throw an error if the colormap supplied is not compatible with the object
  isColorMapCompatible(colormap, se, error = TRUE)

  # Setting up inputs for DT::datatable something to play with.
  gene_data <- as.data.frame(rowData(se))
  rownames(gene_data) <- rownames(se)
  if (ncol(gene_data)==0L) {
    gene_data$Present <- !logical(nrow(gene_data))
  }
  tab_brush_col <- "Selected"
  while (tab_brush_col %in% colnames(gene_data)) {
    tab_brush_col <- paste0("_", tab_brush_col)
  }

  # Defining the maximum number of plots.
  memory <- .setup_memory(se, redDimArgs, colDataArgs, featExprArgs, rowStatArgs, rowDataArgs, heatMapArgs,
                          redDimMax, colDataMax, featExprMax, rowStatMax, rowDataMax, heatMapMax)

  # Defining the initial elements to be plotted.
  active_panels <- .setup_initial(initialPanels, memory)
  memory <- .sanitize_memory(active_panels, memory)

  # For retrieving the annotation
  if (!is.null(annot.orgdb)) {
    if (!annot.keytype %in% keytypes(annot.orgdb)) {
      stop("specified keytype not in org.*.db object")
    }
  }

  #######################################################################
  ## UI definition. ----
  #######################################################################

  iSEE_ui <- dashboardPage(
    dashboardHeader(
      title = paste0("iSEE - interactive SingleCell/Summarized Experiment Explorer v",
                     packageVersion("iSEE")),
      titleWidth = 750,

      dropdownMenu(type = "tasks",
                   icon = icon("wrench fa-1g"),
                   badgeStatus = NULL,
                   headerText = "iSEE diagnostics",
                   notificationItem(
                     text = actionButton('open_linkgraph', label="Examine panel chart",
                                         icon = icon("chain"),
                                         style=.actionbutton_biocstyle
                     ),
                     icon = icon(""), status = "primary"
                   ),
                   notificationItem(
                     text = actionButton('getcode_all', label="Extract the R code",
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
                     text = actionButton("tour_firststeps", "Click me for a quick tour", icon("hand-o-right"),
                                         style=.actionbutton_biocstyle),
                     icon = icon(""), # tricking it to not have additional icon
                     status = "primary"),
                   notificationItem(
                     text = actionButton('open_vignette', label="Open the vignette",
                                         icon = icon("book"),
                                         style=.actionbutton_biocstyle,
                                         onclick = ifelse(run_local, "", "window.open('http://google.com', '_blank')")), # to be replaced with vignette url
                     icon = icon(""), status = "primary"
                   )
        ),

        dropdownMenu(type = "tasks",
                    icon = icon("info fa-1g"),
                    badgeStatus = NULL,
                    headerText = "Additional information",
                    notificationItem(
                     text = actionButton('session_info', label="About this session",
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
      actionButton(paste0("redDimPlot_", .organizationNew), "New reduced dimension plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("colDataPlot_", .organizationNew), "New column data plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("featExprPlot_", .organizationNew), "New feature expression plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("rowStatTable_", .organizationNew), "New row statistics table", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("rowDataPlot_", .organizationNew), "New row data plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("heatMapPlot_", .organizationNew), "New heatmap", class = "btn btn-primary",icon = icon("plus")),
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

    pObjects$brush_links <- .spawn_brush_chart(memory)
    pObjects$table_links <- .spawn_table_links(memory)

    namedbools <- logical(length(all_names))
    names(namedbools) <- all_names
    pObjects$no_rerender <-  namedbools
    pObjects$force_rerender <-  namedbools
    pObjects$extra_plot_cmds <- empty_list
    pObjects$cached_plots <- empty_list

    # Storage for all the reactive objects
    rObjects <- reactiveValues(
        active_panels = active_panels,
        rerendered = 1L,
        relinked = 1L
    )

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot", "rowStatTable", "heatMapPlot")) {
      max_plots <- nrow(pObjects$memory[[mode]])
      for (i in seq_len(max_plots)) {
        rObjects[[paste0(mode, i)]] <- 1L
        rObjects[[paste0(mode, i, "_", .panelLinkInfo)]] <- 1L
        rObjects[[paste0(mode, i, "_", .panelGeneralInfo)]] <- 1L
      }
    }

    mode <- "heatMapPlot"
    max_plots <- nrow(pObjects$memory[[mode]])
    for (i in seq_len(max_plots)) {
        rObjects[[paste0(mode, i, "_", .heatMapLegend)]] <- 1L
    }

    # Help and documentation-related observers.
    intro_firststeps <- read.delim(system.file("extdata", "intro_firststeps.txt",package = "iSEE"), sep=";", stringsAsFactors = FALSE,row.names = NULL)

    observeEvent(input$tour_firststeps, {
      introjs(session,
              options = list(steps= intro_firststeps)
      )
    })

    observeEvent(input$getcode_all, {
      showModal(modalDialog(
        title = "My code", size = "l",fade = TRUE,
        footer = NULL, easyClose = TRUE,
        aceEditor("acereport_r", mode="r",theme = "solarized_light",autoComplete = "live",
                  value = paste0((.track_it_all(rObjects, pObjects, se_name, ecm_name)),collapse="\n"),
                  height="600px")
        ))
    })

    observeEvent(input$browseVignette, {
      # browseVignettes("DESeq2") # this does not work, maybe add another open blank to the local location of the vignette?
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
            .snapshot_graph_linkedpanels(rObjects, pObjects)
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
        .panel_organization(rObjects$active_panels, pObjects$memory)
    })


    # Note: we need "local" so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowStatTable", "rowDataPlot", "heatMapPlot")) {
        # Panel addition.
        local({
            mode0 <- mode
            cur_field <- paste0(mode0, "_", .organizationNew)

            observeEvent(input[[cur_field]], {
                all_active <- rObjects$active_panels
                all.memory <- pObjects$memory[[mode0]]
                first.missing <- setdiff(seq_len(nrow(all.memory)), all_active$ID[all_active$Type==mode0])

                if (length(first.missing)) {
                    rObjects$active_panels <- rbind(all_active, DataFrame(Type=mode0, ID=first.missing[1], Width=4L, Height=500L))

                    # Disabling panel addition if we've reached the maximum.
                    if (length(first.missing)==1L) {
                      disable(cur_field)
                    }
                } else {
                    showNotification(sprintf("maximum number of plots reached for mode '%s'", mode0), type="error")
                }
            })
        })

        max_plots <- nrow(pObjects$memory[[mode]])
        for (i in seq_len(max_plots)) {
            local({
                mode0 <- mode
                i0 <- i
                prefix <- paste0(mode0, i0, "_")
                max_plots0 <- max_plots

                # Panel removal.
                observeEvent(input[[paste0(prefix, .organizationDiscard)]], {
                    all_active <- rObjects$active_panels
                    current_type <- all_active$Type==mode0

                    # Re-enabling panel addition if we're decreasing from the maximum.
                    if (sum(current_type)==max_plots0) {
                      enable(paste0(mode0, "_", .organizationNew))
                    }

                    # Destroying links; either the brush source, or the links from tables.
                    if (mode0=="heatMapPlot") {
                        # do nothing, there are no links.
                    } else if (mode0=="rowStatTable") {
                        .destroy_table(pObjects, paste0(mode0, i0))
                    } else {
                        .destroy_brush_source(pObjects, paste0(mode0, i0))
                        .delete_table_links(mode0, i0, pObjects)
                    }

                    # Triggering re-rendering of the UI via change to active_panels.
                    index <- which(current_type & all_active$ID==i0)
                    rObjects$active_panels <- rObjects$active_panels[-index,]
               }, ignoreInit=TRUE)

                # Panel shifting, up and down.
                observeEvent(input[[paste0(prefix, .organizationUp)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    if (index!=1L) {
                        reindex <- seq_len(nrow(all_active))
                        reindex[index] <- reindex[index]-1L
                        reindex[index-1L] <- reindex[index-1L]+1L
                        rObjects$active_panels <- all_active[reindex,]
                    }
                }, ignoreInit=TRUE)

                observeEvent(input[[paste0(prefix, .organizationDown)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
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
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    cur_width <- all_active$Width[index]
                    cur_height <- all_active$Height[index]

                    showModal(modalDialog(
                        sliderInput(paste0(prefix, .organizationWidth), label="Width",
                                    min=width_limits[1], max=width_limits[2], value=cur_width, step=1),
                        sliderInput(paste0(prefix, .organizationHeight), label="Height",
                                    min=height_limits[1], max=height_limits[2], value=cur_height, step=50),
                        title=paste(.decode_panel_name(mode0, i0), "panel parameters"),
                        easyClose=TRUE, size="m", footer=NULL
                        )
                    )
                })

                width_name <- paste0(prefix, .organizationWidth)
                observeEvent(input[[width_name]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    cur.width <- all_active$Width[index]
                    new.width <- input[[width_name]]
                    if (!isTRUE(all.equal(new.width, cur.width))) {
                        rObjects$active_panels$Width[index] <- new.width
                    }
                }, ignoreInit=TRUE)

                height_name <- paste0(prefix, .organizationHeight)
                observeEvent(input[[height_name]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
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
        for (i in seq_len(max_plots)) {
            for (panel in c(.plotParamPanelOpen, .colorParamPanelOpen, .brushParamPanelOpen)) {
                local({
                    mode0 <- mode
                    i0 <- i
                    panel0 <- panel

                    open_field <- paste0(mode0, i0, "_", panel0)
                    observeEvent(input[[open_field]], {
                        pObjects$memory[[mode0]][[panel0]][i0] <- input[[open_field]]
                    })
                })
            }
        }
    }

    # Panel opening/closing observers for heat map plots.
    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (i in seq_len(max_plots)) {
        for (panel in c(.heatMapFeatNamePanelOpen, .heatMapColDataPanelOpen, .heatMapColorPanelOpen)) {
            local({
                mode0 <- "heatMapPlot"
                i0 <- i
                panel0 <- panel

                open_field <- paste0(mode0, i0, "_", panel0)
                observeEvent(input[[open_field]], {
                    pObjects$memory[[mode0]][[panel0]][i0] <- input[[open_field]]
                })
            })
        }
    }

    # Same for the tables.
    max_tabs <- nrow(pObjects$memory$rowStatTable)
    for (i in seq_len(max_tabs)) {
        local({
            mode0 <- "rowStatTable"
            i0 <- i
            tab_name <- paste0(mode0, i0)
            prefix <- paste0(tab_name, "_")

            brush_open_field <- paste0(prefix, .brushParamPanelOpen)
            observeEvent(input[[brush_open_field]], {
                pObjects$memory[[mode0]][[.brushParamPanelOpen]][i0] <- input[[brush_open_field]]
            })
        })
    }

    #######################################################################
    # Brush observers.
    #######################################################################

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (i in seq_len(max_plots)) {
            local({
                mode0 <- mode
                i0 <- i
                plot_name <- paste0(mode0, i0)
                prefix <- paste0(plot_name, "_")

                ###############

                # Brush choice observer.
                brush_plot_field <- paste0(prefix, .brushByPlot)
                observeEvent(input[[brush_plot_field]], {
                    old_transmitter <- pObjects$memory[[mode0]][i0, .brushByPlot]
                    new_transmitter <- input[[brush_plot_field]]

                    # Determining whether the new and old transmitting plot have brushes.
                    old_out <- .transmitted_brush(old_transmitter, pObjects$memory)
                    old_brush <- old_out$brush
                    old_encoded <- old_out$encoded
                    new_out <- .transmitted_brush(new_transmitter, pObjects$memory)
                    new_brush <- new_out$brush
                    new_encoded <- new_out$encoded

                    # Trying to update the graph, but breaking if it's not a DAG.
                    # We also break if users try to self-brush in restrict mode.
                    # In both cases, we just reset back to the choice they had before.
                    tmp <- .choose_new_brush_source(pObjects$brush_links, plot_name, new_encoded, old_encoded)

                    daggy <- is_dag(simplify(tmp, remove.loops=TRUE))
                    self_restrict <- new_encoded==plot_name &&
                        new_encoded!=.noSelection &&
                        pObjects$memory[[mode0]][i0, .brushEffect]==.brushRestrictTitle

                    if (!daggy || self_restrict) {
                        if (!daggy) {
                            showNotification("brushing relationships cannot be cyclic", type="error")
                        } else if (self_restrict){
                            showNotification("brushing to self is not compatible with 'Restrict'", type="error")
                        }
                        pObjects$memory[[mode0]][i0, .brushByPlot] <- old_transmitter
                        updateSelectInput(session, brush_plot_field, selected=old_transmitter)
                        return(NULL)
                    }

                    pObjects$brush_links <- tmp
                    pObjects$memory[[mode0]][i0, .brushByPlot] <- new_transmitter

                    # Update the elements reporting the links between plots.
                    for (relinked in c(old_encoded, new_encoded, plot_name)) {
                        if (relinked==.noSelection) { next }
                        relink_field <- paste0(relinked, "_", .panelLinkInfo)
                        rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                    }

                    # Not replotting if there were no brushes in either the new or old transmitters.
                    if (!old_brush && !new_brush){
                        return(NULL)
                    }

                    # Triggering self update of the plot.
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                    # Triggering replotting of children, if the current panel is set to restrict;
                    # and we have a brush, so that there was already some brushing in the children.
                    if (pObjects$memory[[mode0]][i0, .brushEffect]==.brushRestrictTitle
                        && .any_point_selection(mode0, i0, pObjects$memory)) {
                        children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
                        for (child_plot in children) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                        }
                    }
                }, ignoreInit=TRUE)

                ###############

                # Brush effect observer.
                brush_effect_field <- paste0(prefix, .brushEffect)
                observeEvent(input[[brush_effect_field]], {
                    cur_effect <- input[[brush_effect_field]]
                    old_effect <- pObjects$memory[[mode0]][i0, .brushEffect]

                    # Storing the new choice into memory, unless self-brushing to restrict.
                    # In which case, we trigger an error and reset to the previous choice.
                    if (cur_effect == .brushRestrictTitle
                        && pObjects$memory[[mode0]][i0, .brushByPlot]==.decode_panel_name(mode0, i0)) {
                        showNotification("brushing to self is not compatible with 'Restrict'", type="error")
                        updateRadioButtons(session, brush_effect_field, selected=old_effect)
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][i0, .brushEffect] <- cur_effect

                    # Avoiding replotting if there was no transmitting brush.
                    transmitter <- pObjects$memory[[mode0]][i0, .brushByPlot]
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
                    # and we have a brush, so there was already some brushing in the children.
                    if ((cur_effect==.brushRestrictTitle || old_effect==.brushRestrictTitle)
                        && .any_point_selection(mode0, i0, pObjects$memory)) {
                        children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
                        for (child_plot in children) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                        }
                    }
                }, ignoreInit=TRUE)

                ###############

                # Brush structure observers.
                brush_id <- paste0(prefix, .brushField)
                observeEvent(input[[brush_id]], {
                    cur_brush <- input[[brush_id]]
                    old_brush <- pObjects$memory[[mode0]][,.brushData][[i0]]
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .brushData, cur_brush)

                    # If the brushes have the same coordinates, we don't bother replotting.
                    replot <- !.identical_brushes(cur_brush, old_brush)

                    # Destroying lasso points upon brush (replotting if existing lasso was not NULL).
                    replot <- replot || !is.null(pObjects$memory[[mode0]][,.lassoData][[i0]])
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .lassoData, NULL)
                    if (!replot) {
                        return(NULL)
                    }

                    # Trigger replotting of self, to draw a more persistent brushing box.
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    pObjects$no_rerender[plot_name] <- TRUE

                    # Trigger replotting of all dependent plots that receive this brush.
                    children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
                    for (child_plot in children) {
                        rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                    }
                }, ignoreInit=TRUE, ignoreNULL=FALSE)
            })
        }
    }

    # Brush choice observers for the tables.
    max_tabs <- nrow(pObjects$memory$rowStatTable)
    for (i in seq_len(max_tabs)) {
        local({
            mode0 <- "rowStatTable"
            i0 <- i
            tab_name <- paste0(mode0, i0)
            prefix <- paste0(tab_name, "_")

            brush_open_field <- paste0(prefix, .brushParamPanelOpen)
            observeEvent(input[[brush_open_field]], {
                pObjects$memory[[mode0]][[.brushParamPanelOpen]][i0] <- input[[brush_open_field]]
            })

            brush_plot_field <- paste0(prefix, .brushByPlot)
            observeEvent(input[[brush_plot_field]], {
                old_transmitter <- pObjects$memory[[mode0]][i0, .brushByPlot]
                new_transmitter <- input[[brush_plot_field]]

                # Determining whether the new and old transmitting plot have brushes.
                old_out <- .transmitted_brush(old_transmitter, pObjects$memory)
                old_brush <- old_out$brush
                old_encoded <- old_out$encoded
                new_out <- .transmitted_brush(new_transmitter, pObjects$memory)
                new_brush <- new_out$brush
                new_encoded <- new_out$encoded

                # Updating the graph (no need for DAG protection here, as tables do not transmit brushes).
                pObjects$brush_links <- .choose_new_brush_source(pObjects$brush_links, tab_name, new_encoded, old_encoded)
                pObjects$memory[[mode0]][i0, .brushByPlot] <- new_transmitter

                # Update the elements reporting the links between plots.
                for (relinked in c(old_encoded, new_encoded, tab_name)) {
                    if (relinked==.noSelection) { next }
                    relink_field <- paste0(relinked, "_", .panelLinkInfo)
                    rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                }

                # Not re-rendering if there were no brushes in either the new or old transmitters.
                if (!old_brush && !new_brush){
                    return(NULL)
                }

                # Triggering update of the table.
                rObjects[[tab_name]] <- .increment_counter(isolate(rObjects[[tab_name]]))
            }, ignoreInit=TRUE)
        })
    }

    #######################################################################
    # Click observers.
    #######################################################################

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (i in seq_len(max_plots)) {
            local({
                mode0 <- mode
                i0 <- i
                plot_name <- paste0(mode0, i0)
                prefix <- paste0(plot_name, "_")

                click_field <- paste0(prefix, .lassoClick)
                observeEvent(input[[click_field]], {
                    cur_click <- input[[click_field]]
                    previous <- pObjects$memory[[mode0]][,.lassoData][[i0]]
                    bump_children <- FALSE

                    # Don't add to waypoints if a brush exists in memory (as they are mutually exclusive).
                    if (!is.null(pObjects$memory[[mode0]][,.brushData][[i0]]) ||
                        !is.null(input[[paste0(prefix, .brushField)]])) {
                        return(NULL)
                    }

                    # Closing the loop if you click close to the starting point.
                    xrange <- cur_click$domain$right - cur_click$domain$left
                    yrange <- cur_click$domain$top - cur_click$domain$bottom
                    if (!is.null(previous)
                        && abs(cur_click$x - previous[1,1]) < xrange/100
                        && abs(cur_click$y - previous[1,2]) < yrange/100) {
                        updated <- rbind(previous, previous[1,])
                        attr(updated, "closed") <- TRUE
                        bump_children <- TRUE

                        # Checking out whether it's flipped.
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

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .lassoData, updated)

                    # Trigger replotting of self, to draw the lasso waypoints.
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    pObjects$no_rerender[plot_name] <- TRUE

                    # Trigger replotting of children.
                    if (bump_children) {
                        children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
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
    # - If you double-click on a brush, you zoom it while wiping the brush.
    # - If you double-click outside a brush, you wipe the brush (this is done automatically).
    #   If an open lasso is present, it is deleted.
    #   If there was no open lasso, you zoom out.

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (i in seq_len(max_plots)) {
            local({
                mode0 <- mode
                i0 <- i
                plot_name <- paste0(mode0, i0)
                prefix <- paste0(plot_name, "_")
                brush_id <- paste0(prefix, .brushField)

                zoom_click_field <- paste0(mode0, i0, "_", .zoomClick)
                observeEvent(input[[zoom_click_field]], {
                    brush <- input[[brush_id]]

                    if (!is.null(brush)) {
                        new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
                        session$resetBrush(brush_id) # This should auto-trigger replotting above.
                        pObjects$force_rerender[plot_name] <- TRUE
                    } else {
                        # Brush is already NULL at this point, so there is no need to reset it.
                        # However, we do need to manually trigger replotting. We don't move this outside the
                        # "else", to avoid two reactive updates of unknown priorities.
                        new_coords <- pObjects$memory[[mode0]][,.zoomData][[i0]]

                        lasso_data <- pObjects$memory[[mode0]][,.lassoData][[i0]]
                        if (!is.null(lasso_data)) {
                            # We wipe out any lasso waypoints if they are present, and trigger replotting with the same scope.
                            pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .lassoData, NULL)
                            pObjects$no_rerender[plot_name] <- TRUE
                            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                        } else {
                            if (!is.null(new_coords)) {
                                # If there are already no lasso waypoints, we zoom out.
                                new_coords <- NULL
                                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                            }
                        }
                    }

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .zoomData, new_coords)
                })
            })
        }
    }

    #######################################################################
    # Selectize updators. ----
    #######################################################################

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    max_plots <- nrow(pObjects$memory$featExprPlot)
    for (i in seq_len(max_plots)) {
        for (axis in c("xaxis", "yaxis")) {
            if (axis=="xaxis") {
                axis_name_choice <- .featExprYAxisFeatName
            } else {
                axis_name_choice <- .featExprXAxisFeatName
            }

            local({
                i0 <- i
                mode0 <- "featExprPlot"
                field0 <- axis_name_choice
                cur_field <- paste0(mode0, i0, "_", field0)

                observe({
                    force(rObjects$rerendered)

                    updateSelectizeInput(session, cur_field, label = NULL, choices = feature_choices, server = TRUE,
                                         selected = pObjects$memory[[mode0]][i0, field0][[1]])
                }, priority=-1) # Lower priority so that it executes AFTER the UI rerender.
            })
        }
    }

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (i in seq_len(max_plots)) {
            local({
                i0 <- i
                mode0 <- mode
                field0 <- .colorByFeatName
                cur_field <- paste0(mode0, i0, "_", field0)

                observe({
                    force(rObjects$rerendered)
                    updateSelectizeInput(session, cur_field, label = NULL, choices = feature_choices, server = TRUE,
                                         selected = pObjects$memory[[mode0]][i0, field0][[1]])
                }, priority=-1) # Lower priority so that it executes AFTER the UI rerender.
            })
        }
    }

    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (i in seq_len(max_plots)) {
        local({
            i0 <- i
            mode0 <- "heatMapPlot"

            observe({
                force(rObjects$rerendered)
                updateSelectizeInput(session, paste0(mode0, i0, "_", .heatMapFeatName), choices = feature_choices,
                                     server = TRUE, selected = pObjects$memory[[mode0]][i0, .heatMapFeatName][[1]])
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

        # Defining fundamental parameters that destroy brushes upon being changed.
        protected <- switch(mode,
                            redDimPlot=c(.redDimType, .redDimXAxis, .redDimYAxis),
                            colDataPlot=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
                            featExprPlot=c(.featExprAssay, .featExprXAxisColData, .featExprYAxisFeatName, .featExprXAxisFeatName),
                            rowDataPlot=c(.rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData))

        # Defining non-fundamental parameters that do not destroy brushes.
        if (mode=="rowDataPlot") {
            nonfundamental <- c(.colorByRowData, .colorByRowTableColor, .colorByFeatNameColor)
        } else {
            nonfundamental <- c(.colorByColData, .colorByRowTableAssay, .colorByFeatNameAssay)
        }
        nonfundamental <- c(nonfundamental, .colorByFeatName, .brushColor, .brushTransAlpha)

        for (i in seq_len(max_plots)) {
            # Observers for the non-fundamental parameter options.
            for (field in nonfundamental) {
                local({
                    i0 <- i
                    mode0 <- mode
                    field0 <- field
                    plot_name <- paste0(mode0, i0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        req(input[[cur_field]])
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[field0]][i0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[field0]][i0] <- matched_input
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }, ignoreInit=TRUE)
                })
            }

            # Observers for the fundamental plot parameters.
            for (field in protected) {
                local({
                    i0 <- i
                    mode0 <- mode
                    field0 <- field
                    plot_name <- paste0(mode0, i0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        req(input[[cur_field]])
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[field0]][i0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[field0]][i0] <- matched_input
                        .regenerate_unselected_plot(mode0, i0, pObjects, rObjects, session, input)
                     }, ignoreInit=TRUE, priority=-2) # executes AFTER the update selectize.
                })
            }

            local({
                i0 <- i
                mode0 <- mode
                FUN0 <- FUN
                plot_name <- paste0(mode0, i0)

                # Observers for the linked color, which updates the table_links information.
                observe({
                    old_tab <- pObjects$memory[[mode0]][i0, .colorByRowTable]

                    replot <- .setup_table_observer(mode0, i0, input, pObjects, .colorByField,
                        .colorByRowTableTitle, .colorByRowTable, param='color')
                    if (replot) {
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }

                    # Update the elements reporting the links between tables and plots.
                    new_tab <- pObjects$memory[[mode0]][i0, .colorByRowTable]
                    for (relinked in c(plot_name, .decoded2encoded(c(new_tab, old_tab)))) {
                        if (relinked=="") { next }
                        relink_field <- paste0(relinked, "_", .panelLinkInfo)
                        rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                    }
                })

                # Defining the rendered plot, and saving the coordinates.
                gen_field <- paste0(plot_name, "_", .panelGeneralInfo)

                output[[plot_name]] <- renderPlot({
                    force(rObjects[[plot_name]])
                    rObjects[[gen_field]] <- .increment_counter(isolate(rObjects[[gen_field]]))

                    # Deciding whether to do a full re-render, or to add to the cached object.
                    if (!pObjects$no_rerender[plot_name] || pObjects$force_rerender[plot_name]) {
                        p.out <- FUN0(i0, pObjects$memory, pObjects$coordinates, se, colormap)
                        gg <- p.out$plot
                        pObjects$cached_plots[[plot_name]] <- gg
                        pObjects$commands[[plot_name]] <- p.out$cmd
                        pObjects$coordinates[[plot_name]] <- p.out$xy[,c("X", "Y")]
                    } else {
                        gg <- pObjects$cached_plots[[plot_name]]
                    }

                    # Resetting flags.
                    pObjects$force_rerender[plot_name] <- FALSE
                    pObjects$no_rerender[plot_name] <- FALSE

                    extra_cmds <- list()
                    to_flip <- is(gg$coordinates, "CoordFlip") # Add a test for this!
                    brush_out <- .self_brush_box(mode0, i0, pObjects$memory, flip=to_flip) # Adding a brush.
                    extra_cmds[["brush_box"]] <- brush_out$cmd
                    lasso_out <- .self_lasso_path(mode0, i0, pObjects$memory, flip=to_flip) # Adding the lasso path.
                    extra_cmds[["lasso_path"]] <- lasso_out$cmd
                    extra_cmds <- unlist(extra_cmds)

                    if (length(extra_cmds) > 0L) {
                        cur.env <- new.env()
                        cur.env$all_brushes <- brush_out$data
                        cur.env$all_lassos <- lasso_out$data

                        for (cmd in extra_cmds) {
                            gg <- gg + eval(parse(text=cmd), envir=cur.env)
                        }
                    }
                    pObjects$extra_plot_cmds[[plot_name]] <- extra_cmds
                    return(gg)
                })

                # Describing some general panel information.
                output[[gen_field]] <- renderUI({
                    force(rObjects[[gen_field]])
                    brush_vals <- pObjects$memory[[mode0]][,.brushData][[i0]]
                    if (is.null(brush_vals)) {
                        return(NULL)
                    }
                    brushed <- brushedPoints(pObjects$coordinates[[plot_name]], brush_vals)
                    n_brushed <- nrow(brushed)
                    n_total <- nrow(pObjects$coordinates[[plot_name]])
                    HTML(sprintf("%i of %i points brushed (%.1f%%)",
                                 n_brushed, n_total, 100*n_brushed/n_total))
                })

                # Describing the links between panels.
                link_field <- paste0(plot_name, "_", .panelLinkInfo)
                output[[link_field]] <- renderUI({
                    force(rObjects[[link_field]])
                    .define_plot_links(plot_name, pObjects$memory, pObjects$brush_links)
                })
            })
        }
    }

    # Feature expression plots need some careful handling, as we need to update the
    # table links and destroy a brush whenever an x/y-axis-specifying parameter changes.
    max_plots <- nrow(pObjects$memory$featExprPlot)
    for (i in seq_len(max_plots)) {
        for (axis in c("xaxis", "yaxis")) {
            if (axis=="xaxis") {
                axis_choice <- .featExprYAxis
                axis_tab_title <- .featExprYAxisRowTableTitle
                axis_tab_choice <- .featExprYAxisRowTable
            } else {
                axis_choice <- .featExprXAxis
                axis_tab_title <- .featExprXAxisRowTableTitle
                axis_tab_choice <- .featExprXAxisRowTable
            }

            local({
                i0 <- i
                mode0 <- "featExprPlot"
                plot_name <- paste0(mode0, i0)

                axis0 <- axis
                axis_choice0 <- axis_choice
                axis_tab_title0 <- axis_tab_title
                axis_tab_choice0 <- axis_tab_choice

                observe({
                    old_tab <- pObjects$memory[[mode0]][i0, axis_tab_choice0]

                    ## Deciding whether to replot based on the table.
                    replot <- .setup_table_observer(mode0, i0, input, pObjects, axis_choice0, axis_tab_title0, axis_tab_choice0, param=axis0)
                    if (replot) {
                        .regenerate_unselected_plot(mode0, i0, pObjects, rObjects, session, input)
                    }

                    # Update the links reporting between tables and plots.
                    new_tab <- pObjects$memory[[mode0]][i0, axis_tab_choice0]
                    for (relinked in c(plot_name, .decoded2encoded(c(old_tab, new_tab)))) {
                        if (relinked=="") { next }
                        relink_field <- paste0(relinked, "_", .panelLinkInfo)
                        rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                    }
                })
            })
        }
    }

    #######################################################################
    # Row table section. ----
    #######################################################################

    # Load the gene level data
    for (i in seq_len(nrow(memory$rowStatTable))) {
      local({
        i0 <- i
        panel_name <- paste0("rowStatTable", i0)

        output[[panel_name]] <- renderDataTable({
            force(rObjects$active_panels) # to trigger recreation when the number of plots is changed.
            force(rObjects[[panel_name]])

            chosen <- pObjects$memory$rowStatTable[i0, .rowStatSelected]
            search <- pObjects$memory$rowStatTable[i0, .rowStatSearch]
            search_col <- pObjects$memory$rowStatTable[,.rowStatColSearch][[i0]]
            search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

            # Adding a "Selected" field to the plotting data, which responds to brushing input.
            # Note that this AUTOMATICALLY updates search_col upon re-rendering via the observer below.
            # The code below keeps search_col valid for the number of columns (i.e., with or wo selection).
            selected <- .process_brushby_choice(pObjects$memory$rowStatTable[i0,], pObjects$memory)
            tmp_gene_data <- gene_data
            if (!is.null(selected$cmd)) { 
                chosen.env <- new.env()
                chosen.env$plot.data <- gene_data 
                chosen.env$all_coordinates <- pObjects$coordinates
                chosen.env$all_brushes <- selected$data
                chosen.env$all_lassos <- selected$data
                eval(parse(text=selected$cmd), envir=chosen.env)

                tmp_gene_data[[tab_brush_col]] <- chosen.env$plot.data$BrushBy
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
            pObjects$memory$rowStatTable[i0, .rowStatSelected] <- chosen

            col_kids <- unique(unlist(pObjects$table_links[[i0]][c("color")]))
            xy_kids <- unique(unlist(pObjects$table_links[[i0]][c("xaxis", "yaxis")]))

            # Triggering the replotting of all color children that are NOT xy children.
            col_kids <- setdiff(col_kids, xy_kids)
            for (kid in col_kids) {
                rObjects[[kid]] <- .increment_counter(isolate(rObjects[[kid]]))
            }

            # Triggering the replotting and brush clearing of all x/y-axis children.
            enc <- .split_encoded(xy_kids)
            for (i in seq_along(xy_kids)) {
                .regenerate_unselected_plot(enc$Type[i], enc$ID[i], pObjects, rObjects, session, input)
            }
        })

        # Updating memory for new selection parameters.
        search_field <- paste0(panel_name, .int_rowStatSearch)
        observe({
            search <- input[[search_field]]
            if (length(search)) {
                pObjects$memory$rowStatTable[i0, .rowStatSearch] <- search
            }
        })

        colsearch_field <- paste0(panel_name, .int_rowStatColSearch)
        observe({
            search <- input[[colsearch_field]]
            if (length(search)) {
                pObjects$memory$rowStatTable <- .update_list_element(
                    pObjects$memory$rowStatTable, i0, .rowStatColSearch, search)
            }
        })

        # Updating the annotation box.
        anno_field <- paste0(panel_name, "_annotation")
        output[[anno_field]] <- renderUI({
            chosen <- input[[select_field]]
            .generate_annotation(annot.orgdb, annot.keytype, annot.keyfield,
                                 gene_data, chosen)
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
    for (i in seq_len(max_plots)) {
        local({
            mode0 <- "heatMapPlot"
            i0 <- i
            plot_name <- paste0(mode0, i0)

            # Triggering an update of the selected elements.
            import_button <- paste0(plot_name, "_", .heatMapImport)
            observeEvent(input[[import_button]], {
                origin <- pObjects$memory[[mode0]][i0, .heatMapImportSource]
                enc <- .encode_panel_name(origin)

                incoming <- NULL
                if (enc$Type=="rowStatTable") {
                    incoming <- input[[paste0(enc$Type, enc$ID, "_rows_all")]]
                } else {
                    brush <- pObjects$memory[[enc$Type]][,.brushData][[enc$ID]]
                    if (!is.null(brush)) {
                        incoming <- brushedPoints(pObjects$coordinates[[paste0(enc$Type, enc$ID)]], brush)
                        incoming <- match(rownames(incoming), rownames(se))
                    }
                }

                limit <- 100
                if (length(incoming) > limit) {
                    showNotification(sprintf("only the first %i features used", limit), type="warning")
                    incoming <- utils::head(incoming, limit)
                }

                combined <- union(pObjects$memory[[mode0]][i0, .heatMapFeatName][[1]], incoming)
                updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices = feature_choices,
                                     server = TRUE, selected = combined)
            }, ignoreInit=TRUE)

            # Updating the import source, but this does NOT trigger replotting, as we need to press the button.
            field0 <- .heatMapImportSource
            cur_field <- paste0(plot_name, "_", field0)
            observeEvent(input[[cur_field]], {
                req(input[[cur_field]])
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                if (identical(input[[cur_field]], pObjects$memory[[mode0]][i0, field0])) {
                    return(NULL)
                }
                pObjects$memory[[mode0]][[field0]][i0] <- matched_input
            }, ignoreInit=TRUE)

            # Defining the rendered plot, and saving the coordinates.
            # Also triggering an update to the accompanying legend plot.
            legend_field <- paste0(plot_name, "_", .heatMapLegend)
            output[[plot_name]] <- renderPlot({
                force(rObjects[[plot_name]])
                rObjects[[legend_field]] <- .increment_counter(isolate(rObjects[[legend_field]]))

                p.out <- .make_heatMapPlot(i0, pObjects$memory, pObjects$coordinates, se, colormap)
                pObjects$commands[[plot_name]] <- p.out$cmd
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

            # Triggering an update of the selected order.
            cluster_button <- paste0(plot_name, "_", .heatMapCluster)
            observeEvent(input[[cluster_button]], {
                emat <- pObjects$coordinates[[plot_name]]
                new_order <- match(.cluster_genes(emat), names(feature_choices))
                updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices = feature_choices,
                                     server = TRUE, selected = new_order)
            })
        })

        # Saving list-based coordinates.
        for (field in c(.heatMapColData, .heatMapFeatName)) {
            local({
                i0 <- i
                mode0 <- "heatMapPlot"
                field0 <- field
                plot_name <- paste0(mode0, i0)
                cur_field <- paste0(plot_name, "_", field0)

                observeEvent(input[[cur_field]], {
                    req(input[[cur_field]])
                    existing <- pObjects$memory[[mode0]][,field0][[i0]]
                    incoming <- as(input[[cur_field]], typeof(existing))
                    if (identical(incoming, existing)) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, field0, incoming)
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                }, ignoreInit=TRUE)
            })
        }

        # Saving other bits and pieces.
        for (field in c(.heatMapAssay, .heatMapCentering, .heatMapScaling, .heatMapLower, .heatMapUpper, .heatMapCenteredColors)) {
            local({
                i0 <- i
                mode0 <- "heatMapPlot"
                field0 <- field
                plot_name <- paste0(mode0, i0)
                cur_field <- paste0(plot_name, "_", field0)

                observeEvent(input[[cur_field]], {
                    req(input[[cur_field]])
                    matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                    if (identical(input[[cur_field]], pObjects$memory[[mode0]][i0, field0])) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[field0]][i0] <- matched_input
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                }, ignoreInit=TRUE)
            })
        }
    }
  } # end of iSEE_server

  #######################################################################
  # Launching the app.
  #######################################################################

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}
