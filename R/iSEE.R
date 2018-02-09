## Not run:
#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' Interactive visualization of single-cell data using a Shiny interface.
#'
#' @param se An object that coercible to \code{\linkS4class{SingleCellExperiment}}.
#' @param redDimArgs A DataFrame similar to that produced by
#' \code{\link{redDimPlotDefaults}}, specifying initial parameters for the plots.
#' @param colDataArgs A DataFrame similar to that produced by
#' \code{\link{colDataPlotDefaults}}, specifying initial parameters for the plots.
#' @param featExprArgs A DataFrame similar to that produced by
#' \code{\link{featExprPlotDefaults}}, specifying initial parameters for the plots.
#' @param rowStatArgs A DataFrame similar to that produced by
#' \code{\link{rowStatTableDefaults}}, specifying initial parameters for the plots.
#' @param redDimMax An integer scalar specifying the maximum number of reduced
#' dimension plots in the interface. 
#' @param colDataMax An integer scalar specifying the maximum number of column
#' data plots in the interface. 
#' @param featExprMax An integer scalar specifying the maximum number of gene
#' expression plots in the interface. 
#' @param rowStatMax An integer scalar specifying the maximum number of gene
#' statistic tables in the interface. 
#' @param initialPanels A DataFrame specifying which panels should be created
#' at initialization. This should contain a \code{Name} character field and a
#' \code{Width} integer field, see Details.
#' @param annot.orgdb An \code{org.*.db} annotation object from which
#' Entrez identifiers can be retrieved.
#' @param annot.keytype A string specifying the keytype to use to query
#' \code{annot.orgdb}.
#' @param annot.keyfield A string specifying the field of \code{rowData(se)}
#' containing the keys of type \code{annot.keytype}. If \code{NULL}, the
#' row names of \code{se} are used as the keys.
#' @param colormap An \linkS4class{ExperimentColorMap} object that defines
#' custom color maps to apply to individual \code{assays}, \code{colData},
#' and \code{rowData} covariates.
#' @param run_local A logical indicating whether the app is to be run
#' locally or remotely on a server, which determines how documentation 
#' will be accessed.
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
  redDimMax=5,
  colDataMax=5,
  featExprMax=5,
  rowStatMax=5,
  rowDataMax=5,
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
  # It must have some columns, so we're just filling it up with _something_.
  gene_data <- as.data.frame(rowData(se))
  rownames(gene_data) <- rownames(se) 
  if (ncol(gene_data)==0L && nrow(gene_data)){ 
    gene_data$Present <- TRUE
  }

  # Defining the maximum number of plots.
  memory <- .setup_memory(se, redDimArgs, colDataArgs, featExprArgs, rowStatArgs, rowDataArgs,
                          redDimMax, colDataMax, featExprMax, rowStatMax, rowDataMax)

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
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
                     ),
                     icon = icon(""), status = "primary"
                   ),
                   notificationItem(
                     text = actionButton('getcode_all', label="Extract the R code",
                                         icon = icon("magic"),
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
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
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
                     icon = icon(""), # tricking it to not have additional icon
                     status = "primary"),
                   notificationItem(
                     text = actionButton('open_vignette', label="Open the vignette", 
                                         icon = icon("book"), 
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4",
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
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
                                         ),
                     icon = icon(""), status = "primary"
                   ),
                   notificationItem(
                     text = actionButton('iSEE_info', label="About iSEE", 
                                         icon = icon("heart"), 
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
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
      hr(),
      uiOutput("panelOrganization")
    ), # end of dashboardSidebar

    dashboardBody(
      useShinyjs(), 
      introjsUI(), # must be included in UI

      # for error message handling
      tags$head(
        tags$style(HTML("
                        .shiny-output-error-validation {
                        font-size: 15px;
                        color: forestgreen;
                        text-align: center;
                        }
                        "))
      ),

      uiOutput("allPanels")
    ), # end of dashboardBody
    skin = "blue"
  ) # end of dashboardPage

  #######################################################################
  ## Server definition. ----
  #######################################################################

  iSEE_server <- function(input, output, session) {

    # Storage for other persistent objects
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$coordinates <- list()
    pObjects$commands <- list()
    pObjects$brush_links <- .spawn_brush_chart(memory) 
    pObjects$table_links <- .spawn_table_links(memory)

    # Storage for all the reactive objects
    rObjects <- reactiveValues(
        active_panels = active_panels
    )
    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowDataPlot")) {
      max_plots <- nrow(pObjects$memory[[mode]])
      for (i in seq_len(max_plots)) {
        rObjects[[paste0(mode, i)]] <- 1L
      }
    }

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
             iSEE_info(), br(), br(),
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
        .panel_organization(rObjects$active_panels, pObjects$memory)
    })


    # Note: we need "local" so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot", "rowStatTable", "rowDataPlot")) {
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
                    if (mode0=="rowStatTable") {
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
    # Panel and brush observers.
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

          # Panel opening/closing observers.
          plot_open_field <- paste0(prefix, .plotParamPanelOpen)
          observeEvent(input[[plot_open_field]], {
            pObjects$memory[[mode0]][[.plotParamPanelOpen]][i0] <- input[[plot_open_field]]
          })

          col_open_field <- paste0(prefix, .colorParamPanelOpen)
          observeEvent(input[[col_open_field]], {
            pObjects$memory[[mode0]][[.colorParamPanelOpen]][i0] <- input[[col_open_field]]
          })

          brush_open_field <- paste0(prefix, .brushParamPanelOpen)
          observeEvent(input[[brush_open_field]], {
            pObjects$memory[[mode0]][[.brushParamPanelOpen]][i0] <- input[[brush_open_field]]
          })

          ###############

          # Brush choice observer. This will fail with an error message if there are cycles
          # across multiple plots. Otherwise it will update the brushing chart.
          brush_plot_field <- paste0(prefix, .brushByPlot) 
          observeEvent(input[[brush_plot_field]], {
            old_transmitter <- pObjects$memory[[mode0]][i0, .brushByPlot]
            new_transmitter <- input[[brush_plot_field]]

            # Determining whether the new and old transmitting plot have brushes.
            old_brush <- new_brush <- FALSE 
            old_encoded <- new_encoded <- ""    
            if (old_transmitter!="") {
              old_enc <- .encode_panel_name(old_transmitter)
              old_encoded <- paste0(old_enc$Type, old_enc$ID)
              if (!is.null(pObjects$memory[[old_enc$Type]][old_enc$ID, .brushData][[1]])) {
                old_brush <- TRUE
              }
            }
            if (new_transmitter!="") {
              new_enc <- .encode_panel_name(new_transmitter)
              new_encoded <- paste0(new_enc$Type, new_enc$ID)
              if (!is.null(pObjects$memory[[new_enc$Type]][new_enc$ID, .brushData][[1]])) {
                new_brush <- TRUE
              }
            }

            # Updating the graph, but breaking if it's not a DAG.
            tmp <- .choose_new_brush_source(pObjects$brush_links, plot_name, new_encoded, old_encoded)
            daggy <- is_dag(simplify(tmp, remove.loops=TRUE)) 
            if (!daggy) {
              showNotification("brushing relationships cannot be cyclic", type="error")
              pObjects$memory[[mode0]][i0, .brushByPlot] <- ""
              updateSelectInput(session, brush_plot_field, selected="")
            } else {
              pObjects$brush_links <- tmp
              pObjects$memory[[mode0]][i0, .brushByPlot] <- new_transmitter
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
                && !is.null(pObjects$memory[[mode0]][i0, .brushData][[1]])) {
              children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
              for (child_plot in children) {
                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
              }
            }
          }, ignoreInit=TRUE)

          # Brush effect observer.
          brush_effect_field <- paste0(prefix, .brushEffect) 
          observeEvent(input[[brush_effect_field]], {
            cur_effect <- input[[brush_effect_field]]
            old_effect <- pObjects$memory[[mode0]][i0, .brushEffect] 
            pObjects$memory[[mode0]][i0, .brushEffect] <- cur_effect
            
            # Avoiding replotting if there was no transmitting brush.
            transmitter <- pObjects$memory[[mode0]][i0, .brushByPlot]
            if (transmitter=="") {
              return(NULL)
            }
            enc <- .encode_panel_name(transmitter)
            if (is.null(pObjects$memory[[enc$Type]][enc$ID, .brushData][[1]])) {
              return(NULL)
            }

            # Triggering self update.
            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

            # Triggering replotting of children, if we are set to or from restrict;
            # and we have a brush, so there was already some brushing in the children.
            if ((cur_effect==.brushRestrictTitle || old_effect==.brushRestrictTitle) 
                && !is.null(pObjects$memory[[mode0]][i0, .brushData][[1]])) {
              children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
              for (child_plot in children) {
                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
              }
            }
          }, ignoreInit=TRUE)

          # Brush structure observers.
          brush_id <- paste0(prefix, .brushField)
          observeEvent(input[[brush_id]], {
            cur_brush <- input[[brush_id]]
            old_brush <- pObjects$memory[[mode0]][,.brushData][[i0]]

            # If it is rebrushing itself in restrict mode, we take the intersection of brushed regions.
            if (!is.null(cur_brush) 
                && pObjects$memory[[mode0]][i0, .brushEffect]==.brushRestrictTitle
                && plot_name==.decoded2encoded(pObjects$memory[[mode0]][i0, .brushByPlot])) {
                cur_brush$xmin <- max(cur_brush$xmin, old_brush$xmin)
                cur_brush$xmax <- min(cur_brush$xmax, old_brush$xmax)
                cur_brush$ymin <- max(cur_brush$ymin, old_brush$ymin)
                cur_brush$ymax <- min(cur_brush$ymax, old_brush$ymax)
            }

            pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .brushData, cur_brush)

            # If the brushes have the same coordinates, we don't bother replotting.
            if (.identical_brushes(cur_brush, old_brush)) {
                return(NULL)
            }

            # Trigger replotting of self, to draw a more persistent brushing box.
            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

            # Trigger replotting of all dependent plots that receive this brush.
            children <- .get_brush_dependents(pObjects$brush_links, plot_name, pObjects$memory)
            for (child_plot in children) {
              rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
            }
          }, ignoreInit=TRUE, ignoreNULL=FALSE)

          ###############

          # Double-click observers.
          zoom_click_field <- paste0(mode0, i0, "_", .zoomClick)
          observeEvent(input[[zoom_click_field]], {
             brush <- input[[brush_id]]

             if (!is.null(brush)) {
               new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
               session$resetBrush(brush_id) # This should auto-trigger replotting above.
             } else {
               new_coords <- NULL
               
               # Brush is already NULL at this point, so resetting it wouldn't help; 
               # we need to manually trigger replotting. We don't move this outside the
               # "else", to avoid two reactive updates of unknown priorities.
               rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
             }

             pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .zoomData, new_coords)
          })
        })
      }
    }

    #######################################################################
    # Plot creation section. ----
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
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
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
                    brush_id <- paste0(plot_name, "_", .brushField)
    
                    observeEvent(input[[cur_field]], {
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        pObjects$memory[[mode0]][[field0]][i0] <- matched_input                
                        
                        if (!is.null(isolate(input[[brush_id]]))) { 
                            # This will trigger replotting via the brush observer above.
                            session$resetBrush(brush_id) 
                        } else { 
                            # Manually triggering replotting.
                            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                        }
                     }, ignoreInit=TRUE)
                })
            }
  
            local({
                i0 <- i
                mode0 <- mode
                FUN0 <- FUN
                plot_name <- paste0(mode0, i0)
  
                # Observers for the linked color, which updates the table_links information.
                observe({
                    replot <- .setup_table_observer(mode0, i0, input, pObjects, .colorByField, 
                        .colorByRowTableTitle, .colorByRowTable, param='color') 
                    if (replot) {
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }
                })
  
                # Defining the rendered plot, and saving the coordinates.
                output[[plot_name]] <- renderPlot({
                    force(rObjects[[plot_name]])
                    p.out <- FUN0(i0, pObjects$memory, pObjects$coordinates, se, colormap)
                    pObjects$commands[[plot_name]] <- p.out$cmd
                    pObjects$coordinates[[plot_name]] <- p.out$xy[,c("X", "Y")]
                    p.out$plot
                })
            })
        }
    }

    # Feature expression plots need some careful handling, as we need to update the
    # table links and destroy a brush whenever an x/y-axis-specifying parameter changes.
    max_plots <- nrow(pObjects$memory$featExprPlot)
    for (i in seq_len(max_plots)) {
        local({
            i0 <- i
            mode0 <- "featExprPlot"
            plot_name <- paste0(mode0, i0)
            brush_id <- paste0(plot_name, "_", .brushField)

            # Y-axis observer:
            observe({
                replot <- .setup_table_observer(mode0, i0, input, pObjects, .featExprYAxis, 
                    .featExprYAxisRowTableTitle, .featExprYAxisRowTable, param='yaxis') 
                if (replot) {
                    if (!is.null(isolate(input[[brush_id]]))) { 
                        # This will trigger replotting. 
                        session$resetBrush(brush_id)
                    } else { 
                        # Manually triggering replotting.
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }
                }
            })

            # X-axis observer:
            observe({
                replot <- .setup_table_observer(mode0, i0, input, pObjects, .featExprXAxis, 
                    .featExprXAxisRowTableTitle, .featExprXAxisRowTable, param='xaxis') 
                if (replot) {
                    if (!is.null(isolate(input[[brush_id]]))) { 
                        # This will trigger replotting. 
                        session$resetBrush(brush_id)
                    } else {
                        # Manually triggering replotting.
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }
                }
            })
        })
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
            (rObjects$active_panels) # to trigger recreation when the number of plots is changed.

            chosen <- pObjects$memory$rowStatTable[i0, .rowStatSelected]
            search <- pObjects$memory$rowStatTable[i0, .rowStatSearch]

            search_col <- pObjects$memory$rowStatTable[i0, .rowStatColSearch][[1]]
            search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

            datatable(gene_data, filter="top", rownames=TRUE,
                      options=list(search=list(search=search),
                                   searchCols=c(list(NULL), search_col), # row names are the first column!
                                   scrollX=TRUE),
                      selection=list(mode="single", selected=chosen))
        })

        # Updating memory for new selection parameters (no need for underscore
        # in 'select_field' definition, as this is already in the '.int' constant).
        select_field <- paste0(panel_name, .int_rowStatSelected)
        observe({
            chosen <- input[[select_field]]
            if (length(chosen)) {
                pObjects$memory$rowStatTable[i0, .rowStatSelected] <- chosen

                col_kids <- unique(unlist(pObjects$table_links[[i0]][c("color")]))
                xy_kids <- unique(unlist(pObjects$table_links[[i0]][c("xaxis", "yaxis")]))
                col_kids <- setdiff(col_kids, xy_kids)

                # Triggering the replotting of all color children that are NOT xy children.
                for (kid in col_kids) {
                    rObjects[[kid]] <- .increment_counter(isolate(rObjects[[kid]]))
                }
                
                # Triggering the replotting and brush clearing of all x/y-axis children.
                brush_ids <- sprintf("%s_%s", xy_kids, .brushField)
                for (i in seq_along(xy_kids)) {
                    brush_id <- brush_ids[i]
                    if (!is.null(isolate(input[[brush_id]]))) { # This will trigger replotting. 
                        session$resetBrush(brush_id)
                    } else { # Manually triggering replotting.
                        kid <- xy_kids[i]
                        rObjects[[kid]] <- .increment_counter(isolate(rObjects[[kid]]))
                    }
                }
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
      })
    }

  } # end of iSEE_server

  #######################################################################
  # Launching the app.
  #######################################################################

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}
