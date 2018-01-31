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
#' @param geneExprArgs A DataFrame similar to that produced by
#' \code{\link{geneExprPlotDefaults}}, specifying initial parameters for the plots.
#' @param geneStatArgs A DataFrame similar to that produced by
#' \code{\link{geneStatTableDefaults}}, specifying initial parameters for the plots.
#' @param redDimMax An integer scalar specifying the maximum number of reduced
#' dimension plots in the interface. 
#' @param colDataMax An integer scalar specifying the maximum number of column
#' data plots in the interface. 
#' @param geneExprMax An integer scalar specifying the maximum number of gene
#' expression plots in the interface. 
#' @param geneStatMax An integer scalar specifying the maximum number of gene
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
#' \code{redDimArgs} and \code{geneExprArgs}. Each object can contain
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
#' \code{"Reduced dimension plot 1"}, \code{"Gene statistics table 2"}.
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
  geneExprArgs=NULL,
  geneStatArgs=NULL,
  redDimMax=5,
  colDataMax=5,
  geneExprMax=5,
  geneStatMax=5,
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
  memory <- .setup_memory(se, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
                          redDimMax, colDataMax, geneExprMax, geneStatMax)

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
      actionButton(paste0("redDim", .organizationNew), "New reduced dimension plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("colData", .organizationNew), "New column data plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("geneExpr", .organizationNew), "New gene expression plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("geneStat", .organizationNew), "New gene table", class = "btn btn-primary",icon = icon("plus")),
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
    for (mode in c("redDim", "geneExpr", "colData")) {
      max_plots <- nrow(pObjects$memory[[mode]])
      for (i in seq_len(max_plots)) {
        rObjects[[paste0(mode, "Plot", i)]] <- 1L
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

    for (mode in c("redDim", "geneExpr", "colData", "geneStat")) {
        # Panel addition.
        local({
            mode0 <- mode
            observeEvent(input[[paste0(mode0, .organizationNew)]], {
                all_active <- rObjects$active_panels
                all.memory <- pObjects$memory[[mode0]]
                first.missing <- setdiff(seq_len(nrow(all.memory)), all_active$ID[all_active$Type==mode0])

                if (length(first.missing)) {
                    rObjects$active_panels <- rbind(all_active, DataFrame(Type=mode0, ID=first.missing[1], Width=4L, Height=500L))

                    # Disabling panel addition if we've reached the maximum.
                    if (length(first.missing)==1L) {
                      disable(paste0(mode0, .organizationNew))
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
                max_plots0 <- max_plots

                # Panel removal.
                observeEvent(input[[paste0(mode0, .organizationDiscard, i0)]], {
                    all_active <- rObjects$active_panels
                    current_type <- all_active$Type==mode0

                    # Re-enabling panel addition if we're decreasing from the maximum.
                    if (sum(current_type)==max_plots0) {
                      enable(paste0(mode0, .organizationNew))
                    }

                    # Destroying links; either the brush source, or the links from tables.
                    if (mode0=="geneStat") {
                        .destroy_table(pObjects, paste0(mode0, "Table", i0))
                    } else {
                        .destroy_brush_source(pObjects, paste0(mode0, "Plot", i0))
                    }
                    
                    # Triggering re-rendering of the UI via change to active_panels.
                    index <- which(current_type & all_active$ID==i0)
                    rObjects$active_panels <- rObjects$active_panels[-index,]
               }, ignoreInit=TRUE)

                # Panel shifting, up and down.
                observeEvent(input[[paste0(mode0, .organizationUp, i0)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    if (index!=1L) {
                        reindex <- seq_len(nrow(all_active))
                        reindex[index] <- reindex[index]-1L
                        reindex[index-1L] <- reindex[index-1L]+1L
                        rObjects$active_panels <- all_active[reindex,]
                    }
                }, ignoreInit=TRUE)

                observeEvent(input[[paste0(mode0, .organizationDown, i0)]], {
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
                observeEvent(input[[paste0(mode0, .organizationModify, i0)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    cur_width <- all_active$Width[index]
                    cur_height <- all_active$Height[index]

                    showModal(modalDialog(
                        sliderInput(paste0(mode0, .organizationWidth, i0), label="Width",
                                    min=width_limits[1], max=width_limits[2], value=cur_width, step=1),
                        sliderInput(paste0(mode0, .organizationHeight, i0), label="Height",
                                    min=height_limits[1], max=height_limits[2], value=cur_height, step=50),
                        title=paste(.decode_panel_name(mode0, i0), "panel parameters"),
                        easyClose=TRUE, size="m", footer=NULL
                        )
                    )  
                })

                observeEvent(input[[paste0(mode0, .organizationWidth, i0)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    cur.width <- all_active$Width[index]
                    new.width <- input[[paste0(mode0, .organizationWidth, i0)]]
                    if (!isTRUE(all.equal(new.width, cur.width))) {
                        rObjects$active_panels$Width[index] <- new.width
                    }
                }, ignoreInit=TRUE)

                observeEvent(input[[paste0(mode0, .organizationHeight, i0)]], {
                    all_active <- rObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    cur.height <- all_active$Height[index]
                    new.height <- input[[paste0(mode0, .organizationHeight, i0)]]
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

    for (mode in c("redDim", "geneExpr", "colData")) {
      max_plots <- nrow(pObjects$memory[[mode]])
      for (i in seq_len(max_plots)) {
        local({
          mode0 <- mode
          i0 <- i
          plot.name <- paste0(mode0, "Plot", i0)

          ###############

          # Panel opening/closing observers.
          observeEvent(input[[paste0(mode0, .plotParamPanelOpen, i0)]], {
            pObjects$memory[[mode0]][[.plotParamPanelOpen]][i0] <- input[[paste0(mode0, .plotParamPanelOpen, i0)]]
          })

          observeEvent(input[[paste0(mode0, .colorParamPanelOpen, i0)]], {
            pObjects$memory[[mode0]][[.colorParamPanelOpen]][i0] <- input[[paste0(mode0, .colorParamPanelOpen, i0)]]
          })

          observeEvent(input[[paste0(mode0, .brushParamPanelOpen, i0)]], {
            pObjects$memory[[mode0]][[.brushParamPanelOpen]][i0] <- input[[paste0(mode0, .brushParamPanelOpen, i0)]]
          })

          ###############

          # Brush choice observer. This will fail with an error message if there are cycles
          # across multiple plots. Otherwise it will update the brushing chart.
          observeEvent(input[[paste0(mode0, .brushByPlot, i0)]], {
            old_transmitter <- pObjects$memory[[mode0]][i0, .brushByPlot]
            new_transmitter <- input[[paste0(mode0, .brushByPlot, i0)]]

            # Determining whether the new and old transmitting plot have brushes.
            old_brush <- new_brush <- FALSE 
            old_encoded <- new_encoded <- ""    
            if (old_transmitter!="") {
              old_enc <- .encode_panel_name(old_transmitter)
              old_encoded <- paste0(old_enc$Type, "Plot", old_enc$ID)
              if (!is.null(pObjects$memory[[old_enc$Type]][old_enc$ID, .brushData][[1]])) {
                old_brush <- TRUE
              }
            }
            if (new_transmitter!="") {
              new_enc <- .encode_panel_name(new_transmitter)
              new_encoded <- paste0(new_enc$Type, "Plot", new_enc$ID)
              if (!is.null(pObjects$memory[[new_enc$Type]][new_enc$ID, .brushData][[1]])) {
                new_brush <- TRUE
              }
            }

            # Updating the graph, but breaking if it's not a DAG.
            tmp <- .choose_new_brush_source(pObjects$brush_links, plot.name, new_encoded, old_encoded)
            daggy <- is_dag(simplify(tmp, remove.loops=TRUE)) 
            if (!daggy) {
              showNotification("brushing relationships cannot be cyclic", type="error")
              pObjects$memory[[mode0]][i0, .brushByPlot] <- ""
              updateSelectInput(session, paste0(mode0, .brushByPlot, i0), selected="")
            } else {
              pObjects$brush_links <- tmp
              pObjects$memory[[mode0]][i0, .brushByPlot] <- input[[paste0(mode0, .brushByPlot, i0)]]
            }
            
            # Not replotting if there were no brushes in either the new or old transmitters.
            if (!old_brush && !new_brush){
              return(NULL)
            }

            # Triggering self update of the plot.
            rObjects[[plot.name]] <- .increment_counter(isolate(rObjects[[plot.name]]))

            # Triggering replotting of children, if the current panel is set to restrict;
            # and we have a brush, so that there was already some brushing in the children.
            if (pObjects$memory[[mode0]][i0, .brushEffect]==.brushRestrictTitle
                && !is.null(pObjects$memory[[mode0]][i0, .brushData][[1]])) {
              children <- .get_brush_dependents(pObjects$brush_links, plot.name, pObjects$memory)
              for (child_plot in children) {
                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
              }
            }
          }, ignoreInit=TRUE)

          # Brush effect observer.
          observeEvent(input[[paste0(mode0, .brushEffect, i0)]], {
            cur_effect <- input[[paste0(mode0, .brushEffect, i0)]]
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
            rObjects[[plot.name]] <- .increment_counter(isolate(rObjects[[plot.name]]))

            # Triggering replotting of children, if we are set to or from restrict;
            # and we have a brush, so there was already some brushing in the children.
            if ((cur_effect==.brushRestrictTitle || old_effect==.brushRestrictTitle) 
                && !is.null(pObjects$memory[[mode0]][i0, .brushData][[1]])) {
              children <- .get_brush_dependents(pObjects$brush_links, plot.name, pObjects$memory)
              for (child_plot in children) {
                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
              }
            }
          }, ignoreInit=TRUE)

          # Brush structure observers.
          observeEvent(input[[paste0(mode0, .brushField, i0)]], {
            cur_brush <- input[[paste0(mode0, .brushField, i0)]]
            old_brush <- pObjects$memory[[mode0]][,.brushData][[i0]]

            # If it is rebrushing itself in restrict mode, we take the intersection of brushed regions.
            if (!is.null(cur_brush) 
                && pObjects$memory[[mode0]][i0, .brushEffect]==.brushRestrictTitle
                && plot.name==.decoded2encoded(pObjects$memory[[mode0]][i0, .brushByPlot])) {
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
            rObjects[[plot.name]] <- .increment_counter(isolate(rObjects[[plot.name]]))

            # Trigger replotting of all dependent plots that receive this brush.
            children <- .get_brush_dependents(pObjects$brush_links, plot.name, pObjects$memory)
            for (child_plot in children) {
              rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
            }
          }, ignoreInit=TRUE, ignoreNULL=FALSE)

          ###############

          # Double-click observers.
          observeEvent(input[[paste0(mode0, .zoomClick, i0)]], {
             brush_id <- paste0(mode0, .brushField, i0)
             brush <- input[[brush_id]]

             if (!is.null(brush)) {
               new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
               session$resetBrush(brush_id) # This should auto-trigger replotting above.
             } else {
               new_coords <- NULL
               
               # Brush is already NULL at this point, so resetting it wouldn't help; 
               # we need to manually trigger replotting. We don't move this outside the
               # "else", to avoid two reactive updates of unknown priorities.
               UPDATE <- paste0(mode0, "Plot", i0) 
               rObjects[[UPDATE]] <- .increment_counter(isolate(rObjects[[UPDATE]]))
             }

             pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], i0, .zoomData, new_coords)
          })
        })
      }
    }

    #######################################################################
    # Plot creation section. ----
    #######################################################################

    for (mode in c("redDim", "geneExpr", "colData")) {
        max_plots <- nrow(pObjects$memory[[mode]]) 
  
        # Defining mode-specific parameters.
        FUN <- switch(mode, 
                      redDim=.make_redDimPlot,
                      geneExpr=.make_geneExprPlot,
                      colData=.make_colDataPlot)
  
        protected <- switch(mode,
                            redDim=c(.redDimType, .redDimXAxis, .redDimYAxis),
                            colData=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
                            geneExpr=c(.geneExprAssay, .geneExprXAxisColData, .geneExprYAxisGeneText, .geneExprXAxisGeneText))
  
        for (i in seq_len(max_plots)) {
            # Observers for the non-fundamental parameter options (.brushByPlot is handled elsewhere).
            for (field in c(.colorByColData, .colorByGeneText, .colorByGeneTableAssay, .colorByGeneTextAssay,
                            .brushColor, .brushTransAlpha)) {
                local({
                    i0 <- i
                    mode0 <- mode
                    field0 <- field
                    cur_field <- paste0(mode0, field0, i0)
                    plot_name <- paste0(mode0, "Plot", i0)
  
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
                    cur_field <- paste0(mode0, field0, i0)
                    cur_brush <- paste0(mode0, .brushField, i0)
                    plot_name <- paste0(mode0, "Plot", i0) 
    
                    observeEvent(input[[cur_field]], {
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        pObjects$memory[[mode0]][[field0]][i0] <- matched_input                
                        
                        if (!is.null(isolate(input[[cur_brush]]))) { 
                            # This will trigger replotting via the brush observer above.
                            session$resetBrush(cur_brush) 
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
                plot_name <- paste0(mode0, "Plot", i0)
  
                # Observers for the linked color, which updates the table_links information.
                observe({
                    replot <- .setup_table_observer(mode0, i0, input, pObjects, .colorByField, 
                        .colorByGeneTableTitle, .colorByGeneTable, param='color') 
                    if (replot) {
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }
                })
  
                # Defining the rendered plot, and saving the coordinates.
                output[[plot_name]] <- renderPlot({
                    force(rObjects[[plot_name]])
                    withProgress({
                        p.out <- FUN0(i0, pObjects$memory, pObjects$coordinates, se, colormap)
                    }, message=.decode_panel_name(mode0, i0))

                    p.out <- FUN0(i0, pObjects$memory, pObjects$coordinates, se, colormap)
                    pObjects$commands[[plot_name]] <- p.out$cmd
                    pObjects$coordinates[[plot_name]] <- p.out$xy
                    p.out$plot
                })
            })
        }
    }

    # Gene expression plots need some careful handling, as we need to update the
    # table links and destroy a brush whenever an x/y-axis-specifying parameter changes.
    max_plots <- nrow(pObjects$memory$geneExpr)
    for (i in seq_len(max_plots)) {
        local({
            i0 <- i
            mode0 <- "geneExpr"
            plot_name <- paste0(mode0, "Plot", i0)
            brush_id <- paste0(mode0, .brushField, i0)

            # Y-axis observer:
            observe({
                replot <- .setup_table_observer(mode0, i0, input, pObjects, .geneExprYAxis, 
                    .geneExprYAxisGeneTableTitle, .geneExprYAxisGeneTable, param='yaxis') 
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
                replot <- .setup_table_observer(mode0, i0, input, pObjects, .geneExprXAxis, 
                    .geneExprXAxisGeneTableTitle, .geneExprXAxisGeneTable, param='xaxis') 
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
    # Gene table section. ----
    #######################################################################

    # Load the gene level data
    for (i in seq_len(nrow(memory$geneStat))) {
      local({
        i0 <- i
        output[[paste0("geneStatTable", i0)]] <- renderDataTable({
            (rObjects$active_panels) # to trigger recreation when the number of plots is changed.

            chosen <- pObjects$memory$geneStat[i0, .geneStatSelected]
            search <- pObjects$memory$geneStat[i0, .geneStatSearch]

            search_col <- pObjects$memory$geneStat[i0, .geneStatColSearch][[1]]
            search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

            datatable(gene_data, filter="top", rownames=TRUE,
                      options=list(search=list(search=search),
                                   searchCols=c(list(NULL), search_col), # row names are the first column!
                                   scrollX=TRUE),
                      selection=list(mode="single", selected=chosen))
        })

        # Updating memory for new selection parameters.
        observe({
            chosen <- input[[paste0("geneStatTable", i0, .int_geneStatSelected)]]
            if (length(chosen)) {
                pObjects$memory$geneStat[i0, .geneStatSelected] <- chosen

                col_kids <- unique(unlist(pObjects$table_links[[i0]][c("color")]))
                xy_kids <- unique(unlist(pObjects$table_links[[i0]][c("xaxis", "yaxis")]))
                col_kids <- setdiff(col_kids, xy_kids)

                # Triggering the replotting of all color children that are NOT xy children.
                enc <- .split_encoded(col_kids)
                brush_ids <- sprintf("%s%s%i", enc$Type, .brushField, enc$ID)
                for (i in seq_along(col_kids)) {
                    kid <- col_kids[i]
                    brush_id <- brush_ids[i]
                    rObjects[[kid]] <- .increment_counter(isolate(rObjects[[kid]]))
                }
                
                # Triggering the replotting and brush clearing of all x/y-axis children.
                enc <- .split_encoded(xy_kids)
                brush_ids <- sprintf("%s%s%i", enc$Type, .brushField, enc$ID)
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
        observe({
            search <- input[[paste0("geneStatTable", i0, .int_geneStatSearch)]]
            if (length(search)) {
                pObjects$memory$geneStat[i0, .geneStatSearch] <- search
            }
        })

        observe({
            search <- input[[paste0("geneStatTable", i0, .int_geneStatColSearch)]]
            if (length(search)) {
                pObjects$memory$geneStat <- .update_list_element(
                    pObjects$memory$geneStat, i0, .geneStatColSearch, search)                         
            }
        })

        # Updating the annotation box.
        output[[paste0("geneStatAnno", i0)]] <- renderUI({
            chosen <- input[[paste0("geneStatTable", i0, .int_geneStatSelected)]]
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
## End(Not run)
