
#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' Interactive visualization of single-cell data using a Shiny interface.
#'
#' @param se A \linkS4class{SingleCellExperiment} object.
#' @param redDimArgs A DataFrame similar to that produced by \code{\link{redDimPlotDefaults}}, specifying initial parameters for the plots.
#' @param colDataArgs A DataFrame similar to that produced by \code{\link{colDataPlotDefaults}}, specifying initial parameters for the plots.
#' @param geneExprArgs A DataFrame similar to that produced by \code{\link{geneExprPlotDefaults}}, specifying initial parameters for the plots.
#' @param geneStatArgs A DataFrame similar to that produced by \code{\link{geneStatTableDefaults}}, specifying initial parameters for the plots.
#' @param redDimMax An integer scalar specifying the maximum number of reduced dimension plots in the interface. 
#' @param colDataMax An integer scalar specifying the maximum number of column data plots in the interface. 
#' @param geneExprMax An integer scalar specifying the maximum number of gene expression plots in the interface. 
#' @param geneStatMax An integer scalar specifying the maximum number of gene statistic tables in the interface. 
#' @param initialPanels A DataFrame specifying which panels should be created
#' at initialization. This should contain a \code{Name} character field and
#' a \code{Width} integer field, see Details.
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
#'
#' @details Users can pass default parameters via DataFrame objects in
#' \code{redDim.args} and \code{geneExpr.args}. Each object can contain
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
#' describing the width of the panel from 4 to 12 (values will be coerced
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
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#' sce <- runPCA(sce)
#' sce <- runTSNE(sce)
#' sce
#'
#' qc_colors <- c("forestgreen", "firebrick1")
#' names(qc_colors) <- c("Y", "N")
#'
#' driver_colors <- RColorBrewer::brewer.pal(3, "Set2")
#' names(driver_colors) <- unique(sce$driver_1_s)
#'
#' ecm <- new("ExperimentColorMap",
#'    assays = list(
#'        counts = viridis::viridis(10),
#'        cufflinks_fpkm = c("black","brown","red","orange","yellow")
#'    ),
#'    colData = list(
#'        passes_qc_checks_s = qc_colors,
#'        driver_1_s = driver_colors
#'    )
#')
#'
#' # launch the app itself
#' if (interactive()) { iSEE(sce, colormap = ecm) }
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
  colormap=ExperimentColorMap()
) {
  # Save the original name of the input object for the command to rename it
  # in the tracker
  se_name <- deparse(substitute(se))
  stopifnot(inherits(se, "SingleCellExperiment"))

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
  if (is.null(initialPanels)) {
    initialPanels <- data.frame(Name=c("Reduced dimension plot 1", "Column data plot 1", 
                                       "Gene expression plot 1", "Gene statistics table 1"),
                                Width=4, stringsAsFactors=FALSE)
  } 

  if (is.null(initialPanels$Name)) {
    stop("need 'Name' field in 'initialPanels'")
  }
  if (is.null(initialPanels$Width)) {
    initialPanels$Width <- 4L
  } else {
    initialPanels$Width <- pmax(4L, pmin(12L, as.integer(initialPanels$Width)))
  }

  encoded <- .encode_panel_name(initialPanels$Name)
  max_each <- unlist(lapply(memory, nrow))
  illegal <- max_each[encoded$Type] < encoded$ID
  if (any(illegal)) {
    badpanel <- which(illegal)[1]
    message(sprintf("'%s' in 'initialPanels' is not available (maximum ID is %i)",
                    initialPanels$Name[badpanel], max_each[encoded$Type[badpanel]]))
  }

  active_plots <- data.frame(Type=encoded$Type, ID=encoded$ID,
                             Width=initialPanels$Width,
                             stringsAsFactors=FALSE)[!illegal,,drop=FALSE]

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
      titleWidth = 800,
      dropdownMenu(type = "tasks",
                   icon = icon("question-circle"),
                   badgeStatus = NULL,
                   headerText = "Want a guided tour?",
                   notificationItem(
                     text = actionButton("tour_firststeps", "Click me for a quick tour", icon("info"),
                                         style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"),
                     icon = icon(""), # tricking it to not have additional icon
                     status = "primary"))
    ), # end of dashboardHeader
    dashboardSidebar(
      # general app settings
      # menuItem("App settings",icon = icon("cogs")),
      # merely oriented to export the plots - if we want to support that capability
      # menuItem("Plot export settings", icon = icon("paint-brush")),
      # quick viewer could display which relevant slots are already populated?
      # menuItem("Quick viewer", icon = icon("flash")),
      # this will cover the part for the first tour of the app
      # menuItem("First steps help", icon = icon("question-circle")
      # ),
      actionButton(paste0("redDim", .organizationNew), "New reduced dimension plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("colData", .organizationNew), "New column data plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("geneExpr", .organizationNew), "New gene expression plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("geneStat", .organizationNew), "New gene table", class = "btn btn-primary",icon = icon("plus")),

      actionButton("getcode_all","Extract the R code!",icon = icon("magic")),
      hr(),

      uiOutput("panelOrganization")
    ), # end of dashboardSidebar

    dashboardBody(
      useShinyjs(), 
      introjsUI(),
      # must be included in UI

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

      uiOutput("allPanels"),

      iSEE_footer()
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
    pObjects$brush <- .spawn_brush_chart(memory) 

    # Storage for all the reactive objects
    rObjects <- reactiveValues(
        active_plots = active_plots
    )
    for (mode in c("redDim", "geneExpr", "colData")) {
      max_plots <- nrow(pObjects$memory[[mode]])
      for (i in seq_len(max_plots)) {
        rObjects[[paste0(mode, "Plot", i)]] <- 1L
      }
    }

    # info boxes, to keep on top of the page  on the left side?

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
                  value = paste0((.track_it_all(rObjects, pObjects, se_name)),collapse="\n"),
                  height="600px")
        ))
    })

    output$codetext_modal <- renderPrint({
      print(.track_it_all(rObjects, pObjects, se_name))
    })

    #######################################################################
    # Multipanel UI generation section. ----
    # This is adapted from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    #######################################################################

    output$allPanels <- renderUI({
        .panel_generation(rObjects$active_plots, pObjects$memory, se)
    })

    output$panelOrganization <- renderUI({
        .panel_organization(rObjects$active_plots, pObjects$memory)
    })

    # Note: we need "local" so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.

   for (mode in c("redDim", "geneExpr", "colData", "geneStat")) {
        # Panel addition.
        local({
            mode0 <- mode
            observeEvent(input[[paste0(mode0, .organizationNew)]], {
                all_active <- rObjects$active_plots
                all.memory <- pObjects$memory[[mode0]]
                first.missing <- setdiff(seq_len(nrow(all.memory)), all_active$ID[all_active$Type==mode0])

                if (length(first.missing)) {
                    rObjects$active_plots <- rbind(all_active, DataFrame(Type=mode0, ID=first.missing[1], Width=4))

                    # Disabling panel addition if we've reached the maximum.
                    if (length(first.missing)==1L) {
                      disable(paste0(mode0, .organizationNew))
                    }
                } else {
                    warning(sprintf("maximum number of plots reached for mode '%s'", mode0))
                }
            })
        })

        max_plots <- nrow(pObjects$memory[[mode]])
        for (i in seq_len(max_plots)) {
            local({
                mode0 <- mode
                i0 <- i

                # Panel removal.
                observeEvent(input[[paste0(mode0, i0, .organizationDiscard)]], {
                    all_active <- rObjects$active_plots
                    current_type <- all_active$Type==mode0
                    index <- which(current_type & all_active$ID==i0)
                    rObjects$active_plots <- rObjects$active_plots[-index,]

                    # Re-enabling panel addition if we're decreasing from the maximum.
                    if (sum(current_type)==max_plots) {
                      enable(paste0(mode0, .organizationNew))
                    }

                    # Destroying the brush source.
                    pObjects$brush <- .destroy_brush_source(pObjects$brush,
                        paste0(mode0, "Plot", i0))                                                            
               }, ignoreInit=TRUE)

                # Panel resizing.
                observeEvent(input[[paste0(mode0, i0, .organizationWidth)]], {
                    all_active <- rObjects$active_plots
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    cur.width <- all_active$Width[index]
                    new.width <- input[[paste0(mode0, i0, .organizationWidth)]]
                    if (!isTRUE(all.equal(new.width, cur.width))) {
                        rObjects$active_plots$Width[index] <- new.width
                    }
                }, ignoreInit=TRUE)

                # Panel shifting, up and down.
                observeEvent(input[[paste0(mode0, i0, .organizationUp)]], {
                    all_active <- rObjects$active_plots
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    if (index!=1L) {
                        reindex <- seq_len(nrow(all_active))
                        reindex[index] <- reindex[index]-1L
                        reindex[index-1L] <- reindex[index-1L]+1L
                        rObjects$active_plots <- all_active[reindex,]
                    }
                }, ignoreInit=TRUE)

                observeEvent(input[[paste0(mode0, i0, .organizationDown)]], {
                    all_active <- rObjects$active_plots
                    index <- which(all_active$Type==mode0 & all_active$ID==i0)
                    if (index!=nrow(all_active)) {
                        reindex <- seq_len(nrow(all_active))
                        reindex[index] <- reindex[index]+1L
                        reindex[index+1L] <- reindex[index+1L]-1L
                        rObjects$active_plots <- all_active[reindex,]
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

          # Brush choice observer. This will fail with an error message
          # if there are cycles across multiple plots. Otherwise it will
          # update the brushing chart and trigger replotting.
          observeEvent(input[[paste0(mode0, .brushByPlot, i0)]], {
            tmp <- .choose_new_brush_source(pObjects$brush, plot.name, 
                .decoded2encoded(input[[paste0(mode0, .brushByPlot, i0)]]),
                .decoded2encoded(pObjects$memory[[mode0]][i0, .brushByPlot]))

            daggy <- is_dag(simplify(tmp, remove.loops=TRUE)) 
            if (!daggy) {
              showNotification("brushing relationships cannot be cyclic", type="error")
              pObjects$memory[[mode0]][i0, .brushByPlot] <- ""
              updateSelectInput(session, paste0(mode0, .brushByPlot, i0), selected="")
            } else {
              pObjects$brush <- tmp
              pObjects$memory[[mode0]][i0, .brushByPlot] <- input[[paste0(mode0, .brushByPlot, i0)]]
            }

            UPDATE <- paste0(mode0, "Plot", i0)
            rObjects[[UPDATE]] <- .increment_counter(isolate(rObjects[[UPDATE]]))
          })

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

            # Trigger replotting of self, to draw a more persistent brushing box.
            rObjects[[plot.name]] <- .increment_counter(isolate(rObjects[[plot.name]]))

            # Trigger replotting of all dependent plots that receive this brush.
            children <- .get_brush_dependents(pObjects$brush, plot.name, pObjects$memory)
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
                          geneExpr=c(.geneExprYAxis, .geneExprYAxisGeneTable, .geneExprYAxisGeneText,
                                     .geneExprXAxis, .geneExprXAxisColData, .geneExprXAxisGeneTable, .geneExprXAxisGeneText))

      for (i in seq_len(max_plots)) {
        local({
          i0 <- i
          mode0 <- mode
          FUN0 <- FUN
          plot.name <- paste0(mode0, "Plot", i0)

          # Defining the rendered plot.
          output[[plot.name]] <- renderPlot({
            force(rObjects[[plot.name]])

            # Updating non-fundamental parameters in the memory store (i.e., these
            # parameters do not change the meaning of the coordinates).
            for (field in ALLEXTRAS_DIRECT) {
                pObjects$memory[[mode0]][[field]][i0] <- input[[paste0(mode0, field, i0)]]
            }
            for (field in ALLEXTRAS_INT) {
                pObjects$memory[[mode0]][[field]][i0] <- as.integer(input[[paste0(mode0, field, i0)]])
            }

             # Creating the plot, with saved coordinates.
            p.out <- FUN0(i0, se, input, pObjects$coordinates, pObjects$memory, colormap)
            pObjects$commands[[plot.name]] <- p.out$cmd
            pObjects$coordinates[[plot.name]] <- p.out$xy
            p.out$plot
          })
        })

        # Defining observers to respond to fundamental parameters.
        for (field in protected) {
          local({
            i0 <- i
            mode0 <- mode
            field0 <- field 
            in_name <- paste0(mode0, field0, i0)

            observeEvent(input[[in_name]], {
              incoming <- input[[in_name]]
              if (is.numeric(pObjects$memory[[mode0]][[field0]])) {
                incoming <- as.integer(incoming)
              }
              pObjects$memory[[mode0]][[field0]][i0] <- incoming

              cur_brush <- paste0(mode0, .brushField, i0)
              if (!is.null(input[[cur_brush]])) {
                session$resetBrush(cur_brush) # This will trigger replotting.
              } else {
                UPDATE <- paste0(mode0, "Plot", i0) # Manually triggering replotting.
                rObjects[[UPDATE]] <- .increment_counter(isolate(rObjects[[UPDATE]]))
              }
            }, ignoreInit=TRUE)
          })
        }
      }
    }

    # Resetting brushes for gene expression plots when the linked gene table changes
    # (only when it is currently selecting x/y-axes from the table, though).
    max_plots <- nrow(pObjects$memory$geneExpr)
    for (i in seq_len(max_plots)) {
      local({
        i0 <- i
        observe({
          yaxischoice <- input[[.inputGeneExpr(.geneExprYAxis, i0)]]
          yaxistab <- input[[.inputGeneExpr(.geneExprYAxisGeneTable, i0)]]
          if (!is.null(yaxischoice) && !is.null(yaxistab)) { 
            if (yaxischoice==.geneExprYAxisGeneTableTitle) {
              stuff <- .find_linked_gene(yaxistab, input)
              session$resetBrush(.inputGeneExpr(.brushField, i0))
            }
          }
        })

        observe({
          xaxischoice <- input[[.inputGeneExpr(.geneExprXAxis, i0)]]
          xaxistab <- input[[.inputGeneExpr(.geneExprXAxisGeneTable, i0)]]
          if (!is.null(xaxischoice) && !is.null(xaxistab)) { 
            if (xaxischoice==.geneExprXAxisGeneTableTitle) {
              stuff <- .find_linked_gene(xaxistab, input)
              session$resetBrush(.inputGeneExpr(.brushField, i0))
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
            (rObjects$active_plots) # to trigger recreation when the number of plots is changed.
            chosen <- pObjects$memory$geneStat$Selected[i0]
            search <- pObjects$memory$geneStat$Search[i0]
            datatable(gene_data, filter="top", rownames=TRUE,
                      options=list(search=list(search=search),
                                   scrollX=TRUE),
                      selection=list(mode="single", selected=chosen))
        })

        # Updating memory for new search/selection parameters.
        observe({
            chosen <- input[[paste0("geneStatTable", i0, "_rows_selected")]]
            if (length(chosen)) {
                pObjects$memory$geneStat$Selected[i0] <- chosen
            }
        })

        observe({
            search <- input[[paste0("geneStatTable", i0, "_search")]]
            if (length(search)) {
                pObjects$memory$geneStat$Search[i0] <- search
            }
        })

        # Updating the annotation box.
        output[[.geneStatAnno(i0)]] <- renderUI({
            .generate_annotation(annot.orgdb, annot.keytype, annot.keyfield, 
                                 gene_data, input, i0)
        }) 
      })
    }

  } # end of iSEE_server

  #######################################################################
  # Launching the app.
  #######################################################################

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}
