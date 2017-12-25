
#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' Interactive visualization of single-cell data using a Shiny interface.
#'
#' @param se A SingleCellExperiment object.
#' @param redDimArgs An integer scalar specifying the maximum number of
#' reduced dimension plots in the interface. Alternatively, a DataFrame 
#' similar to that produced by \code{\link{redDimPlotDefaults}}, specifying 
#' initial parameters for the plots.
#' @param colDataArgs An integer scalar specifying the maximum number of
#' column data plots in the interface. Alternatively, a DataFrame 
#' similar to that produced by \code{\link{colDataPlotDefaults}}, specifying 
#' initial parameters for the plots.
#' @param geneExprArgs An integer scalar specifying the maximum number of
#' gene expression plots in the interface. Alternatively, a DataFrame 
#' similar to that produced by \code{\link{geneExprPlotDefaults}}, specifying
#' initial parameters for the plots.
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
#'
#' @details Users can pass default parameters via DataFrame objects in 
#' \code{redDim.args} and \code{geneExpr.args}. Each object can contain 
#' some or all of the expected fields (see \code{\link{redDimPlotDefaults}}).
#' Any missing fields will be filled in with the defaults.
#'
#' The number of maximum plots for each type of plot is implicitly inferred 
#' from the number of rows of the corresponding DataFrame in \code{*Args},
#' if an integer scalar was not supplied. Users can specify any number of 
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
#' sce
#'
#' # launch the app itself
#' if (interactive()) { iSEE(sce) }
iSEE <- function(
  se,
  redDimArgs=5,
  colDataArgs=5,
  geneExprArgs=5,
  initialPanels=NULL,
  annot.orgdb=NULL,
  annot.keytype="ENTREZID",
  annot.keyfield=NULL
) {

  # for correct usage of the pkg, need explicit call
  # https://ebailey78.github.io/shinyBS/install.html#using_shinybs
  # library(shinyBS)

  cell.data <- colData(se)
  covariates <- colnames(cell.data)
  red.dim <- reducedDim(se)
  red.dim.names <- reducedDimNames(se)
  all.assays <- names(assays(se))
  gene.names <- rownames(se)
  
  gene.data <- as.data.frame(rowData(se))
  rownames(gene.data) <- gene.names
  if (ncol(gene.data)==0L){ # To give it DT::datatable something to play with.
    gene.data$Present <- TRUE
  }
  
  # Setting up parameters for each panel.
  memory <- list()
  if (is.numeric(redDimArgs)) { 
    memory$redDim <- redDimPlotDefaults(se, redDimArgs)
  } else {
    memory$redDim <- redDimPlotDefaults(se, nrow(redDimArgs)) 
    memory$redDim <- .override_defaults(memory$redDim, redDimArgs)
  }
  reddim_max_plots <- nrow(memory$redDim)
                                          
  if (is.numeric(geneExprArgs)) { 
    memory$geneExpr <- geneExprPlotDefaults(se, geneExprArgs)
  } else {
    memory$geneExpr <- geneExprPlotDefaults(se, nrow(geneExprArgs)) 
    memory$geneExpr <- .override_defaults(memory$geneExpr, geneExprArgs)
  }
  geneexpr_max_plots <- nrow(memory$geneExpr)

  if (is.numeric(colDataArgs)) { 
    memory$colData <- colDataPlotDefaults(se, colDataArgs)
  } else {
    memory$colData <- colDataPlotDefaults(se, nrow(colDataArgs)) 
    memory$colData <- .override_defaults(memory$colData, colDataArgs)
  }
  coldata_max_plots <- nrow(memory$colData)
 
  genestat_max_tab <- 5
  memory$geneStat <- DataFrame(Selected=rep(1, genestat_max_tab), Search="")

  # Defining the initial elements to be plotted.
  if (is.null(initialPanels)) { 
      active_plots <- data.frame(Type=c("redDim", "colData", "geneExpr", "geneStat"),
                                 ID=1, Width=4,
                                 stringsAsFactors=FALSE)
  } else {
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
      illegal <- which(max_each[encoded$Type] < encoded$ID) 
      if (length(illegal)) {
          stop(sprintf("'%s' in 'initialPanels' is not available (maximum ID is %i)", 
                       initialPanels$Name[illegal[1]], max_each[illegal[1]]))
      }

      active_plots <- data.frame(Type=encoded$Type, ID=encoded$ID,
                                 Width=initialPanels$Width, 
                                 stringsAsFactors=FALSE)
  }

  # for retrieving the annotation
  if (!is.null(annot.orgdb)) { 
    if (!annot.keytype %in% keytypes(annot.orgdb)) { 
      stop("specified keytype not in org.*.db object")
    }
  }
  
  # general options:
  
  ########## ui definition ##########
  
  iSEE_ui <- dashboardPage(
    dashboardHeader(
      title = paste0("iSEE - interactive SingleCell/Summarized Experiment Explorer v",
                     packageVersion("iSEE")),
      titleWidth = 900
    ), # end of dashboardHeader
    dashboardSidebar(
      width = 280,
      # general app settings
      menuItem("App settings",icon = icon("cogs")),
      # merely oriented to export the plots - if we want to support that capability
      menuItem("Plot export settings", icon = icon("paint-brush")),
      # quick viewer could display which relevant slots are already populated?
      menuItem("Quick viewer", icon = icon("flash")),
      # this will cover the part for the first tour of the app
      menuItem("First steps help", icon = icon("question-circle"),
               actionButton("btn", "Click me for a quick tour", icon("info"),
                            style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4")
               
      ),
      actionButton(paste0("redDim", .organizationNew), "New reduced dimension plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("colData", .organizationNew), "New column data plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("geneExpr", .organizationNew), "New gene expression plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton(paste0("geneStat", .organizationNew), "New gene table", class = "btn btn-primary",icon = icon("plus")),
      uiOutput("panelOrganization")
    ), # end of dashboardSidebar

    dashboardBody(
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
      
      # row for the boxes
      fluidRow(
        valueBoxOutput("box_sce_obj")
      ),

      uiOutput("allPanels"),             
      
      iSEE_footer()
      
    ), # end of dashboardBody
    skin = "blue"
  ) # end of dashboardPage
  
  
  ########## server definition ##########

  iSEE_server <- function(input, output, session) {

    # storage for all the reactive objects
    rObjects <- reactiveValues(
        active_plots = active_plots,
        rebrushed = 1
    )
    
    # storage for other persistent objects
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$coordinates <- list()

    # info boxes, to keep on top of the page  on the left side?
   
    output$box_sce_obj <- renderUI({
      if(!is.null(rObjects$sce)){
        return(valueBox(
          "SCE Object",
          sprintf(
            "%i genes - %i samples",
            nrow(rObjects$sce),
            ncol(rObjects$sce)
          ),
          icon = icon("list"),
          color = "green",
          width = NULL
        ))
      } else {
        return(valueBox(
          "SCE Object",
          "yet to create",
          icon = icon("list"),
          color = "red",width = NULL
        ))
      }
    }) # end of output$box_sce_obj

    #######################################################################
    # Multipanel UI generation section.
    # This is adapted from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    #######################################################################
    
    output$allPanels <- renderUI({
        (rObjects$rebrushed) # Trigger re-rendering if these are selected.
        .panel_generation(rObjects$active_plots, pObjects$memory,
                          redDimNames=red.dim.names, 
                          colDataNames=covariates,
                          assayNames=all.assays)
    })
    
    output$panelOrganization <- renderUI({
        .panel_organization(rObjects$active_plots, pObjects$memory)   
    })

    for (mode in c("redDim", "geneExpr", "colData", "geneStat")) { 
        # Panel addition.
        local({
            mode0 <- mode
            observeEvent(input[[paste0(mode0, .organizationNew)]], {
                all.active <- rObjects$active_plots
                all.memory <- pObjects$memory[[mode0]]
                first.missing <- setdiff(seq_len(nrow(all.memory)), all.active$ID[all.active$Type==mode0])
                if (length(first.missing)) { 
                    rObjects$active_plots <- rbind(all.active, DataFrame(Type=mode0, ID=first.missing[1], Width=4))
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
                    all.active <- rObjects$active_plots
                    index <- which(all.active$Type==mode0 & all.active$ID==i0)
                    rObjects$active_plots <- rObjects$active_plots[-index,]
               }, ignoreInit=TRUE)

                # Panel resizing.
                observeEvent(input[[paste0(mode0, i0, .organizationWidth)]], {
                    all.active <- rObjects$active_plots
                    index <- which(all.active$Type==mode0 & all.active$ID==i0)
                    cur.width <- all.active$Width[index]
                    new.width <- input[[paste0(mode0, i0, .organizationWidth)]]
                    if (!isTRUE(all.equal(new.width, cur.width))) { 
                        rObjects$active_plots$Width[index] <- new.width
                    }
                }, ignoreInit=TRUE)

                # Panel shifting, up and down.
                observeEvent(input[[paste0(mode0, i0, .organizationUp)]], {
                    all.active <- rObjects$active_plots
                    index <- which(all.active$Type==mode0 & all.active$ID==i0)
                    if (index!=1L) { 
                        reindex <- seq_len(nrow(all.active))
                        reindex[index] <- reindex[index]-1L
                        reindex[index-1L] <- reindex[index-1L]+1L
                        rObjects$active_plots <- all.active[reindex,]
                    } 
                }, ignoreInit=TRUE)

                observeEvent(input[[paste0(mode0, i0, .organizationDown)]], {
                    all.active <- rObjects$active_plots
                    index <- which(all.active$Type==mode0 & all.active$ID==i0)
                    if (index!=nrow(all.active)) { 
                        reindex <- seq_len(nrow(all.active))
                        reindex[index] <- reindex[index]+1L
                        reindex[index+1L] <- reindex[index+1L]-1L
                        rObjects$active_plots <- all.active[reindex,]
                    } 
                }, ignoreInit=TRUE)
            })
        }
    }
    
    #######################################################################
    # Reduced dimension plot section.
    #######################################################################

    # Note: we need "local" so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.

    for (i in seq_len(reddim_max_plots)) {
      local({
        i0 <- i
        plot.name <- .redDimPlot(i0)
        output[[plot.name]] <- renderPlot({

          # Updating parameters in the memory store (non-characters need some careful treatment).
          for (field in c(.redDimType, 
                          .colorByField, .colorByColData, .colorByGeneExprs, .colorByGeneExprsAssay,
                          .brushByPlot)) { 
              pObjects$memory$redDim[[field]][i0] <- input[[.inputRedDim(field, i0)]]
          }
          for (field in c(.redDimXAxis, .redDimYAxis)) { 
              pObjects$memory$redDim[[field]][i0] <- as.integer(input[[.inputRedDim(field, i0)]])
          }
         
          # Creating the plot, with saved coordinates.
          p.out <- .make_redDimPlot(se, pObjects$memory$redDim[i0,], input, pObjects$coordinates) 
          pObjects$coordinates[[plot.name]] <- p.out$xy
          p.out$plot
        })

        observeEvent(input[[.inputRedDim(.plotParamPanelName, i0)]], {
          opened <- input[[.inputRedDim(.plotParamPanelName, i0)]] 
          pObjects$memory$redDim[[.plotParamPanelOpen]][i0] <- .plotParamPanelTitle %in% opened
          pObjects$memory$redDim[[.colorParamPanelOpen]][i0] <- .colorParamPanelTitle %in% opened
          pObjects$memory$redDim[[.brushParamPanelOpen]][i0] <- .brushParamPanelTitle %in% opened
        })

        observeEvent(input[[.inputRedDim(.brushActive, i0)]], {
          current <- input[[.inputRedDim(.brushActive, i0)]]
          reference <- pObjects$memory$redDim[[.brushActive]][i0]
          if (!identical(current, reference)) { 
            rObjects$rebrushed <- rObjects$rebrushed + 1L
            pObjects$memory$redDim[[.brushActive]][i0] <- current
          }
        }, ignoreInit=TRUE)
      })
    }
    
    #######################################################################
    # Column data scatter plot section.
    #######################################################################

    for (i in seq_len(coldata_max_plots)) {
      local({
        i0 <- i
        output[[.colDataPlot(i0)]] <- renderPlot({

          # Updating parameters (non-characters need some careful treatment).
          for (field in c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData,
                      .colorByField, .colorByColData, .colorByGeneExprs, .colorByGeneExprsAssay,
                      .brushByPlot)) { 
              pObjects$memory$colData[[field]][i0] <- input[[.inputColData(field, i0)]]
          }
          
          # Creating the plot.
          .make_colDataPlot(se, pObjects$memory$colData[i0,], input)
        })

        observeEvent(input[[.inputColData(.plotParamPanelName, i0)]], {
          opened <- input[[.inputColData(.plotParamPanelName, i0)]] 
          pObjects$memory$colData[[.plotParamPanelOpen]][i0] <- .plotParamPanelTitle %in% opened
          pObjects$memory$colData[[.colorParamPanelOpen]][i0] <- .colorParamPanelTitle %in% opened
          pObjects$memory$colData[[.brushParamPanelOpen]][i0] <- .brushParamPanelTitle %in% opened
        })

        observeEvent(input[[.inputColData(.brushActive, i0)]], {
          current <- input[[.inputColData(.brushActive, i0)]] 
          reference <- pObjects$memory$colData[[.brushActive]][i0]
          if (!identical(current, reference)) { 
            pObjects$memory$colData[[.brushActive]][i0] <- current
            rObjects$rebrushed <- rObjects$rebrushed + 1L
          }
        }, ignoreInit=TRUE)
      })
    }

    #######################################################################
    # Gene expression scatter plot section.
    #######################################################################
    
    for (i in seq_len(geneexpr_max_plots)) {
      local({
        i0 <- i
        output[[.geneExprPlot(i0)]] <- renderPlot({
          # Updating parameters.
          for (field in c(.geneExprID, .geneExprAssay, .geneExprXAxis, .geneExprXAxisColData, .geneExprXAxisGeneExprs,
                          .colorByField, .colorByColData, .colorByGeneExprs, .colorByGeneExprs,
                          .brushByPlot)) {
              pObjects$memory$geneExpr[[field]][i0] <- input[[.inputGeneExpr(field, i0)]]
          }

          # Creating the plot.
          .make_geneExprPlot(se, pObjects$memory$geneExpr[i0,], input)
        }) 

        observeEvent(input[[.inputGeneExpr(.plotParamPanelName, i0)]], {
          opened <- input[[.inputGeneExpr(.plotParamPanelName, i0)]] 
          pObjects$memory$geneExpr[[.plotParamPanelOpen]][i0] <- .plotParamPanelTitle %in% opened
          pObjects$memory$geneExpr[[.colorParamPanelOpen]][i0] <- .colorParamPanelTitle %in% opened
          pObjects$memory$geneExpr[[.brushParamPanelOpen]][i0] <- .brushParamPanelTitle %in% opened
        })

        observeEvent(input[[.inputGeneExpr(.brushActive, i0)]], {
          current <- input[[.inputGeneExpr(.brushActive, i0)]]
          reference <- pObjects$memory$geneExpr[[.brushActive]][i0]
          if (!identical(current, reference)) { 
            pObjects$memory$geneExpr[[.brushActive]][i0] <- current
            rObjects$rebrushed <- rObjects$rebrushed + 1L
          }
        }, ignoreInit=TRUE)
      }) 
    }
    
    #######################################################################
    # Gene table section.
    #######################################################################
    
    # Load the gene level data
    for (i in seq_len(genestat_max_tab)) {
      local({
        i0 <- i
        output[[paste0("geneStatTable", i0)]] <- renderDataTable({
            (rObjects$active_plots) # to trigger recreation when the number of plots is changed.
            chosen <- pObjects$memory$geneStat$Selected[i0]
            search <- pObjects$memory$geneStat$Search[i0]
            datatable(gene.data, filter="top", rownames=TRUE,
                      options=list(search=list(search=search)),
                      selection=list(mode="single", selected=chosen))
        })

        observe({
            chosen <- input[[paste0("geneStatTable", i0, "_rows_selected")]]
            if (length(chosen)) {
                pObjects$memory$geneStat$Selected[i0] <- chosen
            }
            search <- input[[paste0("geneStatTable", i0, "_search")]]
            if (length(search)) { 
                pObjects$memory$geneStat$Search[i0] <- search
            }
        })
      })
    }

    output$geneStatInfoBox <- renderUI({
      if (is.null(annot.orgdb)) {
        return(HTML(""))
      }

      shiny::validate(
        need(!is.null(input$geneStatTab_rows_selected),
             "Select a gene from the table"
        )
      )
     
      if (is.null(annot.keyfield)) { 
        selectedGene <- gene.names[input$geneStatTab_rows_selected]
      } else {
        selectedGene <- gene.data[input$geneStatTab_rows_selected,annot.keyfield]
      }
      
      if (annot.keytype!="ENTREZID") {
        selgene_entrez <- mapIds(annot.orgdb, selectedGene, "ENTREZID", annot.keytype)
      } else {
        selgene_entrez <- selectedGene
      }

      fullinfo <- entrez_summary("gene", selgene_entrez)
      link_pubmed <- paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',
                            selgene_entrez,
                            '" target="_blank" >Click here to see more at NCBI</a>')

      if(fullinfo$summary == "") {
        return(HTML(paste0("<b>",fullinfo$name, "</b><br/><br/>",
                           fullinfo$description,"<br/><br/>",
                           link_pubmed
        )))
      } else {
        return(HTML(paste0("<b>",fullinfo$name, "</b><br/><br/>",
                           fullinfo$description, "<br/><br/>",
                           fullinfo$summary, "<br/><br/>",
                           link_pubmed
        )))
      }
    }) # end of output[[plotname]]
    
  } # end of iSEE_server
  
  #######################################################################
  # Launching the app.
  #######################################################################
  
  shinyApp(ui = iSEE_ui, server = iSEE_server)
}

