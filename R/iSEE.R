
#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' Interactive visualization of single-cell data using a Shiny interface.
#'
#' @param se A SingleCellExperiment object.
#' @param redDim.args An integer scalar specifying the maximum number of
#' reduced dimension plots in the interface. Alternatively, a DataFrame 
#' similar to that produced by \code{\link{redDimPlotDefaults}}, specifying 
#' initial parameters for the plots.
#' @param colData.args An integer scalar specifying the maximum number of
#' column data plots in the interface. Alternatively, a DataFrame 
#' similar to that produced by \code{\link{colDataPlotDefaults}}, specifying 
#' initial parameters for the plots.
#' @param geneExpr.args An integer scalar specifying the maximum number of
#' gene expression plots in the interface. Alternatively, a DataFrame 
#' similar to that produced by \code{\link{geneExprPlotDefaults}}, specifying
#' initial parameters for the plots.
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
#' Users can specify any number of maximum plots, though increasing the 
#' number will increase the time required to generate any given plot. 
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
  redDim.args=5,
  colData.args=5,
  geneExpr.args=5,
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
  
  # Setting up initial reduced dim plot parameters.
  memory <- list()
  if (is.numeric(redDim.args)) { 
    memory$redDim <- redDimPlotDefaults(se, redDim.args)
  } else {
    memory$redDim <- redDimPlotDefaults(se, nrow(redDim.args)) 
    memory$redDim <- .override_defaults(memory$redDim, redDim.args)
  }
  reddim_max_plots <- nrow(memory$redDim)
                                          
  if (is.numeric(geneExpr.args)) { 
    memory$geneExpr <- geneExprPlotDefaults(se, geneExpr.args)
  } else {
    memory$geneExpr <- geneExprPlotDefaults(se, nrow(geneExpr.args)) 
    memory$geneExpr <- .override_defaults(memory$geneExpr, geneExpr.args)
  }
  geneexpr_max_plots <- nrow(memory$geneExpr)

  if (is.numeric(colData.args)) { 
    memory$colData <- colDataPlotDefaults(se, colData.args)
  } else {
    memory$colData <- colDataPlotDefaults(se, nrow(colData.args)) 
    memory$colData <- .override_defaults(memory$colData, colData.args)
  }
  coldata_max_plots <- nrow(memory$colData)
 
  genestat_max_tab <- 5
  memory$geneStat <- DataFrame(Selected=rep(1, genestat_max_tab), Search="", PanelWidth=4)

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
    active_plots <- data.frame(Type=c("redDim", "colData", "geneExpr", "geneStat"),
                               ID=1, 
                               stringsAsFactors=FALSE)

    rObjects <- reactiveValues(
        active_plots = active_plots,
        resized = 1,
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
        (rObjects$resized) 
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
                rObjects$active_plots <- rbind(all.active, DataFrame(Type=mode0, ID=first.missing[1]))
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
                    new.width <- input[[paste0(mode0, i0, .organizationWidth)]]
                    cur.width <- pObjects$memory[[mode0]][[.organizationWidth]][i0]
                    if (!isTRUE(all.equal(new.width, cur.width))) { 
                        pObjects$memory[[mode0]][[.organizationWidth]][i0] <- new.width
                        rObjects$resized <- rObjects$resized + 1L
                    }
                })

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
              if (is.null(input[[.inputColData(field, i0)]])) { next } 
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
              if (is.null(input[[.inputColData(field, i0)]])) { next } ##### Placeholder, to remove!!!
              pObjects$memory$colData[[field]][i0] <- input[[.inputColData(field, i0)]]
          }
          
          # Creating the plot.
          .make_colDataPlot(se, pObjects$memory$colData[i0,], input)
        })

        observeEvent(input[[.inputColData(.plotParamPanelName, i0)]], {
          opened <- input[[.inputColData(.plotParamPanelName, i0)]] 
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
              if (is.null(input[[.inputGeneExpr(field, i0)]])) { next } ##### Placeholder, to remove!!!
              pObjects$memory$geneExpr[[field]][i0] <- input[[.inputGeneExpr(field, i0)]]
          }

          # Creating the plot.
          .make_geneExprPlot(se, pObjects$memory$geneExpr[i0,], input)
        }) 

        observeEvent(input[[.inputGeneExpr(.plotParamPanelName, i0)]], {
          opened <- input[[.inputGeneExpr(.plotParamPanelName, i0)]] 
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

