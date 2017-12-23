
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
  if (is.numeric(redDim.args)) { 
    reddim_plot_param <- redDimPlotDefaults(se, redDim.args)
  } else {
    reddim_plot_param <- redDimPlotDefaults(se, nrow(redDim.args)) 
    reddim_plot_param <- .override_defaults(reddim_plot_param, redDim.args)
  }
  reddim_max_plots <- nrow(reddim_plot_param)
                                          
  if (is.numeric(geneExpr.args)) { 
    geneexpr_plot_param <- geneExprPlotDefaults(se, geneExpr.args)
  } else {
    geneexpr_plot_param <- geneExprPlotDefaults(se, nrow(geneExpr.args)) 
    geneexpr_plot_param <- .override_defaults(geneexpr_plot_param, geneExpr.args)
  }
  geneexpr_max_plots <- nrow(geneexpr_plot_param)

  if (is.numeric(colData.args)) { 
    phenodata_plot_param <- colDataPlotDefaults(se, colData.args)
  } else {
    phenodata_plot_param <- colDataPlotDefaults(se, nrow(colData.args)) 
    phenodata_plot_param <- .override_defaults(phenodata_plot_param, colData.args)
  }
  phenodata_max_plots <- nrow(phenodata_plot_param)
 
  genestat_max_tab <- 5
  genestat_tab_param <- DataFrame(Selected=rep(1, genestat_max_tab))

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
      actionButton("addRedDimPlot", "New reduced dimension plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton("addPhenoDataPlot", "New column data plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton("addGeneExprPlot", "New gene expression plot", class = "btn btn-primary",icon = icon("plus")),
      actionButton("addGeneStatTable", "New gene table", class = "btn btn-primary",icon = icon("plus"))
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

      uiOutput("everything"),             
      
      iSEE_footer()
      
    ), # end of dashboardBody
    skin = "blue"
  ) # end of dashboardPage
  
  
  ########## server definition ##########

  iSEE_server <- function(input, output, session) {

    # storage for all the reactive objects
    rObjects <- reactiveValues(
      active_plots = data.frame(Type=c("redDim", "phenoData", "geneExpr", "geneStat"),
                                ID=1)
    )
    
    # storage for other persistent objects
    pObjects <- new.env()
    pObjects$reddim_plot_param <- reddim_plot_param
    pObjects$geneexpr_plot_param <- geneexpr_plot_param
    pObjects$phenodata_plot_param <- phenodata_plot_param
    pObjects$genestat_tab_param <- genestat_tab_param
    
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
    #######################################################################
    
    # Multiple scatterplots colored by covariates,
    # nicked from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    output$everything <- renderUI({
        collected <- list()
        counter <- 1L
        cur.width <- 0L
        cur.row <- list()
        row.counter <- 1L

        # Defining currently active tables to use in linking.
        active.tab <- rObjects$active_plots$ID[rObjects$active_plots$Type=="geneStat"]

        for (j in seq_len(nrow(rObjects$active_plots))) { 
            mode <- rObjects$active_plots$Type[j]
            i <- rObjects$active_plots$ID[j]

            if (mode=="redDim") {                               
                param_choices <- pObjects$reddim_plot_param[i,]
                stuff <- list(
                     h4(paste("Reduced dimension plot", i)),                              
                     plotOutput(.redDimPlot(i)),
                     selectInput(.inputRedDim(.redDimType, i), label="Type",
                                 choices=red.dim.names, selected=param_choices[[.redDimType]]),
                     textInput(.inputRedDim(.redDimXAxis, i), label="Dimension 1",
                               value=param_choices[[.redDimXAxis]]),
                     textInput(.inputRedDim(.redDimYAxis, i), label="Dimension 2",
                               value=param_choices[[.redDimYAxis]])
                     )
            } else if (mode=="phenoData") {
                param_choices <- pObjects$phenodata_plot_param[i,]
                stuff <- list(
                    h4(paste("Column data plot", i)),                              
                     plotOutput(.phenoDataPlot(i)),
                     selectInput(.inputPhenoData(.phenoDataYAxisColData, i), 
                                 label = "Column of interest (Y-axis):",
                                 choices=covariates, selected=param_choices[[.phenoDataYAxisColData]]),
                     radioButtons(.inputPhenoData(.phenoDataXAxis, i), label="X-axis:", 
                                  inline=FALSE, 
                                  choices=c(.phenoDataXAxisNothingTitle, .phenoDataXAxisColDataTitle),
                                  selected=param_choices[[.phenoDataXAxis]]),
                     selectInput(.inputPhenoData(.phenoDataXAxisColData, i), 
                                 label = "Column of interest (X-axis):",
                                 choices=covariates, selected=param_choices[[.phenoDataXAxisColData]])
                     )
            } else if (mode=="geneExpr") {
                param_choices <- pObjects$geneexpr_plot_param[i,]
                stuff <- list(
                    h4(paste("Gene expression plot", i)),                              
                    plotOutput(.geneExprPlot(i)),
                    selectInput(.inputGeneExpr(.geneExprID, i), label = "Linked gene statistics table:",
                                choices=active.tab, selected=param_choices[[.geneExprID]]),
                     selectInput(.inputGeneExpr(.geneExprAssay, i), label=NULL,
                                 choices=all.assays, selected=param_choices[[.geneExprAssay]]),
                     radioButtons(.inputGeneExpr(.geneExprXAxis, i), label="X-axis:", 
                                  inline=FALSE, 
                                  choices=c(.geneExprXAxisNothingTitle, .geneExprXAxisColDataTitle, .geneExprXAxisGeneExprsTitle),
                                  selected=param_choices[[.geneExprXAxis]]),
                     selectInput(.inputGeneExpr(.geneExprXAxisColData, i), 
                                 label = "X-axis column data:", 
                                 choices=covariates, selected=param_choices[[.geneExprXAxisColData]]),
                     textInput(.inputGeneExpr(.geneExprXAxisGeneExprs, i),
                               label = "X-axis gene expression:", 
                               value=param_choices[[.geneExprXAxisGeneExprs]])
                              )
            } else if (mode=="geneStat") {
                stuff <- list(
                     h4(paste("Gene statistics table", i)),
                     dataTableOutput(paste0("geneStatTable", i)))
            }

            # Adding graphical parameters if we're plotting.
            if (mode!="geneStat") { 
                chosen.open <- character(0)
                if (param_choices[[.redDimPlotPanel]]) {
                    chosen.open <- c(chosen.open, .redDimPlotParamPanelTitle)
                }
    
                param <- list(shinyBS::bsCollapse(
                    id = paste0(mode, .generalPlotPanel, i),
                    open = chosen.open,
                    shinyBS::bsCollapsePanel(
                        title = .generalPlotParamPanelTitle,
                        radioButtons(paste0(mode, .generalColorBy, i), 
                                     label="Color by:", inline=FALSE,
                                     choices=c(.colorByNothingTitle, .colorByColDataTitle, .colorByGeneExprsTitle),
                                     selected=param_choices[[.generalColorBy]]),
                        selectInput(paste0(mode, .generalColorByColData, i), 
                                    label = "Column data:",
                                    choices=covariates, selected=param_choices[[.generalColorByColData]]),
                        selectInput(paste0(mode, .geneExprID, i), label = "Linked gene statistics table:",
                                    choices=active.tab, selected=param_choices[[.generalColorByGeneExprs]]),  
                        selectInput(paste0(mode, .generalColorByGeneExprsAssay, i), label=NULL,
                                    choices=all.assays, selected=param_choices[[.generalColorByGeneExprsAssay]])
                        ) # end of bsCollapsePanel
                    ) # end of bsCollapse
                )
            } else {
                param <- list()
            }
            stuff <- c(stuff, param)
            
            # Deciding whether to continue on the current row, or start a new row.
            extra <- cur.width + 4L
            if (extra > 12L) {
                collected[[counter]] <- do.call(fluidRow, cur.row)
                counter <- counter + 1L
                collected[[counter]] <- hr()
                counter <- counter + 1L
                cur.row <- list()
                row.counter <- 1L
                cur.width <- 0L
            } 

            cur.row[[row.counter]] <- do.call(column, c(list(width=4), stuff))
            row.counter <- row.counter + 1L
            cur.width <- cur.width + 4L
        }

        # Cleaning up the leftovers.
        collected[[counter]] <- do.call(fluidRow, cur.row)
        counter <- counter + 1L
        collected[[counter]] <- hr()

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, collected)
    })
    
    #######################################################################
    # Panel addition and removal
    #######################################################################

    # Reduced dimension plots.
    observeEvent(input$addRedDimPlot, {
        all.active <- rObjects$active_plots
        first.missing <- setdiff(seq_len(reddim_max_plots), all.active$ID[all.active$Type=="redDim"])
        rObjects$active_plots <- rbind(all.active, DataFrame(Type="redDim", ID=first.missing[1]))
    })
    
    for (i in seq_len(reddim_max_plots)) {
      local({
        i0 <- i
        observeEvent(input[[.redDimDiscard(i0)]], {
          rObjects$reddim_active_plots <- setdiff(rObjects$reddim_active_plots, i0)
        })
      })
    }

    # Phenodata plots.
    observeEvent(input$addPhenoDataPlot, {
        all.active <- rObjects$active_plots
        first.missing <- setdiff(seq_len(reddim_max_plots), all.active$ID[all.active$Type=="phenoData"])
        rObjects$active_plots <- rbind(all.active, DataFrame(Type="phenoData", ID=first.missing[1]))
    })
    
    for (i in seq_len(phenodata_max_plots)) {
      local({
        i0 <- i
        observeEvent(input[[.phenoDataDiscard(i0)]], {
          rObjects$phenodata_active_plots <- setdiff(rObjects$phenodata_active_plots, i0)
        })
      })
    }

    # geneExpr plots.
    observeEvent(input$addGeneExprPlot, {
        all.active <- rObjects$active_plots
        first.missing <- setdiff(seq_len(reddim_max_plots), all.active$ID[all.active$Type=="geneExpr"])
        rObjects$active_plots <- rbind(all.active, DataFrame(Type="geneExpr", ID=first.missing[1]))
    })
    
    for (i in seq_len(geneexpr_max_plots)) {
      local({
        i0 <- i
        observeEvent(input[[.geneExprDiscard(i0)]], {
          rObjects$geneexpr_active_plots <- setdiff(rObjects$geneexpr_active_plots, i0)
        })
      }) 
    }
    
    #######################################################################
    # Reduced dimension plot section.
    #######################################################################

    for (i in seq_len(reddim_max_plots)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        i0 <- i
        output[[.redDimPlot(i0)]] <- renderPlot({

          # Updating parameters in the memory store (non-characters need some careful treatment).
          for (field in c(.redDimType, .generalColorBy, .generalColorByColData, 
                          .generalColorByGeneExprs, .generalColorByGeneExprsAssay)) { 
              if (is.null(input[[.inputRedDim(field, i0)]])) { next } ##### Placeholder, to remove!!!
              pObjects$reddim_plot_param[[field]][i0] <- input[[.inputRedDim(field, i0)]]
          }
          for (field in c(.redDimXAxis, .redDimYAxis)) { 
              pObjects$reddim_plot_param[[field]][i0] <- as.integer(input[[.inputRedDim(field, i0)]])
          }
          pObjects$reddim_plot_param[[.generalPlotPanel]][i0] <- .generalPlotParamPanelTitle %in% input[[.inputRedDim(.generalPlotPanel, i0)]] 
          
          # Setting up the parameter choices for this plot.
          param_choices <- pObjects$reddim_plot_param[i0,]
          red.dim <- reducedDim(se, param_choices[[.redDimType]])

          color_choice <- param_choices[[.generalColorBy]]
          if (color_choice==.colorByColDataTitle) {
            covariate.name <- param_choices[[.generalColorByColData]]
            covariate <- cell.data[,covariate.name]
            astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
          } else if (color_choice==.colorByGeneExprsTitle) {
            linked.tab <- paste0("geneStatTable", param_choices[[.generalColorByGeneExprs]], "_rows_selected")
            covariate.name <- gene.names[input[[linked.tab]]]
            covariate <- assay(se, param_choices[[.generalColorByGeneExprsAssay]])[covariate.name,]
            astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
          } else {
            covariate.name <- ""
            covariate <- NULL              
            astr <- aes_string(x="Dim1", y="Dim2")
          }

          plot.data <- data.frame(Dim1=red.dim[,param_choices[[.redDimXAxis]]],
                                  Dim2=red.dim[,param_choices[[.redDimYAxis]]])
          plot.data$Covariate <- covariate
         
          # Creating the plot. 
          ggplot(plot.data, astr) +
            geom_point(size=1.5) +
            labs(color=covariate.name) +
            theme_void()
        })
      })
    }
    
    #######################################################################
    # Phenodata scatter plot section.
    #######################################################################

    for (i in seq_len(phenodata_max_plots)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        i0 <- i
        output[[.phenoDataPlot(i0)]] <- renderPlot({

          # Updating parameters (non-characters need some careful treatment).
          for (field in c(.phenoDataYAxisColData, .phenoDataXAxis, .phenoDataXAxisColData,
                      .generalColorBy, .generalColorByColData, 
                      .generalColorByGeneExprs, .generalColorByGeneExprsAssay)) { 
              if (is.null(input[[.inputPhenoData(field, i0)]])) { next } ##### Placeholder, to remove!!!
              pObjects$phenodata_plot_param[[field]][i0] <- input[[.inputPhenoData(field, i0)]]
          }
          pObjects$phenodata_plot_param[[.generalPlotPanel]][i0] <- .generalPlotParamPanelTitle %in% input[[.inputPhenoData(.generalPlotPanel, i0)]] 
          
          # Setting up the parameter choices for this plot.
          param_choices <- pObjects$phenodata_plot_param[i0,]
          aes_args <- list(y=param_choices[[.phenoDataYAxisColData]])
#          if (param_choices[[.phenoDataXAxis]]!=.phenoDataXAxisNothingTitle) { # Currently not-quite-working as plotPhenoData needs 'x'.
              aes_args$x <- param_choices[[.phenoDataXAxisColData]]
#          }

          color_choice <- param_choices[[.generalColorBy]]
          if (color_choice==.colorByColDataTitle) {
            aes_args$color <- param_choices[[.generalColorByColData]]
          } else if (color_choice==.colorByGeneExprsTitle) {
            aes_args$color <- param_choices[[.generalColorByGeneExprs]]
          }
          aes_final <- do.call(aes_string, aes_args)
        
          # Creating the plot. 
          plotPhenoData(se, aes_final)
        })
      })
    }

    #######################################################################
    # Gene expression scatter plot section.
    #######################################################################
    
    for (i in seq_len(geneexpr_max_plots)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        i0 <- i
        output[[.geneExprPlot(i0)]] <- renderPlot({
          # Updating parameters.
          for (field in c(.geneExprID, .geneExprAssay, .geneExprXAxis, .geneExprXAxisColData, .geneExprXAxisGeneExprs,
                          .generalColorBy, .generalColorByColData, .generalColorByGeneExprs, .generalColorByGeneExprs)) {
              if (is.null(input[[.inputGeneExpr(field, i0)]])) { next } ##### Placeholder, to remove!!!
              pObjects$geneexpr_plot_param[[field]][i0] <- input[[.inputGeneExpr(field, i0)]]
          }
          pObjects$geneexpr_plot_param[[.geneExprPlotPanel]][i0] <- .geneExprPlotParamPanelTitle %in% input[[.inputGeneExpr(.geneExprPlotPanel, i0)]] 
          print(pObjects$genestat_tab_param$Selected)

          # Setting up the parameter choices. 
          param_choices <- pObjects$geneexpr_plot_param[i0,]
          xchoice <- param_choices[[.geneExprXAxis]]
          if (xchoice==.geneExprXAxisColDataTitle) {
            byx <- param_choices[[.geneExprXAxisColData]]
          } else if (xchoice==.geneExprXAxisGeneExprsTitle) {
            byx <- param_choices[[.geneExprXAxisGeneExprs]]
          } else {
            byx <- NULL
          }

          color_choice <- param_choices[[.generalColorBy]]
          if (color_choice==.colorByColDataTitle) {
            covariate.name <- param_choices[[.generalColorByColData]]
          } else if (color_choice==.colorByGeneExprsTitle) {
            covariate.name <- param_choices[[.generalColorByGeneExprs]]
          } else {
            covariate.name <- NULL
          }

          # Getting the gene choice.
          linked.tab <- paste0("geneStatTable", param_choices[[.geneExprID]], "_rows_selected")
          cur.gene <- gene.names[input[[linked.tab]]]
          plotExpression(se, exprs_values=param_choices[[.geneExprAssay]],
                         x=byx,
                         features=cur.gene,
                         colour_by=covariate.name)
        }) # end of output[[plotname]]
      }) # end of local
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
            chosen <- pObjects$genestat_tab_param$Selected[i0]
            print(chosen)
            datatable(gene.data, filter="top", rownames=TRUE,
                      selection=list(mode="single", selected=chosen))
        })

        observe({
            chosen <- input[[paste0("geneStatTable", i0, "_rows_selected")]]
            if (length(chosen)) {
                pObjects$genestat_tab_param$Selected[i0] <- chosen
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

