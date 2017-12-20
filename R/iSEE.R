
#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' iSEE does this
#'
#' and does that in detail
#'
#' @param se somethingaboutit
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
  redDim.default=5,
  geneExpr.default=5 
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
  if (is.numeric(redDim.default)) { 
    reddim_plot_param <- redDimPlotDefaults(se, redDim.default)
  } else {
    reddim_plot_param <- redDimPlotDefaults(se, nrow(redDim.default)) 
    reddim_plot_param <- .override_defaults(reddim_plot_default, redDim.default)
  }
  reddim_max_plots <- nrow(reddim_plot_param)
                                          
  if (is.numeric(geneExpr.default)) { 
    geneexpr_plot_param <- geneExprPlotDefaults(se, geneExpr.default)
  } else {
    geneexpr_plot_param <- geneExprPlotDefaults(se, nrow(geneExpr.default)) 
    geneexpr_plot_param <- .override_defaults(geneexpr_plot_default, geneExpr.default)
  }
  geneexpr_max_plots <- nrow(geneexpr_plot_param)
  
  # for retrieving the annotation
  annoSpecies_df <-
    data.frame(species=c("","Anopheles","Arabidopsis","Bovine","Worm",
                         "Canine","Fly","Zebrafish","E coli strain K12",
                         "E coli strain Sakai","Chicken","Human","Mouse",
                         "Rhesus","Malaria","Chimp","Rat",
                         "Yeast","Streptomyces coelicolor", "Pig","Toxoplasma gondii",
                         "Xenopus"),
               pkg=c("","org.Ag.eg.db", "org.At.tair.db", "org.Bt.eg.db", "org.Ce.eg.db",
                     "org.Cf.eg.db", "org.Dm.eg.db", "org.Dr.eg.db", "org.EcK12.eg.db",
                     "org.EcSakai.eg.db", "org.Gg.eg.db", "org.Hs.eg.db", "org.Mm.eg.db",
                     "org.Mmu.eg.db", "org.Pf.plasmo.db", "org.Pt.eg.db", "org.Rn.eg.db",
                     "org.Sc.sgd.db", "org.Sco.eg.db", "org.Ss.eg.db", "org.Tgondii.eg.db",
                     "org.Xl.eg.db"),
               stringsAsFactors = FALSE)
  annoSpecies_df <- annoSpecies_df[order(annoSpecies_df$species),]
  rownames(annoSpecies_df) <- annoSpecies_df$species # easier to access afterwards
  
  
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
      )
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
      
      # main tabBox
      tabBox(
        width=12,
        
        tabPanel(title = "Reduced dimension plots",  icon = icon("home"), value="tab-reddim",
                 uiOutput("redDimPlots"),
                 actionButton("addRedDimPlot", "New plot",
                              class = "btn btn-primary",icon = icon("plus"))
        ),
        
        tabPanel(title = "Gene expression plots",  icon = icon("flash"), value="tab-geneexpr",
                 uiOutput("geneExprPlots"),
                 actionButton("addGeneExprPlot", "New plot",
                              class = "btn btn-primary",icon = icon("plus"))
        ),
        
        tabPanel(title = "Gene-level statistics",  icon = icon("calendar"), value="tab-genetab",
                 dataTableOutput("geneStatTab"),
                 fluidRow(
                   column(6,
                          selectInput("geneStatSpeciesSelect",
                                      label = "Select the species of your samples",
                                      choices = annoSpecies_df$species,selected=""),
                          verbatimTextOutput("geneStatSpeciesPkg"),
                          selectInput("geneStatIDType", "select the id type in your data", 
                                      choices=c("ENSEMBL","ENTREZID","REFSEQ","SYMBOL"),
                                      selected = "SYMBOL"),
                          verbatimTextOutput("geneStatIDDebug")
                   ),
                   column(6,
                          htmlOutput("geneStatInfoBox")
                   )
                 )
        ),
        
        tabPanel(title = "tab3!",  icon = icon("table"), value="tab-t3",
                 h2("Content t3!")
                 # , content will go here!
        )
      ),
      
      iSEE_footer()
      
    ), # end of dashboardBody
    skin = "blue"
  ) # end of dashboardPage
  
  
  ########## server definition ##########

  iSEE_server <- function(input, output, session) {

    # storage for all the reactive objects
    rObjects <- reactiveValues(
      reddim_active_plots = 1,
      geneexpr_active_plots = 1,
      se = NULL
    )
    
    # storage for other persistent objects
    pObjects <- new.env()
    pObjects$reddim_plot_param <- reddim_plot_param
    pObjects$geneexpr_plot_param <- geneexpr_plot_param
    
    if (!is.null(se)){ rObjects$sce <- as(se, "SingleCellExperiment") }
    
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
    # Reduced dimension scatter plot section.
    #######################################################################
    
    # Multiple scatterplots colored by covariates,
    # nicked from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    output$redDimPlots <- renderUI({
        collected <- vector("list", length(rObjects$reddim_active_plots)*2)
        counter <- 1L

        for (i in rObjects$reddim_active_plots) { 
            param_choices <- pObjects$reddim_plot_param[i,]
            chosen.open <- character(0)
            if (param_choices[[.redDimPlotPanel]]) {
                chosen.open <- c(chosen.open, .redDimPlotParamPanelTitle)
            }

            collected[[counter]] <- fluidRow(
              column(6, plotOutput(.redDimPlot(i))),
              column(3, 
                     selectInput(.inputRedDim(.redDimType, i), label="Type",
                                 choices=red.dim.names, selected=param_choices[[.redDimType]]),
                     textInput(.inputRedDim(.redDimXAxis, i), label="Dimension 1",
                               value=param_choices[[.redDimXAxis]]),
                     textInput(.inputRedDim(.redDimYAxis, i), label="Dimension 2",
                               value=param_choices[[.redDimYAxis]]),
                     actionButton(.redDimDiscard(i), "Remove plot",
                                  icon = icon("trash"),class = "btn btn-warning")
                     ),
              column(3, 
                     shinyBS::bsCollapse(
                       id = .inputRedDim(.redDimPlotPanel, i),
                       open = chosen.open,
                       shinyBS::bsCollapsePanel(
                            title = .redDimPlotParamPanelTitle,
                            radioButtons(.inputRedDim(.redDimColorBy, i), 
                                         label="Color by:", inline=FALSE,
                                         choices=c(.colorByColDataTitle, .colorByGeneExprsTitle),
                                         selected=param_choices[[.redDimColorBy]]),
                            selectInput(.inputRedDim(.redDimColorByColData, i), 
                                        label = "Column data:",
                                        choices=covariates, selected=param_choices[[.redDimColorByColData]]),
                            textInput(.inputRedDim(.redDimColorByGeneExprs, i),
                                      label = "Gene expression:",
                                      value=param_choices[[.redDimColorByGeneExprs]]),
                            selectInput(.inputRedDim(.redDimColorByGeneExprsAssay, i), label=NULL,
                                        choices=all.assays, selected=param_choices[[.redDimColorByGeneExprsAssay]])
                                                ) # end of bsCollapsePanel
                       ) # end of bsCollapse
                     ) # end of column
            ) # end of fluidRow

            counter <- counter + 1L
            collected[[counter]] <- hr()
            counter <- counter + 1L
        }

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, collected)
    })
    
    # Plot addition and removal.
    observeEvent(input$addRedDimPlot, {
        first.missing <- setdiff(seq_len(reddim_max_plots), rObjects$reddim_active_plots)
        rObjects$reddim_active_plots <- c(rObjects$reddim_active_plots, first.missing[1])
    })
    
    for (i in seq_len(reddim_max_plots)) {
      local({
        i0 <- i
        observeEvent(input[[.redDimDiscard(i0)]], {
          rObjects$reddim_active_plots <- setdiff(rObjects$reddim_active_plots, i0)
        })
      })
    }

    for (i in seq_len(reddim_max_plots)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        i0 <- i
        output[[.redDimPlot(i0)]] <- renderPlot({

          # Updating parameters (non-characters need some careful treatment).
          for (field in c(.redDimType,  .redDimColorBy, .redDimColorByColData, 
                          .redDimColorByGeneExprs, .redDimColorByGeneExprsAssay)) { 
              pObjects$reddim_plot_param[[field]][i0] <- input[[.inputRedDim(field, i0)]]
          }
          for (field in c(.redDimXAxis, .redDimYAxis)) { 
              pObjects$reddim_plot_param[[field]][i0] <- as.integer(input[[.inputRedDim(field, i0)]])
          }
          pObjects$reddim_plot_param[[.redDimPlotPanel]][i0] <- .redDimPlotParamPanelTitle %in% input[[.inputRedDim(.redDimPlotPanel, i0)]] 
          
          # Setting up the parameter choices for this plot.
          param_choices <- pObjects$reddim_plot_param[i0,]
          red.dim <- reducedDim(se, param_choices[[.redDimType]])
          if (param_choices[[.redDimColorBy]]==.colorByColDataTitle) {
            covariate <- cell.data[,param_choices[[.redDimColorByColData]]]
          } else {
            gene.id <- param_choices[[.redDimColorByGeneExprs]]
            covariate <- assay(se, param_choices[[.redDimColorByGeneExprsAssay]])[gene.id,]
          }
         
          # Creating the plot. 
          plot.data <- data.frame(Dim1=red.dim[,param_choices[[.redDimXAxis]]],
                                  Dim2=red.dim[,param_choices[[.redDimYAxis]]],
                                  Covariate=covariate)
          ggplot(plot.data, aes_string(x="Dim1", y="Dim2", color="Covariate")) +
            geom_point(size=1.5) +
            labs(color=input$colorBy) +
            theme_void()
        })
      })
    }
    
    #######################################################################
    # Gene expression scatter plot section.
    #######################################################################
    
    # Multiple scatterplots.
    output$geneExprPlots <- renderUI({
      collected <- vector("list", length(rObjects$reddim_active_plots)*2)
      counter <- 1L

      for (i in rObjects$geneexpr_active_plots) { 
        param_choices <- pObjects$geneexpr_plot_param[i,]
        chosen.open <- character(0)
        if (param_choices[[.geneExprPlotPanel]]) {
            chosen.open <- c(chosen.open, .geneExprPlotParamPanelTitle)
        }
           
        collected[[counter]] <- fluidRow(
          column(6, plotOutput(.geneExprPlot(i))),
          column(3,
                 textInput(.inputGeneExpr(.geneExprID, i), label = "Gene of interest (Y-axis):",
                           value=param_choices[[.geneExprID]]),
                 selectInput(.inputGeneExpr(.geneExprAssay, i), label=NULL,
                             choices=all.assays, selected=param_choices[[.geneExprAssay]]),
                 radioButtons(.inputGeneExpr(.geneExprXAxis, i), label="X-axis:", 
                              inline=FALSE, 
                              choices=c(.geneExprXAxisColDataTitle, .geneExprXAxisGeneExprsTitle),
                              selected=param_choices[[.geneExprXAxis]]),
                 selectInput(.inputGeneExpr(.geneExprXAxisColData, i), 
                             label = "X-axis column data:", 
                             choices=covariates, selected=param_choices[[.geneExprXAxisColData]]),
                 textInput(.inputGeneExpr(.geneExprXAxisGeneExprs, i),
                           label = "X-axis gene expression:", 
                           value=param_choices[[.geneExprXAxisGeneExprs]]),
                 actionButton(.geneExprDiscard(i), "Remove plot",
                              icon = icon("trash"),class = "btn btn-warning")
                 ),
          column(3, 
                 shinyBS::bsCollapse(
                   id = .inputGeneExpr(.geneExprPlotPanel, i),
                   open = chosen.open, 
                   shinyBS::bsCollapsePanel(
                     title = .geneExprPlotParamPanelTitle,
                     radioButtons(.inputGeneExpr(.geneExprColorBy, i), 
                                  label="Colour by:", inline=FALSE, 
                                  choices=c(.colorByColDataTitle, .colorByGeneExprsTitle),
                                  selected=param_choices[[.geneExprColorBy]]),
                    selectInput(.inputGeneExpr(.geneExprColorByColData, i),
                                label = "Colour by column data:",
                                choices=covariates, 
                                selected=param_choices[[.geneExprColorByColData]]),
                    textInput(.inputGeneExpr(.geneExprColorByGeneExprs, i),
                              label = "Colour by gene expression:",
                              value=param_choices[[.geneExprColorByGeneExprs]])
                   ) # end of bsCollapsePanel
                 ) # end of bsCollapse
          ) # end of column
        ) # end of fluidRow

        counter <- counter + 1L
        collected[[counter]] <- hr()
        counter <- counter + 1L
      }
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, collected)
    }) # end of output$geneExprPlots
    
    # Plot addition and removal, as well as parameter setting.
    observeEvent(input$addGeneExprPlot, {
      first.missing <- setdiff(seq_len(geneexpr_max_plots), rObjects$geneexpr_active_plots)
      rObjects$geneexpr_active_plots <- c(rObjects$geneexpr_active_plots, first.missing[1])
    })
    
    for (i in seq_len(geneexpr_max_plots)) {
      local({
        i0 <- i
        observeEvent(input[[.geneExprDiscard(i0)]], {
          rObjects$geneexpr_active_plots <- setdiff(rObjects$geneexpr_active_plots, i0)
        }) # end of observeEvent
      }) # end of local
    }
    
    for (i in seq_len(geneexpr_max_plots)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        i0 <- i
        output[[.geneExprPlot(i0)]] <- renderPlot({
          # Updating parameters.
          for (field in c(.geneExprID, .geneExprAssay, .geneExprXAxis, .geneExprXAxisColData, .geneExprXAxisGeneExprs,
                          .geneExprColorBy, .geneExprColorByColData, .geneExprColorByGeneExprs)) { 
              pObjects$geneexpr_plot_param[[field]][i0] <- input[[.inputGeneExpr(field, i0)]]
          }
          pObjects$geneexpr_plot_param[[.geneExprPlotPanel]][i0] <- .geneExprPlotParamPanelTitle %in% input[[.inputGeneExpr(.geneExprPlotPanel, i0)]] 

          # Setting up the parameter choices. 
          param_choices <- pObjects$geneexpr_plot_param[i0,]
          if (param_choices[[.geneExprColorBy]]==.colorByColDataTitle) {
            covariate <- colData(se)[,param_choices[[.geneExprColorByColData]]]
            covariate.name <- param_choices[[.geneExprColorByColData]]
          } else {
            covariate.name <- param_choices[[.geneExprColorByGeneExprs]]
            covariate <- assay(se, param_choices[[.geneExprAssay]])[covariate.name]
          }

          # Creating the plot object.
          if (param_choices[[.geneExprID]] %in% gene.names) {
            plotExpression(se, exprs_values=param_choices[[.geneExprAssay]],
                           x=ifelse(param_choices[[.geneExprXAxis]]==.geneExprXAxisColDataTitle,
                                    param_choices[[.geneExprXAxisColData]], 
                                    param_choices[[.geneExprXAxisGeneExprs]]),
                           features=param_choices[[.geneExprID]],
                           colour_by=setNames(data.frame(covariate),
                                              covariate.name))
          }
        }) # end of output[[plotname]]
      }) # end of local
    }
    
    #######################################################################
    # Gene table section.
    #######################################################################
    
    # Load the gene level data
    output$geneStatTab <- renderDataTable({
      datatable(gene.data, filter="top", rownames=TRUE,
                selection=list(mode="single", selected=1))
    })
    
    # For annotation and gene info box
    output$geneStatSpeciesPkg <- renderText({
      shiny::validate(
        need(input$geneStatSpeciesSelect!="",
             "Select a species - requires the corresponding annotation package"
        )
      )
      
      annopkg <- annoSpecies_df$pkg[annoSpecies_df$species==input$geneStatSpeciesSelect]
      shiny::validate(
        need(require(annopkg,character.only=TRUE),
             paste0("The package ",annopkg, " is not installed/available. Try installing it with biocLite('",annopkg,"')"))
      )
      retmsg <- paste0(annopkg," - package available and loaded")
      retmsg <- paste0(retmsg," - ",gsub(".eg.db","",gsub("org.","",annopkg)))
      retmsg
    })
    
    output$geneStatIDDebug <- renderText({
      dim(annoSpecies_df)
      # annoSpecies_df[input$geneStatSpeciesSelect,]$pkg
      # input$geneExprID1, "ENTREZID", input$geneStatIDType)
      # mapIds(get(annoSpecies_df[input$geneStatSpeciesSelect,]$pkg),
      # selectedGene, "ENTREZID", input$geneStatIDType)
    })
    
    output$geneStatInfoBox <- renderUI({
      shiny::validate(
        need(input$geneStatSpeciesSelect!="",
             "Select a species - requires the corresponding annotation package"
        )
      )
      
      shiny::validate(
        need(!is.null(input$geneStatTab_rows_selected),
             "Select a gene from the table"
        )
      )
      selectedGene <- gene.names[input$geneStatTab_rows_selected]
      
      selgene_entrez <- mapIds(get(annoSpecies_df[input$geneStatSpeciesSelect,]$pkg),
                               selectedGene, "ENTREZID", input$geneStatIDType)
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

