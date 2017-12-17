
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
  se 
) {
  
  cell.data <- colData(se)
  covariates <- colnames(cell.data)
  # attempt to find "categorical" covariates useful for e.g. violin plots.
  # Remove covariates with only one value, and those with too many unique
  # values.
  covariatescat <- 
    colnames(cell.data)[apply(cell.data, 2, 
                              function(w) length(unique(w)) <= 0.5 * length(w) & 
                                length(unique(w)) > 1)]
  red.dim <- reducedDim(se)
  red.dim.names <- reducedDimNames(se)
  allassays <- names(assays(se))
  allgenes <- unique(unlist(lapply(assays(se), function(x) rownames(x))))
  
  # general options:
  max_plots <- 5

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


        tabPanel(title = "Reduced dimension scatter plots",  icon = icon("home"), value="tab-welcome",
                uiOutput("redDimPlots"),
                selectInput("colorBy", label = "Color by", choices=covariates, selected=covariates[1]),
                actionButton("addRedDimPlot", "New plot")
                ),

        tabPanel(title = "Inidividual gene expression",  icon = icon("flash"), value="tab-violin",
                 fluidRow(
                   column(4, selectInput("violinvalues", label = "Type of values", 
                                         choices = allassays, 
                                         selected = ifelse("logcounts" %in% allassays, 
                                                           "logcounts", allassays[1]))),
                   column(4, selectInput("violingenes", label = "Gene",
                                         choices = allgenes, 
                                         selected = allgenes[1], multiple = TRUE, 
                                         selectize = TRUE)),
                   column(4, selectInput("violinX", label = "Group by", 
                                         choices = covariatescat,
                                         selected = covariatescat[1]))
                 ), # end of fluidRow
                 plotOutput("violinPlot")
                 # , content will go here!
        ),
        
        tabPanel(title = "t2!",  icon = icon("calendar"), value="tab-t2",
                 h2("Content t2!")
                 # , content will go here!
                 ),

        tabPanel(title = "tab3!",  icon = icon("table"), value="tab-t3",
                 h2("Content t3!")
                 # , content will go here!
                 )



      ),

      iSEE_footer()

    ), # end of dashboardBody
  skin = "blue"
  )


  ########## server definition ##########

  iSEE_server <- function(input, output) {

    # storage for all the reactive objects
    rObjects <- reactiveValues(
        active_plots = 1,                   
        se = NULL
    )

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

    # Multiple scatterplots colored by covariates,
    # nicked from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    output$redDimPlots <- renderUI({
        plot_output_list <- lapply(rObjects$active_plots, function(i) {
            fluidRow(
                column(6, plotOutput(paste0("redDimPlot", i))),
                column(4, 
                    selectInput(paste0("redDimType", i), label="Type", choices=red.dim.names, selected=red.dim.names[1]),
                    textInput(paste0("redDimChoice", i, "_1"), label="Dimension 1", value=1),
                    textInput(paste0("redDimChoice", i, "_2"), label="Dimension 2", value=2),
                    actionButton(paste0("removeRedDimPlot", i), "Remove plot")
                    )
                )
        })

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })

    for (i in seq_len(max_plots)) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
            plotname <- paste0("redDimPlot", i)
            typename <- paste0("redDimType", i)
            dim1name <- paste0("redDimChoice", i, "_1")
            dim2name <- paste0("redDimChoice", i, "_2")
            output[[plotname]] <- renderPlot({
                red.dim <- reducedDim(se, input[[typename]])
                plot.data <- data.frame(Dim1=red.dim[,as.integer(input[[dim1name]])], 
                                        Dim2=red.dim[,as.integer(input[[dim2name]])], 
                                        Covariate=cell.data[,input$colorBy])
                ggplot(plot.data, aes_string(x="Dim1", y="Dim2", color="Covariate")) +
                    geom_point(size=1.5) +
                    labs(color=input$colorBy) +
                    theme_void()
            })
        })
    }

    # Plot addition and removal.
    observeEvent(input$addRedDimPlot, {
        first.missing <- setdiff(seq_len(max_plots), rObjects$active_plots)
        rObjects$active_plots <- c(rObjects$active_plots, first.missing[1])             
    })

    for (i in seq_len(max_plots)) {
        local({
            my_i <- i
            observeEvent(input[[paste0("removeRedDimPlot", my_i)]], {
                rObjects$active_plots <- setdiff(rObjects$active_plots, my_i)
            })
        })
    }
    
    # Violin plot of one or more genes
    output$violinPlot <- renderPlot({
      if (!all(input$violingenes == "")) {
        violin.data <- as.data.frame(assays(se)[[input$violinvalues]][input$violingenes, , 
                                                                      drop = FALSE]) %>%
          tibble::rownames_to_column("gene") %>% 
          reshape2::melt() %>%
          dplyr::left_join(as.data.frame(cell.data) %>% tibble::rownames_to_column("variable"))
        ggplot(violin.data, aes_string(x=input$violinX, y="value", fill = input$violinX)) + 
          geom_violin() + theme_bw() + facet_wrap(~ gene, scales = "free_y") + 
          ylab(input$violinvalues)
      }
    }) # end of output$violinPlot
    
  } # end of iSEE_server

  ########## launch app ##########

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}

