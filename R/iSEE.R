
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
  red.dim <- reducedDim(se)
  plot.data <- data.frame(Dim1=red.dim[,1], Dim2=red.dim[,2])
  
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
                sliderInput("n", "Number of plots", value=1, min=1, max=max_plots)
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
        plot_output_list <- lapply(1:input$n, function(i) {
                                   plotname <- paste0("redDimPlot", i)
                                   plotOutput(plotname, height = 280, width = 250)
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
            my_i <- i
            plotname <- paste("redDimPlot", my_i, sep="")
            output[[plotname]] <- renderPlot({
                plot.data.copy <- data.frame(plot.data, Covariate=cell.data[,input$colorBy])
                ggplot(plot.data.copy, aes_string(x="Dim1", y="Dim2", color="Covariate")) +
                    geom_point(size=1.5) +
                    labs(color=input$colorBy) +
                    theme_void()
            })
        })
    }
  }

  ########## launch app ##########

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}

