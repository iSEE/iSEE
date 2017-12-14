
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
#' library(SingleCellExperiment)
#' library(scRNAseq)
#' data(allen)
#' class(allen)
#' sce <- as(allen, "SingleCellExperiment")
#' sce
#' # launch the app itself
#' if (interactive()) { iSEE(sce) }
iSEE <- function(
  se = NULL
) {

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


        tabPanel(title = "Welcome!",  icon = icon("home"), value="tab-welcome",
                 h2("Content!")
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



  }


  ########## launch app ##########

  shinyApp(ui = iSEE_ui, server = iSEE_server)
}

