#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' iSEE does this (description)
#'
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importMethodsFrom BiocGenerics ncol nrow
#' @importFrom methods as new
#' @importFrom rintrojs introjsUI introjs
#' @importFrom shiny actionButton fluidRow icon reactiveValues renderUI
#' shinyApp tabPanel HTML br selectInput renderPlot plotOutput uiOutput 
#' sliderInput tagList textInput column observeEvent radioButtons
#' need tags hr brushOpts brushedPoints observe validate checkboxInput 
#' conditionalPanel renderPrint singleton showModal modalDialog 
#' updateSelectInput isolate showNotification p runApp
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#' dashboardSidebar menuItem tabBox valueBox valueBoxOutput dropdownMenu
#' notificationItem box
#' @importFrom utils packageVersion read.delim citation sessionInfo
#' @importFrom shinyjs useShinyjs disable enable disabled
#' @importFrom ggplot2 ggplot geom_point labs theme_void aes_string geom_violin
#'   theme_bw facet_wrap ylab aes theme scale_x_continuous scale_y_continuous
#'   coord_cartesian geom_tile scale_x_discrete scale_y_discrete
#'   scale_size_area guides
#' @importFrom vipor offsetX
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom AnnotationDbi mapIds keytypes
#' @importFrom rentrez entrez_summary
#' @importFrom colourpicker colourInput
#' @importFrom shinyAce aceEditor
#' @importFrom methods new slot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom igraph make_graph topo_sort are_adjacent incident add_edges
#' adjacent_vertices is_dag simplify V delete.vertices set_vertex_attr
#' @importFrom graphics plot
#' @author Aaron Lun \email{alun@@wehi.edu.au}
#' @author Charlotte Soneson \email{charlotte.soneson@@uzh.ch}
#' @author Federico Marini \email{marinif@@uni-mainz.de}
#' @author Kevin Rue-Albrecht \email{kevin.rue-albrecht@@kennedy.ox.ac.uk}
#'
#' Maintainer: John or Jane Doe \email{tobeclarified@@who.does.it}
#' @name iSEE-pkg
#' @docType package
NULL
