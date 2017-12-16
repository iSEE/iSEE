#' iSEE: interactive SingleCell/Summarized Experiment Explorer
#'
#' iSEE does this (description)
#'
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @importMethodsFrom BiocGenerics ncol nrow
#' @importFrom methods as
#' @importFrom rintrojs introjsUI
#' @importFrom shiny actionButton fluidRow icon reactiveValues renderUI
#' shinyApp tabPanel HTML br h1 h2 selectInput renderPlot plotOutput 
#' uiOutput sliderInput
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#' dashboardSidebar menuItem tabBox valueBox valueBoxOutput
#' @importFrom utils packageVersion
#' @importFrom shiny tags
#' @importFrom ggplot2 ggplot geom_point labs theme_void
#'
#' @author Aaron Lun \email{alun@@wehi.edu.au}
#' @author Charlotte Soneson \email{charlotte.soneson@@uzh.ch}
#' @author Federico Marini \email{marinif@@uni-mainz.de}
#' @author Kevin Rue-Albrecht \email{kevin.rue-albrecht@@kennedy.ox.ac.uk}
#'
#' Maintainer: John or Jane Doe \email{tobeclarified@@who.does.it}
#' @name iSEE-pkg
#' @docType package
NULL
