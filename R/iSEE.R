
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
  reddim_active_plots <- which(reddim_plot_param$Active)

  if (is.numeric(geneExpr.args)) {
    geneexpr_plot_param <- geneExprPlotDefaults(se, geneExpr.args)
  } else {
    geneexpr_plot_param <- geneExprPlotDefaults(se, nrow(geneExpr.args))
    geneexpr_plot_param <- .override_defaults(geneexpr_plot_param, geneExpr.args)
  }
  geneexpr_max_plots <- nrow(geneexpr_plot_param)
  geneexpr_active_plots <- which(geneexpr_plot_param$Active)

  if (is.numeric(colData.args)) {
    phenodata_plot_param <- colDataPlotDefaults(se, colData.args)
  } else {
    phenodata_plot_param <- colDataPlotDefaults(se, nrow(colData.args))
    phenodata_plot_param <- .override_defaults(phenodata_plot_param, colData.args)
  }
  phenodata_max_plots <- nrow(phenodata_plot_param)
  phenodata_active_plots <- which(phenodata_plot_param$Active)

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

        tabPanel(title = "Column data plots",  icon = icon("table"), value="tab-phenodata",
                 uiOutput("phenoDataPlots"),
                 actionButton("addPhenoDataPlot", "New plot",
                              class = "btn btn-primary",icon = icon("plus"))
        ),

        tabPanel(title = "Gene expression plots",  icon = icon("flash"), value="tab-geneexpr",
                 uiOutput("geneExprPlots"),
                 actionButton("addGeneExprPlot", "New plot",
                              class = "btn btn-primary",icon = icon("plus"))
        ),

        tabPanel(title = "Gene-level statistics",  icon = icon("calendar"), value="tab-genetab",
                 dataTableOutput("geneStatTab"),
                 hr(),
                 htmlOutput("geneStatInfoBox")
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
      reddim_active_plots = reddim_active_plots,
      geneexpr_active_plots = geneexpr_active_plots,
      phenodata_active_plots = phenodata_active_plots
    )

    # storage for other persistent objects
    pObjects <- new.env()
    pObjects$reddim_plot_param <- reddim_plot_param
    pObjects$geneexpr_plot_param <- geneexpr_plot_param
    pObjects$phenodata_plot_param <- phenodata_plot_param

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
                                         choices=c(.colorByNothingTitle, .colorByColDataTitle, .colorByGeneExprsTitle),
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

          color_choice <- param_choices[[.redDimColorBy]]
          if (color_choice==.colorByColDataTitle) {
            covariate.name <- param_choices[[.redDimColorByColData]]
            covariate <- cell.data[,covariate.name]
            astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
          } else if (color_choice==.colorByGeneExprsTitle) {
            covariate.name <- param_choices[[.redDimColorByGeneExprs]]
            # Check if gene identifier is valid; or suggest close matches
            covariate.idx <- which(rownames(se) == covariate.name)
            if (length(covariate.idx) == 0){
                # multiple matches are not possible in rownames
                # TODO: experiment scanning other selected fields for the gene
                covariate.similar <- agrep(
                    pattern = covariate.name,
                    x = rownames(se),
                    max.distance = 1, # NOTE: may be relaxed/parameterised
                    fixed = FALSE, # NOTE: experimental; allows regex match
                    value = TRUE
                )
                shiny::validate(
                    need(length(covariate.idx) == 1, sprintf(
                        "Invalid gene identifier: %s\nDid you mean: %s",
                        covariate.name,
                        paste(
                            sprintf("%s", covariate.similar),
                            collapse = ", "
                        )
                    ))
                )
            }
            covariate <- assay(se, param_choices[[.redDimColorByGeneExprsAssay]])[covariate.name,]
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

    output$phenoDataPlots <- renderUI({
        collected <- vector("list", length(rObjects$phenodata_active_plots)*2)
        counter <- 1L

        for (i in rObjects$phenodata_active_plots) {
            param_choices <- pObjects$phenodata_plot_param[i,]
            chosen.open <- character(0)
            if (param_choices[[.phenoDataPlotPanel]]) {
                chosen.open <- c(chosen.open, .phenoDataPlotParamPanelTitle)
            }

            collected[[counter]] <- fluidRow(
              column(6, plotOutput(.phenoDataPlot(i))),
              column(3,
                     selectInput(.inputPhenoData(.phenoDataYAxisColData, i),
                                 label = "Column of interest (Y-axis):",
                                 choices=covariates, selected=param_choices[[.phenoDataYAxisColData]]),
                     radioButtons(.inputPhenoData(.phenoDataXAxis, i), label="X-axis:",
                                  inline=FALSE,
                                  choices=c(.phenoDataXAxisNothingTitle, .phenoDataXAxisColDataTitle),
                                  selected=param_choices[[.phenoDataXAxis]]),
                     selectInput(.inputPhenoData(.phenoDataXAxisColData, i),
                                 label = "Column of interest (X-axis):",
                                 choices=covariates, selected=param_choices[[.phenoDataXAxisColData]]),
                     actionButton(.phenoDataDiscard(i), "Remove plot",
                                  icon = icon("trash"),class = "btn btn-warning")
                     ),
              column(3,
                     shinyBS::bsCollapse(
                       id = .inputPhenoData(.phenoDataPlotPanel, i),
                       open = chosen.open,
                       shinyBS::bsCollapsePanel(
                            title = .phenoDataPlotParamPanelTitle,
                            radioButtons(.inputPhenoData(.phenoDataColorBy, i),
                                         label="Color by:", inline=FALSE,
                                         choices=c(.colorByNothingTitle, .colorByColDataTitle, .colorByGeneExprsTitle),
                                         selected=param_choices[[.phenoDataColorBy]]),
                            selectInput(.inputPhenoData(.phenoDataColorByColData, i),
                                        label = "Column data:",
                                        choices=covariates, selected=param_choices[[.phenoDataColorByColData]]),
                            textInput(.inputPhenoData(.phenoDataColorByGeneExprs, i),
                                      label = "Gene expression:",
                                      value=param_choices[[.phenoDataColorByGeneExprs]]),
                            selectInput(.inputPhenoData(.phenoDataColorByGeneExprsAssay, i), label=NULL,
                                        choices=all.assays, selected=param_choices[[.phenoDataColorByGeneExprsAssay]])
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
    observeEvent(input$addPhenoDataPlot, {
        first.missing <- setdiff(seq_len(phenodata_max_plots), rObjects$phenodata_active_plots)
        rObjects$phenodata_active_plots <- c(rObjects$phenodata_active_plots, first.missing[1])
    })

    for (i in seq_len(phenodata_max_plots)) {
      local({
        i0 <- i
        observeEvent(input[[.phenoDataDiscard(i0)]], {
          rObjects$phenodata_active_plots <- setdiff(rObjects$phenodata_active_plots, i0)
        })
      })
    }

    for (i in seq_len(phenodata_max_plots)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        i0 <- i
        output[[.phenoDataPlot(i0)]] <- renderPlot({

          # Updating parameters (non-characters need some careful treatment).
          for (field in c(.phenoDataYAxisColData, .phenoDataXAxis, .phenoDataXAxisColData,
                          .phenoDataColorBy, .phenoDataColorByColData,
                          .phenoDataColorByGeneExprs, .phenoDataColorByGeneExprsAssay)) {
              pObjects$phenodata_plot_param[[field]][i0] <- input[[.inputPhenoData(field, i0)]]
          }
          pObjects$phenodata_plot_param[[.phenoDataPlotPanel]][i0] <- .phenoDataPlotParamPanelTitle %in% input[[.inputPhenoData(.phenoDataPlotPanel, i0)]]

          # Setting up the parameter choices for this plot.
          param_choices <- pObjects$phenodata_plot_param[i0,]
          aes_args <- list(y=param_choices[[.phenoDataYAxisColData]])
#          if (param_choices[[.phenoDataXAxis]]!=.phenoDataXAxisNothingTitle) { # Currently not-quite-working as plotPhenoData needs 'x'.
              aes_args$x <- param_choices[[.phenoDataXAxisColData]]
#          }

          color_choice <- param_choices[[.phenoDataColorBy]]
          if (color_choice==.colorByColDataTitle) {
            aes_args$color <- param_choices[[.phenoDataColorByColData]]
          } else if (color_choice==.colorByGeneExprsTitle) {
            aes_args$color <- param_choices[[.phenoDataColorByGeneExprs]]
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

    output$geneExprPlots <- renderUI({
      collected <- vector("list", length(rObjects$geneexpr_active_plots)*2)
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
                              choices=c(.geneExprXAxisNothingTitle, .geneExprXAxisColDataTitle, .geneExprXAxisGeneExprsTitle),
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
                                  choices=c(.colorByNothingTitle, .colorByColDataTitle, .colorByGeneExprsTitle),
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
          xchoice <- param_choices[[.geneExprXAxis]]
          if (xchoice==.geneExprXAxisColDataTitle) {
            byx <- param_choices[[.geneExprXAxisColData]]
          } else if (xchoice==.geneExprXAxisGeneExprsTitle) {
            byx <- param_choices[[.geneExprXAxisGeneExprs]]
          } else {
            byx <- NULL
          }

          color_choice <- param_choices[[.geneExprColorBy]]
          if (color_choice==.colorByColDataTitle) {
            covariate.name <- param_choices[[.geneExprColorByColData]]
          } else if (color_choice==.colorByGeneExprsTitle) {
            covariate.name <- param_choices[[.geneExprColorByGeneExprs]]
          } else {
            covariate.name <- NULL
          }

          # Creating the plot object.
          if (param_choices[[.geneExprID]] %in% gene.names) {
            plotExpression(se, exprs_values=param_choices[[.geneExprAssay]],
                           x=byx,
                           features=param_choices[[.geneExprID]],
                           colour_by=covariate.name)
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

