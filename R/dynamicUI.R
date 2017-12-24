.panel_organization <- function(active_plots, memory) 
# This function generates the sidebar that organizes the various panels.
# It includes options to move plots up, down, and remove/resize them.
{
    N <- nrow(active_plots)
    collected <- vector("list", N)
    counter <- 1L

    for (i in seq_len(N)) {
        mode <- active_plots$Type[i]
        ID <- active_plots$ID[i]
        panel.width <- memory[[mode]][[.organizationWidth]][ID]

        current <- list(
            h4(.panel_name(mode, ID)),
            actionButton(paste0(mode, ID, .organizationUp), "Up"),
            actionButton(paste0(mode, ID, .organizationDown), "Down"),
            actionButton(paste0(mode, ID, .organizationDiscard), "Remove",
                         icon = icon("trash"), class = "btn btn-warning"),
            sliderInput(paste0(mode, ID, .organizationWidth), "Width",  
                        min=4, max=12, value=panel.width, step=1)
            )

        if (i!=1L) {
            current <- c(list(hr()), current)
        }
        collected[[i]] <- current
    }
    do.call(tagList, collected)
}

.panel_generation <- function(active_plots, memory, redDimNames, colDataNames, assayNames) 
# This function generates the various panels, taking into account their
# variable widths to dynamically assign them to particular rows. We also
# need to check the memory to avoid resetting the plot upon re-rendering.
{ 
    collected <- list()
    counter <- 1L
    cumulative.width <- 0L
    cur.row <- list()
    row.counter <- 1L

    # Defining currently active tables, scatter plots to use in linking.
    all.names <- .decode_panel_name(active_plots$Type, active_plots$ID)
    active.tab <- all.names[active_plots$Type=="geneStat"]
    brushable <- all.names[active_plots$Type!="geneStat"]

    for (i in seq_len(nrow(active_plots))) { 
        mode <- active_plots$Type[i]
        ID <- active_plots$ID[i]
        param_choices <- memory[[mode]][ID,]

        if (mode=="redDim") {                               
            stuff <- list(
                 plotOutput(.redDimPlot(ID), brush = brushOpts(paste0(.redDimPlot(ID), .brushField))),
                 selectInput(.inputRedDim(.redDimType, ID), label="Type",
                             choices=redDimNames, selected=param_choices[[.redDimType]]),
                 textInput(.inputRedDim(.redDimXAxis, ID), label="Dimension 1",
                           value=param_choices[[.redDimXAxis]]),
                 textInput(.inputRedDim(.redDimYAxis, ID), label="Dimension 2",
                           value=param_choices[[.redDimYAxis]])
                 )
        } else if (mode=="phenoData") {
            stuff <- list(
                 plotOutput(.phenoDataPlot(ID), brush = brushOpts(paste0(.phenoDataPlot(ID), .brushField))),
                 selectInput(.inputPhenoData(.phenoDataYAxisColData, ID), 
                             label = "Column of interest (Y-axis):",
                             choices=colDataNames, selected=param_choices[[.phenoDataYAxisColData]]),
                 radioButtons(.inputPhenoData(.phenoDataXAxis, ID), label="X-axis:", 
                              inline=FALSE, 
                              choices=c(.phenoDataXAxisNothingTitle, .phenoDataXAxisColDataTitle),
                              selected=param_choices[[.phenoDataXAxis]]),
                 selectInput(.inputPhenoData(.phenoDataXAxisColData, ID), 
                             label = "Column of interest (X-axis):",
                             choices=colDataNames, selected=param_choices[[.phenoDataXAxisColData]])
                 )
        } else if (mode=="geneExpr") {
            stuff <- list(
                plotOutput(.geneExprPlot(ID), brush = brushOpts(paste0(.geneExprPlot(ID), .brushField))),
                selectInput(.inputGeneExpr(.geneExprID, ID), label = "Y-axis gene linked to:",
                            choices=active.tab, selected=param_choices[[.geneExprID]]),
                 selectInput(.inputGeneExpr(.geneExprAssay, ID), label=NULL,
                             choices=assayNames, selected=param_choices[[.geneExprAssay]]),
                 radioButtons(.inputGeneExpr(.geneExprXAxis, ID), label="X-axis:", 
                              inline=FALSE, 
                              choices=c(.geneExprXAxisNothingTitle, .geneExprXAxisColDataTitle, .geneExprXAxisGeneExprsTitle),
                              selected=param_choices[[.geneExprXAxis]]),
                 selectInput(.inputGeneExpr(.geneExprXAxisColData, ID), 
                             label = "X-axis column data:", 
                             choices=colDataNames, selected=param_choices[[.geneExprXAxisColData]]),
                 selectInput(.inputGeneExpr(.geneExprXAxisGeneExprs, ID),
                             label = "X-axis gene linked to:", 
                             choices=active.tab, selected=param_choices[[.geneExprXAxisGeneExprs]])
                 )
        } else if (mode=="geneStat") {
            stuff <- list(dataTableOutput(paste0("geneStatTable", ID)))
        } else {
            stop(sprintf("'%s' is not a recognized panel mode"), mode)
        }

        # Adding graphical parameters if we're plotting.
        if (mode!="geneStat") { 
            chosen.open <- character(0)
            if (param_choices[[.generalPlotPanel]]) {
                chosen.open <- c(chosen.open, .redDimPlotParamPanelTitle)
            }

            brush.choice <- param_choices[[.brushByPlot]]
            if (is.na(brush.choice) || ! brush.choice %in% all.names) { 
                brush.choice <- all.names[i] # brush by yourself, basically.
            }

            param <- list(shinyBS::bsCollapse(
                id = paste0(mode, .generalPlotPanel, ID),
                open = chosen.open,
                shinyBS::bsCollapsePanel(
                    title = .generalPlotParamPanelTitle,
                    radioButtons(paste0(mode, .generalColorBy, ID), 
                                 label="Color by:", inline=TRUE,
                                 choices=c(.colorByNothingTitle, .colorByColDataTitle, .colorByGeneExprsTitle),
                                 selected=param_choices[[.generalColorBy]]),
                    selectInput(paste0(mode, .generalColorByColData, ID), 
                                label = "Column data:",
                                choices=colDataNames, selected=param_choices[[.generalColorByColData]]),
                    selectInput(paste0(mode, .geneExprID, ID), label = "Gene linked to:",
                                choices=active.tab, selected=param_choices[[.generalColorByGeneExprs]]),  
                    selectInput(paste0(mode, .generalColorByGeneExprsAssay, ID), label=NULL,
                                choices=assayNames, selected=param_choices[[.generalColorByGeneExprsAssay]])
                    ), 
                shinyBS::bsCollapsePanel(
                    title = .brushParamPanelTitle,
                    selectInput(paste0(mode, .brushByPlot, ID), 
                                label = "Brush by:",
                                choices=brushable, selected=brush.choice)
                    )
                ) # end of bsCollapse
            )
        } else {
            param <- list()
        }

        # Deciding whether to continue on the current row, or start a new row.
        panel.width <- param_choices[[.organizationWidth]]
        extra <- cumulative.width + panel.width
        if (extra > 12L) {
            collected[[counter]] <- do.call(fluidRow, cur.row)
            counter <- counter + 1L
            collected[[counter]] <- hr()
            counter <- counter + 1L
            cur.row <- list()
            row.counter <- 1L
            cumulative.width <- 0L
        } 

        # Aggregating together everything into a column.
        cur.row[[row.counter]] <- do.call(column, c(list(width=panel.width, 
                                                         h4(all.names[i])),
                                                    stuff, param))
        row.counter <- row.counter + 1L
        cumulative.width <- cumulative.width + panel.width
    }

    # Cleaning up the leftovers.
    collected[[counter]] <- do.call(fluidRow, cur.row)
    counter <- counter + 1L
    collected[[counter]] <- hr()

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, collected)
}
