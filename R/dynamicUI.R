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
        panel.width <- active_plots$Width[i]

        current <- list(
            h4(.decode_panel_name(mode, ID)),
            fluidRow(
              column(2,actionButton(paste0(mode, ID, .organizationUp), "",icon = icon("arrow-circle-up"))),
              column(2,actionButton(paste0(mode, ID, .organizationDown), "",icon = icon("arrow-circle-down"))),
              column(3,actionButton(paste0(mode, ID, .organizationDiscard), "",
                         icon = icon("trash"), class = "btn btn-warning"))
            ),
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

.panel_generation <- function(active_plots, memory, redDimNames, redDimDims, colDataNames, assayNames) 
# This function generates the various panels, taking into account their
# variable widths to dynamically assign them to particular rows. We also
# need to check the memory to avoid resetting the plot upon re-rendering.
{ 
    collected <- list()
    counter <- 1L
    cumulative.width <- 0L
    cur.row <- list()
    row.counter <- 1L

    # Defining currently active tables for linking.
    all.names <- .decode_panel_name(active_plots$Type, active_plots$ID)
    active.tab <- all.names[active_plots$Type=="geneStat"]
    if (length(active.tab)==0L) { 
        active.tab <- "" 
    }
    
    # Defining brush-transmitting scatter plots to use in linking.
    keep <- logical(nrow(active_plots))
    for (i in which(active_plots$Type!="geneStat")) { 
        keep[i] <- memory[[active_plots$Type[i]]][[.brushActive]][active_plots$ID[i]]
    }
    brushable <- c("", all.names[keep])

    for (i in seq_len(nrow(active_plots))) { 
        mode <- active_plots$Type[i]
        ID <- active_plots$ID[i]
        panel.width <- active_plots$Width[i]
        param_choices <- memory[[mode]][ID,]

        # Checking what to do with brushing.
        if (mode!="geneStat" && param_choices[[.brushActive]]) {
            brush.opts <- brushOpts(paste0(mode, .brushField, ID))
        } else {
            brush.opts <- NULL
        }

        # Creating the plot fields.
        if (mode=="redDim") {
            obj <- plotOutput(.redDimPlot(ID), brush = brush.opts)
            cur_reddim <- param_choices[[.redDimType]]    
            red_choices <- seq_len(redDimDims[[cur_reddim]])
            plot.param <-  list(
                 selectInput(.inputRedDim(.redDimType, ID), label="Type",
                             choices=redDimNames, selected=cur_reddim),
                 selectInput(.inputRedDim(.redDimXAxis, ID), label="Dimension 1",
                             choices=red_choices, selected=param_choices[[.redDimXAxis]]),
                 selectInput(.inputRedDim(.redDimYAxis, ID), label="Dimension 2",
                             choices=red_choices, selected=param_choices[[.redDimYAxis]])
                 )
        } else if (mode=="colData") {
            obj <- plotOutput(.colDataPlot(ID), brush = brush.opts)
            plot.param <- list(
                 selectInput(.inputColData(.colDataYAxis, ID), 
                             label = "Column of interest (Y-axis):",
                             choices=colDataNames, selected=param_choices[[.colDataYAxis]]),
                 radioButtons(.inputColData(.colDataXAxis, ID), label="X-axis:", 
                              inline=FALSE, 
                              choices=c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle),
                              selected=param_choices[[.colDataXAxis]]),
                 selectInput(.inputColData(.colDataXAxisColData, ID), 
                             label = "Column of interest (X-axis):",
                             choices=colDataNames, selected=param_choices[[.colDataXAxisColData]])
                 )
        } else if (mode=="geneExpr") {
            obj <- plotOutput(.geneExprPlot(ID), brush = brush.opts)
            plot.param <- list(
                selectInput(.inputGeneExpr(.geneExprID, ID), label = "Y-axis gene linked to:",
                            choices=active.tab, 
                            selected=.choose_link(param_choices[[.geneExprID]], active.tab, forceDefault=TRUE)),
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
            obj <- list(dataTableOutput(paste0("geneStatTable", ID)))
        } else {
            stop(sprintf("'%s' is not a recognized panel mode"), mode)
        }

        # Adding graphical parameters if we're plotting.
        if (mode!="geneStat") {
            param <- list(tags$div(class = "panel-group", role = "tablist",
                # Panel for fundamental plot parameters. 
                do.call(collapseBox, c(list(id=paste0(mode, .plotParamPanelOpen, ID), 
                                            title="Plotting parameters",
                                            open=param_choices[[.plotParamPanelOpen]]),
                                       plot.param)),

                # Panel for colouring parameters.
                .createColorPanel(mode, ID, param_choices, active.tab, colDataNames, assayNames),
                
                # Panel for brushing parameters.
                .createBrushPanel(mode, ID, param_choices, brushable)  
                ) 
            )
        } else {
            param <- list()
        }

        # Deciding whether to continue on the current row, or start a new row.
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
        cur.row[[row.counter]] <- do.call(column, c(list(width=panel.width, h4(all.names[i]), obj), param))
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

.choose_link <- function(chosen, available, forceDefault=FALSE) 
# Convenience function to choose a linked panel from those available.
# forceDefault=TRUE will pick the first if it is absolutely required.
{
    if (!chosen %in% available) {
        if (forceDefault && length(available)) {
            return(available[1])
        } 
        return("")
    }
    return(chosen)
}

.createColorPanel <- function(mode, ID, param_choices, active.tab, colDataNames, assayNames)
# Convenience function to create the color parameter panel. This
# won't be re-used, it just breaks up the huge UI function above.
{ 
    colorby.field <- paste0(mode, .colorByField, ID)

    collapseBox(
        id = paste0(mode, .colorParamPanelOpen, ID),
        title = "Coloring parameters",
        open = param_choices[[.colorParamPanelOpen]],
        radioButtons(colorby.field, label="Color by:", inline=TRUE,
                     choices=c(.colorByNothingTitle, .colorByColDataTitle, 
                               .colorByGeneTableTitle, .colorByGeneTextTitle),
                     selected=param_choices[[.colorByField]]),

        .conditionalPanelOnRadio(colorby.field, .colorByColDataTitle, 
            selectInput(paste0(mode, .colorByColData, ID), label = NULL,
                        choices=colDataNames, selected=param_choices[[.colorByColData]])
            ),
        .conditionalPanelOnRadio(colorby.field, .colorByGeneTableTitle, 
            tagList(selectInput(paste0(mode, .colorByGeneTable, ID), label = NULL, choices=active.tab, 
                                selected=.choose_link(param_choices[[.colorByGeneTable]], active.tab, forceDefault=TRUE)), 
                    selectInput(paste0(mode, .colorByGeneTableAssay, ID), label=NULL,
                                choices=assayNames, selected=param_choices[[.colorByGeneTableAssay]]))
            ),
        .conditionalPanelOnRadio(colorby.field, .colorByGeneTextTitle, 
            tagList(textInput(paste0(mode, .colorByGeneText, ID), label = NULL, value=param_choices[[.colorByGeneText]]),
                    selectInput(paste0(mode, .colorByGeneTextAssay, ID), label=NULL,
                                choices=assayNames, selected=param_choices[[.colorByGeneTextAssay]]))
            )
        )
} 

.createBrushPanel <- function(mode, ID, param_choices, brushable) 
# Convenience function to create the brushing parameter panel. This
# won't be re-used, it just breaks up the huge UI function above.
{ 
    brush.effect <- paste0(mode, .brushEffect, ID)

    collapseBox(
        id=paste0(mode, .brushParamPanelOpen, ID),
        title = "Brushing parameters",
        open = param_choices[[.brushParamPanelOpen]],
        checkboxInput(paste0(mode, .brushActive, ID), label="Transmit brush", 
                      value=param_choices[[.brushActive]]), 
        selectInput(paste0(mode, .brushByPlot, ID), 
                    label = "Receive brush from:",
                    choices=brushable, 
                    selected=.choose_link(param_choices[[.brushByPlot]], brushable)),

        radioButtons(brush.effect, label="Brush effect:", inline=TRUE,
                     choices=c(.brushRestrictTitle, .brushColorTitle, .brushTransTitle),
                     selected=param_choices[[.brushEffect]]),

        .conditionalPanelOnRadio(brush.effect, .brushColorTitle, 
            colourInput(paste0(mode, .brushColor, ID), label=NULL, 
                        value=param_choices[[.brushColor]])
            ),
        .conditionalPanelOnRadio(brush.effect, .brushTransTitle,
            sliderInput(paste0(mode, .brushTransAlpha, ID), label=NULL, 
                        min=0, max=1, value=param_choices[[.brushTransAlpha]])
            )
        )
}

.conditionalPanelOnRadio <- function(radio.id, radio.choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', radio.id, radio.choice), ...)
}

