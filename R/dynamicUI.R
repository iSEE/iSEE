.panel_organization <- function(active_panels, memory)
# This function generates the sidebar that organizes the various panels.
# It includes options to move plots up, down, and remove/resize them.
{
    N <- nrow(active_panels)
    collected <- vector("list", N)
    counter <- 1L

    for (i in seq_len(N)) {
        mode <- active_panels$Type[i]
        ID <- active_panels$ID[i]

        # Disabling the buttons if we're at the top or bottom.
        upFUN <- downFUN <- identity
        if (i==1L) {
            upFUN <- disabled
        }
        if (i==N) {
            downFUN <- disabled
        }

        collected[[i]] <- box(
            .custom_actionButton(paste0(mode, .organizationDiscard, ID),"", icon = icon("trash fa-2x")),
            upFUN(.custom_actionButton(paste0(mode, .organizationUp, ID),"",icon = icon("arrow-circle-up fa-2x"))),
            downFUN(.custom_actionButton(paste0(mode, .organizationDown, ID),"",icon = icon("arrow-circle-down fa-2x"))),
            .custom_actionButton(paste0(mode, .organizationModify, ID),"", icon = icon("gear fa-2x")),
            title=.decode_panel_name(mode, ID), status=box_status[mode], 
            width=NULL, solidHeader=TRUE
            )

    }
    do.call(tagList, collected)
}

.custom_actionButton <- function(...) { 
    div(style="display:inline-block", actionLink(...))
}

.panel_generation <- function(active_panels, memory, se) 
# This function generates the various panels, taking into account their
# variable widths to dynamically assign them to particular rows. We also
# need to check the memory to avoid resetting the plot upon re-rendering.
{
    collected <- list()
    counter <- 1L
    cumulative.width <- 0L
    cur.row <- list()
    row.counter <- 1L

    # Collecting constants for populating the UI. Note that the assay
    # and reduced dimension names may not be unique, hence the (%i).
    feasibility <- .check_plot_feasibility(se)

    covariates <- colnames(colData(se))

    all_assays_raw <- assayNames(se)
    all_assays <- seq_along(all_assays_raw)
    names(all_assays) <- sprintf("(%i) %s", all_assays, all_assays_raw)

    red_dim_names_raw <- reducedDimNames(se)
    red_dim_names <- seq_along(red_dim_names_raw)
    names(red_dim_names) <- sprintf("(%i) %s", red_dim_names, red_dim_names_raw)
    red_dim_dims <- vapply(red_dim_names, FUN=function(x) ncol(reducedDim(se, x)), FUN.VALUE=0L)
  
    # Defining all transmitting tables and plots for linking.
    all.names <- .decode_panel_name(active_panels$Type, active_panels$ID)
    is_tab <- active_panels$Type=="geneStat"
    active_tab <- all.names[is_tab]
    if (length(active_tab)==0L) {
        active_tab <- ""
    }
    brushable <- c("", all.names[!is_tab])

    for (i in seq_len(nrow(active_panels))) {
        mode <- active_panels$Type[i]
        ID <- active_panels$ID[i]
        panel.width <- active_panels$Width[i]
        param_choices <- memory[[mode]][ID,]

        # Checking what to do with plot-specific parameters (e.g., brushing, clicking, plot height).
        dblclick <- NULL
        brush.opts <- NULL
        panel_height <- NULL
        if (mode!="geneStat") { 
            brush.opts <- brushOpts(paste0(mode, .brushField, ID), resetOnNew=FALSE, 
                                    fill=brush_fill_color[mode], stroke=brush_stroke_color[mode])
            dblclick <- paste0(mode, .zoomClick, ID)
            panel_height <- paste0(active_panels$Height[i], "px")
        }

        # Creating the plot fields.
        if (mode=="redDim") {
            obj <- plotOutput(.redDimPlot(ID), brush = brush.opts, dblclick=dblclick, height=panel_height)
            cur_reddim <- param_choices[[.redDimType]]
            red_choices <- seq_len(red_dim_dims[[cur_reddim]])
            plot.param <-  list(
                 selectInput(.inputRedDim(.redDimType, ID), label="Type",
                             choices=red_dim_names, selected=cur_reddim),
                 selectInput(.inputRedDim(.redDimXAxis, ID), label="Dimension 1",
                             choices=red_choices, selected=param_choices[[.redDimXAxis]]),
                 selectInput(.inputRedDim(.redDimYAxis, ID), label="Dimension 2",
                             choices=red_choices, selected=param_choices[[.redDimYAxis]])
                 )
        } else if (mode=="colData") {
            obj <- plotOutput(.colDataPlot(ID), brush = brush.opts, dblclick=dblclick, height=panel_height)
            plot.param <- list(
                 selectInput(.inputColData(.colDataYAxis, ID),
                             label = "Column of interest (Y-axis):",
                             choices=covariates, selected=param_choices[[.colDataYAxis]]),
                 radioButtons(.inputColData(.colDataXAxis, ID), label="X-axis:", inline=TRUE,
                              choices=c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle),
                              selected=param_choices[[.colDataXAxis]]),
                 .conditionalPanelOnRadio(.inputColData(.colDataXAxis, ID),
                                          .colDataXAxisColDataTitle,
                                          selectInput(.inputColData(.colDataXAxisColData, ID),
                                                      label = "Column of interest (X-axis):",
                                                      choices=covariates, selected=param_choices[[.colDataXAxisColData]]))
                 )
        } else if (mode=="geneExpr") {
            obj <- plotOutput(.geneExprPlot(ID), brush = brush.opts, dblclick=dblclick, height=panel_height)
            xaxis_choices <- c(.geneExprXAxisNothingTitle)
            if (feasibility$colData) {
                xaxis_choices <- c(xaxis_choices, .geneExprXAxisColDataTitle)
            }
            if (feasibility$geneExpr) {
                xaxis_choices <- c(xaxis_choices, .geneExprXAxisGeneTableTitle, .geneExprXAxisGeneTextTitle)
            }

            plot.param <- list(
              radioButtons(.inputGeneExpr(.geneExprYAxis, ID), label="Y-axis:",
                           inline = TRUE, choices=c(.geneExprYAxisGeneTableTitle, .geneExprYAxisGeneTextTitle),
                           selected=param_choices[[.geneExprYAxis]]),
              .conditionalPanelOnRadio(.inputGeneExpr(.geneExprYAxis, ID),
                                       .geneExprYAxisGeneTableTitle,
                                       selectInput(.inputGeneExpr(.geneExprYAxisGeneTable, ID),
                                                   label = "Y-axis gene linked to:",
                                                   choices=active_tab,
                                                   selected=.choose_link(param_choices[[.geneExprYAxisGeneTable]], active_tab, force_default=TRUE))
              ),
              .conditionalPanelOnRadio(.inputGeneExpr(.geneExprYAxis, ID),
                                       .geneExprYAxisGeneTextTitle,
                                       textInput(paste0(mode, .geneExprYAxisGeneText, ID),
                                                 label = "Y-axis gene:",
                                                 value=param_choices[[.geneExprYAxisGeneText]])),
              selectInput(.inputGeneExpr(.geneExprAssay, ID), label=NULL,
                          choices=all_assays, selected=param_choices[[.geneExprAssay]]),
              radioButtons(.inputGeneExpr(.geneExprXAxis, ID), label="X-axis:", inline=TRUE,
                           choices=xaxis_choices, selected=param_choices[[.geneExprXAxis]]),
              .conditionalPanelOnRadio(.inputGeneExpr(.geneExprXAxis, ID),
                                       .geneExprXAxisColDataTitle,
                                       selectInput(.inputGeneExpr(.geneExprXAxisColData, ID),
                                                   label = "X-axis column data:",
                                                   choices=covariates, selected=param_choices[[.geneExprXAxisColData]])),
              .conditionalPanelOnRadio(.inputGeneExpr(.geneExprXAxis, ID),
                                       .geneExprXAxisGeneTableTitle,
                                       selectInput(.inputGeneExpr(.geneExprXAxisGeneTable, ID),
                                                   label = "X-axis gene linked to:",
                                                   choices=active_tab, selected=param_choices[[.geneExprXAxisGeneTable]])),
              .conditionalPanelOnRadio(.inputGeneExpr(.geneExprXAxis, ID),
                                       .geneExprXAxisGeneTextTitle,
                                       textInput(paste0(mode, .geneExprXAxisGeneText, ID), label = "X-axis gene:",
                                                 value=param_choices[[.geneExprXAxisGeneText]]))
                 )
        } else if (mode=="geneStat") {
            obj <- list(dataTableOutput(.geneStatTable(ID)),
                        uiOutput(.geneStatAnno(ID)))
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
                .createColorPanel(mode, ID, param_choices, active_tab, covariates, all_assays, feasibility),

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

        # Aggregating together everything into a box, and then into a column.
        cur.box <- do.call(box, c(list(obj), param, 
           list(title=all.names[i], solidHeader=TRUE, width=NULL, status = box_status[mode])))
        cur.row[[row.counter]] <- column(width=panel.width, cur.box, style='padding:3px;') 
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

.choose_link <- function(chosen, available, force_default=FALSE)
# Convenience function to choose a linked panel from those available.
# force_default=TRUE will pick the first if it is absolutely required.
{
    if (!chosen %in% available) {
        if (force_default && length(available)) {
            return(available[1])
        }
        return("")
    }
    return(chosen)
}

.createColorPanel <- function(mode, ID, param_choices, active_tab, covariates, all_assays, feasibility)
# Convenience function to create the color parameter panel. This
# won't be re-used, it just breaks up the huge UI function above.
{
    colorby.field <- paste0(mode, .colorByField, ID)
    color_choices <- c(.colorByNothingTitle)
    if (feasibility$colData) { 
        color_choices <- c(color_choices, .colorByColDataTitle)
    }
    if (feasibility$geneExpr) {
        color_choices <- c(color_choices, .colorByGeneTableTitle, .colorByGeneTextTitle)
    }

    collapseBox(
        id = paste0(mode, .colorParamPanelOpen, ID),
        title = "Coloring parameters",
        open = param_choices[[.colorParamPanelOpen]],
        radioButtons(colorby.field, label="Color by:", inline=TRUE,
                     choices=color_choices, selected=param_choices[[.colorByField]]
            ),
        .conditionalPanelOnRadio(colorby.field, .colorByColDataTitle,
            selectInput(paste0(mode, .colorByColData, ID), label = NULL,
                        choices=covariates, selected=param_choices[[.colorByColData]])
            ),
        .conditionalPanelOnRadio(colorby.field, .colorByGeneTableTitle,
            tagList(selectInput(paste0(mode, .colorByGeneTable, ID), label = NULL, choices=active_tab,
                                selected=.choose_link(param_choices[[.colorByGeneTable]], active_tab, force_default=TRUE)),
                    selectInput(paste0(mode, .colorByGeneTableAssay, ID), label=NULL,
                                choices=all_assays, selected=param_choices[[.colorByGeneTableAssay]]))
            ),
        .conditionalPanelOnRadio(colorby.field, .colorByGeneTextTitle,
            tagList(textInput(paste0(mode, .colorByGeneText, ID), label = NULL, value=param_choices[[.colorByGeneText]]),
                    selectInput(paste0(mode, .colorByGeneTextAssay, ID), label=NULL,
                                choices=all_assays, selected=param_choices[[.colorByGeneTextAssay]]))
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
        selectInput(paste0(mode, .brushByPlot, ID),
                    label = "Receive brush from:", selectize=FALSE,
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

.conditionalPanelOnRadio <- function(radio_id, radio_choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', radio_id, radio_choice), ...)
}

box_status <- c(redDim="primary", geneExpr="success", colData="warning", geneStat="danger")

brush_fill_color <- c(redDim="#9cf", geneExpr="#9f6", colData="#ff9")

brush_stroke_color <- c(redDim="#06f", geneExpr="#090", colData="#fc0")
brush_stroke_color_full <- c(redDim="#0066ff", geneExpr="#009900", colData="#ffcc00")
