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

        ctrl_panel <- box(
            actionButton(paste0(mode, ID, "_", .organizationDiscard),"", icon = icon("trash fa-2x"), style="display:inline-block; margin:0"),
            upFUN(actionButton(paste0(mode, ID, "_", .organizationUp),"",icon = icon("arrow-circle-up fa-2x"), style="display:inline-block; margin:0")),
            downFUN(actionButton(paste0(mode, ID, "_", .organizationDown),"",icon = icon("arrow-circle-down fa-2x"), style="display:inline-block; margin:0")),
            actionButton(paste0(mode, ID, "_", .organizationModify),"", icon = icon("gear fa-2x"), style="display:inline-block; margin:0"),
            title=.decode_panel_name(mode, ID), status="danger", width=NULL, solidHeader=TRUE
            )

        # Coercing to a different box status ('danger' is a placeholder, above).
        collected[[i]] <- .coerce_box_status(ctrl_panel, mode)
    }
    do.call(tagList, collected)
}

.sanitize_red_dim_names <- function(raw_names){
  red_dim_names <- seq_along(raw_names)
  names(red_dim_names) <- sprintf("(%i) %s", red_dim_names, raw_names)
  return(red_dim_names)
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

    column_covariates <- colnames(colData(se))
    row_covariates <- colnames(rowData(se))
    features <- rownames(se)

    all_assays_raw <- assayNames(se)
    all_assays <- seq_along(all_assays_raw)
    names(all_assays) <- sprintf("(%i) %s", all_assays, all_assays_raw)

    red_dim_names <- .sanitize_red_dim_names(reducedDimNames(se))
    red_dim_dims <- vapply(red_dim_names, FUN=function(x) ncol(reducedDim(se, x)), FUN.VALUE=0L)
  
    # Defining all transmitting tables and plots for linking.
    link_sources <- .define_link_sources(active_panels)
    active_tab <- link_sources$tab
    row_brushable <- c(.noSelection, link_sources$row)
    col_brushable <- c(.noSelection, link_sources$col)
    heatmap_sources <- c(link_sources$row, active_tab)

    for (i in seq_len(nrow(active_panels))) {
        mode <- active_panels$Type[i]
        ID <- active_panels$ID[i]
        panel.width <- active_panels$Width[i]
        param_choices <- memory[[mode]][ID,]
        .input_FUN <- function(field) { paste0(mode, ID, "_", field) }

        # Checking what to do with plot-specific parameters (e.g., brushing, clicking, plot height).
        if (mode!="rowStatTable") { 
            brush.opts <- brushOpts(.input_FUN(.brushField), resetOnNew=FALSE, 
                                    fill=brush_fill_color[mode], stroke=brush_stroke_color[mode], opacity = .brushFillOpacity)
            dblclick <- .input_FUN(.zoomClick)
            clickopt <- .input_FUN(.lassoClick)
            panel_height <- paste0(active_panels$Height[i], "px")
            panel_name <- paste0(mode, ID)
        }

        # Creating the plot fields.
        if (mode=="redDimPlot") {
            obj <- plotOutput(panel_name, brush = brush.opts, dblclick=dblclick, click=clickopt, height=panel_height)
            cur_reddim <- param_choices[[.redDimType]]
            red_choices <- seq_len(red_dim_dims[[cur_reddim]])
            plot.param <-  list(
                 selectInput(.input_FUN(.redDimType), label="Type",
                             choices=red_dim_names, selected=cur_reddim),
                 selectInput(.input_FUN(.redDimXAxis), label="Dimension 1",
                             choices=red_choices, selected=param_choices[[.redDimXAxis]]),
                 selectInput(.input_FUN(.redDimYAxis), label="Dimension 2",
                             choices=red_choices, selected=param_choices[[.redDimYAxis]])
                 )
        } else if (mode=="colDataPlot") {
            obj <- plotOutput(panel_name, brush = brush.opts, dblclick=dblclick, click=clickopt, height=panel_height)
            plot.param <- list(
                 selectInput(.input_FUN(.colDataYAxis),
                             label = "Column of interest (Y-axis):",
                             choices=column_covariates, selected=param_choices[[.colDataYAxis]]),
                 radioButtons(.input_FUN(.colDataXAxis), label="X-axis:", inline=TRUE,
                              choices=c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle),
                              selected=param_choices[[.colDataXAxis]]),
                 .conditionalPanelOnRadio(.input_FUN(.colDataXAxis),
                                          .colDataXAxisColDataTitle,
                                          selectInput(.input_FUN(.colDataXAxisColData),
                                                      label = "Column of interest (X-axis):",
                                                      choices=column_covariates, selected=param_choices[[.colDataXAxisColData]]))
                 )
        } else if (mode=="featExprPlot") {
            obj <- plotOutput(panel_name, brush = brush.opts, dblclick=dblclick, click=clickopt, height=panel_height)
            xaxis_choices <- c(.featExprXAxisNothingTitle)
            if (feasibility$colDataPlot) {
                xaxis_choices <- c(xaxis_choices, .featExprXAxisColDataTitle)
            }
            if (feasibility$featExprPlot) {
                xaxis_choices <- c(xaxis_choices, .featExprXAxisRowTableTitle, .featExprXAxisFeatNameTitle)
            }

            plot.param <- list(
              radioButtons(.input_FUN(.featExprYAxis), label="Y-axis:",
                           inline = TRUE, choices=c(.featExprYAxisRowTableTitle, .featExprYAxisFeatNameTitle),
                           selected=param_choices[[.featExprYAxis]]),
              .conditionalPanelOnRadio(.input_FUN(.featExprYAxis),
                                       .featExprYAxisRowTableTitle,
                                       selectInput(.input_FUN(.featExprYAxisRowTable),
                                                   label = "Y-axis gene linked to:",
                                                   choices=active_tab,
                                                   selected=.choose_link(param_choices[[.featExprYAxisRowTable]], active_tab, force_default=TRUE))
              ),
              .conditionalPanelOnRadio(.input_FUN(.featExprYAxis),
                                       .featExprYAxisFeatNameTitle,
                                       selectizeInput(.input_FUN(.featExprYAxisFeatName),
                                                      label = "Y-axis gene:", choices = NULL, selected = NULL, multiple=FALSE)),
              selectInput(.input_FUN(.featExprAssay), label=NULL,
                          choices=all_assays, selected=param_choices[[.featExprAssay]]),
              radioButtons(.input_FUN(.featExprXAxis), label="X-axis:", inline=TRUE,
                           choices=xaxis_choices, selected=param_choices[[.featExprXAxis]]),
              .conditionalPanelOnRadio(.input_FUN(.featExprXAxis),
                                       .featExprXAxisColDataTitle,
                                       selectInput(.input_FUN(.featExprXAxisColData),
                                                   label = "X-axis column data:",
                                                   choices=column_covariates, selected=param_choices[[.featExprXAxisColData]])),
              .conditionalPanelOnRadio(.input_FUN(.featExprXAxis),
                                       .featExprXAxisRowTableTitle,
                                       selectInput(.input_FUN(.featExprXAxisRowTable),
                                                   label = "X-axis gene linked to:",
                                                   choices=active_tab, selected=param_choices[[.featExprXAxisRowTable]])),
              .conditionalPanelOnRadio(.input_FUN(.featExprXAxis),
                                       .featExprXAxisFeatNameTitle,
                                       selectizeInput(.input_FUN(.featExprXAxisFeatName), 
                                                      label = "X-axis gene:", choices = NULL, selected = NULL, multiple = FALSE))
                 )
        } else if (mode=="rowStatTable") {
            obj <- tagList(dataTableOutput(paste0(mode, ID)), uiOutput(.input_FUN("annotation")))
        } else if (mode=="rowDataPlot") {
            obj <- plotOutput(panel_name, brush = brush.opts, dblclick=dblclick, click=clickopt, height=panel_height)
            plot.param <- list(
                 selectInput(.input_FUN(.rowDataYAxis),
                             label = "Column of interest (Y-axis):",
                             choices=row_covariates, selected=param_choices[[.rowDataYAxis]]),
                 radioButtons(.input_FUN(.rowDataXAxis), label="X-axis:", inline=TRUE,
                              choices=c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle),
                              selected=param_choices[[.rowDataXAxis]]),
                 .conditionalPanelOnRadio(.input_FUN(.rowDataXAxis),
                                          .rowDataXAxisRowDataTitle,
                                          selectInput(.input_FUN(.rowDataXAxisRowData),
                                                      label = "Column of interest (X-axis):",
                                                      choices=row_covariates, selected=param_choices[[.rowDataXAxisRowData]]))
                 )
        } else if (mode=="heatMapPlot") {
            obj <- plotOutput(panel_name, brush=brush.opts, dblclick=dblclick, height=panel_height)
            plot.param <- list(tags$div(class = "panel-group", role = "tablist",
                    collapseBox(id=.input_FUN(.heatMapFeatNamePanelOpen),
                                title="Feature parameters",
                                open=param_choices[[.heatMapFeatNamePanelOpen]],
                        selectizeInput(.input_FUN(.heatMapFeatName),
                                       label="Features:",
                                       choices=NULL, selected=NULL, multiple=TRUE,
                                       options = list(plugins = list('remove_button', 'drag_drop'))),
                        selectInput(.input_FUN(.heatMapAssay), label=NULL,
                                    choices=all_assays, selected=param_choices[[.heatMapAssay]]),
                        selectInput(.input_FUN(.heatMapImportSource), label="Import from", choices=heatmap_sources,
                                    selected=.choose_link(param_choices[[.heatMapImportSource]], heatmap_sources, force_default=TRUE)),
                        actionButton(.input_FUN(.heatMapImport), "Import features"),
                        actionButton(.input_FUN(.heatMapCluster), "Suggest feature order")
                    ),
                    collapseBox(id=.input_FUN(.heatMapColDataPanelOpen),
                                title="Column data parameters",
                                open=param_choices[[.heatMapColDataPanelOpen]],
                        selectizeInput(.input_FUN(.heatMapColData),
                                       label="Column data:",
                                       choices=column_covariates,
                                       multiple = TRUE, 
                                       selected=param_choices[[.heatMapColData]][[1]],
                                       options = list(plugins = list('remove_button', 'drag_drop'))),
                        plotOutput(.input_FUN(.heatMapLegend))
                    ),
                    collapseBox(id=.input_FUN(.heatMapColorPanelOpen),
                                title="Coloring parameters",
                                open=param_choices[[.heatMapColorPanelOpen]],
                        radioButtons(.input_FUN(.heatMapCentering), label="Centering:", inline=TRUE,
                                     choices = c(.heatMapYesTitle, .heatMapNoTitle), 
                                     selected = param_choices[[.heatMapCentering]]),
                        radioButtons(.input_FUN(.heatMapScaling), label="Scaling:", inline=TRUE,
                                     choices = c(.heatMapYesTitle, .heatMapNoTitle), 
                                     selected = param_choices[[.heatMapScaling]]),
                        .conditionalPanelOnRadio(.input_FUN(.heatMapScaling), .heatMapNoTitle,
                                                 textInput(.input_FUN(.heatMapLower), label="Lower bound:",
                                                           value = param_choices[[.heatMapLower]]), 
                                                 textInput(.input_FUN(.heatMapUpper), label="Upper bound:",
                                                           value = param_choices[[.heatMapUpper]])), 
                        .conditionalPanelOnRadio(.input_FUN(.heatMapCentering), .heatMapYesTitle,
                                                 selectInput(.input_FUN(.heatMapCenteredColors), label="Color scale:",
                                                             choices = c("purple-black-yellow", "blue-white-orange"),
                                                             selected = param_choices[[.heatMapCenteredColors]]))
                    )
                )
            )
        } else {
            stop(sprintf("'%s' is not a recognized panel mode"), mode)
        }

        # Adding graphical parameters if we're plotting.
        if (mode=="rowStatTable") {
            param <- list(hr(), tags$div(class = "panel-group", role = "tablist",
                collapseBox(.input_FUN(.brushParamPanelOpen),
                            title = "Brushing parameters",
                            open = param_choices[[.brushParamPanelOpen]],
                            selectInput(.input_FUN(.brushByPlot),
                                        label = "Receive brush from:", 
                                        choices=row_brushable,
                                        selected=.choose_link(param_choices[[.brushByPlot]], row_brushable))
                            )
                )
            )
        } else if (mode=="rowDataPlot") {
            # Slightly different handling of the row data.
            param <- list(tags$div(class = "panel-group", role = "tablist",
                do.call(collapseBox, c(list(id=.input_FUN(.plotParamPanelOpen),
                                            title="Plotting parameters",
                                            open=param_choices[[.plotParamPanelOpen]]),
                                       plot.param)),
                .createColorPanelForRowPlots(mode, ID, param_choices, active_tab, row_covariates),
                .createBrushPanel(mode, ID, param_choices, row_brushable)
                )
            )
        } else if (mode=="heatMapPlot") {
            param <- plot.param
        } else {
            param <- list(tags$div(class = "panel-group", role = "tablist",
                # Panel for fundamental plot parameters.
                do.call(collapseBox, c(list(id=.input_FUN(.plotParamPanelOpen),
                                            title="Plotting parameters",
                                            open=param_choices[[.plotParamPanelOpen]]),
                                       plot.param)),

                # Panel for colouring parameters.
                .createColorPanelForColumnPlots(mode, ID, param_choices, active_tab, column_covariates, all_assays, feasibility),

                # Panel for brushing parameters.
                .createBrushPanel(mode, ID, param_choices, col_brushable)
                )
            )
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
        cur_box <- do.call(box, c(list(obj), param, 
            list(uiOutput(.input_FUN(.panelGeneralInfo)), uiOutput(.input_FUN(.panelLinkInfo))),
            list(title=.decode_panel_name(mode, ID), solidHeader=TRUE, width=NULL, status = "danger")))
        cur_box <- .coerce_box_status(cur_box, mode)
        cur.row[[row.counter]] <- column(width=panel.width, cur_box, style='padding:3px;') 
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

.define_link_sources <- function(active_panels) {
    all_names <- .decode_panel_name(active_panels$Type, active_panels$ID)

    is_tab <- active_panels$Type=="rowStatTable"
    active_tab <- all_names[is_tab]
    if (length(active_tab)==0L) {
        active_tab <- ""
    }

    is_row <- active_panels$Type=="rowDataPlot"
    is_heat <- active_panels$Type=="heatMapPlot"
    row_brushable <- all_names[is_row & !is_heat]
    col_brushable <- all_names[!is_tab & !is_row & !is_heat]

    return(list(tab=active_tab, row=row_brushable, col=col_brushable))
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

.createColorPanelForColumnPlots <- function(mode, ID, param_choices, active_tab, covariates, all_assays, feasibility)
# Convenience function to create the color parameter panel. This
# won't be re-used, it just breaks up the huge UI function above.
{
    colorby_field <- paste0(mode, ID, "_", .colorByField)
    color_choices <- c(.colorByNothingTitle)
    if (feasibility$colDataPlot) { 
        color_choices <- c(color_choices, .colorByColDataTitle)
    }
    if (feasibility$featExprPlot) {
        color_choices <- c(color_choices, .colorByRowTableTitle, .colorByFeatNameTitle)
    }

    collapseBox(
        id = paste0(mode, ID, "_", .colorParamPanelOpen),
        title = "Coloring parameters",
        open = param_choices[[.colorParamPanelOpen]],
        radioButtons(colorby_field, label="Color by:", inline=TRUE,
                     choices=color_choices, selected=param_choices[[.colorByField]]
            ),
        .conditionalPanelOnRadio(colorby_field, .colorByColDataTitle,
            selectInput(paste0(mode, ID, "_", .colorByColData), label = NULL,
                        choices=covariates, selected=param_choices[[.colorByColData]])
            ),
        .conditionalPanelOnRadio(colorby_field, .colorByRowTableTitle,
            tagList(selectInput(paste0(mode, ID, "_", .colorByRowTable), label = NULL, choices=active_tab,
                                selected=.choose_link(param_choices[[.colorByRowTable]], active_tab, force_default=TRUE)),
                    selectInput(paste0(mode, ID, "_", .colorByRowTableAssay), label=NULL,
                                choices=all_assays, selected=param_choices[[.colorByRowTableAssay]]))
            ),
        .conditionalPanelOnRadio(colorby_field, .colorByFeatNameTitle,
            tagList(selectizeInput(paste0(mode, ID, "_", .colorByFeatName), label = NULL, choices = NULL, selected = NULL, multiple = FALSE),
                    selectInput(paste0(mode, ID, "_", .colorByFeatNameAssay), label=NULL,
                                choices=all_assays, selected=param_choices[[.colorByFeatNameAssay]]))
            )
        )
}

.createColorPanelForRowPlots <- function(mode, ID, param_choices, active_tab, covariates)
# Convenience function to create the color parameter panel. This
# won't be re-used, it just breaks up the huge UI function above.
{
    colorby_field <- paste0(mode, ID, "_", .colorByField)
    color_choices <- c(.colorByNothingTitle, .colorByRowDataTitle, .colorByRowTableTitle, .colorByFeatNameTitle)

    collapseBox(
        id = paste0(mode, ID, "_", .colorParamPanelOpen),
        title = "Coloring parameters",
        open = param_choices[[.colorParamPanelOpen]],
        radioButtons(colorby_field, label="Color by:", inline=TRUE,
                     choices=color_choices, selected=param_choices[[.colorByField]]
            ),

        .conditionalPanelOnRadio(colorby_field, .colorByRowDataTitle,
            selectInput(paste0(mode, ID, "_", .colorByRowData), label = NULL,
                        choices=covariates, selected=param_choices[[.colorByRowData]])
            ),
        .conditionalPanelOnRadio(colorby_field, .colorByRowTableTitle,
            tagList(selectInput(paste0(mode, ID, "_", .colorByRowTable), label = NULL, choices=active_tab,
                                selected=.choose_link(param_choices[[.colorByRowTable]], active_tab, force_default=TRUE)),
                    colourInput(paste0(mode, ID, "_", .colorByRowTableColor), label=NULL,
                                value=param_choices[[.colorByRowTableColor]]))
            ),
        .conditionalPanelOnRadio(colorby_field, .colorByFeatNameTitle,
            tagList(selectizeInput(paste0(mode, ID, "_", .colorByFeatName), label = NULL, selected = NULL, choices = NULL, multiple = FALSE),
                    colourInput(paste0(mode, ID, "_", .colorByFeatNameColor), label=NULL,
                                value=param_choices[[.colorByFeatNameColor]]))
            )
        )
}

.createBrushPanel <- function(mode, ID, param_choices, brushable)
# Convenience function to create the brushing parameter panel. This
# won't be re-used, it just breaks up the huge UI function above.
{
    brush_effect <- paste0(mode, ID, "_", .brushEffect)

    collapseBox(
        id=paste0(mode, ID, "_", .brushParamPanelOpen),
        title = "Brushing parameters",
        open = param_choices[[.brushParamPanelOpen]],
        selectInput(paste0(mode, ID, "_", .brushByPlot),
                    label = "Receive brush from:", 
                    choices=brushable,
                    selected=.choose_link(param_choices[[.brushByPlot]], brushable)),

        radioButtons(brush_effect, label="Brush effect:", inline=TRUE,
                     choices=c(.brushRestrictTitle, .brushColorTitle, .brushTransTitle),
                     selected=param_choices[[.brushEffect]]),

        .conditionalPanelOnRadio(brush_effect, .brushColorTitle,
            colourInput(paste0(mode, ID, "_", .brushColor), label=NULL,
                        value=param_choices[[.brushColor]])
            ),
        .conditionalPanelOnRadio(brush_effect, .brushTransTitle,
            sliderInput(paste0(mode, ID, "_", .brushTransAlpha), label=NULL,
                        min=0, max=1, value=param_choices[[.brushTransAlpha]])
            )
        )
}

.conditionalPanelOnRadio <- function(radio_id, radio_choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', radio_id, radio_choice), ...)
}

.coerce_box_status <- function(in_box, mode, old_status="danger") {
    in_box$children[[1]]$attribs$class <- sub(paste0("box-", old_status),
                                              paste0("box-", tolower(mode)), 
                                              in_box$children[[1]]$attribs$class)
    return(in_box)
}

.actionbutton_biocstyle <- "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
