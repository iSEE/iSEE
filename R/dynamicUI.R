#' Generate the panel organization UI
#' 
#' Generates the user interface for the sidebar where the organization of the panels is controlled.
#'
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#'
#' @return 
#' A HTML tag object containing the UI elements for the panel organization sidebar.
#'
#' @details
#' This function will create a \code{\link[shinydashboard]{box}} for each active panel of each type,
#' in the order specified in \code{active_panels}.
#' Each box follows a colour-coding scheme for the panel type.
#'
#' Each box will contain options to move panels up or down, though these options will be disabled for the first and last boxes, respectively.
#' Users can also change the width or height of the panels via a button that opens a modal. 
#' Finally, a button is available to delete the panel.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_panel_organization
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.panel_generation}},
#' \code{\link{.setup_initial}}
#'
#' @importFrom shiny actionButton tagList icon
#' @importFrom shinydashboard box
#' @importFrom shinyjs disabled
.panel_organization <- function(active_panels) {
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

#' Sanitize names
#' 
#' Convert a vector of names into a named integer vector of indices.
#'
#' @param raw_names A character vector of names.
#' 
#' @return
#' An integer vector of \code{1:length(raw_names)}, with names based on \code{raw_names}.
#'
#' @details
#' This function protects against non-unique names by converting them to integer indices, which can be used for indexing within the function.
#' The names are also made unique for display to the user by prefixing them with \code{(<index>)}.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun
#' @rdname INTERNAL_sanitize_names
#' @seealso
#' \code{\link{.panel_generation}}
.sanitize_names <- function(raw_names){
  indices <- seq_along(raw_names)
  names(indices) <- sprintf("(%i) %s", indices, raw_names)
  return(indices)
}

#' Generate the panels in the app body
#'
#' Constructs the active panels in the main body of the app to show the plotting results and tables.
#'
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#' @param se A SingleCellExperiment object.
#'
#' @return 
#' A HTML tag object containing the UI elements for the main body of the app.
#' This includes the output plots/tables as well as UI elements to control them.
#'
#' @details
#' This function generates the various panels in the main body of the app, taking into account their variable widths to dynamically assign them to particular rows.
#' It will try to assign as many panels to the same row until the row is filled, at which point it will start on the next row.
#'
#' Each panel contains the actual endpoint element (i.e., the plot or table to display) as well as a number of control elements to set the parameters.
#' All control elements lie within \code{\link{collapseBox}} elements to avoid cluttering the interface.
#' The open/closed status of these boxes are retrieved from memory, and are generally closed by default.
#'
#' Construction of each panel is done by retrieving all of the memorized parameters and using them to set the initial values of various control elements.
#' This ensures that the plots are not reset during re-rendering.
#' The exception is that of the Shiny brush, which cannot be fully restored in the current version - instead, only the bounding box is shown. 
#' 
#' Note that control of the tables lies within \code{\link{iSEE}} itself.
#' Also, feature name selections will open up a \code{selectizeInput} where the values are filled on the server-side, rather than being sent to the client.
#' This avoids long start-up times during re-rendering.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_panel_generation
#' @seealso
#' \code{\link{iSEE}}
#'
#' @importFrom SummarizedExperiment colData rowData assayNames
#' @importFrom BiocGenerics rownames
#' @importFrom SingleCellExperiment reducedDimNames reducedDim
#' @importFrom shiny actionButton fluidRow selectInput plotOutput uiOutput sliderInput tagList textInput column radioButtons tags hr brushOpts selectizeInput 
.panel_generation <- function(active_panels, memory, se) {
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

    all_assays <- .sanitize_names(assayNames(se))
    red_dim_names <- .sanitize_names(reducedDimNames(se))
    red_dim_dims <- vapply(red_dim_names, FUN=function(x) ncol(reducedDim(se, x)), FUN.VALUE=0L)
  
    # Defining all transmitting tables and plots for linking.
    link_sources <- .define_link_sources(active_panels)
    active_tab <- link_sources$tab
    row_brushable <- c(.noSelection, link_sources$row)
    col_brushable <- c(.noSelection, link_sources$col)
    heatmap_sources <- c(.noSelection, link_sources$row, active_tab)

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
                        textInput(.input_FUN(.heatMapLower), label="Lower bound:",
                                  value = param_choices[[.heatMapLower]]), 
                        textInput(.input_FUN(.heatMapUpper), label="Upper bound:",
                                  value = param_choices[[.heatMapUpper]]), 
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
                                            title="Data parameters",
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
                                            title="Data parameters",
                                            open=param_choices[[.plotParamPanelOpen]]),
                                       plot.param)),

                # Panel for colouring parameters.
                .createColorPanelForColumnPlots(mode, ID, param_choices, active_tab, column_covariates, all_assays, no_rows=nrow(se)==0),

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

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, collected)
}

#' Define link sources
#' 
#' Define all possible sources of links between active panels, i.e., feature selections from row statistics tables or brushing from plots.
#'
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#'
#' @return
#' A list containing:
#' \describe{
#' \item{\code{tab}:}{A character vector of decoded names for all active row statistics tables.}
#' \item{\code{row}:}{A character vector of decoded names for all active row data plots.}
#' \item{\code{col}:}{A character vector of decoded names for all active sample-based plots, i.e., where each point is a sample.}
#' }
#'
#' @details
#' Decoded names are returned as the output values are intended to be displayed to the user.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_link_sources
#' @seealso
#' \code{\link{.sanitize_memory}},
#' \code{\link{.panel_generation}}
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

#' Choose a linked panel
#'
#' Chooses a linked panel from those available, forcing a valid choice if required.
#'
#' @param chosen String specifying the proposed choice, usually a decoded panel name.
#' @param available Character vector containing the valid choices, usually decoded panel names.
#' @param force_default Logical scalar indicating whether a non-empty default should be returned if \code{chosen} is not valid.
#'
#' @return A string containing a valid choice, or an empty string.
#' 
#' @details
#' If \code{chosen} is in \code{available}, it will be directly returned.
#' If not, and if \code{force_default=TRUE} and \code{available} is not empty, the first element of \code{available} is returned.
#' Otherwise, an empty string is returned.
#'
#' Setting \code{force_default=TRUE} is required for panels linking to row statistics tables, where an empty choice would result in an invalid plot.
#' However, a default choice is not necessary for brush transmission, where no selection is perfectly valid.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_link
#' @seealso
#' \code{\link{.panel_generation}}
.choose_link <- function(chosen, available, force_default=FALSE) {
    if (!chosen %in% available) {
        if (force_default && length(available)) {
            return(available[1])
        }
        return("")
    }
    return(chosen)
}

#' Colour parameter box for column plots
#'
#' Create a colour parameter box for column-based plots, i.e., where each sample is a point.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param ID Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param active_tab A character vector of decoded names for available row statistics tables.
#' @param covariates A character vector of column metadata fields.
#' @param all_assays A vector of assay options, usually produced from \code{\link{.sanitize_names}}.
#' @param no_rows A logicals scalar indicating whether there are no rows to select.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with colouring parameters for column-based plots.
#'
#' @details
#' Column-based plots can be coloured by nothing, by column metadata or by the expression of certain features.
#' This function creates a collapsible box that contains all of these options, initialized with the choices in \code{memory}.
#' 
#' Each option, once selected, yields a further subset of nested options.
#' For example, choosing to colour by column metadata will open up a \code{selectInput} to specify the metadata field to use.
#'
#' Choosing to colour by feature name will open up a \code{selectizeInput}.
#' However, the values are filled on the server-side, rather than being sent to the client; this avoids long start times during re-rendering.
#'
#' Note that some options will be disabled depending on the nature of the input. 
#' For example, if there are no column metadata fields, users will not be allowed to colour by column metadata, obviously.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_createColorPanelForColumnPlots
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.createColorPanelForRowPlots}}
#'
#' @importFrom shiny radioButtons tagList selectInput selectizeInput
.createColorPanelForColumnPlots <- function(mode, ID, param_choices, active_tab, covariates, all_assays, no_rows=FALSE) {
    colorby_field <- paste0(mode, ID, "_", .colorByField)
    color_choices <- c(.colorByNothingTitle)
    if (length(covariates)) {
        color_choices <- c(color_choices, .colorByColDataTitle)
    }
    if (!no_rows) {
        color_choices <- c(color_choices, .colorByRowTableTitle, .colorByFeatNameTitle)
    }

    collapseBox(
        id = paste0(mode, ID, "_", .colorParamPanelOpen),
        title = "Visual parameters",
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
            ),
        .add_general_aesthetic_UI(mode, ID, param_choices)
        )
}

#' Colour parameter box for row plots
#'
#' Create a colour parameter box for row-based plots, i.e., where each feature is a point.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param ID Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param active_tab A character vector of decoded names for available row statistics tables.
#' @param covariates A character vector of row metadata fields.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with colouring parameters for row-based plots.
#'
#' @details
#' This is similar to \code{\link{.createColorPanelForColumnPlots}}, with some differences.
#' Row-based plots can be coloured by nothing, by row metadata or by the \emph{selection} of certain features.
#' That is, the single chosen feature will be highlighted on the plot; its expression values are ignored.
#' Options are provided to choose the colour with which the highlighting is performed.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_createColorPanelForRowPlots
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.createColorPanelForColumnPlots}}
#'
#' @importFrom shiny radioButtons tagList selectInput selectizeInput
#' @importFrom colourpicker colourInput
.createColorPanelForRowPlots <- function(mode, ID, param_choices, active_tab, covariates) {
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
            ),
        .add_general_aesthetic_UI(mode, ID, param_choices)
        )
}

.add_general_aesthetic_UI <- function(mode, ID, param_choices) {
    tagList(
        hr(),
        sliderInput(paste0(mode, ID, "_", .plotPointSize), label = "Point size:", 
                    min=0.5, max=2, value=param_choices[,.plotFontSize]),
        sliderInput(paste0(mode, ID, "_", .plotPointTransparency), label = "Point opacity", 
                    min=0.1, max=1, value=param_choices[,.plotPointTransparency]),
        hr(),
        sliderInput(paste0(mode, ID, "_", .plotFontSize), label = "Font size:", 
                    min=0.5, max=2, value=param_choices[,.plotFontSize]),
        radioButtons(paste0(mode, ID, "_", .plotLegendPosition), label = "Legend position:", inline=TRUE,
                     choices=c(.plotLegendBottomTitle, .plotLegendRightTitle), 
                     selected=param_choices[,.plotLegendPosition])
        )
}

#' Brush parameter box 
#'
#' Create a brush parameter box for all point-based plots.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param ID Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param brushable A character vector of decoded names for available brush transmitting panels.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with brushing parameters.
#'
#' @details
#' This function creates a collapsible box that contains brushing options, initialized with the choices in \code{memory}.
#' Options include the choice of transmitting plot and the type of brushing effect.
#' Each effect option, once selected, may yield a further subset of nested options.
#' For example, choosing to colour on brush will open up a choice of colour to use.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_createBrushPanel
#' @seealso 
#' \code{\link{.panel_generation}}
#' 
#' @importFrom shiny sliderInput radioButtons selectInput
#' @importFrom colourpicker colourInput
.createBrushPanel <- function(mode, ID, param_choices, brushable) {
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

#' Conditional elements on radio choice
#'
#' Creates a conditional UI panel that appears upon a certain  choice in a radio selection.
#'
#' @param radio_id String containing the ID of the UI element for the radio buttons.
#' @param radio_choice String containing the choice on which to show the conditional elements.
#' @param ... UI elements to show conditionally.
#'
#' @return
#' A HTML object containing elements that only appear when \code{radio_choice} is selected in the UI element for \code{radio_id}.
#' 
#' @details
#' This function is useful for hiding options that are irrelevant when a different radio choice is performed.
#' In this manner, we can avoid cluttering the UI.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_conditionalPanelOnRadio
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.createBrushPanel}},
#' \code{\link{.createColorPanelForRowPlots}},
#' \code{\link{.createColorPanelForColumnPlots}}
#'
#' @importFrom shiny conditionalPanel
.conditionalPanelOnRadio <- function(radio_id, radio_choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', radio_id, radio_choice), ...)
}

#' Coerce box status to custom classes
#'
#' Coerce the status of a \code{shinydashboard::box} to use a custom \pkg{iSEE} class.
#'
#' @param in_box A HTML tag object corresponding to a \code{box} object from the \pkg{shinydashboard} package.
#' @param mode String specifying the encoded panel type of the current plot.
#' @param old_status String specifying the current status of the \code{box}, to be replaced by \code{mode}.
#'
#' @return A modified \code{in_box} where the status is changed from \code{old_status} to \code{mode}.
#' 
#' @details
#' The \code{\link[shinydashboard]{box}} function does not allow use of custom statuses.
#' As a result, we generate the box using the \code{"danger"} status, and replace it afterwards with our custom status.
#' This gives us full control over the box colours, necessary for proper colour-coding of each panel type.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_coerce_box_status
#' @seealso
#' \code{\link{.panel_organization}},
#' \code{\link{.panel_generation}}
 .coerce_box_status <- function(in_box, mode, old_status="danger") {
    in_box$children[[1]]$attribs$class <- sub(paste0("box-", old_status),
                                              paste0("box-", tolower(mode)), 
                                              in_box$children[[1]]$attribs$class)
    return(in_box)
}

.actionbutton_biocstyle <- "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
