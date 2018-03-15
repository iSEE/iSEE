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
        id <- active_panels$ID[i]

        # Disabling the buttons if we're at the top or bottom.
        upFUN <- downFUN <- identity
        if (i==1L) {
            upFUN <- disabled
        }
        if (i==N) {
            downFUN <- disabled
        }

        ctrl_panel <- box(
            actionButton(paste0(mode, id, "_", .organizationDiscard),"", icon = icon("trash fa-2x"), style="display:inline-block; margin:0"),
            upFUN(actionButton(paste0(mode, id, "_", .organizationUp),"",icon = icon("arrow-circle-up fa-2x"), style="display:inline-block; margin:0")),
            downFUN(actionButton(paste0(mode, id, "_", .organizationDown),"",icon = icon("arrow-circle-down fa-2x"), style="display:inline-block; margin:0")),
            actionButton(paste0(mode, id, "_", .organizationModify),"", icon = icon("gear fa-2x"), style="display:inline-block; margin:0"),
            title=.decode_panel_name(mode, id), status="danger", width=NULL, solidHeader=TRUE
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
#' @importFrom shiny actionButton fluidRow selectInput plotOutput uiOutput
#' sliderInput tagList numericInput column radioButtons tags hr brushOpts
#' selectizeInput checkboxGroupInput
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
    active_tab <- c(.noSelection, link_sources$tab)
    row_selectable <- c(.noSelection, link_sources$row)
    col_selectable <- c(.noSelection, link_sources$col)
    heatmap_sources <- c(.noSelection, link_sources$row, link_sources$tab)

    for (i in seq_len(nrow(active_panels))) {
        mode <- active_panels$Type[i]
        id <- active_panels$ID[i]
        panel.width <- active_panels$Width[i]
        param_choices <- memory[[mode]][id,]
        .input_FUN <- function(field) { paste0(mode, id, "_", field) }

        # Checking what to do with plot-specific parameters (e.g., brushing, clicking, plot height).
        if (mode!="rowStatTable") { 
            brush.opts <- brushOpts(.input_FUN(.brushField), resetOnNew=FALSE, 
                                    direction = ifelse(mode=="heatMapPlot", "y", "xy"), 
                                    fill=brush_fill_color[mode], stroke=brush_stroke_color[mode], 
                                    opacity = .brushFillOpacity)
            dblclick <- .input_FUN(.zoomClick)
            clickopt <- .input_FUN(.lassoClick)
            panel_height <- paste0(active_panels$Height[i], "px")
            panel_name <- paste0(mode, id)
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
                 .conditional_on_radio(.input_FUN(.colDataXAxis),
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
                xaxis_choices <- c(xaxis_choices, .featExprXAxisFeatNameTitle)
            }

            plot.param <- list(
                selectizeInput(.input_FUN(.featExprYAxisFeatName),
                               label = "Y-axis gene:", choices = NULL, selected = NULL, multiple=FALSE),
                selectInput(.input_FUN(.featExprYAxisRowTable), label=NULL, choices=active_tab,
                            selected=.choose_link(param_choices[[.featExprYAxisRowTable]], active_tab, force_default=TRUE)),
                selectInput(.input_FUN(.featExprAssay), label=NULL,
                            choices=all_assays, selected=param_choices[[.featExprAssay]]),
                radioButtons(.input_FUN(.featExprXAxis), label="X-axis:", inline=TRUE,
                             choices=xaxis_choices, selected=param_choices[[.featExprXAxis]]),
                .conditional_on_radio(.input_FUN(.featExprXAxis),
                                         .featExprXAxisColDataTitle,
                                         selectInput(.input_FUN(.featExprXAxisColData),
                                                     label = "X-axis column data:",
                                                     choices=column_covariates, selected=param_choices[[.featExprXAxisColData]])),
                .conditional_on_radio(.input_FUN(.featExprXAxis),
                                         .featExprXAxisFeatNameTitle,
                                         selectizeInput(.input_FUN(.featExprXAxisFeatName), 
                                                        label = "X-axis gene:", choices = NULL, selected = NULL, multiple = FALSE),
                                         selectInput(.input_FUN(.featExprXAxisRowTable), label=NULL,
                                                     choices=active_tab, selected=param_choices[[.featExprXAxisRowTable]]))
                )
        } else if (mode=="rowStatTable") {
            obj <- tagList(dataTableOutput(paste0(mode, id)), uiOutput(.input_FUN("annotation")))
        } else if (mode=="rowDataPlot") {
            obj <- plotOutput(panel_name, brush = brush.opts, dblclick=dblclick, click=clickopt, height=panel_height)
            plot.param <- list(
                 selectInput(.input_FUN(.rowDataYAxis),
                             label = "Column of interest (Y-axis):",
                             choices=row_covariates, selected=param_choices[[.rowDataYAxis]]),
                 radioButtons(.input_FUN(.rowDataXAxis), label="X-axis:", inline=TRUE,
                              choices=c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle),
                              selected=param_choices[[.rowDataXAxis]]),
                 .conditional_on_radio(.input_FUN(.rowDataXAxis),
                                          .rowDataXAxisRowDataTitle,
                                          selectInput(.input_FUN(.rowDataXAxisRowData),
                                                      label = "Column of interest (X-axis):",
                                                      choices=row_covariates, selected=param_choices[[.rowDataXAxisRowData]]))
                 )
        } else if (mode=="heatMapPlot") {
            obj <- plotOutput(panel_name, brush=brush.opts, dblclick=dblclick, height=panel_height)
            plot.param <- list(
                    collapseBox(id=.input_FUN(.heatMapFeatNameBoxOpen),
                                title="Feature parameters",
                                open=param_choices[[.heatMapFeatNameBoxOpen]],
                        selectizeInput(.input_FUN(.heatMapFeatName),
                                       label="Features:",
                                       choices=NULL, selected=NULL, multiple=TRUE,
                                       options = list(plugins = list('remove_button', 'drag_drop'))),
                        selectInput(.input_FUN(.heatMapAssay), label=NULL,
                                    choices=all_assays, selected=param_choices[[.heatMapAssay]]),
                        selectInput(.input_FUN(.heatMapImportSource), label="Import from", choices=heatmap_sources,
                                    selected=.choose_link(param_choices[[.heatMapImportSource]], heatmap_sources, force_default=TRUE)),
                        actionButton(.input_FUN(.heatMapImport), "Import features"),
                        actionButton(.input_FUN(.heatMapCluster), "Suggest feature order"),
                        hr(),
                        checkboxGroupInput(.input_FUN(.heatMapCenterScale), label="Expression values are:", 
                                           selected=param_choices[[.heatMapCenterScale]][[1]],
                                           choices=c(.heatMapCenterTitle, .heatMapScaleTitle), inline=TRUE),
                        numericInput(.input_FUN(.heatMapLower), label="Lower bound:",
                                     value = param_choices[[.heatMapLower]]), 
                        numericInput(.input_FUN(.heatMapUpper), label="Upper bound:",
                                     value = param_choices[[.heatMapUpper]]), 
                        .conditional_on_check(.input_FUN(.heatMapCenterScale), .heatMapCenterTitle,
                                              selectInput(.input_FUN(.heatMapCenteredColors), label="Color scale:",
                                                          choices = c("purple-black-yellow", "blue-white-orange"),
                                                          selected = param_choices[[.heatMapCenteredColors]]))
                    ),
                    collapseBox(id=.input_FUN(.heatMapColDataBoxOpen),
                                title="Column data parameters",
                                open=param_choices[[.heatMapColDataBoxOpen]],
                        selectizeInput(.input_FUN(.heatMapColData),
                                       label="Column data:",
                                       choices=column_covariates,
                                       multiple = TRUE, 
                                       selected=param_choices[[.heatMapColData]][[1]],
                                       options = list(plugins = list('remove_button', 'drag_drop'))),
                        plotOutput(.input_FUN(.heatMapLegend))
                    )
                )
        } else {
            stop(sprintf("'%s' is not a recognized panel mode"), mode)
        }

        # Adding graphical parameters if we're plotting.
        if (mode=="rowStatTable") {
            param <- list(hr(), tags$div(class = "panel-group", role = "tablist",
                collapseBox(.input_FUN(.selectParamBoxOpen),
                            title = "Point selection parameters",
                            open = param_choices[[.selectParamBoxOpen]],
                            selectInput(.input_FUN(.selectByPlot),
                                        label = "Receive selection from:", 
                                        choices=row_selectable,
                                        selected=.choose_link(param_choices[[.selectByPlot]], row_selectable))
                            )
                )
            )
        } else if (mode=="rowDataPlot") {
            # Slightly different handling of the row data.
            param <- list(tags$div(class = "panel-group", role = "tablist",
                do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
                                            title="Data parameters",
                                            open=param_choices[[.dataParamBoxOpen]]),
                                       plot.param)),
                .create_visual_box_for_row_plots(mode, id, param_choices, active_tab, row_covariates),
                .create_selection_param_box(mode, id, param_choices, row_selectable)
                )
            )
        } else if (mode=="heatMapPlot") {
            param <- list(do.call(tags$div, c(list(class = "panel-group", role = "tablist"),
                    plot.param,
                    .create_selection_param_box(mode, id, param_choices, col_selectable)
                    )
                )  
            )
        } else {
            param <- list(tags$div(class = "panel-group", role = "tablist",
                # Options for fundamental plot parameters.
                do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
                                            title="Data parameters",
                                            open=param_choices[[.dataParamBoxOpen]]),
                                       plot.param)),

                # Options for visual parameters.
                .create_visual_box_for_column_plots(mode, id, param_choices, active_tab, column_covariates, all_assays, no_rows=nrow(se)==0),

                # Options for point selection parameters.
                .create_selection_param_box(mode, id, param_choices, col_selectable)
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
            list(title=.decode_panel_name(mode, id), solidHeader=TRUE, width=NULL, status = "danger")))
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
#' Define all possible sources of links between active panels, i.e., feature selections from row statistics tables or point selections from plots.
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
    row_selectable <- all_names[is_row & !is_heat]
    col_selectable <- all_names[!is_tab & !is_row & !is_heat]

    return(list(tab=active_tab, row=row_selectable, col=col_selectable))
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
#' However, a default choice is not necessary for point selection transmission, where no selection is perfectly valid.
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

#' Add a visual parameter box for column plots
#'
#' Create a visual parameter box for column-based plots, i.e., where each sample is a point.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param active_tab A character vector of decoded names for available row statistics tables.
#' @param covariates A character vector of column metadata fields.
#' @param all_assays A vector of assay options, usually produced from \code{\link{.sanitize_names}}.
#' @param no_rows A logicals scalar indicating whether there are no rows to select.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with visual parameters for column-based plots.
#'
#' @details
#' Column-based plots can be coloured by nothing, by column metadata or by the expression of certain features.
#' This function creates a collapsible box that contains all of these options, initialized with the choices in \code{memory}.
#' The box will also contain options for font size, point size and opacity, and legend placement.
#' 
#' Each option, once selected, yields a further subset of nested options.
#' For example, choosing to colour by column metadata will open up a \code{selectInput} to specify the metadata field to use.
#' Choosing to colour by feature name will open up a \code{selectizeInput}.
#' However, the values are filled on the server-side, rather than being sent to the client; this avoids long start times during re-rendering.
#'
#' Note that some options will be disabled depending on the nature of the input. 
#' For example, if there are no column metadata fields, users will not be allowed to colour by column metadata, obviously.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_create_visual_box_for_column_plots
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.create_visual_box_for_row_plots}}
#'
#' @importFrom shiny radioButtons tagList selectInput selectizeInput
#' checkboxGroupInput
#' @importFrom colourpicker colourInput
.create_visual_box_for_column_plots <- function(mode, id, param_choices, active_tab, covariates, all_assays, no_rows=FALSE) {
    colorby_field <- paste0(mode, id, "_", .colorByField)
    color_choices <- c(.colorByNothingTitle)
    if (length(covariates)) {
        color_choices <- c(color_choices, .colorByColDataTitle)
    }
    if (!no_rows) {
        color_choices <- c(color_choices, .colorByFeatNameTitle)
    }

    pchoice_field <- paste0(mode, id, "_", .visualParamChoice)
    collapseBox(
        id = paste0(mode, id, "_", .visualParamBoxOpen),
        title = "Visual parameters",
        open = param_choices[[.visualParamBoxOpen]],
        checkboxGroupInput(inputId=pchoice_field, label=NULL, inline=TRUE, selected=param_choices[[.visualParamChoice]][[1]],
                           choices=c(.visualParamChoiceColorTitle, .visualParamChoicePointTitle, .visualParamChoiceOtherTitle)),
        .conditional_on_check(pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            radioButtons(colorby_field, label="Color by:", inline=TRUE,
                         choices=color_choices, selected=param_choices[[.colorByField]]
                ),
            .conditional_on_radio(colorby_field, .colorByNothingTitle,
                colourInput(paste0(mode, id, "_", .colorByDefaultColor), label=NULL,
                            value=param_choices[[.colorByDefaultColor]])
                ),
            .conditional_on_radio(colorby_field, .colorByColDataTitle,
                selectInput(paste0(mode, id, "_", .colorByColData), label = NULL,
                            choices=covariates, selected=param_choices[[.colorByColData]])
                ),
            .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
                tagList(selectizeInput(paste0(mode, id, "_", .colorByFeatName), label = NULL, choices = NULL, selected = NULL, multiple = FALSE),
                        selectInput(paste0(mode, id, "_", .colorByFeatNameAssay), label=NULL,
                                    choices=all_assays, selected=param_choices[[.colorByFeatNameAssay]])),
                        selectInput(paste0(mode, id, "_", .colorByRowTable), label = NULL, choices=active_tab,
                                    selected=.choose_link(param_choices[[.colorByRowTable]], active_tab, force_default=TRUE))
                )
            ),
        .conditional_on_check(pchoice_field, .visualParamChoicePointTitle,
            hr(), .add_point_UI_elements(mode, id, param_choices)),
        .conditional_on_check(pchoice_field, .visualParamChoiceOtherTitle,
            hr(), .add_other_UI_elements(mode, id, param_choices))
        )
}

#' Visual parameter box for row plots
#'
#' Create a visual parameter box for row-based plots, i.e., where each feature is a point.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param active_tab A character vector of decoded names for available row statistics tables.
#' @param covariates A character vector of row metadata fields.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with visual parameters for row-based plots.
#'
#' @details
#' This is similar to \code{\link{.create_visual_box_for_column_plots}}, with some differences.
#' Row-based plots can be coloured by nothing, by row metadata or by the \emph{selection} of certain features.
#' That is, the single chosen feature will be highlighted on the plot; its expression values are ignored.
#' Options are provided to choose the colour with which the highlighting is performed.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_create_visual_box_for_row_plots
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.create_visual_box_for_column_plots}}
#'
#' @importFrom shiny radioButtons tagList selectInput selectizeInput
#' checkboxGroupInput
#' @importFrom colourpicker colourInput
.create_visual_box_for_row_plots <- function(mode, id, param_choices, active_tab, covariates) {
    colorby_field <- paste0(mode, id, "_", .colorByField)
    color_choices <- c(.colorByNothingTitle, .colorByRowDataTitle, .colorByFeatNameTitle)

    pchoice_field <- paste0(mode, id, "_", .visualParamChoice)
    collapseBox(
        id = paste0(mode, id, "_", .visualParamBoxOpen),
        title = "Visual parameters",
        open = param_choices[[.visualParamBoxOpen]],
        checkboxGroupInput(inputId=pchoice_field, label=NULL, inline=TRUE, selected=param_choices[[.visualParamChoice]][[1]],
                           choices=c(.visualParamChoiceColorTitle, .visualParamChoicePointTitle, .visualParamChoiceOtherTitle)),
        .conditional_on_check(pchoice_field, .visualParamChoiceColorTitle,
            radioButtons(colorby_field, label="Color by:", inline=TRUE,
                         choices=color_choices, selected=param_choices[[.colorByField]]
                ),
            .conditional_on_radio(colorby_field, .colorByNothingTitle,
                colourInput(paste0(mode, id, "_", .colorByDefaultColor), label=NULL,
                            value=param_choices[[.colorByDefaultColor]])
                ),
            .conditional_on_radio(colorby_field, .colorByRowDataTitle,
                selectInput(paste0(mode, id, "_", .colorByRowData), label = NULL,
                            choices=covariates, selected=param_choices[[.colorByRowData]])
                ),
            .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
                tagList(selectizeInput(paste0(mode, id, "_", .colorByFeatName), label = NULL, selected = NULL, choices = NULL, multiple = FALSE),
                        selectInput(paste0(mode, id, "_", .colorByRowTable), label = NULL, choices=active_tab,
                                    selected=.choose_link(param_choices[[.colorByRowTable]], active_tab, force_default=TRUE)),
                        colourInput(paste0(mode, id, "_", .colorByFeatNameColor), label=NULL,
                                    value=param_choices[[.colorByFeatNameColor]]))
                )
            ),
        .conditional_on_check(pchoice_field, .visualParamChoicePointTitle,
            hr(), .add_point_UI_elements(mode, id, param_choices)),
        .conditional_on_check(pchoice_field, .visualParamChoiceOtherTitle,
            hr(), .add_other_UI_elements(mode, id, param_choices))
        )
}

#' General visual parameters 
#'
#' Create UI elements for selection of general visual parameters.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#'
#' @return
#' A HTML tag object containing visual parameter inputs.
#'
#' @details
#' This creates UI elements to choose the font size, point size and opacity, and legend placement.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_add_visual_UI_elements
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.create_visual_box_for_column_plots}},
#' \code{\link{.create_visual_box_for_row_plots}}
#'
#' @importFrom shiny tagList numericInput sliderInput
.add_point_UI_elements <- function(mode, id, param_choices) {
    tagList(
        numericInput(paste0(mode, id, "_", .plotPointSize), label = "Point size:", value=param_choices[,.plotPointSize]),
        sliderInput(paste0(mode, id, "_", .plotPointAlpha), label = "Point opacity", 
                    min=0.1, max=1, value=param_choices[,.plotPointAlpha])
    )
}

#' @rdname INTERNAL_add_visual_UI_elements 
#' @importFrom shiny tagList radioButtons numericInput
.add_other_UI_elements <- function(mode, id, param_choices) { 
    tagList(    
        numericInput(paste0(mode, id, "_", .plotFontSize), label = "Font size:", value=param_choices[,.plotFontSize]),
        radioButtons(paste0(mode, id, "_", .plotLegendPosition), label = "Legend position:", inline=TRUE,
                     choices=c(.plotLegendBottomTitle, .plotLegendRightTitle), 
                     selected=param_choices[,.plotLegendPosition])
    )
}

#' Point selection parameter box 
#'
#' Create a point selection parameter box for all point-based plots.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param selectable A character vector of decoded names for available transmitting panels.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with UI elements for changing point selection parameters.
#'
#' @details
#' This function creates a collapsible box that contains point selection options, initialized with the choices in \code{memory}.
#' Options include the choice of transmitting plot and the type of selection effect.
#' Each effect option, once selected, may yield a further subset of nested options.
#' For example, choosing to colour on the selected points will open up a choice of colour to use.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_create_selection_param_box
#' @seealso 
#' \code{\link{.panel_generation}}
#' 
#' @importFrom shiny sliderInput radioButtons selectInput
#' @importFrom colourpicker colourInput
.create_selection_param_box <- function(mode, id, param_choices, selectable) {
    select_effect <- paste0(mode, id, "_", .selectEffect)

    collapseBox(
        id=paste0(mode, id, "_", .selectParamBoxOpen),
        title = "Selection parameters",
        open = param_choices[[.selectParamBoxOpen]],
        selectInput(paste0(mode, id, "_", .selectByPlot),
                    label = "Receive selection from:", 
                    choices=selectable,
                    selected=.choose_link(param_choices[[.selectByPlot]], selectable)),

        radioButtons(select_effect, label="Selection effect:", inline=TRUE,
                     choices=c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle),
                     selected=param_choices[[.selectEffect]]),

        .conditional_on_radio(select_effect, .selectColorTitle,
            colourInput(paste0(mode, id, "_", .selectColor), label=NULL,
                        value=param_choices[[.selectColor]])
            ),
        .conditional_on_radio(select_effect, .selectTransTitle,
            sliderInput(paste0(mode, id, "_", .selectTransAlpha), label=NULL,
                        min=0, max=1, value=param_choices[[.selectTransAlpha]])
            )
        )
}

#' Conditional elements on radio or checkbox selection 
#'
#' Creates a conditional UI element that appears upon a certain choice in a radio button or checkbox group selection.
#'
#' @param id String containing the id of the UI element for the radio buttons or checkbox group.
#' @param choice String containing the choice on which to show the conditional elements.
#' @param ... UI elements to show conditionally.
#'
#' @return
#' A HTML object containing elements that only appear when \code{choice} is selected in the UI element for \code{id}.
#' 
#' @details
#' This function is useful for hiding options that are irrelevant when a different radio button is selected, or when the corresponding checkbox element is unselected.
#' In this manner, we can avoid cluttering the UI.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_conditional_elements
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.create_selection_param_box}},
#' \code{\link{.create_visual_box_for_row_plots}},
#' \code{\link{.create_visual_box_for_column_plots}}
#'
#' @importFrom shiny conditionalPanel
.conditional_on_radio <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', id, choice), ...)
}

#' @rdname INTERNAL_conditional_elements
#' @importFrom shiny conditionalPanel
.conditional_on_check <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"].includes("%s"))', id, choice), ...)
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
#' Note that the boxes from \pkg{shinydashboard} are used to enclose each plot/table panel in the \code{iSEE} app.
#' They do \emph{not} represent the parameter boxes, which are instead enclosed in Bootstrap panels (see \code{\link{collapseBox}}).
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
