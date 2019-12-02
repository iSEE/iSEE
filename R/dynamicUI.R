#' Generate the panel organization UI
#'
#' Generates the user interface to control the organization of the panels, specifically their sizes.
#'
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#'
#' @return
#' A HTML tag object containing the UI elements for panel sizing.
#'
#' @details
#' This function will create a series of UI elements for all active panels, specifying the width or height of the panels.
#' We use a select element for the width as this is very discrete, and we use a slider for the height.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_panel_organization
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.panel_generation}},
#' \code{\link{.setup_initial}}
#'
#' @importFrom shiny tagList selectInput sliderInput
#' @importFrom shinydashboard box
.panel_organization <- function(active_panels) {
    N <- length(active_panels)
    collected <- vector("list", N)
    counter <- 1L

    for (i in seq_len(N)) {
        instance <- active_panels[[i]]
        mode <- .getEncodedName(instance)
        id <- instance[[.organizationId]]
        prefix <- paste0(mode, id, "_")

        ctrl_panel <- box(
            selectInput(paste0(prefix, .organizationWidth), label="Width",
                choices=seq(width_limits[1], width_limits[2]), selected=instance[[.organizationWidth]]),
            sliderInput(paste0(prefix, .organizationHeight), label="Height",
                min=height_limits[1], max=height_limits[2], value=instance[[.organizationHeight]], step=10),
            title=paste(.getFullName(instance), id), status="danger", width=NULL, solidHeader=TRUE
        )

        # Coercing to a different box status ('danger' is a placeholder, above).
        collected[[i]] <- .coerce_box_status(ctrl_panel, mode)
    }
    do.call(tagList, collected)
}

#' Show and hide panels in the User Interface
#'
#' @param mode Panel mode. See \code{\link{panelCodes}}.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#' @param width Grid width of the new panel (must be between 1 and 12).
#' @param height Height of the new panel (in pixels).
#'
#' @return A data.frame specifying the new set of active panels.
#' @rdname INTERNAL_show_panel
#'
#' @author Kevin Rue-Albrecht
.showPanel <- function(mode, id, active_panels, width=4L, height=500L) {
    active_panels <- rbind(DataFrame(active_panels), DataFrame(Type=mode, ID=id, Width=width, Height=height))

    active_panels
}

#' @param pObjects An environment containing \code{table_links}, a graph produced by \code{\link{.spawn_table_links}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @rdname INTERNAL_show_panel
#' @author Kevin Rue-Albrecht
.hidePanel <- function(mode, id, active_panels, pObjects) {
    current_type <- active_panels$Type == mode
    panel_name <- paste0(mode, id)

    # Destroying links for point selection or tables.
    .destroy_selection_panel(pObjects, panel_name)
    if (mode %in% linked_table_types) {
        .destroy_table(pObjects, panel_name)
    } else if (mode %in% point_plot_types) {
        .delete_table_links(mode, id, pObjects)
    }

    # Triggering re-rendering of the UI via change to active_panels.
    index <- which(current_type & active_panels$ID == id)
    active_panels <- active_panels[-index, ]

    # Return the updated table of active panels
    active_panels
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
#' sliderInput tagList column radioButtons tags hr brushOpts
#' selectizeInput checkboxGroupInput textAreaInput
.panel_generation <- function(memory, se) {
    collected <- list()
    counter <- 1L
    cumulative.width <- 0L
    cur.row <- list()
    row.counter <- 1L

    for (i in seq_along(memory)) {
        instance <- memory[[i]]
        mode <- .getEncodedName(instance)
        id <- instance[[.organizationId]]
        .input_FUN <- function(field) paste0(mode, id, "_", field)
        print(c(mode, id))

        obj <- .defineOutputElement(instance)
        all.params <- .defineParamInterface(instance, se=se, active_panels=memory)
        param <- do.call(tags$div, c(list(class="panel-group", role="tablist"), all.params))

        # Deciding whether to continue on the current row, or start a new row.
        panel_width <- instance[[.organizationWidth]]
        extra <- cumulative.width + panel_width
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
        cur_box <- do.call(box, c(
            list(obj, param),
            list(uiOutput(.input_FUN(.panelGeneralInfo)), uiOutput(.input_FUN(.panelLinkInfo))),
            list(title=paste(.getFullName(instance), id), solidHeader=TRUE, width=NULL, status="danger")
        ))
        cur_box <- .coerce_box_status(cur_box, mode)
        cur.row[[row.counter]] <- column(width=panel_width, cur_box, style='padding:3px;')
        row.counter <- row.counter + 1L
        cumulative.width <- cumulative.width + panel_width
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
.define_link_sources <- function(memory) {
    all_names <- vapply(memory, .getFullName, "")

    is_row_tab <- vapply(memory, FUN=is, class2="RowTable", TRUE)
    is_col_tab <- vapply(memory, FUN=is, class2="ColumnTable", TRUE)
    is_row_plot <- vapply(memory, FUN=is, class2="RowDotPlot", TRUE)
    is_col_plot <- vapply(memory, FUN=is, class2="ColumnDotPlot", TRUE)

    list(
        row_tab=all_names[is_row_tab],
        col_tab=all_names[is_col_tab],
        row_plot=all_names[is_row_plot],
        col_plot=all_names[is_col_plot]
    )
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
#' @param active_row_tab A character vector of decoded names for available row statistics tables.
#' @param active_col_tab A character vector of decoded names for available column statistics tables.
#' @param se A SingleCellExperiment object with precomputed UI information from \code{\link{.precompute_UI_info}}.
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
#' Note that some options will be disabled depending on the nature of the input, namely:
#' \itemize{
#' \item If there are no column metadata fields, users will not be allowed to colour by column metadata, obviously.
#' \item If there are no features, users cannot colour by features.
#' \item If there are no categorical column metadata fields, users will not be allowed to view the faceting options.
#' }
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
.create_visual_box_for_column_plots <- function(mode, id, param_choices, active_row_tab, active_col_tab, se) {
    covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    discrete_covariates <- .get_common_info(se, "ColumnDotPlot")$discrete.colData.names
    numeric_covariates <- .get_common_info(se, "ColumnDotPlot")$continuous.colData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names

    colorby_field <- paste0(mode, id, "_", .colorByField)
    shapeby_field <- paste0(mode, id, "_", .shapeByField)
    sizeby_field <- paste0(mode, id, "_", .sizeByField)
    pchoice_field <- paste0(mode, id, "_", .visualParamChoice)

    collapseBox(
        id=paste0(mode, id, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=param_choices[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=param_choices[[.visualParamChoice]],
            choices=.define_visual_options(discrete_covariates, numeric_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            radioButtons(
                colorby_field, label="Color by:", inline=TRUE,
                choices=.define_color_options_for_column_plots(se, covariates, all_assays),
                selected=param_choices[[.colorByField]]
            ),
            .conditional_on_radio(
                colorby_field, .colorByNothingTitle,
                colourInput(paste0(mode, id, "_", .colorByDefaultColor), label=NULL,
                    value=param_choices[[.colorByDefaultColor]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByColDataTitle,
                selectInput(paste0(mode, id, "_", .colorByColData), label=NULL,
                    choices=covariates, selected=param_choices[[.colorByColData]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByFeatNameTitle,
                tagList(
                    selectizeInput(paste0(mode, id, "_", .colorByFeatName), label=NULL, 
                        choices=NULL, selected=NULL, multiple=FALSE),
                    selectInput(
                        paste0(mode, id, "_", .colorByFeatNameAssay), label=NULL,
                        choices=all_assays, selected=param_choices[[.colorByFeatNameAssay]])),
                selectInput(
                    paste0(mode, id, "_", .colorByRowTable), label=NULL, choices=active_row_tab,
                    selected=.choose_link(param_choices[[.colorByRowTable]], active_row_tab, force_default=TRUE))
            ),
            .conditional_on_radio(colorby_field, .colorBySampNameTitle,
                tagList(
                    selectizeInput(paste0(mode, id, "_", .colorBySampName), 
                        label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
                    selectInput(
                        paste0(mode, id, "_", .colorByColTable), label=NULL, choices=active_col_tab,
                        selected=.choose_link(param_choices[[.colorByColTable]], active_col_tab, force_default=TRUE)),
                    colourInput(
                        paste0(mode, id, "_", .colorBySampNameColor), label=NULL,
                        value=param_choices[[.colorBySampNameColor]]))
            )
        ),
        .conditional_on_check_group(pchoice_field, .visualParamChoiceShapeTitle,
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) .shapeByColDataTitle),
                selected=param_choices[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, .shapeByColDataTitle,
                selectInput(
                    paste0(mode, id, "_", .shapeByColData), label=NULL,
                    choices=discrete_covariates, selected=param_choices[[.shapeByColData]])
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceFacetTitle,
            hr(), .add_facet_UI_elements_for_column_plots(mode, id, param_choices, discrete_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoicePointTitle,
            hr(),
            radioButtons(
                sizeby_field, label="Size by:", inline=TRUE,
                choices=c(.sizeByNothingTitle, if (length(numeric_covariates)) .sizeByColDataTitle),
                selected=param_choices[[.sizeByField]]
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByNothingTitle,
                numericInput(
                    paste0(mode, id, "_", .plotPointSize), label="Point size:",
                    min=0, value=param_choices[[.plotPointSize]])
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByColDataTitle,
                selectInput(paste0(mode, id, "_", .sizeByColData), label=NULL,
                    choices=numeric_covariates, selected=param_choices[[.sizeByColData]])
            ),
            .add_point_UI_elements(mode, id, param_choices)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceOtherTitle,
            hr(),
            checkboxInput(
                inputId=paste0(mode, id, "_", .contourAddTitle),
                label="Add contour (scatter only)",
                value=FALSE),
            .conditional_on_check_solo(
                paste0(mode, id, "_", .contourAddTitle),
                on_select=TRUE,
                colourInput(
                    paste0(mode, id, "_", .contourColor), label=NULL,
                    value=param_choices[[.contourColor]])),
            .add_other_UI_elements(mode, id, param_choices))
    )
}

#' Define colouring options
#'
#' Define the available colouring options for row- or column-based plots,
#' where availability is defined on the presence of the appropriate data in a SingleCellExperiment object.
#'
#' @param se A SingleCellExperiment object.
#'
#' @details
#' Colouring by column data is not available if no column data exists in \code{se} - same for the row data.
#' Colouring by feature names is not available if there are no features in \code{se}.
#' For column plots, we have an additional requirement that there must also be assays in \code{se} to colour by features.
#'
#' @return A character vector of available colouring modes, i.e., nothing, by column/row data or by feature name.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_color_options
.define_color_options_for_column_plots <- function(se, covariates, assay_names) {
    color_choices <- .colorByNothingTitle
    if (length(covariates)) {
        color_choices <- c(color_choices, .colorByColDataTitle)
    }
    if (nrow(se) && length(assay_names)) {
        color_choices <- c(color_choices, .colorByFeatNameTitle)
    }
    if (ncol(se)) {
        color_choices <- c(color_choices, .colorBySampNameTitle)
    }
    color_choices
}

#' Define visual parameter check options
#'
#' Define the available visual parameter check boxes that can be ticked.
#'
#' @param discrete_covariates A character vector of names of categorical covariates.
#' @param numeric_covariates A character vector of names of numeric covariates.
#'
#' @details
#' Currently, the only special case is when there are no categorical covariates, in which case the shaping and faceting check boxes will not be available.
#' The check boxes for showing the colouring, point aesthetics and other options are always available.
#'
#' @return A character vector of check boxes that can be clicked in the UI.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#' @rdname INTERNAL_define_visual_options
.define_visual_options <- function(discrete_covariates, numeric_covariates) {
    pchoices <- c(.visualParamChoiceColorTitle)

    if (length(discrete_covariates)) {
        pchoices <- c(pchoices, .visualParamChoiceShapeTitle)
    }

    # Insert the point choice _after_ the shape aesthetic, if present
    pchoices <- c(pchoices, .visualParamChoicePointTitle)

    if (length(discrete_covariates)) {
        pchoices <- c(pchoices, .visualParamChoiceFacetTitle)
    }
    
    c(pchoices, .visualParamChoiceOtherTitle)
}

#' Visual parameter box for row plots
#'
#' Create a visual parameter box for row-based plots, i.e., where each feature is a point.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param active_row_tab A character vector of decoded names for available row statistics tables.
#' @param active_col_tab A character vector of decoded names for available row statistics tables.
#' @param se A SingleCellExperiment object with precomputed UI information from \code{\link{.precompute_UI_info}}.
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
#' Note that some options will be disabled depending on the nature of the input, namely:
#' \itemize{
#' \item If there are no row metadata fields, users will not be allowed to colour by row metadata, obviously.
#' \item If there are no features, users cannot colour by features.
#' \item If there are no categorical column metadata fields, users will not be allowed to view the faceting options.
#' }
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
.create_visual_box_for_row_plots <- function(mode, id, param_choices, active_row_tab, active_col_tab, se) {
    covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names
    discrete_covariates <- .get_common_info(se, "RowDotPlot")$discrete.rowData.names
    numeric_covariates <- .get_common_info(se, "RowDotPlot")$continuous.rowData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names

    colorby_field <- paste0(mode, id, "_", .colorByField)
    shapeby_field <- paste0(mode, id, "_", .shapeByField)
    sizeby_field <- paste0(mode, id, "_", .sizeByField)
    pchoice_field <- paste0(mode, id, "_", .visualParamChoice)

    collapseBox(
        id=paste0(mode, id, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=param_choices[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=param_choices[[.visualParamChoice]],
            choices=.define_visual_options(discrete_covariates, numeric_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            radioButtons(
                colorby_field, label="Color by:", inline=TRUE,
                choices=.define_color_options_for_row_plots(se, covariates, all_assays),
                selected=param_choices[[.colorByField]]
            ),
            .conditional_on_radio(
                colorby_field, .colorByNothingTitle,
                colourInput(
                    paste0(mode, id, "_", .colorByDefaultColor), label=NULL,
                    value=param_choices[[.colorByDefaultColor]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByRowDataTitle,
                selectInput(
                    paste0(mode, id, "_", .colorByRowData), label=NULL,
                    choices=covariates, selected=param_choices[[.colorByRowData]])
            ),
            .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
                tagList(
                    selectizeInput(paste0(mode, id, "_", .colorByFeatName), 
                        label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
                    selectInput(
                        paste0(mode, id, "_", .colorByRowTable), label=NULL, choices=active_row_tab,
                        selected=.choose_link(param_choices[[.colorByRowTable]], active_row_tab, force_default=TRUE)),
                    colourInput(paste0(mode, id, "_", .colorByFeatNameColor), label=NULL,
                        value=param_choices[[.colorByFeatNameColor]]))
            ),
            .conditional_on_radio(colorby_field, .colorBySampNameTitle,
                tagList(
                    selectizeInput(paste0(mode, id, "_", .colorBySampName), 
                        label=NULL, choices=NULL, selected=NULL, multiple=FALSE),
                    selectInput(
                        paste0(mode, id, "_", .colorBySampNameAssay), label=NULL,
                        choices=all_assays, selected=param_choices[[.colorBySampNameAssay]])),
                selectInput(
                    paste0(mode, id, "_", .colorByColTable), label=NULL, choices=active_col_tab,
                    selected=.choose_link(param_choices[[.colorByColTable]], active_col_tab, force_default=TRUE))
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceShapeTitle,
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) .shapeByRowDataTitle),
                selected=param_choices[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, .shapeByRowDataTitle,
                selectInput(
                    paste0(mode, id, "_", .shapeByRowData), label=NULL,
                    choices=discrete_covariates, selected=param_choices[[.shapeByRowData]])
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceFacetTitle,
            hr(), .add_facet_UI_elements_for_row_plots(mode, id, param_choices, discrete_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoicePointTitle,
            hr(),
            radioButtons(
                sizeby_field, label="Size by:", inline=TRUE,
                choices=c(.sizeByNothingTitle, if (length(discrete_covariates)) .sizeByRowDataTitle),
                selected=param_choices[[.sizeByField]]
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByNothingTitle,
                numericInput(
                    paste0(mode, id, "_", .plotPointSize), label="Point size:",
                    min=0, value=param_choices[[.plotPointSize]])
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByRowDataTitle,
                selectInput(paste0(mode, id, "_", .sizeByRowData), label=NULL,
                            choices=numeric_covariates, selected=param_choices[[.sizeByRowData]])
            ),
            .add_point_UI_elements(mode, id, param_choices)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoicePointTitle,
            hr(), .add_point_UI_elements(mode, id, param_choices)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceOtherTitle,
            hr(), .add_other_UI_elements(mode, id, param_choices))
    )
}

#' @rdname INTERNAL_define_color_options
.define_color_options_for_row_plots <- function(se, covariates, assay_names) {
    color_choices <- .colorByNothingTitle
    if (length(covariates)) {
        color_choices <- c(color_choices, .colorByRowDataTitle)
    }
    if (nrow(se)) {
        color_choices <- c(color_choices, .colorByFeatNameTitle)
    }
    if (ncol(se) && length(assay_names)) {
        color_choices <- c(color_choices, .colorBySampNameTitle)
    }
    color_choices
}

#' Faceting visual parameters
#'
#' Create UI elements for selection of faceting visual parameters.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param covariates Character vector listing available covariates from the \code{colData} or \code{rowData} slot, respectively.
#'
#' @return
#' A HTML tag object containing faceting parameter inputs.
#'
#' @details
#' This creates UI elements to choose the row and column faceting covariates.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_add_facet_UI_elements
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.create_visual_box_for_column_plots}},
#' \code{\link{.create_visual_box_for_row_plots}}
#'
#' @importFrom shiny tagList selectInput
.add_facet_UI_elements_for_column_plots <- function(mode, id, param_choices, covariates) {
    rowId <- paste0(mode, id, "_", .facetByRow)
    columnId <- paste0(mode, id, "_", .facetByColumn)
    tagList(
        checkboxInput(
            rowId, label="Facet by row",
            value=param_choices[[.facetByRow]]),
        .conditional_on_check_solo(
            rowId, on_select=TRUE,
            selectInput(paste0(mode, id, "_", .facetRowsByColData), label=NULL,
                choices=covariates, selected=param_choices[[.facetRowsByColData]])
        ),
        checkboxInput(
            columnId, label="Facet by column",
            value=param_choices[[.facetByColumn]]),
        .conditional_on_check_solo(
            columnId, on_select=TRUE,
            selectInput(paste0(mode, id, "_", .facetColumnsByColData), label=NULL,
                choices=covariates, selected=param_choices[[.facetColumnsByColData]])
        )
    )
}

#' @rdname INTERNAL_add_facet_UI_elements
.add_facet_UI_elements_for_row_plots <- function(mode, id, param_choices, covariates) {
    rowId <- paste0(mode, id, "_", .facetByRow)
    columnId <- paste0(mode, id, "_", .facetByColumn)
    tagList(
        checkboxInput(
            rowId, label="Facet by row",
            value=param_choices[[.facetByRow]]),
        .conditional_on_check_solo(
            rowId, on_select=TRUE,
            selectInput(
                paste0(mode, id, "_", .facetRowsByRowData), label=NULL,
                choices=covariates, selected=param_choices[[.facetRowsByRowData]])
        ),
        checkboxInput(
            columnId, label="Facet by column",
            value=param_choices[[.facetByColumn]]),
        .conditional_on_check_solo(
            columnId, on_select=TRUE,
            selectInput(paste0(mode, id, "_", .facetColumnsByRowData), label=NULL,
                choices=covariates, selected=param_choices[[.facetColumnsByRowData]])
        )
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
#' @importFrom shiny tagList numericInput sliderInput hr checkboxInput
.add_point_UI_elements <- function(mode, id, param_choices) {
    ds_id <- paste0(mode, id, "_", .plotPointDownsample)
    tagList(
        sliderInput(
            paste0(mode, id, "_", .plotPointAlpha), label="Point opacity",
            min=0.1, max=1, value=param_choices[[.plotPointAlpha]]),
        hr(),
        checkboxInput(
            ds_id, label="Downsample points for speed",
            value=param_choices[[.plotPointDownsample]]),
        .conditional_on_check_solo(
            ds_id, on_select=TRUE,
            numericInput(
                paste0(mode, id, "_", .plotPointSampleRes), label="Sampling resolution:",
                min=1, value=param_choices[[.plotPointSampleRes]])
        )
    )
}

#' @rdname INTERNAL_add_visual_UI_elements
#' @importFrom shiny tagList radioButtons numericInput
.add_other_UI_elements <- function(mode, id, param_choices) {
    tagList(
        numericInput(
            paste0(mode, id, "_", .plotFontSize), label="Font size:",
            min=0, value=param_choices[[.plotFontSize]]),
        radioButtons(
            paste0(mode, id, "_", .plotLegendPosition), label="Legend position:", inline=TRUE,
            choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
            selected=param_choices[[.plotLegendPosition]])
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
#' @param source_type Type of the panel that is source of the selection. Either \code{"row"} or \code{"column"}.
#' @param ... Additional arguments passed to \code{\link{collapseBox}}.
#' @param field Column name in the DataFrame of parameters choices for the current plot.
#'
#' @return
#' For \code{.create_selection_param_box} and \code{.create_selection_param_box_define_box},
#' a HTML tag object containing a \code{\link{collapseBox}} with UI elements for changing point selection parameters.
#'
#' For \code{.create_selection_param_box_define_choices}, a HTML tag object containing a \code{selectInput} for choosing the transmitting panels.
#'
#' @details
#' The \code{.create_selection_param_box} function creates a collapsible box that contains point selection options, initialized with the choices in \code{memory}.
#' Options include the choice of transmitting plot and the type of selection effect.
#' Each effect option, once selected, may yield a further subset of nested options.
#' For example, choosing to colour on the selected points will open up a choice of colour to use.
#'
#' The other three functions are helper functions that avoid re-writing related code in the \code{\link{.panel_generation}} function.
#' This is mostly for other panel types that take selections but do not follow the exact structure produced by \code{.create_selection_param_box}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_create_selection_param_box
#' @seealso
#' \code{\link{.panel_generation}}
#'
#' @importFrom shiny sliderInput radioButtons selectInput actionButton hr strong br
#' @importFrom shinyjs disabled
#' @importFrom colourpicker colourInput
.create_selection_param_box <- function(mode, id, param_choices, selectable, source_type=c("row", "column")) {
    select_effect <- paste0(mode, id, "_", .selectEffect)
    source_type <- match.arg(source_type)

    # initialize active "Delete" button only if a preconfigured selection history exists
    deleteFUN <- identity
    deleteLabel <- .buttonDeleteLabel
    if (length(param_choices[[.multiSelectHistory]]) == 0L) {
        deleteFUN <- disabled
        deleteLabel <- .buttonEmptyHistoryLabel
    }

    # initialize active "Save" button only if a preconfigured active selection exists
    saveFUN <- identity
    saveLabel <- .buttonSaveLabel
    cur_brush <- param_choices[[.brushData]]
    if (length(cur_brush)==0L) {
        saveFUN <- disabled
        saveLabel <- .buttonNoSelectionLabel
    }

    .define_selection_param_box(
        mode, id, param_choices,
        .define_selection_choices(mode, id, param_choices, field=.selectByPlot, selectable=selectable, source_type),

        radioButtons(
            select_effect, label="Selection effect:", inline=TRUE,
            choices=c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle),
            selected=param_choices[[.selectEffect]]),

        .conditional_on_radio(
            select_effect, .selectColorTitle,
            colourInput(
                paste0(mode, id, "_", .selectColor), label=NULL,
                value=param_choices[[.selectColor]])
        ),
        .conditional_on_radio(
            select_effect, .selectTransTitle,
            sliderInput(
                paste0(mode, id, "_", .selectTransAlpha), label=NULL,
                min=0, max=1, value=param_choices[[.selectTransAlpha]])
        ),
        hr(),
        strong("Manage multiple selections:"),
        br(),
        saveFUN(actionButton(paste0(mode, id, "_", .multiSelectSave), label=saveLabel)),
        deleteFUN(actionButton(paste0(mode, id, "_", .multiSelectDelete), label=deleteLabel))
    )
}

#' @rdname INTERNAL_create_selection_param_box
.define_selection_param_box <- function(mode, id, param_choices, ...) {
    collapseBox(
        id=paste0(mode, id, "_", .selectParamBoxOpen),
        title="Selection parameters",
        open=param_choices[[.selectParamBoxOpen]],
        ...)
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny selectInput
.define_selection_transmitter <- function(mode, id, param_choices, field, selectable, source_type="row") {
    selectInput(
        paste0(mode, id, "_", field),
        label=sprintf("Receive %s selection from:", source_type),
        choices=selectable,
        selected=.choose_link(param_choices[[field]], selectable)
    )
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny tagList radioButtons selectizeInput
.define_selection_choices <- function(mode, id, param_choices, field, selectable, source_type="row") {
    select_multi_type <- paste0(mode, id, "_", .selectMultiType)
    tagList(
        .define_selection_transmitter(mode, id, param_choices, field, selectable, source_type),

        radioButtons(
            select_multi_type, label=NULL, inline=TRUE,
            choices=c(.selectMultiActiveTitle, .selectMultiUnionTitle, .selectMultiSavedTitle),
            selected=param_choices[[.selectMultiType]]
        ),

        .conditional_on_radio(
            select_multi_type, .selectMultiSavedTitle,
            selectizeInput(
                paste0(mode, id, "_", .selectMultiSaved), label=NULL,
                selected=NULL, choices=NULL, multiple=FALSE)
        )
    )
}

#' Conditional elements on radio or checkbox selection
#'
#' Creates a conditional UI element that appears upon a certain choice in a radio button or checkbox group selection.
#'
#' @param id String containing the id of the UI element for the radio buttons or checkbox group.
#' @param choice String containing the choice on which to show the conditional elements.
#' @param on_select Logical scalar specifying whether the conditional element should be shown upon selection in a check box, or upon de-selection (if \code{FALSE}).
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
.conditional_on_check_solo <- function(id, on_select=TRUE, ...) {
    choice <- ifelse(on_select, 'true', 'false')
    conditionalPanel(condition=sprintf('(input["%s"] == %s)', id, choice), ...)
}

#' @rdname INTERNAL_conditional_elements
#' @importFrom shiny conditionalPanel
.conditional_on_check_group <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(typeof input["%s"] !== "undefined" && input["%s"].includes("%s"))', id, id, choice), ...)
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
    in_box$children[[1]]$attribs$class <- sub(
        paste0("box-", old_status),
        paste0("box-", tolower(mode)),
        in_box$children[[1]]$attribs$class)
    return(in_box)
}

.actionbutton_biocstyle <- "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"


#' Precompute UI information
#'
#' Precompute information to be shown in the UI and store it in the internal metadata of a SingleCellExperiment object.
#'
#' @param se A SingleCellExperiment object.
#' @param data_fun_list A named list of custom plotting functions.
#' @param stat_fun_list A named list of custom statistics functions.
#'
#' @details
#' Precomputed information includes:
#' \itemize{
#' \item Unique-ified selectize choices, to avoid problems with selecting between different unnamed assays, samples or reduced dimension results.
#' \item The names of discrete metadata fields, for use in restricting choices for faceting.
#' \item A list of the custom data plot functions supplied to the \code{\link{iSEE}} function.
#' \item A list of the custom statistics table functions supplied to the \code{\link{iSEE}} function.
#' }
#'
#' Storage in the internal metadata allows us to pass a single argument to various UI functions and for them to extract out the relevant fields.
#' This avoids creating functions with many different arguments, which would be difficult to maintain.
#'
#' @author Aaron Lun
#'
#' @return A SingleCellExperiment with values stored in an \code{iSEE} field in the internal metadata.
#'
#' @seealso
#' \code{\link{.which_groupable}},
#' \code{\link{.sanitize_names}},
#' \code{\link{.get_internal_info}}
#' @rdname INTERNAL_precompute_UI_info
#' @importFrom SingleCellExperiment int_metadata
.precompute_UI_info <- function(se, data_fun_list, stat_fun_list) {
    out <- list(
        column_groupable=colnames(colData(se))[.which_groupable(colData(se))],
        row_groupable=colnames(rowData(se))[.which_groupable(rowData(se))],
        column_numeric=colnames(colData(se))[.which_numeric(colData(se))],
        row_numeric=colnames(rowData(se))[.which_numeric(rowData(se))],
        all_assays=.sanitize_names(assayNames(se)),
        red_dim_names=.sanitize_names(reducedDimNames(se)),
        sample_names=.sanitize_names(colnames(se)),
        custom_data_fun=data_fun_list,
        custom_stat_fun=stat_fun_list
    )

    if (is.null(colnames(se))) {
        out$sample_names <- sprintf("Sample %i", seq_len(ncol(se)))
    }

    int_metadata(se)$iSEE <- out
    return(se)
}

#' Sanitize names
#'
#' Convert a vector of names into a named integer vector of indices.
#'
#' @param raw_names A character vector of names.
#'
#' @return
#' An integer vector of \code{seq_along(raw_names)}, with names based on \code{raw_names}.
#'
#' @details
#' This function protects against non-unique names by converting them to integer indices, which can be used for indexing within the function.
#' The names are also made unique for display to the user by prefixing them with \code{(<index>)}.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun
#' @rdname INTERNAL_sanitize_names
#' @seealso
#' \code{\link{.panel_generation}}
.sanitize_names <- function(raw_names) {
    indices <- seq_along(raw_names)
    names(indices) <- sprintf("(%i) %s", indices, raw_names)
    indices
}

#' Extract internal information
#'
#' Extracts the requested fields from the internal metadata field of a SingleCellExperiment object.
#'
#' @param se A SingleCellExperiment.
#' @param field A string specifying the field to extract.
#' @param empty_fail Logical scalar indicating whether a warning should be raised when no internal info is present.
#'
#' @details This function is only safe to run \emph{after} \code{\link{.precompute_UI_info}} has been called.
#' As such, \code{empty_fail} is set to \code{TRUE} to catch any possible instances of unsafe execution.
#' If you turn this off, you should ensure that the surrounding code will recompute any fields when the returned value is \code{NULL}.
#'
#' @return The value of \code{field} in the internal metadata of \code{se}.
#'
#' @author Aaron Lun
#'
#' @seealso \code{\link{.precompute_UI_info}}
#' @rdname INTERNAL_get_internal_info
#' @importFrom SingleCellExperiment int_metadata
.get_internal_info <- function(se, field, empty_fail=TRUE) {
    info <- int_metadata(se)$iSEE
    if (is.null(info) && empty_fail) {
        stop("no internal metadata in 'se'")
    }
    info[[field]]
}

