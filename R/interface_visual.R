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
.create_visual_box_for_column_plots <- function(param_choices, active_row_tab, active_col_tab, se) {
    covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    discrete_covariates <- .get_common_info(se, "ColumnDotPlot")$discrete.colData.names
    numeric_covariates <- .get_common_info(se, "ColumnDotPlot")$continuous.colData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(param_choices)
    colorby_field <- paste0(plot_name, "_", .colorByField)
    shapeby_field <- paste0(plot_name, "_", .shapeByField)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)
    pchoice_field <- paste0(plot_name, "_", .visualParamChoice)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
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
                colourInput(paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                    value=param_choices[[.colorByDefaultColor]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByColDataTitle,
                selectInput(paste0(plot_name, "_", .colorByColData), label=NULL,
                    choices=covariates, selected=param_choices[[.colorByColData]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByFeatNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorByFeatName), label=NULL, 
                        choices=NULL, selected=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorByFeatNameAssay), label=NULL,
                        choices=all_assays, selected=param_choices[[.colorByFeatNameAssay]])),
                selectInput(
                    paste0(plot_name, "_", .colorByRowTable), label=NULL, choices=active_row_tab,
                    selected=.choose_link(param_choices[[.colorByRowTable]], active_row_tab, force_default=TRUE))
            ),
            .conditional_on_radio(colorby_field, .colorBySampNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorBySampName), 
                        label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorByColTable), label=NULL, choices=active_col_tab,
                        selected=.choose_link(param_choices[[.colorByColTable]], active_col_tab, force_default=TRUE)),
                    colourInput(
                        paste0(plot_name, "_", .colorBySampNameColor), label=NULL,
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
                    paste0(plot_name, "_", .shapeByColData), label=NULL,
                    choices=discrete_covariates, selected=param_choices[[.shapeByColData]])
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceFacetTitle,
            hr(), .add_facet_UI_elements(param_choices, discrete_covariates)),
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
                    paste0(plot_name, "_", .plotPointSize), label="Point size:",
                    min=0, value=param_choices[[.plotPointSize]])
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByColDataTitle,
                selectInput(paste0(plot_name, "_", .sizeByColData), label=NULL,
                    choices=numeric_covariates, selected=param_choices[[.sizeByColData]])
            ),
            .add_point_UI_elements(param_choices)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceOtherTitle,
            hr(),
            checkboxInput(
                inputId=paste0(plot_name, "_", .contourAdd),
                label="Add contour (scatter only)",
                value=FALSE),
            .conditional_on_check_solo(
                paste0(plot_name, "_", .contourAdd),
                on_select=TRUE,
                colourInput(
                    paste0(plot_name, "_", .contourColor), label=NULL,
                    value=param_choices[[.contourColor]])),
            .add_other_UI_elements(param_choices))
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
.create_visual_box_for_row_plots <- function(param_choices, active_row_tab, active_col_tab, se) {
    covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names
    discrete_covariates <- .get_common_info(se, "RowDotPlot")$discrete.rowData.names
    numeric_covariates <- .get_common_info(se, "RowDotPlot")$continuous.rowData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(param_choices)
    colorby_field <- paste0(plot_name, "_", .colorByField)
    shapeby_field <- paste0(plot_name, "_", .shapeByField)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)
    pchoice_field <- paste0(plot_name, "_", .visualParamChoice)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
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
                    paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                    value=param_choices[[.colorByDefaultColor]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByRowDataTitle,
                selectInput(
                    paste0(plot_name, "_", .colorByRowData), label=NULL,
                    choices=covariates, selected=param_choices[[.colorByRowData]])
            ),
            .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorByFeatName), 
                        label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorByRowTable), label=NULL, choices=active_row_tab,
                        selected=.choose_link(param_choices[[.colorByRowTable]], active_row_tab, force_default=TRUE)),
                    colourInput(paste0(plot_name, "_", .colorByFeatNameColor), label=NULL,
                        value=param_choices[[.colorByFeatNameColor]]))
            ),
            .conditional_on_radio(colorby_field, .colorBySampNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorBySampName), 
                        label=NULL, choices=NULL, selected=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorBySampNameAssay), label=NULL,
                        choices=all_assays, selected=param_choices[[.colorBySampNameAssay]])),
                selectInput(
                    paste0(plot_name, "_", .colorByColTable), label=NULL, choices=active_col_tab,
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
                    paste0(plot_name, "_", .shapeByRowData), label=NULL,
                    choices=discrete_covariates, selected=param_choices[[.shapeByRowData]])
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceFacetTitle,
            hr(), .add_facet_UI_elements(param_choices, discrete_covariates)),
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
                    paste0(plot_name, "_", .plotPointSize), label="Point size:",
                    min=0, value=param_choices[[.plotPointSize]])
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByRowDataTitle,
                selectInput(paste0(plot_name, "_", .sizeByRowData), label=NULL,
                            choices=numeric_covariates, selected=param_choices[[.sizeByRowData]])
            ),
            .add_point_UI_elements(param_choices)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceOtherTitle,
            hr(), .add_other_UI_elements(param_choices))
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
.add_facet_UI_elements <- function(param_choices, covariates) {
    plot_name <- .getEncodedName(param_choices)
    rowId <- paste0(plot_name, "_", .facetByRow)
    columnId <- paste0(plot_name, "_", .facetByColumn)
    choices <- c(.noSelection, covariates)
    
    tagList(
        selectInput(paste0(plot_name, "_", .facetByRow), label="Facet by row:",
            choices=choices, selected=param_choices[[.facetByRow]]),
        selectInput(paste0(plot_name, "_", .facetByColumn), label="Facet by column:",
            choices=choices, selected=param_choices[[.facetByColumn]])
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
.add_point_UI_elements <- function(param_choices) {
    plot_name <- .getEncodedName(param_choices)
    ds_id <- paste0(plot_name, "_", .plotPointDownsample)
    tagList(
        sliderInput(
            paste0(plot_name, "_", .plotPointAlpha), label="Point opacity",
            min=0.1, max=1, value=param_choices[[.plotPointAlpha]]),
        hr(),
        checkboxInput(
            ds_id, label="Downsample points for speed",
            value=param_choices[[.plotPointDownsample]]),
        .conditional_on_check_solo(
            ds_id, on_select=TRUE,
            numericInput(
                paste0(plot_name, "_", .plotPointSampleRes), label="Sampling resolution:",
                min=1, value=param_choices[[.plotPointSampleRes]])
        )
    )
}

#' @rdname INTERNAL_add_visual_UI_elements
#' @importFrom shiny tagList radioButtons numericInput
.add_other_UI_elements <- function(param_choices) {
    plot_name <- .getEncodedName(param_choices)
    tagList(
        numericInput(
            paste0(plot_name, "_", .plotFontSize), label="Font size:",
            min=0, value=param_choices[[.plotFontSize]]),
        radioButtons(
            paste0(plot_name, "_", .plotLegendPosition), label="Legend position:", inline=TRUE,
            choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
            selected=param_choices[[.plotLegendPosition]])
    )
}
