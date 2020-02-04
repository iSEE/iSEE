#' Add a visual parameter box for dot plots
#'
#' Create a visual parameter box for row- or column-based dot plots, i.e., where each feature or sample is a point.
#'
#' @param x A DataFrame with one row, containing the parameter choices for the current plot.
#' @param row_selectable A character vector of names for available panels that can transmit a single row selection.
#' @param col_selectable A character vector of names for available panels that can transmit a single column selection.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with visual parameters for row- or column-based plots.
#'
#' @details
#' Column-based plots can be coloured by nothing, by column metadata, by the expression of a single feature or by the identity of a single sample.
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
#' The same logic applies for row-based plots where we swap features with samples (i.e., coloring by feature will highlight a single feature, while coloring by sample will color by the expression of all features in that sample).
#' Similarly, the row metadata is used in place of the column metadata.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{.defineInterface}}, where this function is typically called.
#'
#' @importFrom shiny radioButtons tagList selectInput selectizeInput checkboxGroupInput
#' @importFrom colourpicker colourInput
#'
#' @rdname INTERNAL_create_visual_box_for_column_plots
.create_visual_box_for_column_plots <- function(x, row_selectable, col_selectable, se) {
    covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    discrete_covariates <- .get_common_info(se, "ColumnDotPlot")$discrete.colData.names
    numeric_covariates <- .get_common_info(se, "ColumnDotPlot")$continuous.colData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(x)
    colorby_field <- paste0(plot_name, "_", .colorByField)
    shapeby_field <- paste0(plot_name, "_", .shapeByField)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)
    pchoice_field <- paste0(plot_name, "_", .visualParamChoice)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=x[[.visualParamChoice]],
            choices=.define_visual_options(discrete_covariates, numeric_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            radioButtons(
                colorby_field, label="Color by:", inline=TRUE,
                choices=.define_color_options_for_column_plots(se, covariates, all_assays),
                selected=x[[.colorByField]]
            ),
            .conditional_on_radio(
                colorby_field, .colorByNothingTitle,
                colourInput(paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                    value=x[[.colorByDefaultColor]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByColDataTitle,
                selectInput(paste0(plot_name, "_", .colorByColData), label=NULL,
                    choices=covariates, selected=x[[.colorByColData]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByFeatNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorByFeatName), label=NULL,
                        choices=NULL, selected=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorByFeatNameAssay), label=NULL,
                        choices=all_assays, selected=x[[.colorByFeatNameAssay]])),
                selectInput(
                    paste0(plot_name, "_", .colorByRowTable), label=NULL, choices=row_selectable,
                    selected=.choose_link(x[[.colorByRowTable]], row_selectable))
            ),
            .conditional_on_radio(colorby_field, .colorBySampNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorBySampName),
                        label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorByColTable), label=NULL, choices=col_selectable,
                        selected=.choose_link(x[[.colorByColTable]], col_selectable)),
                    colourInput(
                        paste0(plot_name, "_", .colorBySampNameColor), label=NULL,
                        value=x[[.colorBySampNameColor]]))
            )
        ),
        .conditional_on_check_group(pchoice_field, .visualParamChoiceShapeTitle,
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) .shapeByColDataTitle),
                selected=x[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, .shapeByColDataTitle,
                selectInput(
                    paste0(plot_name, "_", .shapeByColData), label=NULL,
                    choices=discrete_covariates, selected=x[[.shapeByColData]])
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceFacetTitle,
            hr(), .add_facet_UI_elements(x, discrete_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoicePointTitle,
            hr(),
            radioButtons(
                sizeby_field, label="Size by:", inline=TRUE,
                choices=c(.sizeByNothingTitle, if (length(numeric_covariates)) .sizeByColDataTitle),
                selected=x[[.sizeByField]]
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByNothingTitle,
                numericInput(
                    paste0(plot_name, "_", .plotPointSize), label="Point size:",
                    min=0, value=x[[.plotPointSize]])
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByColDataTitle,
                selectInput(paste0(plot_name, "_", .sizeByColData), label=NULL,
                    choices=numeric_covariates, selected=x[[.sizeByColData]])
            ),
            .add_point_UI_elements(x)),
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
                    value=x[[.contourColor]])),
            .add_other_UI_elements(x))
    )
}

#' Define colouring options
#'
#' Define the available colouring options for row- or column-based plots,
#' where availability is defined on the presence of the appropriate data in a SingleCellExperiment object.
#'
#' @param se A \linkS4class{SummarizedExperiment} object.
#' @param covariates Character vector of available covariates to use for coloring.
#' @param assay_names Character vector of available assay names to use for coloring.
#'
#' @details
#' Colouring by column data is not available if no column data exists in \code{se} - same for the row data.
#' Colouring by feature names is not available if there are no features in \code{se}.
#' There must also be assays in \code{se} to colour by features (in column-based plots) or samples (in row-based plots).
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
#' @param discrete_covariates A character vector of names of categorical covariates in the metadata.
#' @param numeric_covariates A character vector of names of numeric covariates in the metadata.
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

#' @importFrom shiny radioButtons tagList selectInput selectizeInput checkboxGroupInput
#' @importFrom colourpicker colourInput
#' @rdname INTERNAL_create_visual_box_for_column_plots
.create_visual_box_for_row_plots <- function(x, row_selectable, col_selectable, se) {
    covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names
    discrete_covariates <- .get_common_info(se, "RowDotPlot")$discrete.rowData.names
    numeric_covariates <- .get_common_info(se, "RowDotPlot")$continuous.rowData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(x)
    colorby_field <- paste0(plot_name, "_", .colorByField)
    shapeby_field <- paste0(plot_name, "_", .shapeByField)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)
    pchoice_field <- paste0(plot_name, "_", .visualParamChoice)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=x[[.visualParamChoice]],
            choices=.define_visual_options(discrete_covariates, numeric_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            radioButtons(
                colorby_field, label="Color by:", inline=TRUE,
                choices=.define_color_options_for_row_plots(se, covariates, all_assays),
                selected=x[[.colorByField]]
            ),
            .conditional_on_radio(
                colorby_field, .colorByNothingTitle,
                colourInput(
                    paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                    value=x[[.colorByDefaultColor]])
            ),
            .conditional_on_radio(
                colorby_field, .colorByRowDataTitle,
                selectInput(
                    paste0(plot_name, "_", .colorByRowData), label=NULL,
                    choices=covariates, selected=x[[.colorByRowData]])
            ),
            .conditional_on_radio(colorby_field, .colorByFeatNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorByFeatName),
                        label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorByRowTable), label=NULL, choices=row_selectable,
                        selected=.choose_link(x[[.colorByRowTable]], row_selectable)),
                    colourInput(paste0(plot_name, "_", .colorByFeatNameColor), label=NULL,
                        value=x[[.colorByFeatNameColor]]))
            ),
            .conditional_on_radio(colorby_field, .colorBySampNameTitle,
                tagList(
                    selectizeInput(paste0(plot_name, "_", .colorBySampName),
                        label=NULL, choices=NULL, selected=NULL, multiple=FALSE),
                    selectInput(
                        paste0(plot_name, "_", .colorBySampNameAssay), label=NULL,
                        choices=all_assays, selected=x[[.colorBySampNameAssay]])),
                selectInput(
                    paste0(plot_name, "_", .colorByColTable), label=NULL, choices=col_selectable,
                    selected=.choose_link(x[[.colorByColTable]], col_selectable))
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceShapeTitle,
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) .shapeByRowDataTitle),
                selected=x[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, .shapeByRowDataTitle,
                selectInput(
                    paste0(plot_name, "_", .shapeByRowData), label=NULL,
                    choices=discrete_covariates, selected=x[[.shapeByRowData]])
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceFacetTitle,
            hr(), .add_facet_UI_elements(x, discrete_covariates)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoicePointTitle,
            hr(),
            radioButtons(
                sizeby_field, label="Size by:", inline=TRUE,
                choices=c(.sizeByNothingTitle, if (length(discrete_covariates)) .sizeByRowDataTitle),
                selected=x[[.sizeByField]]
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByNothingTitle,
                numericInput(
                    paste0(plot_name, "_", .plotPointSize), label="Point size:",
                    min=0, value=x[[.plotPointSize]])
            ),
            .conditional_on_radio(
                sizeby_field, .sizeByRowDataTitle,
                selectInput(paste0(plot_name, "_", .sizeByRowData), label=NULL,
                            choices=numeric_covariates, selected=x[[.sizeByRowData]])
            ),
            .add_point_UI_elements(x)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceOtherTitle,
            hr(), .add_other_UI_elements(x))
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

#' Add a visual parameter box for heatmap plots
#'
#' Create a visual parameter box for heatmap plots, i.e., where features are rows and samples are columns.
#'
#' @param x A DataFrame with one row, containing the parameter choices for the current plot.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with visual parameters for heatmap plots.
#'
#' @details
#' Heatmap plots can be annotated by row and column metadata.
#' Rows or the heatmap matrix can be transformed using centering and scaling.
#' This function creates a collapsible box that contains all of these options, initialized with the choices in \code{memory}.
#' The box will also contain options for color scales and limits, visibility of row and column names, and legend placement and direction.
#'
#' Each option, once selected, yields a further subset of nested options.
#' For example, choosing to center the heatmap rows will open a \code{selectInput} to specify the divergent colorscale to use.
#'
#' @author Kevin Rue-Albrecht
#' @seealso
#' \code{\link{.defineInterface}}, where this function is typically called.
#'
#' @importFrom shiny checkboxGroupInput selectizeInput checkboxInput numericInput radioButtons disabled
#'
#' @rdname INTERNAL_create_visual_box_for_column_plots
.create_visual_box_for_complexheatmap <- function(x, se) {
    plot_name <- .getEncodedName(x)

    all_coldata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.colData.names
    all_rowdata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.rowData.names

    assay_name <- x[[.heatMapAssay]]
    assay_discrete <- assay_name %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.assay.names

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    pchoice_field <- .input_FUN(.visualParamChoice)

    ABLEFUN <- if (assay_discrete) {
        disabled
    } else {
        identity
    }

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=x[[.visualParamChoice]],
            choices=c(.visualParamChoiceMetadataTitle, .visualParamChoiceTransformTitle, .visualParamChoiceColorTitle,
                .visualParamChoiceLabelsTitle, .visualParamChoiceLegendTitle)),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceMetadataTitle,
            hr(),
            selectizeInput(.input_FUN(.heatMapColData), label="Column annotations:",
                selected=x[[.heatMapColData]], choices=all_coldata, multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop'))),
            selectizeInput(.input_FUN(.heatMapRowData), label="Row annotations:",
                selected=x[[.heatMapRowData]], choices=all_rowdata, multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop')))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceTransformTitle,
            hr(),
            strong("Row transformations:"),
            ABLEFUN(checkboxInput(.input_FUN(.assayCenterRows), "Center", value=x[[.assayCenterRows]])),
            .conditional_on_check_solo(.input_FUN(.assayCenterRows), on_select = TRUE,
                ABLEFUN(checkboxInput(.input_FUN(.assayScaleRows), "Scale", value=x[[.assayScaleRows]])),
                ABLEFUN(selectizeInput(.input_FUN(.heatMapCenteredColormap), label="`Centered` assay colormap:",
                    selected=x[[.heatMapCenteredColormap]],
                    choices=c(.colormapPurpleBlackYellow, .colormapBlueWhiteOrange, .colormapBlueWhiteRed, .colormapGreenWhiteRed))))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            ABLEFUN(checkboxInput(.input_FUN(.heatMapCustomAssayBounds), "Use custom colorscale bounds",
                value = x[[.heatMapCustomAssayBounds]])),
            .conditional_on_check_solo(.input_FUN(.heatMapCustomAssayBounds), on_select = TRUE,
                ABLEFUN(numericInput(.input_FUN(.assayLowerBound), "Lower bound",
                    value=x[[.assayLowerBound]], min = -Inf, max = Inf)),
                ABLEFUN(numericInput(.input_FUN(.assayUpperBound), "Upper bound",
                    value=x[[.assayUpperBound]], min = -Inf, max = Inf)))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceLabelsTitle,
            hr(),
            checkboxGroupInput(
                inputId=.input_FUN(.showDimnames), label="Show names:", inline=TRUE,
                selected=x[[.showDimnames]],
                choices=c(.showNamesRowTitle, .showNamesColumnTitle))
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceLegendTitle,
            hr(),
            radioButtons(.input_FUN(.plotLegendPosition), label="Legend position:", inline=TRUE,
                choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
                selected=x[[.plotLegendPosition]]),
            radioButtons(.input_FUN(.plotLegendDirection), label="Legend direction:", inline=TRUE,
                choices=c(.plotLegendHorizontalTitle, .plotLegendVerticalTitle),
                selected=x[[.plotLegendDirection]])
        )
    )
}

#' Faceting visual parameters
#'
#' Create UI elements for selection of faceting visual parameters.
#'
#' @param x An instance of the \linkS4class{Panel} class.
#' @param covariates Character vector listing categorical metadata columns that can be used for faceting.
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
#' \code{\link{.create_visual_box_for_column_plots}},
#' \code{\link{.create_visual_box_for_row_plots}}
#'
#' @importFrom shiny tagList selectInput
.add_facet_UI_elements <- function(x, covariates) {
    plot_name <- .getEncodedName(x)
    rowId <- paste0(plot_name, "_", .facetByRow)
    columnId <- paste0(plot_name, "_", .facetByColumn)
    choices <- c(.noSelection, covariates)

    tagList(
        selectInput(paste0(plot_name, "_", .facetByRow), label="Facet by row:",
            choices=choices, selected=x[[.facetByRow]]),
        selectInput(paste0(plot_name, "_", .facetByColumn), label="Facet by column:",
            choices=choices, selected=x[[.facetByColumn]])
    )
}

#' General visual parameters
#'
#' Create UI elements for selection of general visual parameters.
#'
#' @param x An instance of a \linkS4class{Panel} class.
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
#' \code{\link{.create_visual_box_for_column_plots}},
#' \code{\link{.create_visual_box_for_row_plots}}
#'
#' @importFrom shiny tagList numericInput sliderInput hr checkboxInput
.add_point_UI_elements <- function(x) {
    plot_name <- .getEncodedName(x)
    ds_id <- paste0(plot_name, "_", .plotPointDownsample)
    tagList(
        sliderInput(
            paste0(plot_name, "_", .plotPointAlpha), label="Point opacity",
            min=0.1, max=1, value=x[[.plotPointAlpha]]),
        hr(),
        checkboxInput(
            ds_id, label="Downsample points for speed",
            value=x[[.plotPointDownsample]]),
        .conditional_on_check_solo(
            ds_id, on_select=TRUE,
            numericInput(
                paste0(plot_name, "_", .plotPointSampleRes), label="Sampling resolution:",
                min=1, value=x[[.plotPointSampleRes]])
        )
    )
}

#' @rdname INTERNAL_add_visual_UI_elements
#' @importFrom shiny tagList radioButtons numericInput
.add_other_UI_elements <- function(x) {
    plot_name <- .getEncodedName(x)
    tagList(
        numericInput(
            paste0(plot_name, "_", .plotFontSize), label="Font size:",
            min=0, value=x[[.plotFontSize]]),
        radioButtons(
            paste0(plot_name, "_", .plotLegendPosition), label="Legend position:", inline=TRUE,
            choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
            selected=x[[.plotLegendPosition]])
    )
}
