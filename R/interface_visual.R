#' Add a visual parameter box for dot plots
#'
#' Create a visual parameter box for row- or column-based dot plots, i.e., where each feature or sample is a point.
#'
#' @param x A DataFrame with one row, containing the parameter choices for the current plot.
#' @param select_info A list of character vectors named \code{row} and \code{column} which specifies the names of panels available for transmitting single selections on the rows/columns.
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
#' @importFrom stats setNames
#'
#' @rdname INTERNAL_create_visual_box
.create_visual_box <- function(x, se, select_info) {
    ui <- list(
        .defineVisualColorInterface(x, se, select_info),
        .defineVisualShapeInterface(x, se),
        .defineVisualSizeInterface(x, se),
        .defineVisualPointInterface(x, se),
        .defineVisualFacetInterface(x, se),
        .defineVisualTextInterface(x, se),
        .defineVisualOtherInterface(x)
    )
    names(ui) <- c(
        .visualParamChoiceColorTitle,
        .visualParamChoiceShapeTitle,
        .visualParamChoiceSizeTitle,
        .visualParamChoicePointTitle,
        .visualParamChoiceFacetTitle,
        .visualParamChoiceTextTitle,
        .visualParamChoiceOtherTitle
    )
    stopifnot(all(names(ui)!=""))
    keep <- !vapply(ui, is.null, logical(1))
    ui <- ui[keep]

    plot_name <- .getEncodedName(x)
    pchoice_field <- paste0(plot_name, "_", iSEEslots$visualParamChoice)
    collected <- lapply(names(ui), function(title)
        .conditionalOnCheckGroup(pchoice_field, title, ui[[title]])
    )

    collapseBox(
        id=paste0(plot_name, "_", iSEEslots$visualParamBoxOpen),
        title="Visual parameters",
        open=slot(x, iSEEslots$visualParamBoxOpen),
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=slot(x, iSEEslots$visualParamChoice),
            choices=names(ui)
        ),
        do.call(tagList, collected)
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
    c(color_choices, .colorByColSelectionsTitle)
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
    c(color_choices, .colorByRowSelectionsTitle)
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
#' @importFrom shiny checkboxGroupInput selectizeInput checkboxInput numericInput radioButtons
#' @importFrom shinyjs disabled
#'
#' @rdname INTERNAL_create_visual_box_for_complexheatmap
.create_visual_box_for_complexheatmap <- function(x, se) {
    plot_name <- .getEncodedName(x)

    all_coldata <- .getCachedCommonInfo(se, "ComplexHeatmapPlot")$valid.colData.names
    all_rowdata <- .getCachedCommonInfo(se, "ComplexHeatmapPlot")$valid.rowData.names

    assay_name <- slot(x, .heatMapAssay)
    assay_discrete <- assay_name %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.assay.names

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    pchoice_field <- .input_FUN(iSEEslots$visualParamChoice)

    ABLEFUN <- if (assay_discrete) {
        disabled
    } else {
        identity
    }

    .addSpecificTour(class(x)[1], .heatMapColData, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapColData, " + .selectize-control"),
                    intro = "Here, we can select column annotations to show as color bars above the heat map.
This will also order the columns of the heat map by the values of the selected annotations (in the specified order, if multiple annotations are specified). This is useful for providing some structure to the heatmap."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapRowData, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapRowData, " + .selectize-control"),
                    intro = "Here, we can select row annotations to show as color bars on the left of the heat map.
<br/><br/>
This will <em>not</em> affect the order of rows in the heat map, as this is controlled in the <i>Data parameters</i> box."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapShowSelection, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapShowSelection),
                    intro = "Ticked, this checkbox displays a color bar above the heat map, indicating data points received from an incoming multiple column selection.
<br/><br/>
It also reveals another checkbox that can be used to order columns of the heat maps using the selection, with selected points on the left and unselected point on the right."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapOrderSelection, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapOrderSelection),
                    intro = "Ticked, this checkbox orders columns of the heat map using the incoming selection, if any, with selected points on the left and unselected point on the right.
<br/><br/>
This ordering takes precedence over the ordering by column annotations.
This is useful to compare features of selected data points to the rest of the data set."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .assayCenterRows, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .assayCenterRows),
                    intro = "Here, we can dynamically center the values for each row shown in the heat map, i.e. to a mean value of 0.
It also reveals another checkbox that can be used to scale values for each row.
<br/><br/>
This does not alter any value in the data set; centered values are only computed on the fly for the purpose of the heat map."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .assayScaleRows, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .assayScaleRows),
                    intro = "Here, we can dynamically scale the values for each row shown in the heat map, i.e. to a standard deviation of 1.
This row transformation is only available when row values are centered using the checkbox above.
<br/><br/>
This does not alter any value in the data set; scaled values are only computed on the fly for the purpose of the heat map."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapCenteredColormap, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapCenteredColormap, " + .selectize-control"),
                    intro = "Here, we can select from a choice of diverging color maps, when row values are centered using the checkbox above.
<br/><br/>
This is useful to visualize deviations from the mean, in particular when row values are also scaled using the second checkbox above."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .heatMapCustomAssayBounds, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .heatMapCustomAssayBounds),
                    intro = "Ticked, this checkbox reveals numeric fields that let us manually set custom lower and upper bounds for the color scale of the heat map.
<br/><br/>
This is useful to override the default range of the color scale, which is automatically fit to the range of values observed in the heat map."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .assayLowerBound, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .assayLowerBound),
                    intro = "Here, we can manually override the lower bound of the heat map color scale."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .assayUpperBound, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .assayUpperBound),
                    intro = "Here, we can manually override the upper bound of the heat map color scale."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .showDimnames, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .showDimnames),
                    intro = "Here, we can control whether to show row names or column names."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .namesRowFontSize, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .namesRowFontSize),
                    intro = "Here, we can control the font size of the row names."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .namesColumnFontSize, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .namesColumnFontSize),
                    intro = "Here, we can control the font size of the column names."
                )
            )
        )
    })


    .addSpecificTour(class(x)[1], iSEEslots$plotLegendPosition, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", iSEEslots$plotLegendPosition),
                    intro = "Changes the position of the legend on the plot, if any legend exists.
On the bottom, on the right; the choice is yours."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .plotLegendDirection, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", .plotLegendDirection),
                    intro = "Changes the orientation of the legend on the plot, if any legend exists.
Horizontal, vertical; the choice is yours."
                )
            )
        )
    })

    collapseBox(
        id=paste0(plot_name, "_", iSEEslots$visualParamBoxOpen),
        title="Visual parameters",
        open=slot(x, iSEEslots$visualParamBoxOpen),
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=slot(x, iSEEslots$visualParamChoice),
            choices=c(.visualParamChoiceMetadataTitle, .visualParamChoiceTransformTitle, .visualParamChoiceColorTitle,
                .visualParamChoiceLabelsTitle, .visualParamChoiceLegendTitle)),
        .conditionalOnCheckGroup(
            pchoice_field, .visualParamChoiceMetadataTitle,
            hr(),
            .selectizeInput.iSEE(x, .heatMapColData,
                label = "Column annotations:",
                selected = slot(x, .heatMapColData),
                choices = all_coldata,
                multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop')),
                help = TRUE),
            .selectizeInput.iSEE(x, .heatMapRowData,
                label = "Row annotations:",
                selected = slot(x, .heatMapRowData),
                choices = all_rowdata,
                multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop')),
                help = TRUE),
            .checkboxInput.iSEE(x, .heatMapShowSelection,
                label = "Show column selection",
                value=slot(x, .heatMapShowSelection),
                help = TRUE),
            .conditionalOnCheckSolo(.input_FUN(.heatMapShowSelection), on_select = TRUE,
                .checkboxInput.iSEE(x, .heatMapOrderSelection,
                    label = "Order by column selection",
                    value=slot(x, .heatMapOrderSelection),
                    help = TRUE),
            )
        ),
        .conditionalOnCheckGroup(
            pchoice_field, .visualParamChoiceTransformTitle,
            hr(),
            strong("Row transformations:"),

            ABLEFUN(.checkboxInput.iSEE(x, .assayCenterRows,
                label = "Center",
                value=slot(x, .assayCenterRows),
                help = TRUE)),
            .conditionalOnCheckSolo(.input_FUN(.assayCenterRows), on_select = TRUE,
                ABLEFUN(
                    .checkboxInput.iSEE(x, .assayScaleRows,
                        label = "Scale",
                        value=slot(x, .assayScaleRows),
                        help = TRUE)
                    ),
                ABLEFUN(
                    .selectizeInput.iSEE(x, .heatMapCenteredColormap,
                        label = "Centered assay colormap:",
                        selected=slot(x, .heatMapCenteredColormap),
                        choices=c(.colormapPurpleBlackYellow, .colormapBlueWhiteOrange, .colormapBlueWhiteRed, .colormapGreenWhiteRed),
                        help = TRUE)
                    ))
        ),
        .conditionalOnCheckGroup(
            pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            ABLEFUN(
                .checkboxInput.iSEE(x, .heatMapCustomAssayBounds,
                    label = "Use custom colorscale bounds",
                    value = slot(x, .heatMapCustomAssayBounds),
                    help = TRUE)),
            .conditionalOnCheckSolo(.input_FUN(.heatMapCustomAssayBounds), on_select = TRUE,
                .numericInput.iSEE(x, .assayLowerBound,
                    label = "Lower bound",
                    value=slot(x, .assayLowerBound), min = -Inf, max = Inf,
                    help = TRUE),
                .numericInput.iSEE(x, .assayUpperBound,
                    label = "Upper bound",
                    value=slot(x, .assayUpperBound), min = -Inf, max = Inf,
                    help = TRUE))
        ),
        .conditionalOnCheckGroup(
            pchoice_field, .visualParamChoiceLabelsTitle,
            hr(),
            .checkboxGroupInput.iSEE(x, .showDimnames,
                label = "Show names:",
                inline=TRUE,
                selected=slot(x, .showDimnames),
                choices=c(.showNamesRowTitle, .showNamesColumnTitle),
                help = TRUE),
            .conditionalOnCheckGroup(
                .input_FUN(.showDimnames), .showNamesRowTitle,
                .numericInput.iSEE(x, .namesRowFontSize,
                                   label="Row names fontsize",
                                   value=slot(x, .namesRowFontSize),
                                   help=TRUE)),
            .conditionalOnCheckGroup(
                .input_FUN(.showDimnames), .showNamesColumnTitle,
                .numericInput.iSEE(x, .namesColumnFontSize,
                                   label="Column names fontsize",
                                   value=slot(x, .namesColumnFontSize),
                                   help=TRUE)),
        ),
        .conditionalOnCheckGroup(
            pchoice_field, .visualParamChoiceLegendTitle,
            hr(),
            .radioButtons.iSEE(x, iSEEslots$plotLegendPosition,
                label = "Legend position:",
                inline=TRUE,
                selected=slot(x, iSEEslots$plotLegendPosition),
                choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
                help = TRUE),
            .radioButtons.iSEE(x, .plotLegendDirection,
                label = "Legend direction:",
                inline=TRUE,
                selected=slot(x, .plotLegendDirection),
                choices=c(.plotLegendHorizontalTitle, .plotLegendVerticalTitle),
                help = TRUE)
        )
    )
}
