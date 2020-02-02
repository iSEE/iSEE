#' Process heatmap colorscales
#'
#' These functions generate sets of commands ultimately evaluated  in the panel environment to generate various colorscale for the \code{\link{ComplexHeatmap}} plot.
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#' @param envir An environment containing \code{plot.data}.
#'
#' @return
#' \code{.process_heatmap_assay_colormap} defines \code{heatmap_col}, which is either a named set of color for discrete assays, or a \code{\link{colorRamp2}} function that interpolates colors for continuous assays.
#'
#' \code{.process_heatmap_column_annotations_colorscale} defines \code{column_annot}, a \code{\link{columnAnnotation}}, if any column metadata is selected. Otherwise returns an empty vector.
#'
#' \code{.process_heatmap_row_annotations_colorscale} defines \code{row_annot}, a \code{\link{rowAnnotation}}., if any row metadata is selected. Otherwise returns an empty vector.
#'
#' \code{.process_heatmap_continuous_annotation_colorscale} defines \code{.col_FUN}, a function which accepts a vector of numeric values and returns interpolated colors.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_assay_colormap
.process_heatmap_assay_colormap <- function(x, se, envir) {
    assay_name <- x[[.heatMapAssay]]

    cmds <- c()

    if (assay_name %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.assay.names) {
        is_centered <- x[[.assayCenterRowsTitle]]
        if (is_centered) {
            choice_colors <- x[[.heatMapDivergentColormap]]
            choice_colors <- strsplit(choice_colors, split = " < ", fixed = TRUE)[[1]]
            cmds <- c(cmds, sprintf(".col_colors <- %s", deparse(choice_colors)))
        } else {
            cmds <- c(cmds, sprintf(".col_colors <- assayColorMap(colormap, %s, discrete=FALSE)(21L)", deparse(assay_name)))
        }
        .text_eval(cmds, envir)
        cmds <- c(cmds, .process_heatmap_continuous_assay_colorscale(x, envir, is_centered))
        cmds <- c(cmds, "heatmap_col <- .col_FUN")
    } else if (assay_name %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.assay.names) {
        cmds <- c(cmds, ".col_values <- as.vector(plot.data)")
        cmds <- c(cmds, '.col_values <- setdiff(.col_values, NA)')
        cmds <- c(cmds, sprintf(".col_colors <- colDataColorMap(colormap, %s, discrete=TRUE)(%s)",
            deparse(assay_name), 'length(unique(.col_values))'))
        cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
        cmds <- c(cmds, "heatmap_col <- .col_colors")
    }

    cmds
}

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation
#'
#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_column_annotations_colorscale
.process_heatmap_column_annotations_colorscale <- function(x, se, envir) {
    cmds <- c()
    if (length(x[[.heatMapColData]]) || x[[.selectEffect]] == .selectColorTitle) {
        cmds <- c(cmds, "# Keep all samples to compute the full range of continuous annotations")
        cmds <- c(cmds, sprintf("column_data <- colData(se)[, %s, drop=FALSE]", .deparse_for_viewing(x[[.heatMapColData]])))
        # Process selected points
        if (x[[.selectEffect]] == .selectColorTitle) {
            cmds <- c(cmds, 'column_data[["Selected points"]] <- rep(FALSE, nrow(column_data))')
            if (exists("col_selected", envir=envir, inherits=FALSE)){
                cmds <- c(cmds, 'column_data[unlist(col_selected), "Selected points"] <- TRUE')
            }
        }
        .text_eval(cmds, envir)
        # Collect color maps
        cmds <- c(cmds, "", "column_col <- list()", "")
        for (annot in x[[.heatMapColData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(envir, annot, "column_data"))
            # Need to store and evaluate this command, without re-evaluating all the previous ones (TODO: use a command store)
            cmd_get_value <- sprintf(".col_values <- column_data[[%s]]", deparse(annot))
            cmds <- c(cmds, cmd_get_value)
            .text_eval(cmd_get_value, envir)
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.colData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- colDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot)))
                cmds <- c(cmds, .process_heatmap_continuous_annotation_colorscale(x, envir))
                cmds <- c(cmds, sprintf("column_col[[%s]] <- .col_FUN", deparse(annot)))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.colData.names) {
                cmds <- c(cmds, ".col_values <- setdiff(.col_values, NA)")
                cmds <- c(cmds, sprintf(".col_colors <- colDataColorMap(colormap, %s, discrete=TRUE)(%s)",
                    deparse(annot), 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf("column_col[[%s]] <- .col_colors", deparse(annot)))
            }
            cmds <- c(cmds, "")
        }
        # Add color map for selected points
        if (x[[.selectEffect]] == .selectColorTitle) {
            cmds <- c(cmds,
                sprintf('column_col[["Selected points"]] <- c("TRUE"=%s, "FALSE"="white")', deparse(x[[.selectColor]])),
                "")
        }
        cmds <- c(cmds, 'column_data <- column_data[.heatmap.columns, , drop=FALSE]')
        cmds <- c(cmds, 'column_data <- as.data.frame(column_data, optional=TRUE)') # preserve colnames
        if (length(x[[.heatMapColData]])) {
            cmds <- c(cmds, sprintf(".column_annot_order <- with(column_data, order(%s))",
                paste0(x[[.heatMapColData]], collapse=", ")))
            cmds <- c(cmds, "column_data <- column_data[.column_annot_order, , drop=FALSE]")
        }
        cmds <- c(cmds, sprintf("column_annot <- columnAnnotation(df=column_data, col=column_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))))
    }
    cmds
}

#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_row_annotations_colorscale
.process_heatmap_row_annotations_colorscale <- function(x, se, envir) {
    cmds <- c()
    if (length(x[[.heatMapRowData]])) {
        cmds <- c(cmds, "# Keep all features to compute the full range of continuous annotations")
        cmds <- c(cmds, sprintf("row_data <- rowData(se)[, %s, drop=FALSE]", .deparse_for_viewing(x[[.heatMapRowData]])))
        .text_eval(cmds, envir)
        # column color maps
        cmds <- c(cmds, "", "row_col <- list()", "")
        for (annot in x[[.heatMapRowData]]) {
            cmds <- c(cmds, .coerce_dataframe_columns(envir, annot, "row_data"))
            # Need to store and evaluate this command, without re-evaluating all the previous ones (TODO: use a command store)
            cmd_get_value <- sprintf('.col_values <- row_data[[%s]]', deparse(annot))
            cmds <- c(cmds, cmd_get_value)
            .text_eval(cmd_get_value, envir)
            if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$continuous.rowData.names) {
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, %s, discrete=FALSE)(21L)', deparse(annot)))
                cmds <- c(cmds, .process_heatmap_continuous_annotation_colorscale(x, envir))
                cmds <- c(cmds, sprintf('row_col[[%s]] <- .col_FUN', deparse(annot)))
            } else if (annot %in% .get_common_info(se, "ComplexHeatmapPlot")$discrete.rowData.names) {
                cmds <- c(cmds, sprintf('.col_values <- setdiff(.col_values, NA)', annot))
                cmds <- c(cmds, sprintf('.col_colors <- rowDataColorMap(colormap, %s, discrete=TRUE)(%s)',
                    deparse(annot), 'length(unique(.col_values))'))
                cmds <- c(cmds, 'if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }')
                cmds <- c(cmds, sprintf('row_col[[%s]] <- .col_colors', deparse(annot)))
            }
            cmds <- c(cmds, "")
        }
        cmds <- c(cmds, 'row_data <- row_data[.heatmap.rows, , drop=FALSE]')
        cmds <- c(cmds, 'row_data <- as.data.frame(row_data, optional=TRUE)') # preserve colnames
        cmds <- c(cmds, sprintf("row_annot <- rowAnnotation(df=row_data, col=row_col, annotation_legend_param=list(direction=%s))",
            deparse(tolower(x[[.plotLegendDirection]]))))
    }
    cmds
}

#' @rdname INTERNAL_process_heatmap_colormap
#' @aliases .process_heatmap_continuous_annotation_colorscale
.process_heatmap_continuous_annotation_colorscale <- function(x, envir) {
    cmds <- c()

    col_values <- as.vector(envir$plot.data)
    col_range <- range(col_values, na.rm = TRUE)

    col_range <- .safe_nonzero_range(col_range, centered = FALSE)

    return(sprintf(
        '.col_FUN <- colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = .col_colors)',
        col_range[1], col_range[2]))
}

#' @param centered A logical scalar that indicates whether \code{plot.data} rows are centered.
#'
#' @rdname INTERNAL_process_heatmap_colormap
.process_heatmap_continuous_assay_colorscale <- function(x, envir, centered=FALSE) {
    cmds <- c()

    if (x[[.heatMapCustomAssayBounds]]) {
        lower_bound <- x[[.assayLowerBound]]
        upper_bound <- x[[.assayUpperBound]]

        if (any(is.na(c(lower_bound, upper_bound)))) {
            col_range <- range(envir$plot.data, na.rm = TRUE)
            if (is.na(lower_bound)) {
                lower_bound <- col_range[1]
            }
            if (is.na(upper_bound)) {
                upper_bound <- col_range[2]
            }
        }
        col_range <- c(lower_bound, upper_bound)
    } else {
        col_range <- range(envir$plot.data, na.rm = TRUE)
    }

    col_range <- .safe_nonzero_range(col_range, centered)

    if (centered) {
        cmds <- c(cmds, sprintf(
            '.col_FUN <- colorRamp2(breaks = c(%s, 0, %s), colors = .col_colors)',
            col_range[1], col_range[2]))
    } else {
        cmds <- c(cmds, sprintf(
            ".col_FUN <- colorRamp2(breaks = seq(%s, %s, length.out = 21L), colors = .col_colors)",
            col_range[1], col_range[2]))
    }

    return(cmds)
}

#' Process transfomations applied to rows of a heatmap matrix
#'
#' @param x An instance of a \linkS4class{ComplexHeatmapPlot} class.
#'
#' @return A character vector of commands that apply transformations to \code{plot.data}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_process_heatmap_assay_row_transformations
.process_heatmap_assay_row_transformations <- function(x) {
    cmds <- c()

    if (x[[.assayCenterRowsTitle]]) {
        cmds <- c(cmds, "plot.data <- plot.data - rowMeans(plot.data)")
        if (x[[.assayScaleRowsTitle]]) {
            cmds <- c(cmds, "plot.data <- plot.data / rowSds(plot.data)")
        }
    }

    return(cmds)
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
#'
#' @rdname INTERNAL_create_visual_box_for_column_plots
.create_visual_box_for_complexheatmap <- function(x, se) {
    plot_name <- .getEncodedName(x)

    all_coldata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.colData.names
    all_rowdata <- .get_common_info(se, "ComplexHeatmapPlot")$valid.rowData.names

    assay_range <- range(assay(se, x[[.heatMapAssay]]), na.rm = TRUE)

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    pchoice_field <- .input_FUN(.visualParamChoice)

    collapseBox(
        id=paste0(plot_name, "_", .visualParamBoxOpen),
        title="Visual parameters",
        open=x[[.visualParamBoxOpen]],
        checkboxGroupInput(
            inputId=pchoice_field, label=NULL, inline=TRUE,
            selected=x[[.visualParamChoice]],
            choices=c(.visualParamChoiceMetadataTitle, .visualParamChoiceTransformTitle, .visualParamChoiceColorTitle, .visualParamChoiceLabelsTitle, .visualParamChoiceLegendTitle)),
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
            checkboxInput(.input_FUN(.assayCenterRowsTitle), "Center", value=x[[.assayCenterRowsTitle]]),
            .conditional_on_check_solo(.input_FUN(.assayCenterRowsTitle), on_select = TRUE,
                checkboxInput(.input_FUN(.assayScaleRowsTitle), "Scale", value=x[[.assayCenterRowsTitle]]),
                selectizeInput(.input_FUN(.heatMapDivergentColormap), label="Divergent assay colormap:",
                    selected=x[[.heatMapDivergentColormap]],
                    choices=c(.colormapPurpleBlackYellow, .colormapBlueWhiteOrange, .colormapBlueWhiteRed, .colormapGreenWhiteRed))
            )
        ),
        .conditional_on_check_group(
            pchoice_field, .visualParamChoiceColorTitle,
            hr(),
            checkboxInput(.input_FUN(.heatMapCustomAssayBounds), "Use custom colorscale bounds",
                value = x[[.heatMapCustomAssayBounds]]),
            .conditional_on_check_solo(.input_FUN(.heatMapCustomAssayBounds), on_select = TRUE,
                numericInput(.input_FUN(.assayLowerBound), "Lower bound",
                    value=x[[.assayLowerBound]], min = -Inf, max = Inf),
                numericInput(.input_FUN(.assayUpperBound), "Upper bound",
                    value=x[[.assayUpperBound]], min = -Inf, max = Inf))
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

#' Define additional heatmap observers
#'
#' Define a series of observers to track additional parameters specific to the \code{ComplexHeatmapPlot} panel.
#' These register input changes to each specified parameter in the app's memory
#' and request an update to the output of the affected panel.
#'
#' @param plot_name String containing the name of the panel.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' Observers are set up to monitor the UI elements that can change various parameters specific to the \code{ComplexHeatmapPlot} panel.
#' A \code{NULL} is invisibly returned.
#'
#'
#' @seealso
#' \code{\link{.requestUpdate}} and \code{\link{.requestCleanUpdate}},
#' used to trigger updates to the panel output.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_create_heatmap_extra_observers
#' @importFrom shiny observeEvent updateNumericInput
.create_heatmap_extra_observers <- function(plot_name, se, input, session, pObjects, rObjects) {

    .input_FUN <- function(field) paste0(plot_name, "_", field)
    # nocov start
    observeEvent(input[[.input_FUN(.heatMapAssay)]], {
        # .createUnprotectedParameterObservers with a twist
        matched_input <- as(input[[.input_FUN(.heatMapAssay)]], typeof(pObjects$memory[[plot_name]][[.heatMapAssay]]))
        if (identical(matched_input, pObjects$memory[[plot_name]][[.heatMapAssay]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.heatMapAssay]] <- matched_input

        # Twist: clear and update the limits of lower/upper bounds based on the new data
        plot_range <- range(assay(se, input[[.input_FUN(.heatMapAssay)]]), na.rm = TRUE)
        updateNumericInput(session, .input_FUN(.assayLowerBound), value = numeric(0), min = -Inf, max = 0)
        updateNumericInput(session, .input_FUN(.assayUpperBound), value = numeric(0), min = 0, max = Inf)

        .requestUpdate(plot_name, rObjects)
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    # nocov end
    # nocov start
    observeEvent(input[[.input_FUN(.assayCenterRowsTitle)]], {
        # .createUnprotectedParameterObservers with a twist
        matched_input <- as(input[[.input_FUN(.assayCenterRowsTitle)]], typeof(pObjects$memory[[plot_name]][[.assayCenterRowsTitle]]))
        if (identical(matched_input, pObjects$memory[[plot_name]][[.assayCenterRowsTitle]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.assayCenterRowsTitle]] <- matched_input

        # Twist: clear and update the limits of lower/upper bounds based on the new data
        updateNumericInput(session, .input_FUN(.assayLowerBound), value = numeric(0), min = -Inf, max = 0)
        updateNumericInput(session, .input_FUN(.assayUpperBound), value = numeric(0), min = 0, max = Inf)

        .requestUpdate(plot_name, rObjects)
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    # nocov end
    # nocov start
    observeEvent(input[[.input_FUN(.assayScaleRowsTitle)]], {
        # .createUnprotectedParameterObservers with a twist
        matched_input <- as(input[[.input_FUN(.assayScaleRowsTitle)]], typeof(pObjects$memory[[plot_name]][[.assayScaleRowsTitle]]))
        if (identical(matched_input, pObjects$memory[[plot_name]][[.assayScaleRowsTitle]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.assayScaleRowsTitle]] <- matched_input

        # Twist: clear and update the limits of lower/upper bounds based on the new data
        updateNumericInput(session, .input_FUN(.assayLowerBound), value = numeric(0), min = -Inf, max = 0)
        updateNumericInput(session, .input_FUN(.assayUpperBound), value = numeric(0), min = 0, max = Inf)

        .requestUpdate(plot_name, rObjects)
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    # nocov end
    # nocov start
    observeEvent(input[[.input_FUN(.assayLowerBound)]], {

        cur_value <- input[[.input_FUN(.assayLowerBound)]]

        if (is.null(cur_value)) {
            return(NULL)
        }

        if (is.na(cur_value)) {
            pObjects$memory[[plot_name]][[.assayLowerBound]] <- NA_real_
            .requestUpdate(plot_name, rObjects)
            return(NULL)
        }

        pObjects$memory[[plot_name]][[.assayLowerBound]] <- cur_value

        # The upper bound cannot be lower than the lower bound
        upper_bound <- input[[.input_FUN(.assayUpperBound)]]
        if (!is.null(upper_bound) && !is.na(upper_bound) && cur_value > upper_bound) {
            # set identical values; 0-length range is handled later
            pObjects$memory[[plot_name]][[.assayUpperBound]] <- cur_value
            updateNumericInput(session, .input_FUN(.assayUpperBound), value = cur_value)
        }

        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    # nocov end
    # nocov start
    observeEvent(input[[.input_FUN(.assayUpperBound)]], {

        cur_value <- input[[.input_FUN(.assayUpperBound)]]

        if (is.null(cur_value)) {
            return(NULL)
        }

        if (is.na(cur_value)) {
            pObjects$memory[[plot_name]][[.assayUpperBound]] <- NA_real_
            .requestUpdate(plot_name, rObjects)
            return(NULL)
        }

        pObjects$memory[[plot_name]][[.assayUpperBound]] <- cur_value

        # The lower bound cannot be higher than the upper bound
        lower_bound <- input[[.input_FUN(.assayLowerBound)]]
        if (!is.null(lower_bound) && !is.na(lower_bound) && cur_value < lower_bound) {
            # set identical values; 0-length range is handled later
            pObjects$memory[[plot_name]][[.assayLowerBound]] <- cur_value
            updateNumericInput(session, .input_FUN(.assayLowerBound), value = cur_value)
        }

        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    # nocov end
    invisible(NULL)
}

#' @importFrom shiny modalDialog removeModal fluidRow column h4 actionButton br
#' @importFrom shinyAce aceEditor updateAceEditor
#' @rdname INTERNAL_create_heatmap_extra_observers
.create_heatmap_modal_observers <- function(plot_name, se, input, session, pObjects, rObjects) {
    apply_field <- "INTERNAL_ApplyFeatNameChanges"
    order_field <- "INTERNAL_OrderFeatNames"
    import_field <- "INTERNAL_ImportFeatNames"
    validate_field <- "INTERNAL_ValidateFeatNames"
    clear_field <- "INTERNAL_ClearFeatNames"

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    # nocov start
    observeEvent(input[[.input_FUN(.featureNamesEdit)]], {
        instance <- pObjects$memory[[plot_name]]
        transmitter <- pObjects$memory[[instance[[.selectRowSource]]]]

        modal_ui <- modalDialog(
            title=paste("Custom feature names for", .getFullName(instance)),
            size="l", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            fluidRow(
                column(width = 8,
                    aceEditor(.input_FUN(.heatMapFeatNameText),
                        mode="text",
                        theme="xcode",
                        autoComplete="disabled",
                        value=instance[[.heatMapFeatNameText]],
                        height="500px")
                ),
                column(width = 4,
                    actionButton(.input_FUN(clear_field), "Clear editor"), br(), br(),
                    actionButton(.input_FUN(import_field), "Import selection"), br(), br(),
                    tagList("Receiving selection from", em(strong(.getFullName(transmitter)))), br(), br(),
                    actionButton(.input_FUN(order_field), "Order alphabetically"), br(), br(),
                    actionButton(.input_FUN(validate_field), "Validate names"), br(), br(),
                    actionButton(.input_FUN(apply_field), label="Apply", style=.actionbutton_biocstyle)
                )
            )
        )

        showModal(modal_ui)
    }, ignoreInit=TRUE)
    # nocov end

    # The button that imports incoming selection into the aceEditor
    # nocov start
    observeEvent(input[[.input_FUN(import_field)]], {
        instance <- pObjects$memory[[plot_name]]

        # Compute names for the incoming selection, if any
        plot_env <- new.env()
        select_cmds <- .processMultiSelections(pObjects$memory[[plot_name]], pObjects$memory, pObjects$contents, plot_env)
        if (exists("row_selected", envir=plot_env, inherits=FALSE)){
            incoming_names <- unique(unlist(get("row_selected", envir=plot_env)))
        } else {
            incoming_names <- NULL
        }

        editor_text <- input[[.input_FUN(.heatMapFeatNameText)]]
        if (!is.null(incoming_names)) {
            editor_names <- strsplit(gsub("\n$", "", editor_text), split="\n")[[1]]
            editor_names <- union(editor_names, incoming_names)
            editor_text <- paste0(editor_names, collapse = "\n")
        }

        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = editor_text)
    })
    # nocov end

    # Button to clear the editor
    # nocov start
    observeEvent(input[[.input_FUN(clear_field)]], {
        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = "")
    })
    # nocov end

    # Button to comment out invalid names
    # nocov start
    observeEvent(input[[.input_FUN(validate_field)]], {
        instance <- pObjects$memory[[plot_name]]

        editor_text <- input[[.input_FUN(.heatMapFeatNameText)]]
        editor_lines <- strsplit(editor_text, split="\n")[[1]]
        invalid_idx <- !editor_lines %in% rownames(se) & !grepl("^[ ]*#", editor_lines)
        editor_lines[invalid_idx] <- paste0("# ", editor_lines[invalid_idx])
        editor_text <- paste0(editor_lines, collapse = "\n")
        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = editor_text)
    })
    # nocov end

    # Button to order names alphabetically
    # nocov start
    observeEvent(input[[.input_FUN(order_field)]], {
        instance <- pObjects$memory[[plot_name]]

        editor_text <- input[[.input_FUN(.heatMapFeatNameText)]]
        editor_lines <- strsplit(editor_text, split="\n")[[1]]
        editor_lines <- sort(editor_lines)
        editor_text <- paste0(editor_lines, collapse = "\n")
        updateAceEditor(session, editorId = .input_FUN(.heatMapFeatNameText), value = editor_text)
    })
    # nocov end

    # The button that actually updates the FeatNameText field.
    # nocov start
    observeEvent(input[[.input_FUN(apply_field)]], {
        pObjects$memory[[plot_name]][[.heatMapFeatNameText]] <- input[[.input_FUN(.heatMapFeatNameText)]]
        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
        removeModal()
    })
    # nocov end
}
