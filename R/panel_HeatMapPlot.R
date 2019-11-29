#' Heat map plot panel
#'
#' Create a panel with a heatmap where features are rows and samples are columns.
#'
#' Plots reduced dimensions. What more do I have to say?
#'
#' @section Constructor:
#' \code{HeatMapPlot()} creates an instance of a HeatMapPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the elements to show on the heatmap.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- HeatMapPlot()
#' x[["ColorScale"]]
#' x[["Lower"]] <- -5
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' # Returns NULL if there is not enough information.
#' sce0 <- .cacheCommonInfo(x, sce[,0])
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases HeatMapPlot HeatMapPlot-class
#' .defineParamInterface,HeatMapPlot-method
#' .createParamObservers,HeatMapPlot-method
#' @name HeatMapPlot
NULL

#' @export
HeatMapPlot <- function() {
    new("HeatMapPlot")
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "HeatMapPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .heatMapAssay)
    .Object <- .empty_default(.Object, .heatMapFeatNameBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .heatMapImportSource, .noSelection)
    .Object <- .empty_default(.Object, .heatMapColDataBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .heatMapColorBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .heatMapCenterScale, .heatMapCenterTitle)
    .Object <- .empty_default(.Object, .heatMapLower, -Inf)
    .Object <- .empty_default(.Object, .heatMapUpper, Inf)
    .Object <- .empty_default(.Object, .heatMapCenteredColors, "purple-black-yellow")

    .Object <- .empty_default(.Object, .selectEffect, .selectTransTitle)
    .Object <- .empty_default(.Object, .selectColor, "red")
    .Object <- .empty_default(.Object, .selectTransAlpha, 0.1)

    .Object
})

.heatMapCenterTitle <- "Centered"
.heatMapScaleTitle <- "Scaled"

setValidity2("HeatMapPlot", function(object) {
    msg <- character(0)

    for (box in c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen, .heatMapColorBoxOpen)) {
        if (length(val <- object[[box]])!=1L || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA logical scalar for '%s'", box, class(object)[1]))
        }
    }

    # Checks for the numeric limits.
    for (lim in c(.heatMapLower, .heatMapUpper) ){
        if (length(val <- object[[lim]])!=1L || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA numeric scalar for '%s'", lim, class(object)[1]))
        }
    }

    if (object[[.heatMapLower]] >= object[[.heatMapUpper]]) {
        msg <- c(msg, sprintf("'%s' should have a lower value than '%s' for '%s'",
            .heatMapLower, .heatMapUpper, class(object)[1]))
    }

    # Checks for the assorted string fields.
    for (field in c(.heatMapAssay, .heatMapImportSource, .heatMapCenteredColors)) {
        if (!isSingleString(object[[field]])) {
            msg <- c(msg, sprintf("'%s' should be a single string for '%s'", field, class(object)[1]))
        }
    }

    for (field in c(.heatMapImportSource, .heatMapCenteredColors)) {
        if (is.na(object[[field]])) {
            msg <- c(msg, sprintf("'%s' should be a non-NA string for '%s'", field, class(object)[1]))
        }
    }

    # Checks for centering/scaling.
    choices <- object[[.heatMapCenterScale]]
    allowable <- c(.heatMapCenterTitle, .heatMapScaleTitle)
    if (any(!choices %in% allowable)) {
        msg <- c(msg, sprintf("values in '%s' should only contain %s", .heatMapCenterScale,
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }

    msg <- .allowable_choice_error(msg, object, .selectEffect,
        c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))

    msg <- .transparency_error(msg, object, .selectByTransAlpha)

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom SummarizedExperiment colData assayNames
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "HeatMapPlot", function(x, se) {
    if (is.null(.get_common_info(se, "HeatMapPlot"))) {
        # Only using named assays.
        named_assays <- assayNames(se)
        named_assays <- named_assays[named_assays!=""]

        # Only allowing atomic covariates.
        covariates <- .find_atomic_fields(colData(se))

        se <- .set_common_info(se, "HeatMapPlot",
            valid.assay.names=named_assays,
            valid.colData.names=covariates)
    }

    callNextMethod()
})

#' @importFrom methods callNextMethod
setMethod(".refineParameters", "HeatMapPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (any(dim(se)==0L)) {
        warning(sprintf("no dimensions for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    mode <- .getEncodedName(x)
    all_assays <- .get_common_info(se, "HeatMapPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no named 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    assay_choice <- x[[.heatMapAssay]]
    if (is.na(assay_choice) || !assay_choice %in% all_assays) {
        x[[.heatMapAssay]] <- all_assays[1]
    }

    feat_choice <- x[[.heatMapFeatName]]
    x[[.heatMapFeatName]] <- feat_choice[feat_choice %in% rownames(se)]

    cov_choice <- x[[.heatMapColData]]
    column_covariates <- .get_common_info(se, "HeatMapPlot")$valid.colData.names
    x[[.heatMapColData]] <- cov_choice[cov_choice %in% column_covariates]

    x
})

.heatMapFeaturesTextInput <- "FeatTextInput"
.heatMapFeaturesTextSubmit <- "FeatTextSubmit"
.heatMapFeaturesFileInput <- "FeatFileInput"

.heatMapModalSummary <- "ModalSummary"
.heatMapModalTable <- "ModalTable"
.heatMapImportFeatures <- "Import"
.heatMapClearFeatures <- "Clear"
.heatMapCluster <- "Clustered"
.heatMapLegend <- "Legend"

.heatMapRelHeightColorBar <- 0.1
.heatMapRelHeightHeatmap <- 1
.heatMapRelHeightAnnot <- 0.1

#' @export
#' @importFrom shiny selectInput actionButton selectizeInput hr
#' checkboxGroupInput numericInput plotOutput
setMethod(".defineParamInterface", "HeatMapPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    plot_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    column_covariates <- colnames(colData(se))
    all_assays <- .get_internal_info(se, "all_assays")
    link_sources <- .define_link_sources(active_panels)
    heatmap_sources <- c(.customSelection, link_sources$row_plot, link_sources$row_tab)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    list(
        collapseBox(
            id=.input_FUN(.heatMapFeatNameBoxOpen),
            title="Feature parameters",
            open=param_choices[[.heatMapFeatNameBoxOpen]],
            selectInput(
                .input_FUN(.heatMapImportSource), label="Import from", choices=heatmap_sources,
                selected=.choose_link(param_choices[[.heatMapImportSource]], heatmap_sources, force_default=TRUE)),
            actionButton(.input_FUN(.heatMapImportFeatures), "Import features"),
            actionButton(.input_FUN(.heatMapCluster), "Cluster features"),
            actionButton(.input_FUN(.heatMapClearFeatures), "Clear features"),
            selectizeInput(
                .input_FUN(.heatMapFeatName),
                label="Features:",
                choices=NULL, selected=NULL, multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop'))),
            selectInput(
                .input_FUN(.heatMapAssay), label=NULL,
                choices=all_assays, selected=param_choices[[.heatMapAssay]]),
            hr(),
            checkboxGroupInput(
                .input_FUN(.heatMapCenterScale), label="Expression values are:",
                selected=param_choices[[.heatMapCenterScale]][[1]],
                choices=c(.heatMapCenterTitle, .heatMapScaleTitle), inline=TRUE),
            numericInput(
                .input_FUN(.heatMapLower), label="Lower bound:",
                value=param_choices[[.heatMapLower]]),
            numericInput(
                .input_FUN(.heatMapUpper), label="Upper bound:",
                value=param_choices[[.heatMapUpper]]),
            .conditional_on_check_group(
                .input_FUN(.heatMapCenterScale), .heatMapCenterTitle,
                selectInput(
                    .input_FUN(.heatMapCenteredColors), label="Color scale:",
                    choices=c("purple-black-yellow", "blue-white-orange"),
                    selected=param_choices[[.heatMapCenteredColors]]))
        ),
        collapseBox(
            id=.input_FUN(.heatMapColDataBoxOpen),
            title="Column data parameters",
            open=param_choices[[.heatMapColDataBoxOpen]],
            selectizeInput(
                .input_FUN(.heatMapColData),
                label="Column data:",
                choices=column_covariates,
                multiple=TRUE,
                selected=param_choices[[.heatMapColData]][[1]],
                options=list(plugins=list('remove_button', 'drag_drop'))),
            plotOutput(.input_FUN(.heatMapLegend))
        ),
        .create_selection_param_box(mode, id, param_choices, col_selectable, "column")
    )
})

#' @export
setMethod(".createParamObservers", "HeatMapPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    plot_name <- paste0(mode, id)

    .define_box_observers(mode, id, c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen, .selectParamBoxOpen), input, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=character(0),
        nonfundamental=c(.heatMapAssay, .heatMapLower, .heatMapUpper, .heatMapCenteredColors),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_heatmap_import_observer(mode, id, se, input, session, pObjects, rObjects)

    .create_heatmap_feature_observers(mode, id, se, input, session, pObjects, rObjects)

    .create_heatmap_button_observers(mode, id, se, input, session, pObjects, rObjects)

    # Updating the import source, but this does NOT trigger replotting, as we need to press the button.
    cur_field <- paste0(plot_name, "_", .heatMapImportSource)
    observeEvent(input[[cur_field]], {
        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode]][[.heatMapImportSource]]))
        if (identical(input[[cur_field]], pObjects$memory[[mode]][id, .heatMapImportSource])) {
            return(NULL)
        }
        pObjects$memory[[mode]][[.heatMapImportSource]][id] <- matched_input
    }, ignoreInit=TRUE)

    # Saving list-based values.
    # TODO: generalize .define_plot_parameter_observers to be able to handle this kind of stuff.
    for (field in c(.heatMapColData, .heatMapFeatName, .heatMapCenterScale)) {
        local({
            field0 <- field
            cur_field <- paste0(plot_name, "_", field0)

            observeEvent(input[[cur_field]], {
                existing <- pObjects$memory[[mode]][,field0][[id]]
                incoming <- as(input[[cur_field]], typeof(existing))
                if (identical(incoming, existing)) {
                    return(NULL)
                }
                pObjects$memory[[mode]] <- .update_list_element(pObjects$memory[[mode]], id, field0, incoming)
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            }, ignoreInit=TRUE, ignoreNULL=(field0==.heatMapFeatName))

            # ignoreNULL necessary for FeatName where updateSelectize generates a temporary NULL;
            # this would trigger re-rendering of the plot upon re-rendering of the UI.
        })
    }
})

#' @importFrom shiny observeEvent fluidRow column actionButton modalDialog showModal
#' @importFrom shinyAce aceEditor
.create_heatmap_import_observer <- function(mode, id, se, input, session, pObjects, rObjects) {
    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    plot_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    import_button <- .input_FUN(.heatMapImportFeatures)
    observeEvent(input[[import_button]], {
        origin <- pObjects$memory[[mode]][id, .heatMapImportSource]
        if (origin == .customSelection) {
            current_choices <- rownames(se)[pObjects$memory[[mode]][[.heatMapFeatName]][[id]]]
            showModal(modalDialog(
                title=sprintf("Features for %s", .decode_panel_name(mode, id)),
                size="l", fade=TRUE,
                footer=NULL, easyClose=TRUE,
                fluidRow(
                    column(width=6,
                        aceEditor(.input_FUN(.heatMapFeaturesTextInput),
                            mode="text",
                            theme="xcode",
                            autoComplete="disabled",
                            value=paste0(c(current_choices, ""), collapse="\n"),
                            height="500px")
                    ),
                    column(width=6,
                        verticalLayout(
                            uiOutput(.input_FUN(.heatMapModalSummary)),
                            tableOutput(.input_FUN(.heatMapModalTable))
                        )
                    )
                ),
                fluidRow(
                    column(width=1, actionButton(.input_FUN(.heatMapFeaturesTextSubmit), label="Apply")),
                    column(width=5, fileInput(
                        inputId=.input_FUN(.heatMapFeaturesFileInput),
                        label=NULL, buttonLabel="Import from file ...",
                        placeholder="No file selected",
                        accept=c(
                            "text/csv", "text/comma-separated-values,text/plain", ".csv",
                            "text/txt", ".txt",
                            "text/tsv", ".tsv"
                        )))
                )
            ))
            return(NULL)
        }
        enc <- .encode_panel_name(origin)

        incoming <- NULL
        if (enc$Type == "rowStatTable") {
            incoming <- input[[paste0(enc$Type, enc$ID, "_rows_all")]]
        } else {
            selected <- .get_selected_points(rownames(se), origin, pObjects$memory, pObjects$coordinates)
            if (is.null(selected)) {
                showNotification("Invalid: empty selection", type="warning")
                return(NULL) # avoid corner case: which(NULL)
            }
            incoming <- which(selected)

        }

        limit <- 100
        if (length(incoming) > limit) {
            showNotification(sprintf("only the first %i features used", limit), type="warning")
            incoming <- head(incoming, limit)
        }

        combined <- union(pObjects$memory[[mode]][id, .heatMapFeatName][[1]], incoming)
        updateSelectizeInput(
            session, .input_FUN(.heatMapFeaturesTextInput), choices=feature_choices,
            server=TRUE, selected=combined)
    }, ignoreInit=TRUE)
}

#' @importFrom shiny updateSelectizeInput observeEvent
#' @importFrom shinyAce updateAceEditor
.create_heatmap_feature_observers <- function(mode, id, se, input, session, pObjects, rObjects) {
    plot_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    .get_feature_names_from_input <- function() {
        # Split the input field value into a character vector
        strsplit(input[[.input_FUN(.heatMapFeaturesTextInput)]], "\n")[[1]]
    }

    observeEvent(input[[.input_FUN(.heatMapFeaturesTextSubmit)]], {
        submitted_names <- .get_feature_names_from_input()
        matched_ids <- unique(setdiff(feature_choices[submitted_names], NA))
        updateSelectizeInput(
            session, .input_FUN(.heatMapFeatName), choices=feature_choices,
            server=TRUE, selected=matched_ids)
        new_value <- paste0(c(rownames(se)[matched_ids], ""), collapse="\n")
        updateAceEditor(
            session, .input_FUN(.heatMapFeaturesTextInput),
            value=new_value)
    })

    observeEvent(input[[.input_FUN(.heatMapFeaturesFileInput)]], {
        current_names <- .get_feature_names_from_input()
        input_filepath <- input[[.input_FUN(.heatMapFeaturesFileInput)]][["datapath"]]
        names_from_file <- tryCatch(
            scan(file=input_filepath, what="character"),
            error=function(err){paste0("Could not read file\n", conditionMessage(err))}
        )
        new_names <- c(current_names, names_from_file)
        updateAceEditor(
            session, .input_FUN(.heatMapFeaturesTextInput),
            value=paste0(c(new_names, ""), collapse="\n"))
    })
}

#' @importFrom shiny observeEvent updateSelectizeInput
.create_heatmap_button_observers <- function(mode, id, se, input, session, pObjects, rObjects) {
    plot_name <- paste0(mode, id)

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    # Triggering an update of the selected elements : clear features, trigger replotting (caught by validate)
    clear_button <- paste0(plot_name, "_", .heatMapClearFeatures)
    observeEvent(input[[clear_button]], {
        pObjects$memory[[mode]][[.heatMapFeatName]][[id]] <- integer()
        updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices=feature_choices,
            server=TRUE, selected=integer())
        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
    }, ignoreInit=TRUE)

    # Triggering an update of the selected order.
    cluster_button <- paste0(plot_name, "_", .heatMapCluster)
    observeEvent(input[[cluster_button]], {
        emat <- pObjects$coordinates[[plot_name]]
        new_order <- match(.cluster_genes(emat), names(feature_choices))
        updateSelectizeInput(
            session, paste0(plot_name, "_", .heatMapFeatName), choices=feature_choices,
            server=TRUE, selected=new_order)
    })
}

#' @export
setMethod(".defineOutputElement", "HeatMapPlot", function(x, id, height) {
    mode <- .getEncodedName(x)
    .create_plot_ui(mode, id, brush_direction="x",
        height=height,
        brush_fill=brush_fill_color[mode],
        brush_stroke=brush_stroke_color[mode]
    )
})

#' @export
setMethod(".getEncodedName", "HeatMapPlot", function(x) "heatMapPlot")

#' @export
#' @importFrom shiny renderPlot renderUI renderTable
setMethod(".createRenderedOutput", "HeatMapPlot", function(x, id, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    plot_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    # Defining the rendered plot, and saving the coordinates.
    # Also triggering an update to the accompanying legend plot.
    legend_field <- paste0(plot_name, "_", .heatMapLegend)
    output[[plot_name]] <- renderPlot({
        force(rObjects[[plot_name]])
        rObjects[[legend_field]] <- .increment_counter(isolate(rObjects[[legend_field]]))

        p.out <- .make_heatMapPlot(id, pObjects$memory, pObjects$coordinates, se, colormap)
        pObjects$commands[[plot_name]] <- p.out$cmd_list
        pObjects$coordinates[[plot_name]] <- p.out$xy # Caching the expression matrix.
        pObjects$cached_info[[plot_name]] <- p.out$legends # Caching the legend plot for downstream use.
        p.out$plot
    })

    # Defining the legend.
    output[[legend_field]] <- renderPlot({
        force(rObjects[[legend_field]])
        gg <- pObjects$cached_info[[plot_name]]
        cowplot::plot_grid(plotlist=gg, ncol=1)
    })

    # Defining link information.
    link_field <- paste0(plot_name, "_", .panelLinkInfo)
    output[[link_field]] <- renderUI({
        force(rObjects[[link_field]])
        select_in <- pObjects$memory$heatMapPlot[[id, .selectByPlot]]
        if (select_in==.noSelection) {
            return(NULL)
        }
        tagList("Receiving selection from", em(strong(select_in)), br())
    })

    # TODO: pass "input" as an argument
    output[[.input_FUN(.heatMapModalSummary)]] <- renderUI({
        current_text_value <- input[[.input_FUN(.heatMapFeaturesTextInput)]]
        current_names <- strsplit(current_text_value, "\n")[[1]]
        invalid_ids <- which(!current_names %in% names(feature_choices))
        invalid_count <- length(invalid_ids)
        if (invalid_count > 0) {
            return(tagList(
                p(
                    format(invalid_count, big.mark=","), " ",
                    ifelse(invalid_count > 1, "entries", "entry"), " ",
                    ifelse(invalid_count > 1, "do", "does"), " ",
                    "not exist in",
                    code("rownames(se)"),
                    "and will be ignored.",
                    ifelse(invalid_count > 10, "The first 10 are shown below.", ""))
            ))
        }
        return(list())
    })

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    output[[.input_FUN(.heatMapModalTable)]] <- renderTable({
        current_text_value <- input[[.input_FUN(.heatMapFeaturesTextInput)]]
        current_names <- strsplit(current_text_value, "\n")[[1]]
        invalid_ids <- which(!current_names %in% names(feature_choices))
        invalid_count <- length(invalid_ids)
        if (invalid_count > 0) {
            df <- data.frame(
                Feature=current_names[head(invalid_ids, 10)],
                Line=head(invalid_ids, 10)
            )
            return(df)
        }
        return(NULL)
    }, striped=TRUE)
})
