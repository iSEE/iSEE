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
#' \code{\link{.defineInterface}} will create parameter elements for choosing the elements to show on the heatmap.
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
#' .defineInterface,HeatMapPlot-method
#' .createObservers,HeatMapPlot-method
#' @name HeatMapPlot
NULL

#' @export
HeatMapPlot <- function() {
    new("HeatMapPlot")
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "HeatMapPlot", function(.Object, ...) {
    args <- list(...)

    args <- .empty_default(args, .heatMapAssay, NA_character_)
    args <- .empty_default(args, .heatMapFeatName, NA_character_)
    args <- .empty_default(args, .heatMapFeatNameBoxOpen, FALSE)

    args <- .empty_default(args, .heatMapImportSource, .noSelection)

    args <- .empty_default(args, .heatMapColData, NA_character_)
    args <- .empty_default(args, .heatMapColDataBoxOpen, FALSE)

    args <- .empty_default(args, .heatMapCenterScale, .heatMapCenterTitle)
    args <- .empty_default(args, .heatMapLower, -Inf)
    args <- .empty_default(args, .heatMapUpper, Inf)
    args <- .empty_default(args, .heatMapCenteredColors, "purple-black-yellow")

    args <- .empty_default(args, .selectEffect, .selectTransTitle)
    args <- .empty_default(args, .selectColor, "red")
    args <- .empty_default(args, .selectTransAlpha, 0.1)

    do.call(callNextMethod, c(list(.Object), args))
})

.heatMapCenterTitle <- "Centered"
.heatMapScaleTitle <- "Scaled"

setValidity2("HeatMapPlot", function(object) {
    msg <- character(0)

    msg <- .valid_logical_error(msg, object, 
        fields=c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen))

    # Checks for the numeric limits.
    msg <- .valid_number_error(msg, object, .heatMapLower, -Inf, Inf)
    msg <- .valid_number_error(msg, object, .heatMapUpper, -Inf, Inf)

    if (object[[.heatMapLower]] >= object[[.heatMapUpper]]) {
        msg <- c(msg, sprintf("'%s' should have a lower value than '%s' for '%s'",
            .heatMapLower, .heatMapUpper, class(object)[1]))
    }

    # Checks for the assorted string fields.
    msg <- .single_string_error(msg, object, .heatMapAssay)

    msg <- .valid_string_error(msg, object, 
        c(.heatMapImportSource, .heatMapCenteredColors))

    # Checks for centering/scaling.
    msg <- .multiple_choice_error(msg, object, .heatMapCenterScale,
        c(.heatMapCenterTitle, .heatMapScaleTitle))

    msg <- .allowable_choice_error(msg, object, .selectEffect,
        c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))

    msg <- .valid_number_error(msg, object, .selectTransAlpha, lower=0, upper=1)

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

    all_assays <- .get_common_info(se, "HeatMapPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no named 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replace_na_with_first(x, .heatMapFeatName, rownames(se))
    feat_choice <- x[[.heatMapFeatName]]
    x[[.heatMapFeatName]] <- feat_choice[feat_choice %in% rownames(se)]

    x <- .replace_na_with_first(x, .heatMapAssay, all_assays)

    column_covariates <- .get_common_info(se, "HeatMapPlot")$valid.colData.names
    x <- .replace_na_with_first(x, .heatMapColData, column_covariates)
    cov_choice <- x[[.heatMapColData]]
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
setMethod(".defineInterface", "HeatMapPlot", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    common_info <- .get_common_info(se, "HeatMapPlot")
    all_assays <- common_info$valid.assay.names
    column_covariates <- common_info$valid.colData.names
    heatmap_sources <- select_info$multi$row
    col_selectable <- select_info$multi$column

    select_effect <- .input_FUN(.selectEffect)

    list(
        collapseBox(
            id=.input_FUN(.heatMapFeatNameBoxOpen),
            title="Feature parameters",
            open=x[[.heatMapFeatNameBoxOpen]],
            selectInput(
                .input_FUN(.heatMapImportSource), label="Import from", choices=heatmap_sources,
                selected=.choose_link(x[[.heatMapImportSource]], heatmap_sources)),
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
                choices=all_assays, selected=x[[.heatMapAssay]]),
            hr(),
            checkboxGroupInput(
                .input_FUN(.heatMapCenterScale), label="Expression values are:",
                selected=x[[.heatMapCenterScale]],
                choices=c(.heatMapCenterTitle, .heatMapScaleTitle), inline=TRUE),
            numericInput(
                .input_FUN(.heatMapLower), label="Lower bound:",
                value=x[[.heatMapLower]]),
            numericInput(
                .input_FUN(.heatMapUpper), label="Upper bound:",
                value=x[[.heatMapUpper]]),
            .conditional_on_check_group(
                .input_FUN(.heatMapCenterScale), .heatMapCenterTitle,
                selectInput(
                    .input_FUN(.heatMapCenteredColors), label="Color scale:",
                    choices=c("purple-black-yellow", "blue-white-orange"),
                    selected=x[[.heatMapCenteredColors]]))
        ),
        collapseBox(
            id=.input_FUN(.heatMapColDataBoxOpen),
            title="Column data parameters",
            open=x[[.heatMapColDataBoxOpen]],
            selectizeInput(
                .input_FUN(.heatMapColData),
                label="Column data:",
                choices=column_covariates,
                multiple=TRUE,
                selected=x[[.heatMapColData]],
                options=list(plugins=list('remove_button', 'drag_drop'))),
            plotOutput(.input_FUN(.heatMapLegend))
        ),
        collapseBox(
            id=.input_FUN(.selectParamBoxOpen), 
            title="Selection parameters", 
            open=x[[.selectParamBoxOpen]],
            .define_selection_choices(x, by_field=.selectColSource, 
                type_field=.selectColType, saved_field=.selectColSaved,
                selectable=col_selectable, source_type="column"),
            radioButtons(
                select_effect, label="Selection effect:", inline=TRUE,
                choices=c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle),
                selected=x[[.selectEffect]]
            ),
            .conditional_on_radio(
                select_effect, .selectColorTitle,
                colourInput(.input_FUN(.selectColor), label=NULL, value=x[[.selectColor]])
            ),
            .conditional_on_radio(
                select_effect, .selectTransTitle,
                sliderInput(.input_FUN(.selectTransAlpha), label=NULL, min=0, max=1, value=x[[.selectTransAlpha]])
            )
        )
    )
})

#' @export
setMethod(".createObservers", "HeatMapPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .safe_reactive_init(rObjects, paste0(plot_name, "_", .heatMapLegend))

    .create_box_observers(plot_name, c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen, .selectParamBoxOpen), input, pObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.heatMapAssay, .heatMapLower, .heatMapUpper, .heatMapCenteredColors,
            .heatMapFeatName, .heatMapColData),
        input=input, pObjects=pObjects, rObjects=rObjects)

    # Don't ignore empty inputs.
    .createUnprotectedParameterObservers(plot_name, fields=.heatMapCenterScale,
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL=FALSE)

    .create_heatmap_import_observer(plot_name, .getFullName(x), se, input, session, pObjects, rObjects)

    .create_heatmap_feature_observers(plot_name, se, input, session, pObjects, rObjects)

    observe({
        force(rObjects$rerendered)
        updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName),
            choices=rownames(se), selected=pObjects$memory[[plot_name]][[.heatMapFeatName]], server=TRUE)
    })

    .create_heatmap_button_observers(plot_name, se, input, session, pObjects, rObjects)

    # Updating the import source, but this does NOT trigger replotting, as we need to press the button.
    cur_field <- paste0(plot_name, "_", .heatMapImportSource)
    observeEvent(input[[cur_field]], {
        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[plot_name]][[.heatMapImportSource]]))
        if (identical(input[[cur_field]], pObjects$memory[[plot_name]][[.heatMapImportSource]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.heatMapImportSource]] <- matched_input
    }, ignoreInit=TRUE)
})

setMethod(".generateOutput", "HeatMapPlot", function(x, se, all_memory, all_contents) {
    # Placeholder for the time being.
    list()
})

#' @importFrom shiny observeEvent fluidRow column actionButton modalDialog showModal
#' @importFrom shinyAce aceEditor
.create_heatmap_import_observer <- function(plot_name, full_name, se, input, session, pObjects, rObjects) {
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    import_button <- .input_FUN(.heatMapImportFeatures)
    observeEvent(input[[import_button]], {
        origin <- pObjects$memory[[plot_name]][[.heatMapImportSource]]

        if (origin == .customSelection) {
            current_choices <- pObjects$memory[[plot_name]][[.heatMapFeatName]]
            showModal(modalDialog(
                title=sprintf("Features for %s", full_name),
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

        # TODO: replace this with .multiSelectionCommands().
        incoming <- NULL
        if (enc$Type == "rowStatTable") {
            incoming <- input[[paste0(enc$Type, enc$ID, "_rows_all")]]
        } else {
            selected <- .get_brushed_points(pObjects$contents, pObjects$memory[[.brushData]])
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

        combined <- union(pObjects$memory[[plot_name]][[.heatMapFeatName]], incoming)
        updateSelectizeInput(
            session, .input_FUN(.heatMapFeaturesTextInput), choices=rownames(se),
            server=TRUE, selected=combined)
    }, ignoreInit=TRUE)
}

#' @importFrom shiny updateSelectizeInput observeEvent
#' @importFrom shinyAce updateAceEditor
.create_heatmap_feature_observers <- function(plot_name, se, input, session, pObjects, rObjects) {
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    .get_feature_names_from_input <- function(input) {
        # Split the input field value into a character vector
        strsplit(input[[.input_FUN(.heatMapFeaturesTextInput)]], "\n")[[1]]
    }

    observeEvent(input[[.input_FUN(.heatMapFeaturesTextSubmit)]], {
        submitted_names <- .get_feature_names_from_input(input)
        matched_ids <- intersect(submitted_names, rownames(se))
        updateSelectizeInput(
            session, .input_FUN(.heatMapFeatName), choices=rownames(se),
            server=TRUE, selected=matched_ids)
        new_value <- paste0(c(matched_ids, ""), collapse="\n")
        updateAceEditor(
            session, .input_FUN(.heatMapFeaturesTextInput),
            value=new_value)
    })

    observeEvent(input[[.input_FUN(.heatMapFeaturesFileInput)]], {
        current_names <- .get_feature_names_from_input(input)
        input_filepath <- input[[.input_FUN(.heatMapFeaturesFileInput)]][["datapath"]]
        names_from_file <- tryCatch(
            scan(file=input_filepath, what="character"),
            error=function(err){paste0("Could not read file\n", conditionMessage(err))} # TODO: should be a notification.
        )
        new_names <- c(current_names, names_from_file)
        updateAceEditor(
            session, .input_FUN(.heatMapFeaturesTextInput),
            value=paste0(c(new_names, ""), collapse="\n"))
    })
}

#' @importFrom shiny observeEvent updateSelectizeInput
.create_heatmap_button_observers <- function(plot_name, se, input, session, pObjects, rObjects) {
    # Triggering an update of the selected elements : clear features, trigger replotting (caught by validate)
    clear_button <- paste0(plot_name, "_", .heatMapClearFeatures)
    observeEvent(input[[clear_button]], {
        pObjects$memory[[plot_name]][[.heatMapFeatName]] <- integer()
        updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices=rownames(se),
            server=TRUE, selected=integer())
        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
    }, ignoreInit=TRUE)

    # Triggering an update of the selected order.
    cluster_button <- paste0(plot_name, "_", .heatMapCluster)
    observeEvent(input[[cluster_button]], {
        emat <- pObjects$contents[[plot_name]]
        updateSelectizeInput(
            session, paste0(plot_name, "_", .heatMapFeatName), choices=rownames(se),
            server=TRUE, selected=.cluster_genes(emat))
    })
}

#' @export
setMethod(".defineOutput", "HeatMapPlot", function(x) {
    plot_name <- .getEncodedName(x)
    plotOutput(plot_name, height=paste0(x[[.organizationHeight]], "px"))
})

#' @export
setMethod(".fullName", "HeatMapPlot", function(x) "Heatmap")

#' @export
setMethod(".panelColor", "HeatMapPlot", function(x) "#7C378A")

#' @export
#' @importFrom shiny renderPlot renderUI renderTable
setMethod(".renderOutput", "HeatMapPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    # Defining the rendered plot, and saving the coordinates.
    # Also triggering an update to the accompanying legend plot.
    legend_field <- paste0(plot_name, "_", .heatMapLegend)
    output[[plot_name]] <- renderPlot({
        force(rObjects[[plot_name]])
        rObjects[[legend_field]] <- .increment_counter(isolate(rObjects[[legend_field]]))

        p.out <- .make_heatMapPlot(pObjects$memory[[plot_name]], pObjects$memory, pObjects$contents, se, metadata(se)$colormap)
        pObjects$commands[[plot_name]] <- p.out$cmd_list
        pObjects$contents[[plot_name]] <- p.out$xy # Caching the expression matrix.
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
        select_in <- pObjects$memory[[plot_name]][[.selectColSource]]
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
