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
#' @importFrom shinyjs disable enable
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

        # Twist2: toggle UI related to discrete/continuous assays
        ABLEFUN <- if (matched_input %in% .getCachedCommonInfo(se, "ComplexHeatmapPlot")$discrete.assay.names) {
            disable
        } else {
            enable
        }

        ABLEFUN(.input_FUN(.assayCenterRows))
        ABLEFUN(.input_FUN(.assayScaleRows))
        ABLEFUN(.input_FUN(.heatMapCenteredColormap))
        ABLEFUN(.input_FUN(.heatMapCustomAssayBounds))
        ABLEFUN(.input_FUN(.assayLowerBound))
        ABLEFUN(.input_FUN(.assayUpperBound))
        ABLEFUN(.input_FUN(.heatMapClusterFeatures))
        ABLEFUN(.input_FUN(.heatMapClusterDistanceFeatures))
        ABLEFUN(.input_FUN(.heatMapClusterMethodFeatures))

        .requestUpdate(plot_name, rObjects)
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    # nocov end
    
    # nocov start
    observeEvent(input[[.input_FUN(.heatMapCustomAssayBounds)]], {
        # .createUnprotectedParameterObservers with a twist
        matched_input <- as(input[[.input_FUN(.heatMapCustomAssayBounds)]],
            typeof(pObjects$memory[[plot_name]][[.heatMapCustomAssayBounds]]))
        
        if (identical(matched_input, pObjects$memory[[plot_name]][[.heatMapCustomAssayBounds]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.heatMapCustomAssayBounds]] <- matched_input
        
        # Twist: do not rerender if both custom assay bounds UI are empty
        if (is.na(input[[.input_FUN(.assayLowerBound)]]) && is.na(input[[.input_FUN(.assayUpperBound)]])) {
            return(NULL)
        }
        
        .requestUpdate(plot_name, rObjects)
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    # nocov end
    
    # nocov start
    for (field in c(.assayCenterRows, .assayScaleRows)) {
        local({
            field0 <- field
            observeEvent(input[[.input_FUN(field0)]], {
                # .createUnprotectedParameterObservers with a twist
                matched_input <- as(input[[.input_FUN(field0)]],
                    typeof(pObjects$memory[[plot_name]][[field0]]))
                if (identical(matched_input, pObjects$memory[[plot_name]][[field0]])) {
                    return(NULL)
                }
                pObjects$memory[[plot_name]][[field0]] <- matched_input

                # Twist: clear and update the limits of lower/upper bounds based on the new data
                updateNumericInput(session, .input_FUN(.assayLowerBound), value = numeric(0), min = -Inf, max = 0)
                updateNumericInput(session, .input_FUN(.assayUpperBound), value = numeric(0), min = 0, max = Inf)

                .requestUpdate(plot_name, rObjects)
            }, ignoreInit=TRUE, ignoreNULL=TRUE)
        })
    }
    # nocov end

    # nocov start
    all.bounds <- c(.assayLowerBound, .assayUpperBound)
    for (bound in all.bounds) {
        local({
            bound0 <- bound
            other <- setdiff(all.bounds, bound)

            observeEvent(input[[.input_FUN(bound0)]], {
                cur_value <- input[[.input_FUN(bound0)]]
                if (is.null(cur_value)) {
                    return(NULL)
                }
                if (is.na(cur_value)) {
                    pObjects$memory[[plot_name]][[bound0]] <- NA_real_
                    .requestUpdate(plot_name, rObjects)
                    return(NULL)
                }
                pObjects$memory[[plot_name]][[bound0]] <- cur_value

                # The upper bound cannot be lower than the lower bound.
                other_bound <- pObjects$memory[[plot_name]][[other]]
                if (!is.null(other_bound) && !is.na(other_bound) &&
                    ((bound0==.assayLowerBound && cur_value > other_bound) ||
                        (bound0==.assayUpperBound && cur_value < other_bound)))
                {
                    # set identical values; 0-length range is handled later
                    pObjects$memory[[plot_name]][[other]] <- cur_value
                    updateNumericInput(session, .input_FUN(other), value = cur_value)
                }

                # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
                .requestUpdate(plot_name,rObjects)
            }, ignoreInit=TRUE, ignoreNULL=FALSE)
        })
    }
    # nocov end

    invisible(NULL)
}

#' @importFrom shiny modalDialog removeModal fluidRow column h4 actionButton br tagList em strong
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs disabled
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
        transmitter <- instance[[.selectRowSource]]
        if (transmitter==.noSelection) {
            txt <- "No panel chosen for row selection"
            FUN <- disabled
        } else {
            transmitter <- .getFullName(pObjects$memory[[transmitter]])
            txt <- tagList("Receiving row selection from", em(strong(transmitter)))
            FUN <- identity
        }

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
                    FUN(actionButton(.input_FUN(import_field), "Import selection")), br(), br(),
                    txt, br(), br(),
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
