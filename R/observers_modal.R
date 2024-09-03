#' Create observers for a modal for custom dimnames
#'
#' Create observers to launch a modal where users can input a list of custom row or column names.
#' These observers register input changes in the app's memory and request an update to the affected panel.
#'
#' @param plot_name String containing the name of the current panel.
#' @param slot_name String specifying the slot of containing the names of the custom features.
#' This will be modified by user interactions with the modal.
#' @param button_name String containing the name of the button in the panel UI that launches the modal.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param source_type String specifying the type of the panel that is source of the selection, either \code{"row"} or \code{"column"}.
#'
#' @return
#' Observers are set up to launch the modal and monitor its UI elements.
#' A \code{NULL} is invisibly returned.
#'
#' @details
#' This should be called in \code{\link{.createObservers}} for the target panel.
#' It assumes that a button element with the suffix \code{button_name} is available in the UI (i.e., the full name is created by concatenated \code{plot_name} with \code{button_name}).
#'
#' The modal UI provides options to sort the dimnames, validate them, clear the current text and import a selection from a specified row transmitter.
#' These are all transient until \dQuote{Apply} is clicked, at which point the app's memory is modified and an update is requested.
#'
#' The custom names are stored in the \code{slot_name} as a single string with names separated by newlines.
#' Hashes are treated as comments and any content after a hash is ignored when interpreting the names.
#' Any leading and trailing whitespace is also ignored during interpretation.
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @importFrom shiny modalDialog removeModal fluidRow column h4 actionButton br tagList em strong
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs disabled
#' @rdname createCustomDimnamesModalObservers
.createCustomDimnamesModalObservers <- function(plot_name, slot_name, button_name, se,
    input, session, pObjects, rObjects, source_type)
{
    apply_field <- sprintf("INTERNAL_%s_ApplyNameChanges", source_type)
    order_field <- sprintf("INTERNAL_%s_OrderNames", source_type)
    import_field <- sprintf("INTERNAL_%s_ImportNames", source_type)
    validate_field <- sprintf("INTERNAL_%s_ValidateNames", source_type)
    clear_field <- sprintf("INTERNAL_%s_ClearNames", source_type)

    .input_FUN <- function(field) paste0(plot_name, "_", field)

    if (source_type == "row") {
        source_field <- .selectRowSource
    } else {
        source_field <- .selectColumnSource
    }

    # nocov start
    observeEvent(input[[.input_FUN(button_name)]], {
        instance <- pObjects$memory[[plot_name]]

        transmitter <- slot(instance, source_field)
        if (transmitter==.noSelection) {
            txt <- sprintf("No panel chosen for %s selection", source_type)
            FUN <- disabled
        } else {
            full_trans <- .getFullName(pObjects$memory[[transmitter]])
            if (.transmitted_selection(transmitter, pObjects$memory)) {
                txt <- tagList(sprintf("Receiving %s selection from", source_type), em(strong(full_trans)))
                FUN <- identity
            } else {
                txt <- tagList(sprintf("No %s selection in", source_type), em(strong(full_trans)))
                FUN <- disabled 
            }
        }

        modal_ui <- modalDialog(
            title=paste(sprintf("Custom %s names for", source_type), .getFullName(instance)),
            size="l", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            fluidRow(
                column(width = 8,
                       aceEditor(.input_FUN(slot_name),
                                 mode="text",
                                 theme="xcode",
                                 value=slot(instance, slot_name),
                                 debounce=100,
                                 height="500px",
                                 autoComplete = "live",
                                 autoCompleters = "static",
                                 autoCompleteList = list(rownames = rownames(se)))
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
    selector <- if (source_type == "row") "row_selected" else "col_selected"

    # nocov start
    observeEvent(input[[.input_FUN(import_field)]], {
        # Compute names for the incoming selection, if any
        plot_env <- new.env()
        select_cmds <- .processMultiSelections(pObjects$memory[[plot_name]], 
            pObjects$memory, pObjects$contents, plot_env)
        if (exists(selector, envir=plot_env, inherits=FALSE)){
            incoming_names <- unique(unlist(get(selector, envir=plot_env)))
        } else {
            incoming_names <- NULL
        }

        editor_text <- input[[.input_FUN(slot_name)]]
        if (!is.null(incoming_names)) {
            editor_names <- strsplit(gsub("\n$", "", editor_text), split="\n")[[1]]
            editor_names <- union(editor_names, incoming_names)
            editor_text <- paste0(editor_names, collapse = "\n")
        }

        updateAceEditor(session, editorId = .input_FUN(slot_name), value = editor_text)
    }, ignoreInit=TRUE)
    # nocov end

    # Button to clear the editor
    # nocov start
    observeEvent(input[[.input_FUN(clear_field)]], {
        updateAceEditor(session, editorId = .input_FUN(slot_name), value = "")
    }, ignoreInit=TRUE)
    # nocov end

    # Button to comment out invalid names
    dimnamesFUN <- if (source_type == "row") rownames else colnames 

    # nocov start
    observeEvent(input[[.input_FUN(validate_field)]], {
        editor_text <- input[[.input_FUN(slot_name)]]

        # Figuring out which ones aren't real names.
        pure.names <- .convert_text_to_names(editor_text)
        invalid_idx <- !pure.names %in% dimnamesFUN(se) & nzchar(pure.names)

        editor_lines <- strsplit(editor_text, split="\n")[[1]]
        editor_lines[invalid_idx] <- paste0("# ", editor_lines[invalid_idx])
        editor_text <- paste0(editor_lines, collapse = "\n")

        updateAceEditor(session, editorId = .input_FUN(slot_name), value = editor_text)
    }, ignoreInit=TRUE)
    # nocov end

    # Button to order names alphabetically
    # nocov start
    observeEvent(input[[.input_FUN(order_field)]], {
        editor_text <- input[[.input_FUN(slot_name)]]

        editor_lines <- strsplit(editor_text, split="\n")[[1]]
        editor_lines <- sort(editor_lines)
        editor_text <- paste0(editor_lines, collapse = "\n")

        updateAceEditor(session, editorId = .input_FUN(slot_name), value = editor_text)
    }, ignoreInit=TRUE)
    # nocov end

    # The button that actually updates the FeatNameText field.
    # nocov start
    observeEvent(input[[.input_FUN(apply_field)]], {
        pObjects$memory[[plot_name]][[slot_name]] <- input[[.input_FUN(slot_name)]]
        # ComplexHeatmapPlot cannot send selections, thus a simple update is enough
        .requestUpdate(plot_name,rObjects)
        removeModal()
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}
