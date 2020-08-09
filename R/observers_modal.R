#' Define observers for a modal for custom dimnames
#' 
#' Define a series of observers that launch a modal where users can input a list of row names or column names.
#' These observers register input changes in the app's memory, and request an update to the output of the affected panel.
#'
#' @param plot_name String containing the name of the panel.
#' @param button_name Name of the button that launches the modal.
#' @param se A \linkS4class{SummarizedExperiment} object after running \code{\link{.cacheCommonInfo}}.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param source_type String specifying the type of the panel that is source of the selection, either \code{"row"} or \code{"column"}.
#'
#' @return
#' Observers are set up to monitor the UI elements that can change various parameters specific to the \code{ComplexHeatmapPlot} panel.
#' A \code{NULL} is invisibly returned.
#'
#' @author Kevin Rue-Albrecht
#' 
#' @importFrom shiny modalDialog removeModal fluidRow column h4 actionButton br tagList em strong
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs disabled
#' @rdname INTERNAL_create_modal_observers_for_dimnames
.create_modal_observers_for_dimnames <- function(plot_name, slot_name, button_name, se, input, session, pObjects, rObjects, source_type) {
  apply_field <- "INTERNAL_ApplyFeatNameChanges"
  order_field <- "INTERNAL_OrderFeatNames"
  import_field <- "INTERNAL_ImportFeatNames"
  validate_field <- "INTERNAL_ValidateFeatNames"
  clear_field <- "INTERNAL_ClearFeatNames"
  
  .input_FUN <- function(field) paste0(plot_name, "_", field)
  
  # nocov start
  observeEvent(input[[.input_FUN(button_name)]], {
    instance <- pObjects$memory[[plot_name]]
    
    transmitter <- if (source_type == "row") instance[[.selectRowSource]] else instance[[.selectColSource]]
    if (transmitter==.noSelection) {
      txt <- sprintf("No panel chosen for %s selection", source_type)
      FUN <- disabled
    } else {
      transmitter <- .getFullName(pObjects$memory[[transmitter]])
      txt <- tagList(sprintf("Receiving %s selection from", source_type), em(strong(transmitter)))
      FUN <- identity
    }
    
    dim_label <- if (source_type == "row") "feature" else "sample" # TODO: generic mapping "row" and "column" to labels depending on the panel class
    modal_ui <- modalDialog(
      title=paste(sprintf("Custom %s names for", dim_label), .getFullName(instance)),
      size="l", fade=TRUE,
      footer=NULL, easyClose=TRUE,
      fluidRow(
        column(width = 8,
               aceEditor(.input_FUN(slot_name),
                         mode="text",
                         theme="xcode",
                         autoComplete="disabled",
                         value=instance[[slot_name]],
                         debounce=100,
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
  # nocov start
  observeEvent(input[[.input_FUN(validate_field)]], {
    instance <- pObjects$memory[[plot_name]]
    
    dimnamesFUN <- if (source_type == "row") rownames else colnames # TODO: generic mapping "row" and "column" to labels depending on the panel class
    
    editor_text <- input[[.input_FUN(slot_name)]]
    editor_lines <- strsplit(editor_text, split="\n")[[1]]
    invalid_idx <- !editor_lines %in% dimnamesFUN(se) & !grepl("^[ ]*#", editor_lines) # TODO switch dimnames function
    editor_lines[invalid_idx] <- paste0("# ", editor_lines[invalid_idx])
    editor_text <- paste0(editor_lines, collapse = "\n")
    updateAceEditor(session, editorId = .input_FUN(slot_name), value = editor_text)
  }, ignoreInit=TRUE)
  # nocov end
  
  # Button to order names alphabetically
  # nocov start
  observeEvent(input[[.input_FUN(order_field)]], {
    instance <- pObjects$memory[[plot_name]]
    
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
