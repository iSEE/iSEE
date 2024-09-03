#' Voice control observers
#'
#' Observers to handle voice control.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate renderUI updateSelectizeInput
#' showNotification removeNotification
#' @rdname INTERNAL_create_voice_observers
.create_voice_observers <- function(input, output, session, se, pObjects, rObjects) {
    # nocov start
    observeEvent(input[[.voiceCreatePanelInput]], {
        voice <- input[[.voiceCreatePanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Create panel> %s", voice), type="message")
        }

        new_panel <- .nearestPanelByType(voice, pObjects$reservoir, max.edits=Inf)
        if (identical(new_panel, integer(0))) { return(NULL) }
        new_panel <- pObjects$reservoir[[new_panel]]

        # From hereon, a combination of "panel_order" and "update_ui" observers

        # Set panel ID
        mode <- class(new_panel)
        idx <- pObjects$counter[[mode]] + 1L
        new_panel[[.organizationId]] <- idx

        # Update memory and counter
        new_panel_list <- list(new_panel)
        names(new_panel_list) <- paste0(mode, idx)
        pObjects$memory <- append(pObjects$memory, new_panel_list)
        pObjects$counter[[mode]] <- idx

        # Create observers and render output
        .createObservers(new_panel, se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
        .renderOutput(new_panel, se, output=output, pObjects=pObjects, rObjects=rObjects)

        pObjects$selection_links <- .spawn_multi_selection_graph(pObjects$memory)
        pObjects$aesthetics_links <- .spawn_single_selection_graph(pObjects$memory)

        rObjects$rerender <- .increment_counter(rObjects$rerender)

        # Memorize the added panel identity (only if the command succeeded)
        added_full_name <- .getFullName(new_panel)
        added_encoded_name <- .getEncodedName(new_panel)
        showNotification(sprintf("<Create panel> %s", added_full_name), type="message")
        pObjects[[.voiceActivePanel]] <- added_encoded_name
        showNotification(sprintf("Active panel: %s", added_full_name), id=.voiceActivePanel, duration=NULL)
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.voiceRemovePanelInput]], {
        voice <- input[[.voiceRemovePanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Remove panel> %s", voice), type="message")
        }

        target_idx <- .nearestPanelByName(voice, pObjects$memory, max.edits=Inf)
        target_panel <- pObjects$memory[[target_idx]]
        pObjects$memory[[target_idx]] <- NULL

        # TODO: refactor-start (run when creating panels too)
        pObjects$selection_links <- .spawn_multi_selection_graph(pObjects$memory)
        pObjects$aesthetics_links <- .spawn_single_selection_graph(pObjects$memory)
        rObjects$rerender <- .increment_counter(rObjects$rerender)
        # TODO: refactor-end (run when creating panels too)

        full_name <- .getFullName(target_panel)
        encoded_name <- .getEncodedName(target_panel)

        showNotification(sprintf("<Remove panel> %s", full_name), type="message")
        # If panel was under voice control, clear memory.
        if (identical(encoded_name, pObjects[[.voiceActivePanel]])) {
            pObjects[[.voiceActivePanel]] <- NA_character_
            removeNotification(.voiceActivePanel, session)
            showNotification("Active panel cleared", type="message")
        }
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.voiceControlPanelInput]], {
        voice <- input[[.voiceControlPanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Control panel> %s", voice), type="message")
        }

        target_idx <- .nearestPanelByName(voice, pObjects$memory, max.edits=Inf)
        if (identical(target_idx, integer(0))) { return(NULL) }
        active_panel <- pObjects$memory[[target_idx]]

        full_name <- .getFullName(active_panel)
        encoded_name <- .getEncodedName(active_panel)

        # Memorize the panel
        showNotification(sprintf("<Control panel> %s", full_name), type="message")
        pObjects[[.voiceActivePanel]] <- encoded_name
        showNotification(sprintf("Active panel: %s", full_name), id=.voiceActivePanel, duration=NULL)
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.voiceShowActivePanelInput]], {
        # TODO: refactor next 4 lines into function
        active_panel <- pObjects[[.voiceActivePanel]]
        if (is.na(active_panel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        active_panel <- pObjects$memory[[active_panel]]

        full_name <- .getFullName(active_panel)
        showNotification(sprintf("Active panel: %s", full_name), id=.voiceActivePanel, duration=NULL)
    })
    # nocov end

    # nocov start
    observeEvent(input[[.voiceColorUsingInput]], {
        # TODO: refactor next 4 lines into function
        active_panel <- pObjects[[.voiceActivePanel]]
        if (is.na(active_panel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceColorUsingInput]]
        if (voice != "") {
            showNotification(sprintf("<Color using> %s", voice), type="message")
        }

        active_panel <- pObjects$memory[[active_panel]]

        # Check if the choice matches one of the available titles
        if (is(active_panel, "RowDotPlot")) {
            covariates <- .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
            create_FUN <- .define_color_options_for_row_plots
        } else if (is(active_panel, "ColumnDotPlot")) {
            covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
            create_FUN <- .define_color_options_for_column_plots
        } else {
            return(NULL)
        }
        all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names

        choices <- create_FUN(se, covariates, all_assays)
        matchedChoice <- .nearestValidChoice(voice, choices, max.edits=Inf)
        if (length(matchedChoice) != 1L) {
            return(NULL)
        }

        encoded_name <- .getEncodedName(active_panel)

        updateSelectizeInput(session, paste0(encoded_name, "_", "ColorBy"), selected=matchedChoice)
        showNotification(sprintf("<Color using> %s", matchedChoice), type="message")
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.voiceColorByInput]], {
        # TODO: refactor next 4 lines into function
        active_panel <- pObjects[[.voiceActivePanel]]
        if (is.na(active_panel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceColorByInput]]
        if (voice != "") {
            showNotification(sprintf("<Color by> %s", voice), type="message")
        }

        active_panel <- pObjects$memory[[active_panel]]
        encoded_name <- .getEncodedName(active_panel)

        colorby_field <- paste0(encoded_name, "_", .colorByField)
        colorby_title <- isolate(input[[colorby_field]])

        # Fetch the available choices
        choices <- .colorByChoices(colorby_title, se)

        # Check if the choice matches one of the available values
        matchedChoice <- character(0)
        if (colorby_title == .colorByNothingTitle) {
            return(NULL)
        } else if (colorby_title == .colorByColDataTitle) {
            colorby_param <- .colorByColData
            matchedChoice <- .nearestValidChoice(voice, choices, max.edits=Inf)
        } else if (colorby_title == .colorByRowDataTitle) {
            colorby_param <- .colorByRowData
            matchedChoice <- .nearestValidChoice(voice, choices, max.edits=Inf)
        } else if (colorby_title == .colorByFeatNameTitle) {
            colorby_param <- .colorByFeatName
            matchedChoice <- .nearestValidNamedChoice(voice, choices, max.edits=Inf)
        } else if (colorby_title == .colorBySampNameTitle) {
            colorby_param <- .colorBySampName
            matchedChoice <- .nearestValidNamedChoice(voice, choices, max.edits=Inf)
        }

        if (length(matchedChoice) != 1L) {
            return(NULL)
        }

        updateSelectizeInput(session, paste0(encoded_name, "_", colorby_param),
            selected=matchedChoice, choices=choices, server=TRUE)
        showNotification(sprintf("<Color by> %s", matchedChoice), type="message")
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.voiceReceiveFromInput]], {
        # TODO: refactor next 4 lines into function
        active_panel <- pObjects[[.voiceActivePanel]]
        if (is.na(active_panel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceReceiveFromInput]]
        if (voice != "") {
            showNotification(sprintf("<Receive from> %s", voice), type="message")
        }

        active_panel <- pObjects$memory[[active_panel]]
        active_encoded <- .getEncodedName(active_panel)

        target_idx <- .nearestPanelByName(voice, pObjects$memory, max.edits=Inf)
        if (identical(target_idx, integer(0))) { return(NULL) }

        target_panel <- pObjects$memory[[target_idx]]
        target_encoded_name <- .getEncodedName(target_panel)
        target_full_name <- .getFullName(target_panel)
        names(target_encoded_name) <- target_full_name

        if (is(target_panel, "ColumnDotPlot")) {
            select_field <- .selectColumnSource
        } else if (is(target_panel, "RowDotPlot")) {
            select_field <- .selectRowSource
        } else {
            return(NULL)
        }

        updateSelectizeInput(session, paste0(active_encoded, "_", select_field), selected=target_encoded_name)

        showNotification(sprintf("<Receive from> %s", target_full_name), type="message")
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.voiceSendToInput]], {
        # TODO: refactor next 4 lines into function
        active_panel <- pObjects[[.voiceActivePanel]]
        if (is.na(active_panel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceSendToInput]]
        if (voice != "") {
            showNotification(sprintf("<Send to> %s", voice), type="message")
        }

        active_panel <- pObjects$memory[[active_panel]]
        active_encoded <- .getEncodedName(active_panel)
        active_full_name <- .getFullName(active_panel)
        names(active_encoded) <- active_full_name

        target_idx <- .nearestPanelByName(voice, pObjects$memory, max.edits=Inf)
        if (identical(target_idx, 0L)) { return(NULL) }

        target_panel <- pObjects$memory[[target_idx]]
        target_encoded_name <- .getEncodedName(target_panel)
        target_full_name <- .getFullName(target_panel)

        if (is(active_panel, "ColumnDotPlot")) {
            select_field <- .selectColumnSource
        } else if (is(active_panel, "RowDotPlot")) {
            select_field <- .selectRowSource
        } else {
            return(NULL)
        }

        updateSelectizeInput(session, paste0(target_encoded_name, "_", select_field), selected=active_encoded)

        showNotification(sprintf("<Send to> %s", target_full_name), type="message")
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[["voiceGoodBoyInput"]], {
        showNotification(HTML("<p style='font-size:300%; text-align:right;'>&#x1F357; &#x1F436;</p>"), type="message")
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}
