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
    observeEvent(input[[.voiceAddPanelInput]], {
        voice <- input[[.voiceAddPanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Add panel> %s", voice), type="message")
        }

        new_panel <- .nearestPanelType(voice, pObjects$reservoir, max.edits=Inf)

        if (is.null(new_panel)) {
            return(NULL)
        }

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
        showNotification(sprintf("<Add panel> %s", added_full_name), type="message")
        pObjects[[.voiceActivePanel]] <- added_encoded_name
        showNotification(sprintf("Active panel: %s", added_full_name), id=.voiceActivePanel, duration=NULL)
    })

    observeEvent(input[[.voiceRemovePanelInput]], {
        voice <- input[[.voiceRemovePanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Hide panel> %s", voice), type="message")
        }

        decodedPanel <- .nearestPanelType(voice, pObjects$memory, max.edits=Inf)
        if (is.null(decodedPanel)) { return(NULL) }
        encodedPanel <- .decoded2encoded(decodedPanel)
        encodedSplit <- .split_encoded(encodedPanel)

        # Remove the panel to the active table if it is currently there
        all_active <- rObjects$active_panels
        panelIndex <- which(all_active$Type==encodedSplit$Type & all_active$ID==encodedSplit$ID)
        if (length(panelIndex) == 0) {
            showNotification(sprintf("Panel %s is not currently active", decodedPanel), type="error")
            return(NULL)
        }

        rObjects$active_panels <- .hidePanel(encodedSplit$Type, encodedSplit$ID, all_active, pObjects)

        showNotification(sprintf("<Hide panel> %s", decodedPanel), type="message")
        # Clear memory of last panel accessed, as this one is now inactive
        pObjects[[.voiceActivePanel]] <- NA_character_
        removeNotification(.voiceActivePanel, session)
        showNotification("Panel memory cleared", type="message")
    })

    observeEvent(input[[.voiceControlPanelInput]], {
        voice <- input[[.voiceControlPanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Control panel> %s", voice), type="message")
        }

        decodedPanel <- .nearestPanelType(voice, pObjects$memory, max.edits=Inf)
        if (is.null(decodedPanel)) { return(NULL) }
        encodedPanel <- .decoded2encoded(decodedPanel)
        encodedSplit <- .split_encoded(encodedPanel)

        # Take control of the panel if it is currently there
        all_active <- rObjects$active_panels
        panelIndex <- which(all_active$Type==encodedSplit$Type & all_active$ID==encodedSplit$ID)
        if (length(panelIndex) == 0) {
            showNotification(sprintf("Panel %s is not currently active", decodedPanel), type="error")
            return(NULL)
        }

        # Memorize last valid panel (only if the command succeeded)
        showNotification(sprintf("<Control panel> %s", decodedPanel), type="message")
        pObjects[[.voiceActivePanel]] <- encodedPanel
        showNotification(sprintf("Active panel: %s", decodedPanel), id=.voiceActivePanel, duration=NULL)
    })

    observeEvent(input[[.voiceShowActivePanelInput]], {
        # TODO: refactor next 4 lines into function
        activePanel <- pObjects[[.voiceActivePanel]]
        if (is.na(activePanel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }
        activeSplit <- .split_encoded(activePanel)
        activeDecoded <- .decode_panel_name(activeSplit$Type, activeSplit$ID)
        showNotification(sprintf("Active panel: %s", activeDecoded), id=.voiceActivePanel, duration=NULL)
    })

    observeEvent(input[[.voiceColorUsingInput]], {
        # TODO: refactor next 4 lines into function
        activePanel <- pObjects[[.voiceActivePanel]]
        if (is.na(activePanel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceColorUsingInput]]
        if (voice != "") {
            showNotification(sprintf("<Color using> %s", voice), type="message")
        }

        activeSplit <- .split_encoded(activePanel)

        # Check if the choice matches one of the available titles
        if (activeSplit$Type %in% row_point_plot_types) {
            create_FUN <- .define_color_options_for_row_plots
        } else {
            create_FUN <- .define_color_options_for_column_plots
        }
        choices <- create_FUN(se)
        matchedChoice <- .nearestValidChoice(voice, choices, max.edits=Inf)
        if (length(matchedChoice) != 1L) {
            return(NULL)
        }

        updateSelectizeInput(session, paste(activePanel, "ColorBy", sep="_"), selected=matchedChoice)
        showNotification(sprintf("<Color using> %s", matchedChoice), type="message")
    })

    observeEvent(input[[.voiceColorByInput]], {
        # TODO: refactor next 4 lines into function
        activePanel <- pObjects[[.voiceActivePanel]]
        if (is.na(activePanel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceColorByInput]]
        if (voice != "") {
            showNotification(sprintf("<Color by> %s", voice), type="message")
        }

        activeSplit <- .split_encoded(activePanel)

        colorby_field <- paste0(activeSplit$Type, activeSplit$ID, "_", .colorByField)
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

        updateSelectizeInput(session, paste(activePanel, colorby_param, sep="_"),
            selected=matchedChoice, choices=choices, server=TRUE)
        showNotification(sprintf("<Color by> %s", matchedChoice), type="message")
    })

    observeEvent(input[[.voiceReceiveFromInput]], {
        # TODO: refactor next 4 lines into function
        activePanel <- pObjects[[.voiceActivePanel]]
        if (is.na(activePanel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        voice <- input[[.voiceReceiveFromInput]]
        if (voice != "") {
            showNotification(sprintf("<Receive from> %s", voice), type="message")
        }

        decodedPanel <- .nearestPanelType(voice, pObjects$memory, max.edits=Inf)
        if (is.null(decodedPanel)) { return(NULL) }

        updateSelectizeInput(session, paste(activePanel, .selectByPlot, sep="_"), selected=decodedPanel)

        showNotification(sprintf("<Receive from> %s", decodedPanel), type="message")
    })

    observeEvent(input[[.voiceSendToInput]], {
        # TODO: refactor next 4 lines into function
        activePanel <- pObjects[[.voiceActivePanel]]
        if (is.na(activePanel)) {
            showNotification("No active panel", type="error")
            return(NULL)
        }

        activeSplit <- .split_encoded(activePanel)
        activeDecoded <- .decode_panel_name(activeSplit$Type, activeSplit$ID)

        voice <- input[[.voiceSendToInput]]
        if (voice != "") {
            showNotification(sprintf("<Send to> %s", voice), type="message")
        }

        decodedPanel <- .nearestPanelType(voice, pObjects$memory, max.edits=Inf)
        if (is.null(decodedPanel)) { return(NULL) }
        encodedPanel <- .decoded2encoded(decodedPanel)
        encodedSplit <- .split_encoded(encodedPanel)

        updateSelectizeInput(session, paste(encodedPanel, .selectByPlot, sep="_"), selected=activeDecoded)

        showNotification(sprintf("<Send to> %s", decodedPanel), type="message")
    })

    observeEvent(input[["voiceGoodBoyInput"]], {
        showNotification(HTML("<p style='font-size:300%; text-align:right;'>&#x1F357; &#x1F436;</p>"), type="message")
    })

    invisible(NULL)
}
