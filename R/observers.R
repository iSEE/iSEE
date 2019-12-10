#nocov start

#' General observers for \code{\link{iSEE}}
#'
#' A function to set up observers for general (i.e., not panel-specific) observers used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param tour A data.frame of tour steps to use in \code{\link{introjs}}.
#' @param runLocal A logical scalar indicating whether this app is run locally or on a server.
#' @param se_name,ecm_name,cdf_name,csf_name Strings containing variable names to be passed to \code{\link{.track_it_all}}.
#' @param se_cmds String containing the command used to clean the metadata of the \linkS4class{SummarizedExperiment}.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom utils read.delim
#' @importFrom shiny observeEvent showModal modalDialog
#' HTML br renderPrint tagList showNotification
#' @importFrom rintrojs introjs
#' @importFrom shinyAce aceEditor
#'
#' @rdname INTERNAL_general_observers
.general_observers <- function(input, session, pObjects, rObjects, tour, runLocal,
    se_name, ecm_name, cdf_name, csf_name, se_cmds)
{
    observeEvent(input$tour_firststeps, {
        if(is.null(tour)) {
            tour <- read.delim(system.file("extdata", "intro_firststeps.txt", package="iSEE"),
                sep=";", stringsAsFactors=FALSE, row.names=NULL, quote="")
        }
        introjs(session, options=list(steps=tour))
    })

    if (!is.null(tour)) {
        # Only triggers _after_ panels are fully setup, so observers are properly ID'd.
        session$onFlushed(function() { introjs(session, options=list(steps=tour)) })
    }

    observeEvent(input$getcode_all, {
        showModal(modalDialog(
            title="My code", size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            p("You can click anywhere in the code editor and select all the code using",
              "a keyboard shortcut that depends on your operating system (e.g. Ctrl/Cmd + A",
              "followed by Ctrl/Cmd + C).",
              "This will copy the selected parts to the clipboard."),
            aceEditor("report_all_cmds", mode="r", theme="solarized_light", autoComplete="live",
                value=paste0(.track_it_all(rObjects$active_panels, pObjects,
                        se_name, ecm_name, cdf_name, csf_name, se_cmds), collapse="\n"),
                height="600px")
        ))
    })

    observeEvent(input$get_panel_settings, {
        showModal(modalDialog(
            title="Panel settings", size="l", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            aceEditor("acereport_r", mode="r", theme="solarized_light", autoComplete="live",
                value=paste0(.report_memory(rObjects$active_panels, pObjects$memory), collapse="\n"),
                height="600px")
        ))
    })

    observeEvent(input$session_info, {
        showModal(modalDialog(
            title="Session information", size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            tagList(renderPrint({
                sessionInfo()
            }))
        ))
    })

    observeEvent(input$iSEE_info, {
        showModal(modalDialog(
            title="About iSEE", size="m", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            tagList(
                iSEE_info, br(), br(),
                HTML("If you use this package, please use the following citation information:"),
                renderPrint({
                    citation("iSEE")
                })
            )
        ))
    })

    observeEvent(input$open_linkgraph, {
        showModal(modalDialog(
            title="Graph of inter-panel links", size="l",
            fade=TRUE, footer=NULL, easyClose=TRUE,
            renderPlot({
                .snapshot_graph_linkedpanels(rObjects$active_panels, pObjects)
            })
        ))
    })

    if (runLocal) {
        observeEvent(input$open_vignette, {
            path <- system.file("doc", "basic.html", package="iSEE")
            if (path=="") {
                showNotification("vignette has not been built on this system", type="error")
            } else {
                browseURL(path)
            }
        })
    }

    invisible(NULL)
}

#' Custom panel observers
#'
#' Observers to set up parameters for the custom panels as well as to trigger their rendering.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param customSendAll A logical scalar indicating whether all saved selections should be sent to custom panels.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate observe renderUI renderPlot
#' @rdname INTERNAL_custom_panel_observers
.custom_panel_observers <- function(input, output, session, se, pObjects, rObjects, customSendAll) {
    for (mode in custom_panel_types) {
        max_plots <- nrow(pObjects$memory[[mode]])

        for (id in seq_len(max_plots)) {
            # UI containing transmission information.
            local({
                id0 <- id
                mode0 <- mode
                panel_name <- paste0(mode0, id0)

                link_field <- paste0(panel_name, "_", .panelLinkInfo)
                output[[link_field]] <- renderUI({
                    force(rObjects[[link_field]])
                    output <- list()
                    for (src in c(.customRowSource, .customColSource)) {
                        select_in <- pObjects$memory[[mode0]][id0,src]
                        if (select_in!=.noSelection) {
                            output <- c(output, list("Receiving selection from", em(strong(select_in)), br()))
                        }
                    }
                    do.call(tagList, output)
                })
            })

            # Field defining the function to use.
            for (field in c(.customFun)) {
                local({
                    id0 <- id
                    mode0 <- mode
                    field0 <- field
                    panel_name <- paste0(mode0, id0)
                    cur_field <- paste0(panel_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[field0]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                    }, ignoreInit=TRUE)
                })
            }

            local({
                id0 <- id
                mode0 <- mode
                panel_name <- paste0(mode0, id0)
                cur_vis_field <- paste0(panel_name, "_", .customVisibleArgs)
                cur_submit <- paste0(panel_name, "_", .customSubmit)

                # Switch button class depending on whether the arguments and visible arguments are different.
                observeEvent(input[[cur_vis_field]], {
                    matched_input <- as(input[[cur_vis_field]], typeof(pObjects$memory[[mode0]][[.customVisibleArgs]]))
                    pObjects$memory[[mode0]][[.customVisibleArgs]][id0] <- matched_input

                    .disableButtonIf(
                        id=cur_submit,
                        condition=identical(matched_input, pObjects$memory[[mode0]][[.customArgs]][id0]),
                        inactiveLabel=.buttonUpToDateLabel, activeLabel=.buttonUpdateLabel, session)
                }, ignoreInit=TRUE)

                # Switch visible arguments with Arguments upon button click.
                observeEvent(input[[cur_submit]], {
                    visible <- pObjects$memory[[mode0]][[.customVisibleArgs]][[id0]]
                    if (identical(visible, pObjects$memory[[mode0]][[.customArgs]][id0])) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[.customArgs]][[id0]] <- visible
                    rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                    disable(cur_submit)
                    updateActionButton(session, cur_submit, .buttonUpToDateLabel)
                }, ignoreInit=TRUE)
            })

            # Specifying the row/column selection.
            for (src in c(.customRowSource, .customColSource)) {
                local({
                    id0 <- id
                    src0 <- src
                    mode0 <- mode
                    panel_name <- paste0(mode0, id0)
                    select_panel_field <- paste0(panel_name, "_", src0)

                    observeEvent(input[[select_panel_field]], {
                        old_transmitter <- pObjects$memory[[mode0]][id0, src0]
                        new_transmitter <- input[[select_panel_field]]

                        # Determining whether the new and old transmitting panel have selections.
                        old_encoded <- old_transmitter
                        if (old_transmitter!=.noSelection) {
                            old_encoded <- .decoded2encoded(old_transmitter)
                        }
                        new_encoded <- new_transmitter
                        if (new_transmitter!=.noSelection) {
                            new_encoded <- .decoded2encoded(new_transmitter)
                        }

                        # Trying to update the graph. No need to worry about DAGs as custom panels cannot transmit.
                        pObjects$selection_links <- .choose_new_selection_source(pObjects$selection_links,
                            panel_name, new_encoded, old_encoded)
                        pObjects$memory[[mode0]][id0, src0] <- new_transmitter

                        # Update the elements reporting the links between panels.
                        for (relinked in setdiff(c(old_encoded, new_encoded, panel_name), .noSelection)) {
                            relink_field <- paste0(relinked, "_", .panelLinkInfo)
                            rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                        }

                        # Not recreating panels if there were no selection in either the new or old transmitters.
                        # We use "Union" here to trigger replotting if customSendAll=TRUE as custom plots will
                        # receive everything from their upstream transmitters. Otherwise, if customSendAll=TRUE,
                        # we only respond to the active selection in the upstream transmitter.
                        select_type <- if (customSendAll) .selectMultiUnionTitle else .selectMultiActiveTitle
                        no_old_selection <- !.transmitted_selection(old_encoded, pObjects$memory,
                            select_type=select_type, select_saved=0L)
                        no_new_selection <- !.transmitted_selection(new_encoded, pObjects$memory,
                            select_type=select_type, select_saved=0L)
                        if (no_old_selection && no_new_selection) {
                            return(NULL)
                        }

                        # Triggering self update of the panel.
                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                    })
                })
            }
        }
    }

    # Defining the custom plots.
    for (id in seq_len(nrow(pObjects$memory$customDataPlot))) {
        local({
            id0 <- id
            plot_name <- paste0("customDataPlot", id0)

            output[[plot_name]] <- renderPlot({
                force(rObjects[[plot_name]])
                p.out <- .make_customDataPlot(id0, pObjects$memory, pObjects$coordinates, se, select_all=customSendAll)
                pObjects$commands[[plot_name]] <- p.out$cmd_list
                p.out$plot
            })
        })
    }

    # Defining the custom tables.
    for (id in seq_len(nrow(pObjects$memory$customStatTable))) {
        local({
            id0 <- id
            panel_name <- paste0("customStatTable", id0)

            output[[panel_name]] <- renderDataTable({
                force(rObjects$active_panels) # to trigger recreation when the number of plots is changed.
                force(rObjects[[panel_name]])
                param_choices <- pObjects$memory$customStatTable[id0,]
                chosen_fun <- param_choices[[.customFun]]
                if (chosen_fun==.noSelection) {
                    return(NULL)
                }

                select_out <- .process_custom_selections(param_choices, pObjects$memory, select_all=customSendAll)
                brushes <- lassos <- histories <- list()

                for (i in seq_along(select_out$transmitter)) {
                    current <- select_out$transmitter[[i]]
                    transmit_param <- pObjects$memory[[current$Type]][current$ID,]
                    temp_env <- new.env()
                    .populate_selection_environment(transmit_param, temp_env)

                    # Storing the extracted brushes and such.
                    brushes <- c(brushes, temp_env$all_brushes)
                    lassos <- c(lassos, temp_env$all_lassos)
                    histories <- c(histories, temp_env$all_select_histories)
                }

                eval_env <- new.env()
                eval_env$all_coordinates <- pObjects$coordinates
                eval_env$all_brushes <- brushes
                eval_env$all_lassos <- lassos
                eval_env$all_select_histories <- histories
                .text_eval(select_out$cmds, eval_env)

                row_selected <- eval_env$row.names
                col_selected <- eval_env$col.names

                chosen_args <- param_choices[[.customArgs]]
                FUN <- .get_internal_info(se, "custom_stat_fun")[[chosen_fun]]
                tmp_df <- do.call(FUN, c(list(se, row_selected, col_selected), as.list(.text2args(chosen_args))))

                search <- param_choices[[.customStatSearch]]
                datatable(tmp_df, filter="top", rownames=TRUE,
                    options=list(search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE), scrollX=TRUE))
            })

            # Updating memory for new selection parameters.
            search_field <- paste0(panel_name, .int_customStatSearch)
            observe({
                search <- input[[search_field]]
                if (length(search)) {
                    pObjects$memory$customStatTable[id0, .customStatSearch] <- search
                }
            })
        })
    }
    invisible(NULL)
}

#' Voice control observers
#'
#' Observers to handle voice control.
#'
#' @param input The Shiny input object from the server function.
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
#' @rdname INTERNAL_voice_control_observers
.voice_control_observers <- function(input, session, se, pObjects, rObjects) {
    observeEvent(input[[.voiceShowPanelInput]], {
        voice <- input[[.voiceShowPanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Show panel> %s", voice), type="message")
        }

        decodedPanel <- .nearestDecodedPanel(voice, pObjects$memory, max.edits=5)
        if (is.null(decodedPanel)) { return(NULL) }
        encodedPanel <- .decoded2encoded(decodedPanel)
        encodedSplit <- .split_encoded(encodedPanel)

        # Add the panel to the active table if not there yet
        all_active <- rObjects$active_panels
        if (any(all_active$Type==encodedSplit$Type & all_active$ID==encodedSplit$ID)) {
            return(NULL)
        }

        rObjects$active_panels <- .showPanel(encodedSplit$Type, encodedSplit$ID, all_active)

        # Memorize last valid panel (only if the command succeeded)
        showNotification(sprintf("<Show panel> %s", decodedPanel), type="message")
        pObjects[[.voiceActivePanel]] <- encodedPanel
        showNotification(sprintf("Active panel: %s", decodedPanel), id=.voiceActivePanel, duration=NULL)
    })

    observeEvent(input[[.voiceHidePanelInput]], {
        voice <- input[[.voiceHidePanelInput]]
        if (voice != "") {
            showNotification(sprintf("<Hide panel> %s", voice), type="message")
        }

        decodedPanel <- .nearestDecodedPanel(voice, pObjects$memory, max.edits=5)
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

        decodedPanel <- .nearestDecodedPanel(voice, pObjects$memory, max.edits=5)
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
        matchedChoice <- .nearestValidChoice(voice, choices, max.edits=5)
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
            matchedChoice <- .nearestValidChoice(voice, choices, max.edits=5)
        } else if (colorby_title == .colorByRowDataTitle) {
            colorby_param <- .colorByRowData
            matchedChoice <- .nearestValidChoice(voice, choices, max.edits=5)
        } else if (colorby_title == .colorByFeatNameTitle) {
            colorby_param <- .colorByFeatName
            matchedChoice <- .nearestValidNamedChoice(voice, choices, max.edits=5)
        } else if (colorby_title == .colorBySampNameTitle) {
            colorby_param <- .colorBySampName
            matchedChoice <- .nearestValidNamedChoice(voice, choices, max.edits=5)
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

        decodedPanel <- .nearestDecodedPanel(voice, pObjects$memory, max.edits=5)
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

        decodedPanel <- .nearestDecodedPanel(voice, pObjects$memory, max.edits=5)
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

#nocov end
