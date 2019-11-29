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

#' Panel organization observers for \code{\link{iSEE}}
#'
#' A function to set up observers for the panel organization observers used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_organization_observers
#' @importFrom shiny renderUI reactiveValues observeEvent
#' showModal modalDialog isolate
.organization_observers <- function(input, output, se, pObjects, rObjects) {
    # This is adapted from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
    output$allPanels <- renderUI({
        rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
        .panel_generation(rObjects$active_panels, pObjects$memory, se)
    })

    n_available <- vapply(pObjects$memory, nrow, FUN.VALUE=0L)
    enc_ids <- unlist(lapply(n_available, FUN=seq_len))
    enc_names <- rep(names(pObjects$memory), n_available)
    available_panels <- paste0(enc_names, enc_ids)
    names(available_panels) <- .decode_panel_name(enc_names, enc_ids)

    # Persistent objects to give the modal a 'working memory'.
    # These get captured in the current environment to persist
    # when observeEvent's expression actually gets executed.
    org_pObjects <- reactiveValues(active_panels=isolate(rObjects$active_panels))
    org_rObjects <- reactiveValues(rerender=0)

    # Panel ordering, addition and deletion.
    observeEvent(input$organize_panels, {
        active_panels <- paste0(rObjects$active_panels$Type, rObjects$active_panels$ID)
        names(active_panels) <- .decode_panel_name(rObjects$active_panels$Type, rObjects$active_panels$ID)
        inactive_panels <- available_panels[which(!available_panels %in% active_panels)]
        ordered_panel_choices <- c(active_panels, inactive_panels)
        org_pObjects$active_panels <- rObjects$active_panels

        showModal(modalDialog(
            title="Panel organization", size="m", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            actionButton("update_ui", "Apply settings", icon=icon("object-ungroup"), width='100%'),
            hr(),
            selectizeInput("panel_order", label=NULL, choices=ordered_panel_choices, multiple=TRUE,
                selected=active_panels,
                options=list(plugins=list('remove_button', 'drag_drop')), width="500px"),
            uiOutput("panelParams")
        ))
    })

    output$panelParams <- renderUI({
        force(org_rObjects$rerender)
        .panel_organization(org_pObjects$active_panels)
    })

    for (mode in all_panel_types) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            # Note: we need "local" so that each item gets its own number. Without it, the value
            # of 'id' in the renderPlot() will be the same across all instances, because
            # of when the expression is evaluated.
            local({
                mode0 <- mode
                id0 <- id
                prefix <- paste0(mode0, id0, "_")
                max_plots0 <- max_plots

                width_name <- paste0(prefix, .organizationWidth)
                observeEvent(input[[width_name]], {
                    all_active <- org_pObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    cur.width <- all_active$Width[index]
                    new.width <- as.integer(input[[width_name]])
                    if (!isTRUE(all.equal(new.width, cur.width))) {
                        org_pObjects$active_panels$Width[index] <- new.width
                    }
                })

                height_name <- paste0(prefix, .organizationHeight)
                observeEvent(input[[height_name]], {
                    all_active <- org_pObjects$active_panels
                    index <- which(all_active$Type==mode0 & all_active$ID==id0)
                    cur.height <- all_active$Height[index]
                    new.height <- input[[height_name]]
                    if (!isTRUE(all.equal(new.height, cur.height))) {
                        org_pObjects$active_panels$Height[index] <- new.height
                    }
                })
            })
        }
    }

    ### Reorder/add/remove panels ###
    observeEvent(input$panel_order, {
        cur_active <- paste0(org_pObjects$active_panels$Type, org_pObjects$active_panels$ID)
        if (identical(input$panel_order, cur_active)) {
            return(NULL)
        }

        m <- match(input$panel_order, cur_active)
        new_active_panels <- org_pObjects$active_panels[m,,drop=FALSE]

        to_add <- is.na(m)
        if (any(to_add)) {
            enc_add <- .split_encoded(input$panel_order[to_add])
            new_active_panels[to_add,] <- data.frame(Type=enc_add$Type, ID=enc_add$ID,
                Width=4, Height=500L, stringsAsFactors=FALSE)
        }

        org_pObjects$active_panels <- new_active_panels
        .increment_counter(org_rObjects$rerender)
    })

    observeEvent(input$update_ui, {
        rObjects$active_panels <- org_pObjects$active_panels
    })

    invisible(NULL)
}

#' Box opening observers for \code{\link{iSEE}}
#'
#' A function to set up observers for box opening observers used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_box_observers
#' @importFrom shiny observeEvent
.box_observers <- function(input, pObjects) {
    for (mode in all_panel_types) {
        if (mode %in% point_plot_types) {
            box_types <- c(.dataParamBoxOpen, .visualParamBoxOpen, .selectParamBoxOpen)
        } else if (mode=="heatMapPlot") {
            box_types <- c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen, .selectParamBoxOpen)
        } else if (mode %in% custom_panel_types) {
            box_types <- c(.dataParamBoxOpen, .selectParamBoxOpen)
        } else if (mode %in% linked_table_types) {
            box_types <- .selectParamBoxOpen
        } else {
            box_types <- character(0)
        }

        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            for (boxtype in box_types) {
                local({
                    mode0 <- mode
                    id0 <- id
                    box0 <- boxtype

                    open_field <- paste0(mode0, id0, "_", box0)
                    observeEvent(input[[open_field]], {
                        pObjects$memory[[mode0]][[box0]][id0] <- input[[open_field]]
                    })
                })
            }
        }
    }

    invisible(NULL)
}

#' Selection parameter observers
#'
#' A function to set up observers for selection parameter observers used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate
#' showNotification updateSelectInput updateRadioButtons
#' @importFrom igraph is_dag simplify
#' @rdname INTERNAL_selection_parameter_observers
.selection_parameter_observers <- function(input, session, pObjects, rObjects) {
    # Selection choice observer; applicable to all non-custom panels.
    for (mode in c(point_plot_types, linked_table_types, "heatMapPlot")) {
        max_panels <- nrow(pObjects$memory[[mode]])

        for (id in seq_len(max_panels)) {
            local({
                mode0 <- mode
                id0 <- id
                panel_name <- paste0(mode0, id0)
                select_panel_field <- paste0(panel_name, "_", .selectByPlot)
                repop_field <- paste0(panel_name, "_repopulated")
                can_transmit <- mode %in% point_plot_types

                observeEvent(input[[select_panel_field]], {
                    old_transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    new_transmitter <- input[[select_panel_field]]
                    if (old_transmitter==new_transmitter) {
                        return(NULL)
                    }

                    old_encoded <- old_transmitter
                    if (old_transmitter!=.noSelection) {
                        old_encoded <- .decoded2encoded(old_transmitter)
                    }
                    new_encoded <- new_transmitter
                    if (new_transmitter!=.noSelection) {
                        new_encoded <- .decoded2encoded(new_transmitter)
                    }
                    tmp <- .choose_new_selection_source(pObjects$selection_links, panel_name, new_encoded, old_encoded)

                    # Trying to update the graph, but breaking if it's not a DAG.
                    # We also break if users try to self-select in restrict mode.
                    # These concerns are only relevant for transmitting panels (i.e., point plots).
                    if (can_transmit) {
                        daggy <- is_dag(simplify(tmp, remove.loops=TRUE))
                        self_restrict <- new_encoded==panel_name &&
                            new_encoded!=.noSelection &&
                            pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle

                        if (!daggy || self_restrict) {
                            if (!daggy) {
                                showNotification("point selection relationships cannot be cyclic", type="error")
                            } else if (self_restrict){
                                showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                            }
                            updateSelectInput(session, select_panel_field, selected=old_transmitter)
                            return(NULL)
                        }
                    }

                    pObjects$selection_links <- tmp
                    pObjects$memory[[mode0]][id0, .selectByPlot] <- new_transmitter

                    # Update the elements reporting the links between panels.
                    for (relinked in setdiff(c(old_encoded, new_encoded, panel_name), .noSelection)) {
                        relink_field <- paste0(relinked, "_", .panelLinkInfo)
                        rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                    }

                    # Update the multi-selection selectize.
                    saved_select_name <- paste0(panel_name, "_", .selectMultiSaved)
                    rObjects[[saved_select_name]] <- .increment_counter(isolate(rObjects[[saved_select_name]]))

                    saved_val <- pObjects$memory[[mode0]][id0, .selectMultiSaved]
                    if (saved_val!=0L && new_encoded!=.noSelection) {
                        new_enc <- .split_encoded(new_encoded)
                        if (saved_val > length(pObjects$memory[[new_enc$Type]][,.multiSelectHistory][[new_enc$ID]])) {
                            pObjects$memory[[mode0]][id0, .selectMultiSaved] <- 0L
                        }
                    }

                    # Checking if there were active/saved selections in either the new or old transmitters.
                    no_old_selection <- !.transmitted_selection(old_encoded, pObjects$memory, mode=mode0, id=id0)
                    no_new_selection <- !.transmitted_selection(new_encoded, pObjects$memory, mode=mode0, id=id0)
                    if (no_old_selection && no_new_selection) {
                        return(NULL)
                    }

                    rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))

                    # Updating children, if the current panel is set to restrict
                    # (and thus the point population changes with a new transmitted selection).
                    if (can_transmit && pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle) {
                        rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                    }
                }, ignoreInit=TRUE)
            })
        }
    }

    # Selection effect observer.
    for (mode in c(point_plot_types, "heatMapPlot")) {
        max_plots <- nrow(pObjects$memory[[mode]])

        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                panel_name <- paste0(mode0, id0)
                select_effect_field <- paste0(panel_name, "_", .selectEffect)
                repop_field <- paste0(panel_name, "_repopulated")
                can_transmit <- mode %in% point_plot_types

                observeEvent(input[[select_effect_field]], {
                    cur_effect <- input[[select_effect_field]]
                    old_effect <- pObjects$memory[[mode0]][id0, .selectEffect]

                    # Storing the new choice into memory, unless self-selecting to restrict.
                    # In which case, we trigger an error and reset to the previous choice.
                    if (can_transmit) {
                        if (cur_effect == .selectRestrictTitle
                                && pObjects$memory[[mode0]][id0, .selectByPlot]==.decode_panel_name(mode0, id0)) {
                            showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                            updateRadioButtons(session, select_effect_field, selected=old_effect)
                            return(NULL)
                        }
                    }
                    pObjects$memory[[mode0]][id0, .selectEffect] <- cur_effect

                    # Avoiding replotting if there was no transmitting selection.
                    transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    if (!.transmitted_selection(transmitter, pObjects$memory, mode=mode0, id=id0, encoded=FALSE)) {
                        return(NULL)
                    }

                    rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))

                    # Updating children if the selection in the current plot changes due to gain/loss of Restrict.
                    if (cur_effect==.selectRestrictTitle || old_effect==.selectRestrictTitle) {
                        rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                    }
                }, ignoreInit=TRUE)
            })
        }
    }

    invisible(NULL)
}

#' Brush observers
#'
#' A function to set up observers for brushing on point-based plots, as used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @details
#' There are three "phases" of a Shiny brush:
#' \itemize{
#' \item the Javascript (JS) brush, which is what the user draws and the observer responds to.
#'   This is eliminated upon replotting for various consistency reasons.
#' \item the active brush, which is what is stored in the \code{.brushData} field of the memory.
#' \item the saved brush(es), stored in the \code{.multiSelectHistory} field of the memory.
#' }
#' This particular observer only deals with the first and second elements, updating them as necessary.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @rdname INTERNAL_brush_observers
#' @author Aaron Lun
#' @importFrom shiny observeEvent
.brush_observers <- function(input, session, pObjects, rObjects) {
    for (mode in point_plot_types) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                act_field <- paste0(mode0, id0, "_reactivated")
                save_field <- paste0(plot_name, "_", .multiSelectSave)

                brush_id <- paste0(plot_name, "_", .brushField)
                observeEvent(input[[brush_id]], {
                    cur_brush <- input[[brush_id]]
                    old_brush <- pObjects$memory[[mode0]][,.brushData][[id0]]
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, cur_brush)

                    # If the Shiny brushes have the same coordinates, we don't bother replotting.
                    replot <- !.identical_brushes(cur_brush, old_brush)

                    # Destroying lasso points upon Shiny brush (replotting if existing lasso was not NULL).
                    replot <- replot || !is.null(pObjects$memory[[mode0]][,.lassoData][[id0]])
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, NULL)
                    if (!replot) {
                        return(NULL)
                    }

                    .disableButtonIf(
                        save_field,
                        !.any_active_selection(mode0, id0, pObjects$memory),
                        .buttonNoSelectionLabel, .buttonSaveLabel, session
                    )

                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
                }, ignoreInit=TRUE)
            })
        }
    }
    invisible(NULL)
}

#' Child propagating observer
#'
#' These observers decide whether child panels needs to be regenerated based on changes in the transmitting (parent) panels.
#'
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param customSendAll A logical scalar indicating whether all saved selections should be sent to custom panels.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @details
#' We expect \code{rObjects} to contain \code{X_repopulated}, \code{X_reactivated} and \code{X_resaved} for each panel \code{X}.
#' These are simply integer counters that get triggered every time \code{X} changes.
#'
#' \code{X_repopulated} is bumped when the population of points changes in \code{X}.
#' This refers to changes in the points retained by restriction of selections from transmitters upstream of \code{X}.
#' Bumping will trigger replotting of the children of \code{X}, based on whether they are receiving the active or saved selection.
#' It will also bump the children's \code{X_repopulated} if they are selecting by restriction.
#'
#' \code{X_reactivated} is bumped when the current selection of points in \code{X} changes.
#' \code{X_resaved} is bumped when the saved selection of points in \code{X} changes.
#' These use separate observers from \code{X_repopulated} because they only trigger replotting if
#' the children are receiving active or saved selections, respectively.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny isolate observe
#' @rdname INTERNAL_child_propagation_observers
.child_propagation_observers <- function(pObjects, rObjects, customSendAll) {
    for (mode in point_plot_types) {
        max_panels <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_panels)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                repop_field <- paste0(plot_name, "_repopulated")

                ## Observer for changes in the point population in the current panel. ---
                observe({
                    force(rObjects[[repop_field]])
                    has_active <- .any_active_selection(mode0, id0, pObjects$memory)
                    has_saved <- .any_saved_selection(mode0, id0, pObjects$memory)

                    children <- .get_direct_children(pObjects$selection_links, plot_name)
                    for (child_plot in children) {
                        child_enc <- .split_encoded(child_plot)
                        child_mode <- child_enc$Type
                        child_id <- child_enc$ID

                        # We have special rules for custom panel types,
                        # as these don't have .selectMultiType or .selectMultiSaved.
                        if (child_mode %in% custom_panel_types) {
                            if (has_active || (customSendAll && has_saved)) {
                                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                            }
                            next
                        }

                        # To warrant replotting of the child, there needs to be
                        # Active or Saved selections in the current panel.
                        # Any point population changes in the current panel will
                        # result in new subsets if those selections are available.
                        replot <- FALSE
                        select_mode <- pObjects$memory[[child_mode]][child_id, .selectMultiType]
                        if (select_mode==.selectMultiActiveTitle) {
                            if (has_active) replot <- TRUE
                        } else if (select_mode==.selectMultiSavedTitle &&
                                pObjects$memory[[child_mode]][child_id, .selectMultiSaved]!=0L) {
                            if (has_saved) replot <- TRUE
                        } else if (select_mode==.selectMultiUnionTitle) {
                            if (has_saved || has_active) replot <- TRUE
                        }

                        if (replot) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))

                            # To warrant replotting of the grandchildren, the child must itself be restricted.
                            if (child_mode %in% point_plot_types &&
                                pObjects$memory[[child_mode]][child_id, .selectEffect]==.selectRestrictTitle)
                            {
                                react_child <- paste0(child_mode, child_id, "_repopulated")
                                rObjects[[react_child]] <- .increment_counter(isolate(rObjects[[react_child]]))
                            }
                        }
                    }
                })

                ## Observer for changes in the active selection in the current panel. ---
                act_field <- paste0(plot_name, "_reactivated")
                observe({
                    force(rObjects[[act_field]])

                    children <- .get_direct_children(pObjects$selection_links, plot_name)
                    for (child_plot in children) {
                        child_enc <- .split_encoded(child_plot)
                        child_mode <- child_enc$Type
                        child_id <- child_enc$ID

                        # Custom panels don't have any other settings, so we just replot and move on.
                        if (child_mode %in% custom_panel_types) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                            next
                        }

                        select_mode <- pObjects$memory[[child_mode]][child_id, .selectMultiType]
                        if (select_mode==.selectMultiActiveTitle || select_mode==.selectMultiUnionTitle) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))

                            # To warrant replotting of the grandchildren, the child must itself be restricted.
                            if (child_mode %in% point_plot_types &&
                                pObjects$memory[[child_mode]][child_id, .selectEffect]==.selectRestrictTitle)
                            {
                                react_child <- paste0(child_mode, child_id, "_repopulated")
                                rObjects[[react_child]] <- .increment_counter(isolate(rObjects[[react_child]]))
                            }
                        }
                    }
                })

                ## Observer for changes in the saved selections in the current panel. ---
                save_field <- paste0(plot_name, "_resaved")
                observe({
                    force(rObjects[[save_field]])
                    Nsaved <- length(pObjects$memory[[mode0]][,.multiSelectHistory][[id0]])

                    children <- .get_direct_children(pObjects$selection_links, plot_name)
                    for (child_plot in children) {
                        child_enc <- .split_encoded(child_plot)
                        child_mode <- child_enc$Type
                        child_id <- child_enc$ID

                        # Custom panels don't have any other settings, so we just replot and move on.
                        if (child_mode %in% custom_panel_types) {
                            if (customSendAll) {
                                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                            }
                            next
                        }

                        reset <- pObjects$memory[[child_mode]][child_id, .selectMultiSaved] > Nsaved
                        if (reset) {
                            pObjects$memory[[child_mode]][child_id, .selectMultiSaved] <- 0L
                        }

                        child_select_type <- pObjects$memory[[child_mode]][child_id, .selectMultiType]
                        if (child_select_type==.selectMultiUnionTitle || (child_select_type==.selectMultiSavedTitle && reset)) {
                            rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))

                            # To warrant replotting of the grandchildren, the child must itself be restricted.
                            if (child_mode %in% point_plot_types &&
                                pObjects$memory[[child_mode]][child_id, .selectEffect]==.selectRestrictTitle)
                            {
                                react_child <- paste0(child_plot, "_repopulated")
                                rObjects[[react_child]] <- .increment_counter(isolate(rObjects[[react_child]]))
                            }
                        }

                        # Updating the selectize as well.
                        child_saved <- paste0(child_plot, "_", .selectMultiSaved)
                        rObjects[[child_saved]] <- .increment_counter(isolate(rObjects[[child_saved]]))
                    }
                })
            })
        }
    }
    invisible(NULL)
}

#' Multi-select parameter observers
#'
#' Observers for the multiple (i.e., saved) selection parameter choices.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent observe updateSelectInput isolate
#' @rdname INTERNAL_multiselect_param_observers
.multiselect_param_observers <- function(input, session, pObjects, rObjects) {
    for (mode in c(point_plot_types, linked_table_types, "heatMapPlot")) {
        max_panels <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_panels)) {
            local({
                mode0 <- mode
                id0 <- id
                panel_name <- paste0(mode0, id0)
                repop_field <- paste0(mode0, id0, "_repopulated")

                ## Type field observers. ---
                type_field <- paste0(mode0, id0, "_", .selectMultiType)
                observeEvent(input[[type_field]], {
                    old_type <- pObjects$memory[[mode0]][[.selectMultiType]][id0]
                    new_type <- as(input[[type_field]], typeof(old_type))
                    if (identical(new_type, old_type)) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[.selectMultiType]][id0] <- new_type

                    # Skipping if neither the old or new types were relevant.
                    transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    no_old_selection <- !.transmitted_selection(transmitter, pObjects$memory,
                        select_type=old_type, mode=mode0, id=id0, encoded=FALSE)
                    no_new_selection <- !.transmitted_selection(transmitter, pObjects$memory, mode=mode0, id=id0, encoded=FALSE)
                    if (no_old_selection && no_new_selection) {
                        return(NULL)
                    }

                    rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                    if (pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle) {
                        rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                    }
                }, ignoreInit=TRUE)

                ## Saved field observers. ---
                saved_select <- paste0(panel_name, "_", .selectMultiSaved)
                observeEvent(input[[saved_select]], {
                    req(input[[saved_select]]) # Required to defend against empty strings before updateSelectizeInput runs.
                    matched_input <- as(input[[saved_select]], typeof(pObjects$memory[[mode0]][[.selectMultiSaved]]))
                    if (identical(matched_input, pObjects$memory[[mode0]][[.selectMultiSaved]][id0])) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[.selectMultiSaved]][id0] <- matched_input

                    transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    if (transmitter==.noSelection) {
                        return(NULL)
                    }

                    # Switch of 'Saved' will ALWAYS change the current plot, so no need for other checks.
                    rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                    if (pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle) {
                        rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                    }
                }, ignoreInit=TRUE)

                ## Selectize observer. ---
                # Do NOT be tempted to centralize code by setting .selectMultiSaved to zero here.
                # This needs to be done in upstream observers, otherwise there is no guarantee
                # that the zero is set before plotting observers that use it.
                observe({
                    force(rObjects[[saved_select]])
                    force(rObjects$rerendered)

                    transmitter <- pObjects$memory[[mode0]][id0,.selectByPlot]
                    if (transmitter==.noSelection) {
                        available_choices <- integer(0)
                    } else {
                        trans <- .encode_panel_name(transmitter)
                        N <- length(pObjects$memory[[trans$Type]][,.multiSelectHistory][[trans$ID]])
                        available_choices <- seq_len(N)
                        names(available_choices) <- available_choices
                    }

                    no_choice <- 0L
                    names(no_choice) <- .noSelection
                    available_choices <- c(no_choice, available_choices)
                    updateSelectizeInput(session, saved_select, choices=available_choices, server=TRUE,
                        selected=pObjects$memory[[mode0]][id0, .selectMultiSaved])
                })
            })
        }
    }
    invisible(NULL)
}

#' Lasso selection observers
#'
#' Observers for the lasso selection.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate
#' @rdname INTERNAL_lasso_observers
.lasso_observers <- function(input, session, pObjects, rObjects) {
    for (mode in point_plot_types) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                click_field <- paste0(plot_name, "_", .lassoClick)
                brush_field <- paste0(plot_name, "_", .brushField)
                act_field <- paste0(plot_name, "_reactivated")
                save_field <- paste0(plot_name, "_", .multiSelectSave)

                observeEvent(input[[click_field]], {
                    # Hack to resolve https://github.com/rstudio/shiny/issues/947.
                    # By luck, the triggering of the click field seems to be delayed enough
                    # that input data is sent to the brush field first. Thus, we can
                    # check the brush field for a non-NULL value avoid action if
                    # the user had brushed rather than clicked. A separate click should
                    # continue past this point, as any Shiny brush would be wiped upon
                    # replotting and thus would not have any value in the input.
                    if (!is.null(input[[brush_field]])) {
                        return(NULL)
                    }

                    # Don't add to waypoints if a Shiny brush exists in memory, but instead, destroy the brush.
                    # Also destroy any closed lassos, or update open lassos.
                    reactivated <- FALSE
                    if (!is.null(pObjects$memory[[mode0]][,.brushData][[id0]])) {
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, NULL)
                        reactivated <- TRUE
                    } else {
                        prev_lasso <- pObjects$memory[[mode0]][,.lassoData][[id0]]
                        was_closed <- if(is.null(prev_lasso)) FALSE else prev_lasso$closed

                        if (was_closed) {
                            new_lasso <- NULL
                            reactivated <- TRUE
                        } else {
                            new_lasso <- .update_lasso(input[[click_field]], prev_lasso)
                            if (new_lasso$closed) {
                                reactivated <- TRUE
                            }
                        }

                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, new_lasso)
                    }

                    .disableButtonIf(
                        save_field,
                        !.any_active_selection(mode0, id0, pObjects$memory),
                        .buttonNoSelectionLabel, .buttonSaveLabel, session
                    )

                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                    if (reactivated) {
                        rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
                    }
                })
            })
        }
    }
    invisible(NULL)
}

#' Zoom observers
#'
#' Observers for the zoom functionality.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate
#' @rdname INTERNAL_zoom_observers
.zoom_observers <- function(input, session, pObjects, rObjects) {
    for (mode in point_plot_types) {
        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                dblclick_field <- paste0(plot_name, "_", .zoomClick)
                act_field <- paste0(plot_name, "_reactivated")
                save_field <- paste0(plot_name, "_", .multiSelectSave)

                observeEvent(input[[dblclick_field]], {
                    existing_brush <- pObjects$memory[[mode0]][,.brushData][[id0]]
                    existing_lasso <- pObjects$memory[[mode0]][,.lassoData][[id0]]

                    # Zooming destroys all active brushes or lassos.
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, NULL)
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, NULL)

                    new_coords <- NULL
                    if (!is.null(existing_brush)) {
                        dblclick_vals <- input[[dblclick_field]]
                        if (dblclick_vals$x >= existing_brush$xmin
                                && dblclick_vals$x <= existing_brush$xmax
                                && dblclick_vals$y >= existing_brush$ymin
                                && dblclick_vals$y <= existing_brush$ymax) {

                            # Panels are either NULL or not.
                            if (identical(dblclick_vals$panelvar1, existing_brush$panelvar1)
                                    && identical(dblclick_vals$panelvar2, existing_brush$panelvar2)) {
                                new_coords <- c(xmin=existing_brush$xmin, xmax=existing_brush$xmax,
                                    ymin=existing_brush$ymin, ymax=existing_brush$ymax)
                            }
                        }
                        .disableButtonIf(
                            save_field,
                            !.any_active_selection(mode0, id0, pObjects$memory),
                            .buttonNoSelectionLabel, .buttonSaveLabel, session
                        )
                    }

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .zoomData, new_coords)
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                    if (!is.null(existing_brush) || (!is.null(existing_lasso) && existing_lasso$closed)) {
                        rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
                    }
                })
            })
        }
    }

    # Zoom observers for the heatmaps.
    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_plots)) {
        local({
            mode0 <- "heatMapPlot"
            id0 <- id
            plot_name <- paste0(mode0, id0)
            prefix <- paste0(plot_name, "_")

            brush_id <- paste0(prefix, .brushField)
            dblclick_field <- paste0(prefix, .zoomClick)

            observeEvent(input[[dblclick_field]], {
                brush <- input[[brush_id]]
                if (!is.null(brush)) {
                    new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
                    if (is.null(pObjects$memory$heatMapPlot[id0,][[.zoomData]][[1]])) { # if we haven't already zoomed in
                        inp_rows <- seq_along(pObjects$memory$heatMapPlot[id0,][[.heatMapFeatName]][[1]])
                    } else {
                        inp_rows <- pObjects$memory$heatMapPlot[id0,][[.zoomData]][[1]]
                    }

                    # Is the heatmap receiving a color brush (in that case the number of annotations should be increased by 1)
                    trans_enc <- .decoded2encoded(pObjects$memory$heatMapPlot[id0, .selectByPlot])
                    is_receiving_color_selection <- pObjects$memory$heatMapPlot[id0, .selectByPlot]!=.noSelection &&
                        pObjects$memory$heatMapPlot[id0,.selectEffect]==.selectColorTitle &&
                        .transmitted_selection(trans_enc, pObjects$memory, mode=mode0, id=id0)

                    # Update data.
                    n.annot <- length(pObjects$memory$heatMapPlot[,.heatMapColData][[id0]]) + is_receiving_color_selection
                    ymin <- .transform_global_to_local_y(new_coords["ymin"], n.genes=length(inp_rows), n.annot=n.annot)
                    ymax <- .transform_global_to_local_y(new_coords["ymax"], n.genes=length(inp_rows), n.annot=n.annot)
                    new_rows <- inp_rows[ymin:ymax]

                } else {
                    new_rows <- NULL # Zoom out.
                }

                pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .zoomData, new_rows)
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            }, ignoreInit=TRUE)
        })
    }
    invisible(NULL)
}

#' Selectize update observers
#'
#' Observers to update the selectize elements for the feature and sample names.
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
#' @importFrom shiny observe updateSelectizeInput
#' @rdname INTERNAL_selectize_update_observers
.selectize_update_observers <- function(input, session, se, pObjects, rObjects) {
    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)
    sample_choices <- seq_len(ncol(se))
    names(sample_choices) <- colnames(se)

    # Selectize updates for features.
    for (mode in c(point_plot_types, "heatMapPlot")) {
        if (mode=="featAssayPlot") {
            fields <- c(.featAssayYAxisFeatName, .featAssayXAxisFeatName, .colorByFeatName)
        } else if (mode %in% point_plot_types) {
            fields <- .colorByFeatName
        } else if (mode=="heatMapPlot") {
            fields <- .heatMapFeatName
        }

        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            for (field in fields) {
                local({
                    id0 <- id
                    mode0 <- mode
                    field0 <- field
                    cur_field <- paste0(mode0, id0, "_", field0)

                    observe({
                        force(rObjects$rerendered)
                        updateSelectizeInput(session, cur_field, choices=feature_choices, server=TRUE,
                            selected=pObjects$memory[[mode0]][id0, field0][[1]])
                    })
                })
            }
        }
    }

    # Selectize updates for samples.
    for (mode in point_plot_types) {
        if (mode=="sampAssayPlot") {
            fields <- c(.sampAssayYAxisSampName, .sampAssayXAxisSampName, .colorBySampName)
        } else if (mode %in% point_plot_types) {
            fields <- .colorBySampName
        }

        max_plots <- nrow(pObjects$memory[[mode]])
        for (id in seq_len(max_plots)) {
            for (field in fields) {
                local({
                    id0 <- id
                    mode0 <- mode
                    field0 <- field
                    cur_field <- paste0(mode0, id0, "_", field0)

                    observe({
                        force(rObjects$rerendered)
                        updateSelectizeInput(session, cur_field, choices=sample_choices, server=TRUE,
                            selected=pObjects$memory[[mode0]][id0, field0][[1]])
                    })
                })
            }
        }
    }
    invisible(NULL)
}

#' Multiple selection observers
#'
#' Observers to change the multiple selections by saving the active selection or deleting existing saved selections.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate
#' @rdname INTERNAL_multiple_select_observers
.multiple_select_observers <- function(input, session, pObjects, rObjects) {
    for (mode in point_plot_types) {
        max_plots <- nrow(pObjects$memory[[mode]])

        for (id in seq_len(max_plots)) {
            local({
                mode0 <- mode
                id0 <- id
                plot_name <- paste0(mode0, id0)
                save_field <- paste0(plot_name, "_", .multiSelectSave)
                del_field <- paste0(plot_name, "_", .multiSelectDelete)
                info_field <- paste0(plot_name, "_", .panelGeneralInfo)
                saved_field <- paste0(plot_name, "_", .selectMultiSaved)
                resaved_field <- paste0(plot_name, "_resaved")

                ## Save selection observer. ---
                observeEvent(input[[save_field]], {
                    current <- pObjects$memory[[mode0]][,.multiSelectHistory][[id0]]

                    to_store <- pObjects$memory[[mode0]][,.brushData][[id0]]
                    if (is.null(to_store)) {
                        to_store <- pObjects$memory[[mode0]][,.lassoData][[id0]]
                        if (is.null(to_store) || !to_store$closed) {
                            return(NULL)
                        }
                    }

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0,
                        .multiSelectHistory, c(current, list(to_store)))

                    # Updating self (replot to get number).
                    rObjects[[info_field]] <- .increment_counter(isolate(rObjects[[info_field]]))
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                    transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    if (transmitter!=.noSelection && .decoded2encoded(transmitter)==plot_name) {
                        rObjects[[saved_field]] <- .increment_counter(isolate(rObjects[[saved_field]]))
                        if (pObjects$memory[[mode0]][id0, .selectMultiType]==.selectMultiUnionTitle) {
                            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                        }
                    }

                    # Updating children.
                    rObjects[[resaved_field]] <- .increment_counter(isolate(rObjects[[resaved_field]]))

                    .disableButtonIf(
                        del_field,
                        !.any_saved_selection(mode0, id0, pObjects$memory),
                        .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
                    )
                })

                ## Deleted selection observer. ---
                observeEvent(input[[del_field]], {
                    current <- pObjects$memory[[mode0]][,.multiSelectHistory][[id0]]
                    current <- head(current, -1)
                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .multiSelectHistory, current)

                    # Updating self.
                    rObjects[[info_field]] <- .increment_counter(isolate(rObjects[[info_field]]))
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                    transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                    if (transmitter!=.noSelection && .decoded2encoded(transmitter)==plot_name) {
                        rObjects[[saved_field]] <- .increment_counter(isolate(rObjects[[saved_field]]))

                        reset <- pObjects$memory[[mode0]][id0, .selectMultiSaved] > length(current)
                        if (reset) {
                            pObjects$memory[[mode0]][id0, .selectMultiSaved] <- 0L
                        }
                    }

                    # Updating children.
                    rObjects[[resaved_field]] <- .increment_counter(isolate(rObjects[[resaved_field]]))

                    .disableButtonIf(
                        del_field,
                        !.any_saved_selection(mode0, id0, pObjects$memory),
                        .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
                    )
                })
            })
        }
    }
    invisible(NULL)
}

#' Dot plot observers
#'
#' Observers to set up parameters for the point-based plots as well as to trigger their rendering.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param colormap An \linkS4class{ExperimentColorMap} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate observe renderPlot
#' @rdname INTERNAL_dot_plot_observers
.dot_plot_observers <- function(input, output, session, se, colormap, pObjects, rObjects) {
    for (mode in point_plot_types) {
        print(mode)
        max_plots <- nrow(pObjects$memory[[mode]])

        # To be replaced once we change the underlying architecture to operate directly on objects.
        instance <- switch(mode,
            redDimPlot=RedDimPlot(),
            featAssayPlot=FeatAssayPlot(),
            colDataPlot=ColDataPlot(),
            rowDataPlot=RowDataPlot(),
            sampAssayPlot=SampAssayPlot(),
            heatMapPlot=HeatMapPlot()
        )

        for (id in seq_len(max_plots)) {
            .createParamObservers(instance, id, se, input=input,
                session=session, pObjects=pObjects, rObjects=rObjects)
            .createRenderedOutput(instance, id, se=se, colormap=colormap,
                output=output, pObjects=pObjects, rObjects=rObjects)
        }
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

#' Linked table observers
#'
#' Observers to monitor changes to the linking to and from table panels, as well as to trigger their creation.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param annotFun A function describing whether and how annotations should be reported for selected features.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate observe renderUI updateSelectizeInput
#' @importFrom DT renderDataTable datatable
#' @importFrom SummarizedExperiment rowData colData
#' @rdname INTERNAL_linked_table_observers
.linked_table_observers <- function(input, output, session, se, pObjects, rObjects, annotFun) {
    for (mode in linked_table_types) {
        max_plots <- nrow(pObjects$memory[[mode]])

        for (id in seq_len(max_plots)) {
            current <- switch(mode,
                colStatTable=ColStatTable(),
                rowStatTable=RowStatTable()
            )

            .createParamObservers(current, id, se=se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
            .createRenderedOutput(current, id, se=se, colormap=NULL, output=output, pObjects=pObjects, rObjects=rObjects)
        }
    }
    return(NULL)
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

#' Heatmap observers
#'
#' Observers to handle parameters and generation of the heatmaps.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param colormap An \linkS4class{ExperimentColorMap} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate renderUI renderPlot renderTable updateSelectizeInput uiOutput tableOutput verticalLayout
#' @importFrom shinyAce updateAceEditor
#' @rdname INTERNAL_heatmap_observers
.heatmap_observers <- function(input, output, session, se, colormap, pObjects, rObjects) {
    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    max_plots <- nrow(pObjects$memory$heatMapPlot)
    for (id in seq_len(max_plots)) {
        .createParamObservers(HeatMapPlot(), id, se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
        .createRenderedOutput(HeatMapPlot(), id, se, input=input, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)
    }
    invisible(NULL)
}

#nocov end
