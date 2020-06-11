.generalOrganizePanels <- "iSEE_INTERNAL_organize_panels"

#' Panel organization observers for \code{\link{iSEE}}
#'
#' A function to set up observers for the panel organization observers used in the app.
#' These handle the creation, destruction, re-ordering and resizing of all panels.
#'
#' @param session The Shiny session object from the server function.
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @details
#' The application does not immediately respond to user changes in the reorganization modal.
#' Rather, the user must press the \dQuote{Apply settings} for the application to fully re-render.
#' This avoids wasting time in repeated re-renderings before the reorganization is fully complete.
#'
#' The exception to the above rule lies in the interface elements for changing panel sizes.
#' These are immediately re-rendered upon changes in panel organization while preserving any user-set values of the height/width.
#' Achieving this requires the use of a \dQuote{working memory}.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_organization_observers
#' @importFrom shiny renderUI reactiveValues observeEvent
#' showModal modalDialog isolate removeModal
.create_organization_observers <- function(se, input, output, session, pObjects, rObjects) {
    # nocov start
    output$allPanels <- renderUI({
        force(rObjects$rerender)
        rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
        .panel_generation(se, pObjects$memory)
    })
    # nocov end

    # Persistent objects to give the modal a 'working memory'.
    # These get captured in the current environment to persist
    # when observeEvent's expression actually gets executed.
    org_pObjects <- new.env()
    org_pObjects$initialized <- FALSE
    org_pObjects$ui_updating <- FALSE
    org_rObjects <- reactiveValues(rerender=0)

    available_enc <- vapply(pObjects$reservoir, .encodedName, "")
    names(available_enc) <- vapply(pObjects$reservoir, .fullName, "")

    # nocov start
    observeEvent(input[[.generalOrganizePanels]], {
        enc_names <- .define_memory_panel_choices(pObjects$memory)
        org_pObjects$memory <- pObjects$memory
        org_pObjects$counter <- pObjects$counter

        if (!org_pObjects$initialized) {
            for (x in org_pObjects$memory) {
                .create_width_height_observers(x, input, org_pObjects)
            }
            org_pObjects$initialized <- TRUE
        }

        # Here we disable the observers until the UI has been rerendered with
        # the latest memory. Otherwise, we'll be stuck in a tricky situation
        # where the modal is launched before 'panelParams' has a chance to
        # rerender, causing the old UI elements to trigger the width/height
        # observers to set the old defaults.
        org_pObjects$ui_updating <- TRUE
        org_rObjects$rerender <- .increment_counter(org_rObjects$rerender)

        showModal(modalDialog(
            title="Panel organization", size="m", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            actionButton("update_ui", "Apply settings", icon=icon("object-ungroup"), width='100%'),
            hr(),
            selectizeInput("panel_order", label=NULL,
                selected=enc_names,
                choices=c(enc_names, available_enc), # Choosing the unnumbered panel choice will add a new panel.
                multiple=TRUE,
                options=list(plugins=list('remove_button', 'drag_drop')), width="500px"),
            uiOutput("panelParams")
        ))
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    output$panelParams <- renderUI({
        force(org_rObjects$rerender)
        org_pObjects$ui_updating <- FALSE
        .panel_organization(org_pObjects$memory)
    })
    # nocov end

    # nocov start
    observeEvent(input$panel_order, {
        ipo <- unname(input$panel_order)
        enc_names <- .define_memory_panel_choices(org_pObjects$memory, named=FALSE)
        if (identical(ipo, enc_names)) {
            return(NULL)
        }

        adjusted <- org_pObjects$memory[ipo]

        # Adding some newly created panels. This assumes that
        # the reservoir types have already been cached and refined.
        if (length(added <- which(ipo %in% available_enc))) {
            for (a in added) {
                mode <- ipo[a]
                latest <- pObjects$reservoir[[mode]]
                idx <- org_pObjects$counter[[mode]] + 1L
                latest[[.organizationId]] <- idx

                adjusted[[a]] <- latest
                names(adjusted)[a] <- paste0(mode, idx)
                org_pObjects$counter[[mode]] <- idx

                .create_width_height_observers(latest, input, org_pObjects)
            }
            updated_names <- .define_memory_panel_choices(adjusted)
            updateSelectizeInput(session, 'panel_order',
                choices=c(updated_names, available_enc), selected=updated_names)
        }

        org_pObjects$memory <- adjusted
        org_rObjects$rerender <- .increment_counter(org_rObjects$rerender)
    }, ignoreInit=TRUE,
    ignoreNULL=FALSE) # necessary when users remove the last panel from the UI
    # nocov end

    # nocov start
    observeEvent(input$update_ui, {
        left <- names(org_pObjects$memory)
        right <- names(pObjects$memory)
        pObjects$memory <- org_pObjects$memory
        pObjects$counter <- org_pObjects$counter

        added <- setdiff(left, right)
        for (a in added) {
            instance <- pObjects$memory[[a]]
            .createObservers(instance, se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
            .renderOutput(instance, se, output=output, pObjects=pObjects, rObjects=rObjects)
        }

        pObjects$selection_links <- .spawn_multi_selection_graph(pObjects$memory)
        pObjects$aesthetics_links <- .spawn_single_selection_graph(pObjects$memory)

        # NOTE: there should be no need to updateSelectize on the choice of
        # linkable panels; this should be handled by the rerendering of the UI.
        # This should also trigger the corresponding observers to update the
        # memory and propagate the required downstream changes, which avoids us
        # having to recapitulate all of that in this observer. As a consequence,
        # though, some downstream observers need to be robustified to references
        # to panels that no longer exist in the brief time after panel deletion
        # but before the memory is resync'd. See .add_interpanel_link() and
        # .create_dimname_observers() for some affected functions.

        rObjects$rerender <- .increment_counter(rObjects$rerender)

        removeModal(session)
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}

#' List active panels in memory
#'
#' @param memory A list of \linkS4class{Panel} instances representing the app state.
#' @param named A logical scalar indicating whether to name the return value with panel full names.
#'
#' @return A character name of encoded panel identifiers, optionally named with their full name.
#'
#' @rdname INTERNAL_define_memory_panel_choices
.define_memory_panel_choices <- function(memory, named=TRUE) {
    enc_names <- vapply(memory, .getEncodedName, "")
    if (named) {
        names(enc_names) <- vapply(memory, .getFullName, "")
    }
    enc_names
}

#' @importFrom shiny observeEvent
.create_width_height_observers <- function(x, input, org_pObjects) {
    panel_name <- .getEncodedName(x)

    width_name <- paste0(panel_name, "_", .organizationWidth)

    # nocov start
    observeEvent(input[[width_name]], {
        if (org_pObjects$ui_updating) {
            return(NULL)
        }

        cur.width <- org_pObjects$memory[[panel_name]][[.organizationWidth]]
        new.width <- as.integer(input[[width_name]])
        if (!isTRUE(all.equal(new.width, cur.width))) {
            org_pObjects$memory[[panel_name]][[.organizationWidth]] <- new.width
        }
    }, ignoreInit=TRUE)
    # nocov end

    height_name <- paste0(panel_name, "_", .organizationHeight)

    # nocov start
    observeEvent(input[[height_name]], {
        if (org_pObjects$ui_updating) {
            return(NULL)
        }

        cur.height <- org_pObjects$memory[[panel_name]][[.organizationHeight]]
        new.height <- as.integer(input[[height_name]])
        if (!isTRUE(all.equal(new.height, cur.height))) {
            org_pObjects$memory[[panel_name]][[.organizationHeight]] <- new.height
        }
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}
