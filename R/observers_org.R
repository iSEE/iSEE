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
.organization_observers <- function(se, colormap, input, output, session, pObjects, rObjects) {
    output$allPanels <- renderUI({
        force(rObjects$rerender)
        rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
        .panel_generation(pObjects$memory, se)
    })

    # Persistent objects to give the modal a 'working memory'.
    # These get captured in the current environment to persist
    # when observeEvent's expression actually gets executed.
    org_pObjects <- reactiveValues(memory=pObjects$memory, counter=pObjects$counter)
    org_rObjects <- reactiveValues(rerender=0)

    available_enc <- vapply(pObjects$memory, function(x) class(x)[1], "")
    names(available_enc) <- vapply(pObjects$memory, .getFullName, "")

    .define_choices <- function(memory, named=TRUE) {
        ids <- vapply(memory, "[[", i=.organizationId, 0L)
        enc_names <- vapply(memory, .getEncodedName, "")
        enc_names <- paste0(enc_names, ids)
        if (named) {
            names(enc_names) <- paste(vapply(memory, .getFullName, ""), ids)
        }
        enc_names
    }

    observeEvent(input$organize_panels, {
        enc_names <- .define_choices(org_pObjects$memory)
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
    })

    output$panelParams <- renderUI({
        force(org_rObjects$rerender)
        .panel_organization(org_pObjects$memory)
    })

    observeEvent(input$panel_order, {
        ipo <- unname(input$panel_order)
        enc_names <- .define_choices(org_pObjects$memory, named=FALSE)
        if (identical(ipo, enc_names)) {
            return(NULL)
        }

        adjusted <- org_pObjects$memory[ipo]

        # Adding some newly created panels. This assumes that 
        # the common cache already exists for new types.
        if (length(added <- which(ipo %in% available_enc))) {
            for (a in added) {
                latest <- new(ipo[a])
                latest <- .refineParameters(latest, se)

                mode <- .getEncodedName(latest)
                idx <- org_pObjects$counter[[mode]] + 1L
                latest[[.organizationId]] <- idx

                adjusted[[a]] <- latest 
                names(adjusted)[a] <- paste0(mode, idx)
                org_pObjects$counter[[mode]] <- idx
            }
            updated_names <- .define_choices(adjusted)
            updateSelectizeInput(session, 'panel_order', 
                choices=c(updated_names, available_enc), selected=updated_names)
        }

        org_pObjects$memory <- adjusted
        org_rObjects$rerender <- .increment_counter(org_rObjects$rerender)
    })

    observeEvent(input$update_ui, {
        added <- setdiff(names(org_pObjects$memory), names(pObjects$memory))
        for (a in added) {
            instance <- org_pObjects$memory[[a]]
            .createParamObservers(instance, se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
            .createRenderedOutput(instance, se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)

            # TODO: add all the other stuff that we set up in the main app.
            mode <- .getEncodedName(instance)
            id <- instance[[.organizationId]]
            rObjects[[paste0(mode, id)]] <- 1L

            rObjects[[paste0(mode, id, "_", .panelLinkInfo)]] <- 1L
            rObjects[[paste0(mode, id, "_", .panelGeneralInfo)]] <- 1L
        }

        pObjects$memory <- org_pObjects$memory
        pObjects$counter <- org_pObjects$counter
        rObjects$rerender <- .increment_counter(rObjects$rerender)
    })

    invisible(NULL)
}
