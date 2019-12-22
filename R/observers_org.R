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
#' showModal modalDialog isolate removeModal
.organization_observers <- function(se, colormap, input, output, session, pObjects, rObjects) {
    output$allPanels <- renderUI({
        force(rObjects$rerender)
        rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
        .panel_generation(pObjects$memory, se)
    })

    # Persistent objects to give the modal a 'working memory'.
    # These get captured in the current environment to persist
    # when observeEvent's expression actually gets executed.
    org_pObjects <- new.env()
    org_pObjects$initialized <- FALSE
    org_rObjects <- reactiveValues(rerender=0)

    available_enc <- vapply(pObjects$reservoir, .encodedName, "")
    names(available_enc) <- vapply(pObjects$reservoir, .fullName, "")

    .define_choices <- function(memory, named=TRUE) {
        enc_names <- vapply(memory, .getEncodedName, "")
        if (named) {
            names(enc_names) <- vapply(memory, .getFullName, "")
        }
        enc_names
    }

    observeEvent(input$organize_panels, {
        enc_names <- .define_choices(pObjects$memory)
        org_pObjects$memory <- pObjects$memory
        org_pObjects$counter <- pObjects$counter

        if (!org_pObjects$initialized) {
            for (x in org_pObjects$memory) {
                .define_width_height_observers(x, input, org_pObjects)
            }
            org_pObjects$initialized <- TRUE
        }

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

                .define_width_height_observers(latest, input, org_pObjects)
            }
            updated_names <- .define_choices(adjusted)
            updateSelectizeInput(session, 'panel_order', 
                choices=c(updated_names, available_enc), selected=updated_names)
        }

        org_pObjects$memory <- adjusted
        org_rObjects$rerender <- .increment_counter(org_rObjects$rerender)
    })

    observeEvent(input$update_ui, {
        left <- names(org_pObjects$memory)
        right <- names(pObjects$memory)

        pObjects$memory <- org_pObjects$memory
        pObjects$counter <- org_pObjects$counter

        added <- setdiff(left, right)           
        for (a in added) {
            instance <- pObjects$memory[[a]]
            .createObservers(instance, se, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
            .renderOutput(instance, se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)
        }

        lost <- setdiff(right, left)
        for (l in lost) {
            pObjects$selection_links <- .destroy_parent(pObjects$selection_links, l)
            pObjects$aesthetics_links <- .destroy_parent(pObjects$aesthetics_links, l)
        }

        # NOTE: there should be no need to updateSelectize on the choice of
        # linkable panels; this should be handled by the rerendering. This
        # should also trigger the corresponding observers to update the memory
        # and propagate the required downstream changes.

        rObjects$rerender <- .increment_counter(rObjects$rerender)
    })

    invisible(NULL)
}

#' @importFrom shiny observeEvent
.define_width_height_observers <- function(panel, input, org_pObjects) {
    panel_name <- .getEncodedName(panel)

    width_name <- paste0(panel_name, "_", .organizationWidth)
    observeEvent(input[[width_name]], {
        cur.width <- org_pObjects$memory[[panel_name]][[.organizationWidth]]
        new.width <- as.integer(input[[width_name]])
        if (!isTRUE(all.equal(new.width, cur.width))) {
            org_pObjects$memory[[panel_name]][[.organizationWidth]] <- new.width
        }
    }, ignoreInit=TRUE)

    height_name <- paste0(panel_name, "_", .organizationHeight)
    observeEvent(input[[height_name]], {
        cur.height <- org_pObjects$memory[[panel_name]][[.organizationHeight]]
        new.height <- as.integer(input[[height_name]])
        if (!isTRUE(all.equal(new.height, cur.height))) {
            org_pObjects$memory[[panel_name]][[.organizationHeight]] <- new.height
        }
    }, ignoreInit=TRUE)
}
