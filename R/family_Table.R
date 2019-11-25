#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutputElement", "Table", function(x, id, ...) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    tagList(dataTableOutput(panel_name), hr())
})

#' @export
setMethod(".createParamObservers", "Table", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_box_observers(mode, id, .selectParamBoxOpen, input, pObjects)

    # Updating memory for new selection parameters.
    # Note that '.int' variables already have underscores, so these are not necessary.
    panel_name <- paste0(mode, id)
    search_field <- paste0(panel_name, .int_statTableSearch)
    observe({
        search <- input[[search_field]]
        if (length(search)) {
            pObjects$memory[[mode]][id, .statTableSearch] <- search
        }
    })

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
    observe({
        search <- input[[colsearch_field]]
        if (length(search)) {
            pObjects$memory[[mode]]<- .update_list_element(pObjects$memory[[mode]], id, .statTableColSearch, search)
        }
    })
})


