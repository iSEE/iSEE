#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "Table", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .TableSelected, 1L)
    .Object <- .empty_default(.Object, .TableSearch, "")
    .Object
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("Table", function(object) {
    msg <- character(0)

    if (length(chosen <- object[[.TableSelected]])!=1L || is.na(chosen) || chosen <= 0L) {
        msg <- c(msg, sprintf("'%s' should be a positive integer for '%s'", .TableSelected, class(object)[1]))
    }

    if (!isSingleString(val <- object[[.TableSearch]]) || is.na(val)) {
        msg <- c(msg, sprintf("'%s' should be a single non-NA string for '%s'", .TableSearch, class(object)[1]))
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutputElement", "Table", function(x, ...) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    tagList(dataTableOutput(panel_name), hr())
})

#' @export
setMethod(".createParamObservers", "Table", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)

    .define_box_observers(panel_name, .selectParamBoxOpen, input, pObjects)

    # Updating memory for new selection parameters.
    # Note that '.int' variables already have underscores, so these are not necessary.
    panel_name <- paste0(mode, id)
    search_field <- paste0(panel_name, .int_statTableSearch)
    observe({
        search <- input[[search_field]]
        if (length(search)) {
            pObjects$memory[[panel_name]][[.statTableSearch]] <- search
        }
    })

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
    observe({
        search <- input[[colsearch_field]]
        if (length(search)) {
            pObjects$memory[[panel_name]][[.statTableColSearch]] <- search
        }
    })
})


