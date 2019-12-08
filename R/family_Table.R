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
#' @importFrom shiny observeEvent
#' @importFrom utils head
setMethod(".createParamObservers", "Table", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)

    .define_box_observers(panel_name, .selectParamBoxOpen, input, pObjects)

    .define_table_selection_observer(panel_name, input, session, pObjects, rObjects)

    # Updating memory for new selection parameters.
    # Note that '.int' variables already have underscores, so these are not necessary.
    panel_name <- paste0(mode, id)
    act_field <- paste0(panel_name, "_reactivated")
    search_field <- paste0(panel_name, .int_statTableSearch)

    observeEvent(input[[search_field]], {
        pObjects$memory[[panel_name]][[.statTableSearch]] <- input[[search_field]]
        .safe_reactive_bump(rObjects, act_field)
    })

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
    observeEvent(input[[colsearch_field]], {
        search <- input[[colsearch_field]]

        # Usually getting rid of the secret column added to filter the table.
        if (ncol(pObjects$coordinates[[panel_name]]) < length(search)) {
            search <- head(search, -1)
        }

        pObjects$memory[[panel_name]][[.statTableColSearch]] <- search
        .safe_reactive_bump(rObjects, act_field)
    })
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".createRenderedOutput", "Table", function(x, se, ..., output, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    .define_table_output(mode, id, FUN=.getTableFunction(x),
        se=se, output=output, pObjects=pObjects, rObjects=rObjects)
})
