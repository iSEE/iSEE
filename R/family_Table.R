#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "Table", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .TableSelected)
    .Object <- .empty_default(.Object, .TableSearch, "")
    .Object
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("Table", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object, .TableSelected)

    msg <- .valid_string_error(msg, object, .TableSearch)

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod(".processTransmission", "Table", function(x, index) {
    filter_cmds <- .generate_table_filter(x, varname="transmitter")
    if (!is.null(filter_cmds)) {
        sprintf("selected <- rownames(transmitter)[%s]", filter_cmds)
    } else {
        NULL
    }
})

#' @export
setMethod(".hasActiveSelection", "Table", function(x) {
    x[[.TableSearch]]!="" || any(x[[.TableColSearch]]!="")
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

    .define_table_observers(panel_name, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".createRenderedOutput", "Table", function(x, se, ..., output, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    .define_table_output(mode, id, FUN=.getTableFunction(x),
        se=se, output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".hideInterfaceElement", "Table", function(x, field) {
    if (field %in% .multiSelectHistory) {
        TRUE
    } else {
        callNextMethod()
    }
})
