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
setMethod(".defineParamInterface", "Table", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    link_sources <- .define_link_sources(active_panels)
    row_selectable <- c(.noSelection, link_sources$row_plot, link_sources$row_tab)
    col_selectable <- c(.noSelection, link_sources$col_plot, link_sources$col_tab)

    .define_selection_param_box(mode, id, x,
        .define_selection_choices(mode, id, x, by_field=.selectRowSource, 
            type_field=.selectRowType, saved_field=.selectRowSaved,
            selectable=row_selectable, "row"),
        .define_selection_choices(mode, id, x, by_field=.selectColSource, 
            type_field=.selectColType, saved_field=.selectColSaved,
            selectable=col_selectable, "column")
    )
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
