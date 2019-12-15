#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "ColumnTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .TableSelected, colnames(se))

    x
})

#' @export
setMethod(".createParamObservers", "ColumnTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- paste0(.getEncodedName(x), x[[.organizationId]])
    .define_dimname_propagation_observer(panel_name, choices=colnames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".transmittedDimension", "ColumnTable", function(x) "column")

#' @export
setMethod(".hideInterfaceElement", "ColumnDotPlot", function(x, field) {
    if (field %in% c(.selectRowSource, .selectRowType, .selectRowSaved)) {
        TRUE
    } else {
        callNextMethod()
    }
})
