#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "RowTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .TableSelected, rownames(se))

    x
})

#' @export
setMethod(".createParamObservers", "RowTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- paste0(.getEncodedName(x), x[[.organizationId]])
    .define_dimname_propagation_observer(panel_name, choices=rownames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".transmittedDimension", "RowTable", function(x) "row")
