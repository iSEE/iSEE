#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "RowTable", function(x, se) {
    df <- rowData(se)
    available <- .find_atomic_fields(df)
    df <- df[,available,drop=FALSE]

    out <- callNextMethod()
    out$RowTable <- list(df=data.frame(df, check.names=FALSE))
    out
})

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "RowTable", function(x, se, active_panels) {
    df <- .get_common_info(se, .getEncodedName(x))$RowTable$df
    if (nrow(df)==0) {
        warning(sprintf("no rows in 'se' to create '%s'", class(x)[1]))
        return(NULL)
    }

    # First, expanding out so that we cover all columns.
    search_vals <- x[[.TableColSearch]]
    if (length(search_vals)!=ncol(rowData(se))) {
        search_vals <- character(ncol(rowData(se)))
    }

    # Then, contracting only to those columns that survived.
    keep <- match(colnames(df), colnames(rowData(se)))
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    callNextMethod()    
})

#' @export
setMethod(".defineParamInterface", "RowTable", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    link_sources <- .define_link_sources(active_panels)
    row_selectable <- c(.noSelection, link_sources$row_plot)

    .define_selection_param_box(mode, id, param_choices,
       .define_selection_choices(mode, id, param_choices, .selectByPlot, row_selectable, "row")
    )
})

#' @export
setMethod(".createParamObservers", "RowTable", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    .define_table_selection_observer(mode, id, 
        x_field=.featAssayXAxisFeatName, 
        y_field=.featAssayYAxisFeatName,
        col_field=.colorByFeatName,
        choices=feature_choices,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})
