#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".cacheCommonInfo", "ColumnTable", function(x, se) {
    df <- colData(se)
    available <- .find_atomic_fields(df)
    df <- df[,available,drop=FALSE]

    out <- callNextMethod()
    out$ColumnTable <- list(df=data.frame(df, check.names=FALSE))
    out
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".refineParameters", "ColumnTable", function(x, se, active_panels) {
    df <- .get_common_info(se, .getEncodedName(x))$ColumnTable$df
    if (nrow(df)==0) {
        warning(sprintf("no columns in 'se' to create '%s'", class(x)[1]))
        return(NULL)
    }

    # First, expanding out so that we cover all columns.
    search_vals <- x[[.TableColSearch]]
    if (length(search_vals)!=ncol(colData(se))) {
        search_vals <- character(ncol(colData(se)))
    }

    # Then, contracting only to those columns that survived.
    keep <- match(colnames(df), colnames(colData(se)))
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    callNextMethod()    
})

#' @export
setMethod(".defineParamInterface", "ColumnTable", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    link_sources <- .define_link_sources(active_panels)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    .define_selection_param_box(mode, id, param_choices,
       .define_selection_choices(mode, id, param_choices, .selectByPlot, col_selectable, "column")
    )
})

#' @export
setMethod(".createParamObservers", "ColumnTable", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)

    sample_choices <- seq_len(nrow(se))
    names(sample_choices) <- rownames(se)

    .define_table_selection_observer(mode, id,
        x_field=.sampAssayXAxisSampName,
        y_field=.sampAssayYAxisSampName,
        col_field=.colorBySampName,
        choices=sample_choices,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

