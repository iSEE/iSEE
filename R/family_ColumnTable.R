#' @export
#' @importFrom SummarizedExperiment colData
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "ColumnTable", function(x, se) {
    if (is.null(.get_common_info(se, "ColumnTable"))) {
        df <- colData(se)
        available <- .find_atomic_fields(df)
        df <- df[,available,drop=FALSE]
        se <- .set_common_info(se, "ColumnTable",
            valid.colData.df=data.frame(df, check.names=FALSE))
    }

    callNextMethod()
})

#' @export
#' @importFrom SummarizedExperiment colData
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "ColumnTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    df <- .get_common_info(se, "ColumnTable")$valid.colData.df

    # First, expanding out so that we cover all columns.
    search_vals <- x[[.TableColSearch]]
    if (length(search_vals)!=ncol(colData(se))) {
        search_vals <- character(ncol(colData(se)))
    }

    # Then, contracting only to those columns that survived.
    keep <- match(colnames(df), colnames(colData(se)))
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    x
})

#' @export
setMethod(".defineParamInterface", "ColumnTable", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    link_sources <- .define_link_sources(active_panels)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    .define_selection_param_box(mode, id, x,
       .define_selection_choices(mode, id, x, .selectByPlot, col_selectable, "column")
    )
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "ColumnTable", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)

    .define_table_selection_observer(panel_name,
        x_field=.sampAssayXAxisSampName,
        y_field=.sampAssayYAxisSampName,
        col_field=.colorBySampName,
        choices=colnames(se),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

