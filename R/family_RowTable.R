#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "RowTable", function(x, se) {
    if (is.null(.get_common_info(se, "ColumnTable"))) {
        df <- rowData(se)
        available <- .find_atomic_fields(df)
        df <- df[,available,drop=FALSE]
        se <- .set_common_info(se, "RowTable",
            valid.rowData.df=data.frame(df, check.names=FALSE))
    }

    callNextMethod()
})

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "RowTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    df <- .get_common_info(se, "RowTable")$valid.rowData.df

    # First, expanding out so that we cover all columns.
    search_vals <- x[[.TableColSearch]]
    if (length(search_vals)!=ncol(rowData(se))) {
        search_vals <- character(ncol(rowData(se)))
    }

    # Then, contracting only to those columns that survived.
    keep <- match(colnames(df), colnames(rowData(se)))
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    x
})

#' @export
setMethod(".defineParamInterface", "RowTable", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    link_sources <- .define_link_sources(active_panels)
    row_selectable <- c(.noSelection, link_sources$row_plot)

    .define_selection_param_box(mode, id, x,
       .define_selection_choices(mode, id, x, .selectByPlot, row_selectable, "row")
    )
})

#' @export
setMethod(".createParamObservers", "RowTable", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    .define_table_selection_observer(mode, id, 
        x_field=.featAssayXAxisFeatName, 
        y_field=.featAssayYAxisFeatName,
        col_field=.colorByFeatName,
        choices=rownames(se),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})
