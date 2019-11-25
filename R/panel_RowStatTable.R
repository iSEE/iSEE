#' @export
RowStatTable <- function() {
    new("RowStatTable")
}

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".createRenderedOutput", "RowStatTable", function(x, id, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    feature_data <- data.frame(rowData(se), check.names=FALSE)
    rownames(feature_data) <- rownames(se)
    if (identical(ncol(feature_data), 0L)) {
        feature_data$Present <- !logical(nrow(feature_data))
    }
    feature_data_select_col <- .safe_field_name("Selected", colnames(feature_data))

    .define_table_output(mode, id, 
        tab=feature_data, select_col=feature_data_select_col,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".getEncodedName", "RowStatTable", function(x) "rowStatTable")
