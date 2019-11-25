#' @export
ColStatTable <- function() {
    new("ColStatTable")
}

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".createRenderedOutput", "ColStatTable", function(x, id, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    sample_data <- data.frame(colData(se), check.names=FALSE)
    rownames(sample_data) <- colnames(se)
    if (identical(ncol(sample_data), 0L)) {
        sample_data$Present <- !logical(nrow(sample_data))
    }
    sample_data_select_col <- .safe_field_name("Selected", colnames(sample_data))

    .define_table_output(mode, id, 
        tab=sample_data, select_col=sample_data_select_col,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".getEncodedName", "ColStatTable", function(x) "colStatTable")
