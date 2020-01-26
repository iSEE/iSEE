context("API")

test_that(".refineParameters handles NULL x", {

    x_classes <- c("ColumnDotPlot", "ColumnTable", "DotPlot", "Panel",
        "RowDotPlot", "RowTable", "Table", "ColDataPlot", "ColStatTable",
        "ComplexHeatmapPlot", "FeatAssayPlot", "RedDimPlot", "RowDataPlot",
        "RowStatTable", "SampAssayPlot"
    )

    for (x_class in x_classes) {
        FUN <- selectMethod(".refineParameters", signature=x_class)
        out <- FUN(NULL, sce)
        expect_null(out, NULL)
    }

})

test_that(".refineParameters identifies impossible ColDataPlot", {

    x <- ColDataPlot()

    colData(sce) <- DataFrame(row.names = colnames(sce))
    sce <- .cacheCommonInfo(x, sce)
    expect_warning(.refineParameters(x, sce),
        "no valid 'colData' fields for 'ColDataPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce)
    expect_null(out)

})

test_that(".refineParameters identifies impossible RowDataPlot", {

    x <- RowDataPlot()

    rowData(sce) <- DataFrame(row.names = rownames(sce))
    sce <- .cacheCommonInfo(x, sce)
    expect_warning(.refineParameters(x, sce),
        "no atomic 'rowData' fields for 'RowDataPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce)
    expect_null(out)

})

test_that(".refineParameters identifies impossible SampAssayPlot", {

    x <- SampAssayPlot()

    sce0 <- sce[, 0]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no columns for plotting 'SampAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no named 'assays' for plotting 'SampAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

})

test_that(".refineParameters identifies impossible FeatAssayPlot", {

    x <- FeatAssayPlot()

    sce0 <- sce[0, ]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no rows available for plotting 'FeatAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no valid 'assays' for plotting 'FeatAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

})

test_that(".colorDotPlot returns NULL when coloring DotPlot by nothing", {

    x <- ColDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByNothingTitle
    out <- .colorDotPlot(x, LETTERS)
    expect_null(out)

    x <- RowDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByNothingTitle
    out <- .colorDotPlot(x, LETTERS)
    expect_null(out)

})

test_that(".cacheCommonInfo identifies valid reduced dimension names for RedDimPlot", {

    x <- RedDimPlot()

    reducedDim(sce, "empty") <- matrix(numeric(0), nrow = ncol(sce), ncol = 0)
    out <- .cacheCommonInfo(x, sce)
    expect_false("empty" %in% .get_common_info(out, "RedDimPlot")[["valid.reducedDim.names"]])

    se <- as(sce, "SummarizedExperiment")
    out <- .cacheCommonInfo(x, se)
    expect_identical(.get_common_info(out, "RedDimPlot")[["valid.reducedDim.names"]], character(0))

})

test_that(".cacheCommonInfo detects earlier cache", {

    x_classes <- c("ColDataPlot", "ColStatTable", "ComplexHeatmapPlot",
        "FeatAssayPlot", "RedDimPlot", "RowDataPlot", "RowStatTable", "SampAssayPlot"
    )

    for (x_class in x_classes) {
        x_instance <- new(x_class)
        for (i in seq_len(2)) {
            sce <- .cacheCommonInfo(x_instance, sce)
            # Run again to trigger !is.null(.get_common_info(se, "CLASS"))
            sce <- .cacheCommonInfo(x_instance, sce)
        }
    }

})
