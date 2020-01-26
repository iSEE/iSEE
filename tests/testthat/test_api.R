context("API")

test_that(".refineParameters handles corner case where x is NULL", {

    FUN <- selectMethod(".refineParameters", signature="ColumnDotPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="ColumnTable")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="DotPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="Panel")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="RowDotPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="RowTable")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="Table")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="ColDataPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="ColStatTable")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="ComplexHeatmapPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="FeatAssayPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="RedDimPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="RowDataPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="RowStatTable")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    FUN <- selectMethod(".refineParameters", signature="SampAssayPlot")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

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
