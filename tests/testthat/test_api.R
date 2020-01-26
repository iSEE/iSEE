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
