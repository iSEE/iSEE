context("Observers")

memory <- list(
    RedDimPlot(),
    ColDataPlot(),
    ColDataPlot(),
    FeatAssayPlot(),
    FeatAssayPlot(),
    RowStatTable(),
    RowStatTable(),
    SampAssayPlot(),
    ColStatTable()
)

test_that("Observers return NULL", {

    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

    x <- RedDimPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- ColDataPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- RowDataPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- FeatAssayPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- SampAssayPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

})
