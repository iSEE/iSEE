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

    out <- .create_voice_observers(NULL, NULL, NULL, sce, NULL, NULL)
    expect_null(out)

})


test_that(".mark_panel_as_modified appends the requested modes to rObjects", {

    rObjects <- new.env()
    rObjects$modified=list("RedDimPlot1"=character(0))

    out <- .mark_panel_as_modified(panel_name = "RedDimPlot1", mode = iSEE:::.panelResaved, rObjects = rObjects)
    expect_null(out)
    expect_identical(rObjects$modified[["RedDimPlot1"]], iSEE:::.panelResaved)

})
