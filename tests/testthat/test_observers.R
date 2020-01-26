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

test_that(".create_organization_observers returns NULL", {

    input <- new.env()
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .create_organization_observers(sce, input, output, session = NULL, pObjects = pObjects, rObjects = rObjects)
    expect_null(out)

    expect_named(output, c("allPanels", "panelParams"))
    expect_is(output$allPanels, "shiny.render.function")
    expect_is(output$panelParams, "shiny.render.function")

})

test_that(".create_width_height_observers returns NULL", {

    x <- RedDimPlot()
    input <- new.env()
    pObjects <- new.env()

    out <- .create_width_height_observers(x, input, pObjects)
    expect_null(out)

})
