context("Observers")

memory <- list(
    ReducedDimensionPlot(PanelId=1L),
    ColumnDataPlot(PanelId=1L),
    ColumnDataPlot(PanelId=1L),
    FeatureAssayPlot(PanelId=1L),
    FeatureAssayPlot(PanelId=1L),
    RowDataTable(PanelId=1L),
    RowDataTable(PanelId=1L),
    SampleAssayPlot(PanelId=1L),
    ColumnDataTable(PanelId=1L)
)

test_that("Observers return NULL", {

    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

    x <- ColumnDataPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- ColumnDataTable()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- FeatureAssayPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- ReducedDimensionPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- RowDataPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- RowDataTable()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    x <- SampleAssayPlot()
    x <- .refineParameters(x, sce)
    out <- .createObservers(x, sce, NULL, NULL, NULL, NULL)
    expect_null(out)

    out <- .create_voice_observers(NULL, NULL, NULL, sce, NULL, NULL)
    expect_null(out)

})

test_that(".mark_panel_as_modified appends the requested modes to rObjects", {

    rObjects <- new.env()
    rObjects$modified=list("ReducedDimensionPlot1"=character(0))

    out <- .mark_panel_as_modified(panel_name = "ReducedDimensionPlot1", mode = iSEE:::.panelResaved, rObjects = rObjects)
    expect_null(out)
    expect_identical(rObjects$modified[["ReducedDimensionPlot1"]], iSEE:::.panelResaved)

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

    x <- ReducedDimensionPlot()
    input <- new.env()
    pObjects <- new.env()

    out <- .create_width_height_observers(x, input, pObjects)
    expect_null(out)

})

test_that(".create_table_observers returns NULL", {

    input <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .create_table_observers("ReducedDimensionPlot1", input, session = NULL, pObjects, rObjects)
    expect_null(out)

})

test_that(".define_memory_panel_choices returns expected values", {

    UNNAMED <- NAMED <- c("ReducedDimensionPlot1", "ColumnDataPlot1", "ColumnDataPlot1",
        "FeatureAssayPlot1",  "FeatureAssayPlot1", "RowDataTable1", "RowDataTable1",
        "SampleAssayPlot1",  "ColumnDataTable1")

    names(NAMED) <- c("Reduced dimension plot 1", "Column data plot 1",
        "Column data plot 1",  "Feature assay plot 1", "Feature assay plot 1",
        "Row statistics table 1",  "Row statistics table 1", "Sample assay plot 1",
        "Column statistics table 1")

    out <- .define_memory_panel_choices(memory, named = FALSE)
    expect_identical(out, UNNAMED)

    out <- .define_memory_panel_choices(memory, named = TRUE)
    expect_identical(out, NAMED)

})

test_that(".create_child_propagation_observer returns NULL", {

    pObjects <- new.env()
    rObjects <- new.env()

    out <- .create_child_propagation_observer(sce, pObjects, rObjects)
    expect_null(out)

})
