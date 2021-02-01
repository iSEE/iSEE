# Tests the table linking functions for point-based plots.
# library(iSEE); library(testthat); source("setup_sce.R"); source("setup_mimic_live_app.R"); source("test_dimnames.R")

context("table_links")

test_that("dimname observers work to change the source", {
    memory <- list(
        FeatureAssayPlot(YAxisFeatureSource="RowDataTable1"),
        RowDataTable(),
        RowDataTable()
    )

    pObjects <- mimic_live_app(sce, memory)
    rObjects <- new.env()
    input <- list()

    # The y-axis table choice has not changed.
    out <- iSEE:::.setup_dimname_source_observer(
        "FeatureAssayPlot1", use_mode_field=NA, use_value=NA,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.featAssayYAxisFeatName,
        tab_field=iSEE:::.featAssayYAxisRowTable,
        choices=NULL)

    expect_false(out)
    expect_identical(pObjects$memory$FeatureAssayPlot1[["YAxisFeatureSource"]], "RowDataTable1")
    expect_true(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "FeatureAssayPlot1"))

    # Changing the table.
    input$FeatureAssayPlot1_YAxisFeatureSource <- "RowDataTable2"

    out <- iSEE:::.setup_dimname_source_observer(
        "FeatureAssayPlot1", use_mode_field=NA, use_value=NA,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.featAssayYAxisFeatName,
        tab_field=iSEE:::.featAssayYAxisRowTable,
        choices=NULL)

    expect_false(out)
    expect_identical(pObjects$memory$FeatureAssayPlot1[["YAxisFeatureSource"]], "RowDataTable2")
    expect_false(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "FeatureAssayPlot1"))
    expect_true(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable2", "FeatureAssayPlot1"))

    # Erasing the table.
    input$FeatureAssayPlot1_YAxisFeatureSource <- "---"

    out <- iSEE:::.setup_dimname_source_observer(
        "FeatureAssayPlot1", use_mode_field=NA, use_value=NA,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.featAssayYAxisFeatName,
        tab_field=iSEE:::.featAssayYAxisRowTable,
        choices=NULL)

    expect_false(out)
    expect_identical(pObjects$memory$FeatureAssayPlot1[["YAxisFeatureSource"]], "---")
    expect_false(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "FeatureAssayPlot1"))
    expect_false(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable2", "FeatureAssayPlot1"))
})

test_that("dimname observers work to change the usage mode", {
    memory <- list(
        ReducedDimensionPlot(),
        FeatureAssayPlot(YAxisFeatureSource="RowDataTable1"),
        RowDataTable()
    )

    pObjects <- mimic_live_app(sce, memory)
    rObjects <- new.env()
    input <- list()

    # The color choice has changed.
    input$ReducedDimensionPlot1_ColorBy <- "Feature name"
    input$ReducedDimensionPlot1_ColorByFeatureSource <- "RowDataTable1"
    expect_identical(pObjects$memory$ReducedDimensionPlot1[["ColorBy"]], "None")
    expect_false(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "ReducedDimensionPlot1"))

    out <- iSEE:::.setup_dimname_source_observer(
        "ReducedDimensionPlot1",
        use_mode_field=iSEE:::.colorByField, use_value=iSEE:::.colorByFeatNameTitle,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.colorByFeatName,
        tab_field=iSEE:::.colorByRowTable,
        choices=NULL)

    expect_true(out)
    expect_identical(pObjects$memory$ReducedDimensionPlot1[["ColorBy"]], "Feature name")
    expect_true(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "ReducedDimensionPlot1"))

    # Rerunning the observer will continue to restore the link,
    # even if the memory has already been updated (to handle
    # situations where multiple observers respond to the same change).
    old_memory <- pObjects$memory
    pObjects$aesthetics_links <- iSEE:::.delete_interpanel_link(pObjects$aesthetics_links,
        "ReducedDimensionPlot1", "RowDataTable1", field=iSEE:::.colorByFeatName)
    expect_false(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "ReducedDimensionPlot1"))

    out <- iSEE:::.setup_dimname_source_observer(
        "ReducedDimensionPlot1",
        use_mode_field=iSEE:::.colorByField, use_value=iSEE:::.colorByFeatNameTitle,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.colorByFeatName,
        tab_field=iSEE:::.colorByRowTable,
        choices=NULL)

    expect_identical(old_memory, pObjects$memory)
    expect_true(igraph::are_adjacent(pObjects$aesthetics_links, "RowDataTable1", "ReducedDimensionPlot1"))

    # The feature choice has changed.
    input$FeatureAssayPlot1_XAxis <- "Feature name"
    input$FeatureAssayPlot1_XAxisFeatureSource <- "RowDataTable1"
    expect_identical(pObjects$memory$FeatureAssayPlot1[["XAxis"]], "None")

    id <- igraph::get.edge.ids(pObjects$aesthetics_links, c("RowDataTable1", "FeatureAssayPlot1"))
    expect_identical(igraph::E(pObjects$aesthetics_links)$fields[[id]], iSEE:::.featAssayYAxisFeatName)

    out <- iSEE:::.setup_dimname_source_observer(
        "FeatureAssayPlot1",
        use_mode_field=iSEE:::.featAssayXAxis, use_value=iSEE:::.featAssayXAxisFeatNameTitle,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.featAssayXAxisFeatName,
        tab_field=iSEE:::.featAssayXAxisRowTable,
        choices=NULL)

    expect_true(out)
    expect_identical(pObjects$memory$FeatureAssayPlot1[["XAxis"]], "Feature name")
    expect_identical(igraph::E(pObjects$aesthetics_links)$fields[[id]],
        c(iSEE:::.featAssayYAxisFeatName, iSEE:::.featAssayXAxisFeatName))

    # Changing it back deletes the link.
    input$FeatureAssayPlot1_XAxis <- "None"

    out <- iSEE:::.setup_dimname_source_observer(
        "FeatureAssayPlot1",
        use_mode_field=iSEE:::.featAssayXAxis, use_value=iSEE:::.featAssayXAxisFeatNameTitle,
        pObjects=pObjects, rObjects=rObjects, input=input, session=NULL,
        name_field=iSEE:::.featAssayXAxisFeatName,
        tab_field=iSEE:::.featAssayXAxisRowTable,
        choices=NULL)

    expect_true(out)
    expect_identical(pObjects$memory$FeatureAssayPlot1[["XAxis"]], "None")
    expect_identical(igraph::E(pObjects$aesthetics_links)$fields[[id]], iSEE:::.featAssayYAxisFeatName)
})

test_that(".setup_dimname_source_observer works with a mimicked app", {

    memory <- list(
        ReducedDimensionPlot(),
        FeatureAssayPlot(YAxisFeatureSource="RowDataTable1"),
        RowDataTable(Selected=tail(rownames(sce), 1))
    )

    pObjects <- mimic_live_app(sce, memory)
    rObjects <- new.env()
    input <- list()

    # The color choice has changed
    # This is expected to make iSEE:::.setup_dimname_source_observer return TRUE
    input$ReducedDimensionPlot1_ColorBy <- "Feature name"
    input$ReducedDimensionPlot1_ColorByFeatureSource <- "RowDataTable1"

    # Define dummy functions called by updateSelectizeInput
    session <- shiny::MockShinySession$new()

    out <- iSEE:::.setup_dimname_source_observer(
        "ReducedDimensionPlot1",
        use_mode_field=iSEE:::.colorByField, use_value=iSEE:::.colorByFeatNameTitle,
        name_field=iSEE:::.colorByFeatName,
        tab_field=iSEE:::.colorByRowTable,
        choices=letters,
        input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)
    expect_true(out)

})
