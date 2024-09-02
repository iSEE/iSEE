# This tests various aspects of the UI generation.
# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_mimic_live_app.R"); source("test_ui.R")

context("ui")

memory <- list(ReducedDimensionPlot(), RowDataTable(), FeatureAssayPlot(),
    ColumnDataPlot(), RowDataPlot(), SampleAssayPlot(), ColumnDataTable(),
    ComplexHeatmapPlot())

pObjects <- mimic_live_app(sce, memory)
sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

test_that(".panel_generation works", {
    out <- iSEE:::.panel_generation(sce, pObjects$memory)
    expect_is(out, "shiny.tag.list")
})

test_that(".panel_organization works", {
    out <- iSEE:::.panel_organization(pObjects$memory)
    expect_is(out, "shiny.tag.list")
})

test_that(".choose_links behaves as expected", {
    chosenValue <- "chosen"
    availableValues <- c(chosenValue, head(letters))

    # Return chosen value when available
    out <- iSEE:::.choose_link(chosenValue, availableValues)
    expect_identical(out, chosenValue)

    availableValues <- head(letters)

    # Return first available value if chosen is not available, and default is forced
    out <- iSEE:::.choose_link(chosenValue, availableValues)
    expect_identical(out, availableValues[1])

    # Return "" is there are no available values
    out <- iSEE:::.choose_link(chosenValue, character(0))
    expect_identical(out, "")
})

test_that("assorted hidden modes work correctly", {
    X <- ReducedDimensionPlot()
    expect_match(as.character(iSEE:::.selectizeInputHidden(X, iSEE:::.selectRowRestrict, choices=letters, label="YAY")), "hide")
    expect_false(grepl("hide", as.character(iSEE:::.selectizeInputHidden(X, iSEE:::.selectColumnRestrict, choices=letters, label="YAY"))))
})
