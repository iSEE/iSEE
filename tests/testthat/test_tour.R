# This tests the various utilities used to construct the tour.
# library(testthat); library(iSEE); source("setup_sce.R"); source("test_tour.R")

test_that("defaultTour() works correctly", {
    out <- defaultTour()
    expect_true(nrow(out) > 1L)
    expect_identical(colnames(out), c("element", "intro"))
})

all_defaults <- list(
    ReducedDimensionPlot(PanelId=1L), 
    RowDataTable(PanelId=1L), 
    FeatureAssayPlot(PanelId=1L), 
    ColumnDataPlot(PanelId=1L),
    RowDataPlot(PanelId=1L), 
    SampleAssayPlot(PanelId=1L), 
    ColumnDataTable(PanelId=1L), 
    ComplexHeatmapPlot(PanelId=1L)
)

test_that(".assemble_tour works correctly for the default example", {
    out <- iSEE:::.assemble_tour(sce, all_defaults)
    expect_identical(out, defaultTour())
})

test_that(".assemble_tour breaks out in every other case", {
    for (i in seq_along(all_defaults)) {
        test <- iSEE:::.assemble_tour(sce, all_defaults[-i])
        expect_identical(test, iSEE:::.truncated_tour)
    }

    # Responds to missing reduced dimensions.
    sce0 <- sce
    reducedDims(sce0) <- NULL
    test <- iSEE:::.assemble_tour(sce0, all_defaults)
    expect_identical(test, iSEE:::.truncated_tour)

    # Responds to missing colData variables.
    sce0 <- sce
    sce0$passes_qc_checks_s <- NULL
    test <- iSEE:::.assemble_tour(sce0, all_defaults)
    expect_identical(test, iSEE:::.truncated_tour)

    sce0 <- sce
    sce0$dissection_s <- NULL
    test <- iSEE:::.assemble_tour(sce0, all_defaults)
    expect_identical(test, iSEE:::.truncated_tour)

    sce0 <- sce
    sce0$driver_1_s <- NULL
    test <- iSEE:::.assemble_tour(sce0, all_defaults)
    expect_identical(test, iSEE:::.truncated_tour)
})

