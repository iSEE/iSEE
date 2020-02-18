# library(iSEE); library(testthat); source("setup_sce.R"); source("test_voice.R")
context("voice")

# Do NOT move to setup; re-defined here to keep tests self-contained.

# Setting up the memory.
memory <- list(
    ReducedDimensionPlot(PanelId=1L),
    ColumnDataPlot(PanelId=1L),
    FeatureAssayPlot(PanelId=1L),
    RowDataTable(PanelId=1L),
    RowDataPlot(PanelId=1L),
    SampleAssayPlot(PanelId=1L),
    ColumnDataTable(PanelId=1L),
    ComplexHeatmapPlot(PanelId=1L))

# Setting up the reservoir.
reservoir <- list(
    ReducedDimensionPlot(),
    ColumnDataPlot(),
    FeatureAssayPlot(),
    RowDataTable(),
    RowDataPlot(),
    SampleAssayPlot(),
    ColumnDataTable(),
    ComplexHeatmapPlot()
)

# Set up alternative object.
sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

# prepareSpeechRecognition ----

test_that("prepareSpeechRecognition loads", {

    out <- iSEE:::prepareSpeechRecognition(use = TRUE)

    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

})

# .digitalizeText ----

test_that("numbers can be numeralized from text", {

    out <- iSEE:::.digitalizeText("one")
    expect_identical(out, 1)

    out <- iSEE:::.digitalizeText("two")
    expect_identical(out, 2)

    # allow some vocal typos
    out <- iSEE:::.digitalizeText("to")
    expect_identical(out, 2)

    out <- iSEE:::.digitalizeText("too")
    expect_identical(out, 2)

})

# .allDigits ----

test_that(".allDigits works", {

    expect_true(iSEE:::.allDigits("123"))
    expect_false(iSEE:::.allDigits("oen hundred and three"))

})

# .nearestPanelNameType ----

test_that(".nearestPanelByType handles vocal typos", {

    out <- iSEE:::.nearestPanelByType("definitely not a proper panel type", reservoir, max.edits = 5L)
    expect_length(out, 0L)

    out <- iSEE:::.nearestPanelByType("reduce dimension plus", reservoir)
    expect_identical(out, 1L) # "Reduced dimension plot"

    out <- iSEE:::.nearestPanelByType("row data table", reservoir)
    expect_identical(out, 4L) # "Row data table"

    out <- iSEE:::.nearestPanelByType("row data plot", reservoir)
    expect_identical(out, 5L) # "Row data plot"
})

# .nearestPanelByName ----

test_that(".nearestPanelByName handles vocal typos", {

    out <- iSEE:::.nearestPanelByName("definitely not a proper panel type", memory, max.edits = 5L)
    expect_length(out, 0L)

    out <- iSEE:::.nearestPanelByName("reduce dimension plus 1", reservoir)
    expect_identical(out, 1L) # "Reduced dimension plot 1"

    out <- iSEE:::.nearestPanelByName("row data table 1", reservoir)
    expect_identical(out, 4L) # "Row data table 1"

    out <- iSEE:::.nearestPanelByName("row data plot 1", reservoir)
    expect_identical(out, 5L) # "Row data plot 1"
})

# .nearestValidChoice ----

test_that(".nearestValidChoice handles vocal typos", {

    reservoir_types <- vapply(reservoir, .fullName, character(1))

    out <- iSEE:::.nearestValidChoice("input that cannot be matched", reservoir_types, max.edits = 5)
    expect_identical(out, character(0L))

    out <- iSEE:::.nearestValidChoice("reduced dimension plot", reservoir_types)
    expect_identical(out, "Reduced dimension plot")

})

# .nearestValidNamedChoice ----

test_that(".nearestValidNamedChoice handles vocal typos", {

    reservoir_types <- vapply(reservoir, .fullName, character(1))
    names(reservoir_types) <- vapply(reservoir, class, character(1))

    out <- iSEE:::.nearestValidNamedChoice("reduced dimension", reservoir_types)
    expect_identical(out, c(ReducedDimensionPlot = "Reduced dimension plot"))

})

# .colorByChoices ----

test_that(".colorByChoices works", {

    out <- iSEE:::.colorByChoices("None", sce)
    expect_identical(out, character(0L))

    out <- iSEE:::.colorByChoices("Column data", sce)
    expect_identical(out, iSEE:::.get_common_info(sce, "ColumnDotPlot")$valid.colData.names)

    out <- iSEE:::.colorByChoices("Row data", sce)
    expect_identical(out, iSEE:::.get_common_info(sce, "RowDotPlot")$valid.rowData.names)

    out <- iSEE:::.colorByChoices("Feature name", sce)
    expectedValue <- seq_len(nrow(sce))
    names(expectedValue) <- rownames(sce)
    expect_identical(out, expectedValue)

    out <- iSEE:::.colorByChoices("Sample name", sce)
    expectedValue <- seq_len(ncol(sce))
    names(expectedValue) <- colnames(sce)
    expect_identical(out, expectedValue)

})
