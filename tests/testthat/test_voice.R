context("voice")

# Do NOT move to setup; re-defined here to keep tests self-contained.

# Setting up the memory.
memory <- list(
    RedDimPlot(PanelId=1L),
    ColDataPlot(PanelId=1L),
    FeatAssayPlot(PanelId=1L),
    RowStatTable(PanelId=1L),
    RowDataPlot(PanelId=1L),
    SampAssayPlot(PanelId=1L),
    ColStatTable(PanelId=1L),
    HeatMapPlot(PanelId=1L))

# Setting up the reservoir.
reservoir <- list(
    RedDimPlot(),
    ColDataPlot(),
    FeatAssayPlot(),
    RowStatTable(),
    RowDataPlot(),
    SampAssayPlot(),
    ColStatTable(),
    HeatMapPlot()
)

# Set up alternative object.
sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

# prepareSpeechRecognition ----

test_that("prepareSpeechRecognition loads", {

    out <- prepareSpeechRecognition(use = TRUE)

    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

})

# .digitalizeText ----

test_that("numbers can be numeralized from text", {

    out <- .digitalizeText("one")
    expect_identical(out, 1)

    out <- .digitalizeText("two")
    expect_identical(out, 2)

    # allow some vocal typos
    out <- .digitalizeText("to")
    expect_identical(out, 2)

    out <- .digitalizeText("too")
    expect_identical(out, 2)

})

# .allDigits ----

test_that(".allDigits works", {

    expect_true(.allDigits("123"))
    expect_false(.allDigits("oen hundred and three"))

})

# .nearestPanelNameType ----

test_that(".nearestPanelByType handles vocal typos", {

    out <- .nearestPanelByType("definitely not a proper panel type", reservoir, max.edits = 5L)
    expect_length(out, 0L)

    out <- .nearestPanelByType("reduce dimension plus", reservoir)
    expect_identical(out, 1L) # "Reduced dimension plot"

    out <- .nearestPanelByType("row statistics table", reservoir)
    expect_identical(out, 4L) # "Row statistics table"

    out <- .nearestPanelByType("row statistics plot", reservoir)
    expect_identical(out, 4L) # "Row statistics table"
    # NOTE: closer to "Row statistics table" than "Row data plot"
})

# .nearestPanelByName ----

test_that(".nearestPanelByName handles vocal typos", {

    out <- .nearestPanelByName("definitely not a proper panel type", memory, max.edits = 5L)
    expect_length(out, 0L)

    out <- .nearestPanelByName("reduce dimension plus 1", reservoir)
    expect_identical(out, 1L) # "Reduced dimension plot 1"

    out <- .nearestPanelByName("row statistics table 1", reservoir)
    expect_identical(out, 4L) # "Row statistics table 1"

    out <- .nearestPanelByName("row statistics plot 1", reservoir)
    expect_identical(out, 4L) # "Row statistics table 1"
    # NOTE: closer to "Row statistics table" than "Row data plot"
})

# .nearestValidChoice ----

test_that(".nearestValidChoice handles vocal typos", {

    reservoir_types <- vapply(reservoir, .fullName, character(1))

    out <- .nearestValidChoice("input that cannot be matched", reservoir_types, max.edits = 5)
    expect_identical(out, character(0L))

    out <- .nearestValidChoice("reduced dimension plot", reservoir_types)
    expect_identical(out, "Reduced dimension plot")

})

# .nearestValidNamedChoice ----

test_that(".nearestValidNamedChoice handles vocal typos", {

    reservoir_types <- vapply(reservoir, .fullName, character(1))
    names(reservoir_types) <- vapply(reservoir, class, character(1))

    out <- .nearestValidNamedChoice("reddimplot", reservoir_types)
    expect_identical(out, c(RedDimPlot = "Reduced dimension plot"))

})

# .colorByChoices ----

test_that(".colorByChoices works", {

    out <- .colorByChoices("None", sce)
    expect_identical(out, character(0L))

    out <- .colorByChoices("Column data", sce)
    expect_identical(out, .get_common_info(sce, "ColumnDotPlot")$valid.colData.names)

    out <- .colorByChoices("Row data", sce)
    expect_identical(out, .get_common_info(sce, "RowDotPlot")$valid.rowData.names)

    out <- .colorByChoices("Feature name", sce)
    expectedValue <- seq_len(nrow(sce))
    names(expectedValue) <- rownames(sce)
    expect_identical(out, expectedValue)

    out <- .colorByChoices("Sample name", sce)
    expectedValue <- seq_len(ncol(sce))
    names(expectedValue) <- colnames(sce)
    expect_identical(out, expectedValue)

})
