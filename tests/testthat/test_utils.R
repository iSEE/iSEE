# utils_colors.R ----
context("Color utilities")

test_that(".getPanelColor returns the expected colors", {

    x <- SampleDataPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#DB0230")

    x <- ColumnDataTable()
    out <- .getPanelColor(x)
    expect_identical(out, "#B00258")

    x <- ComplexHeatmapPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#440154FF")

    x <- FeatureAssayPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#7BB854")

    x <- ReducedDimensionPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#3565AA")

    x <- FeatureDataPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#F2B701")

    x <- RowDataTable()
    out <- .getPanelColor(x)
    expect_identical(out, "#E47E04")

    x <- SampleAssayPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#07A274")

    options(iSEE_panel_colors=c(ReducedDimensionPlot="dodgerblue"))

    x <- ReducedDimensionPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "dodgerblue")

})

# utils_class.R ----
context("Class utilities")

test_that(".single_string_error detects issues", {

    msg <- character(0)

    x <- ReducedDimensionPlot()
    x[[iSEE:::.colorByField]] <- character(0)

    out <- iSEE:::.single_string_error(msg, x, fields = iSEE:::.colorByField)
    expect_identical(out, "'ColorBy' should be a single string for 'ReducedDimensionPlot'")

})

test_that(".valid_logical_error detects issues", {

    msg <- character(0)

    x <- ReducedDimensionPlot()
    x[[iSEE:::.visualParamBoxOpen]] <- NA

    out <- iSEE:::.valid_logical_error(msg, x, fields = iSEE:::.visualParamBoxOpen)
    expect_identical(out, "'VisualBoxOpen' should be a non-NA logical scalar for 'ReducedDimensionPlot'")

})

test_that(".valid_string_error detects issues", {

    msg <- character(0)

    x <- ReducedDimensionPlot()
    x[[iSEE:::.colorByDefaultColor]] <- c("a", "b")

    out <- iSEE:::.valid_string_error(msg, x, fields = iSEE:::.colorByDefaultColor)
    expect_identical(out, "'ColorByDefaultColor' should be a non-NA string for 'ReducedDimensionPlot'")

})

test_that(".allowable_choice_error detects issues", {

    msg <- character(0)

    x <- ReducedDimensionPlot()
    x[[iSEE:::.selectEffect]] <- "other"

    out <- iSEE:::.allowable_choice_error(msg, x, .selectEffect, c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))
    expect_identical(out, "'SelectEffect' for 'ReducedDimensionPlot' should be one of 'Restrict', 'Color', 'Transparent'")

})

test_that(".multiple_choice_error detects issues", {

    msg <- character(0)

    x <- ReducedDimensionPlot()
    x[[iSEE:::.visualParamChoice]] <- "other"

    out <- .multiple_choice_error(msg, x, .visualParamChoice,
        c(.visualParamChoiceColorTitle, .visualParamChoiceShapeTitle, .visualParamChoicePointTitle, .visualParamChoiceFacetTitle, .visualParamChoiceOtherTitle))
    expect_identical(out, "values of 'VisualChoices' for 'ReducedDimensionPlot' should be in 'Color', 'Shape', 'Points', 'Facets', 'Other'")

})

test_that(".valid_number_error detects issues", {

    msg <- character(0)

    x <- ReducedDimensionPlot()
    x[[iSEE:::.selectTransAlpha]] <- 2

    out <- iSEE:::.valid_number_error(msg, x, .selectTransAlpha, lower=0, upper=1)
    expect_identical(out, "'SelectAlpha' for 'ReducedDimensionPlot' should be a numeric scalar in [0, 1]")

})

# utils_reactive.R ----
context("Reactive utilities")

test_that(".retrieveOutput detects cached panels", {

    pObjects <- new.env()
    rObjects <- new.env()

    cachedInfo <- list(
        commands = c("ggplot()"),
        contents = data.frame()
    )

    pObjects$cached <- list(ReducedDimensionPlot1=cachedInfo)

    out <- .retrieveOutput("ReducedDimensionPlot1", sce, pObjects, rObjects)
    expect_identical(out, cachedInfo)
    expect_identical(out$commands, cachedInfo$commands)
    expect_identical(out$contents, cachedInfo$contents)

})

test_that(".requestUpdate updates rObjects", {

    rObjects <- new.env()
    rObjects$modified <- list()

    .requestUpdate("ReducedDimensionPlot1", rObjects)

    expect_identical(rObjects$modified, list(ReducedDimensionPlot1 = character(0)))
})

test_that(".requestCleanUpdate updates rObjects", {

    pObjects <- new.env()
    pObjects$memory <- list()
    rObjects <- new.env()

    x <- ReducedDimensionPlot()
    x[[iSEE:::.brushData]] <- list(xmin=1, xmax=2, ymin=1, ymax=2)
    x[[iSEE:::.multiSelectHistory]] <- list(list(xmin=1, xmax=2, ymin=1, ymax=2))
    pObjects$memory <- list(ReducedDimensionPlot1=x)

    .requestCleanUpdate("ReducedDimensionPlot1", pObjects, rObjects)

    expect_identical(pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]], list())
    expect_identical(pObjects$memory$ReducedDimensionPlot1[[iSEE:::.multiSelectHistory]], list())
})

test_that(".requestActiveSelectionUpdate updates rObjects", {

    rObjects <- new.env()
    rObjects$modified <- list()

    .requestActiveSelectionUpdate("ReducedDimensionPlot1", rObjects, update_output = TRUE)

    expect_identical(rObjects$ReducedDimensionPlot1_INTERNAL_multi_select, 2L)
    expect_identical(rObjects$modified, list(ReducedDimensionPlot1 = "Reactivated"))

    .requestActiveSelectionUpdate("ReducedDimensionPlot1", rObjects, update_output = FALSE)

    expect_identical(rObjects$ReducedDimensionPlot1_INTERNAL_multi_select, 3L)
    expect_identical(rObjects$modified, list(ReducedDimensionPlot1 = c("Reactivated", "Norender")))
})

test_that(".trackSingleSelection forces evaluation of .flagSingleSelect", {

    rObjects <- new.env()

    panel_name <- "ReducedDimensionPlot1"
    rObjects[[paste0(panel_name, "_", iSEE:::.flagSingleSelect)]] <- "forced output"

    out <- .trackSingleSelection("ReducedDimensionPlot1", rObjects)
    expect_identical(out, "forced output")
})

test_that(".trackMultiSelection forces evaluation of .flagMultiSelect", {

    rObjects <- new.env()

    panel_name <- "ReducedDimensionPlot1"
    rObjects[[paste0(panel_name, "_", iSEE:::.flagMultiSelect)]] <- "forced output"

    out <- .trackMultiSelection("ReducedDimensionPlot1", rObjects)
    expect_identical(out, "forced output")
})

test_that(".trackRelinkedSelection forces evaluation of .flagRelinkedSelect", {

    rObjects <- new.env()

    panel_name <- "ReducedDimensionPlot1"
    rObjects[[paste0(panel_name, "_", iSEE:::.flagRelinkedSelect)]] <- "forced output"

    out <- .trackRelinkedSelection("ReducedDimensionPlot1", rObjects)
    expect_identical(out, "forced output")
})

test_that(".safe_reactive_bump reset counter to 0 at a threhsold", {

    rObjects <- new.env()
    rObjects$counter <- 8L

    iSEE:::.safe_reactive_bump(rObjects, "counter", 10L)
    expect_identical(rObjects$counter, 9L)

    iSEE:::.safe_reactive_bump(rObjects, "counter", 10L)
    expect_identical(rObjects$counter, 0L)

})

test_that(".increment_counter works", {

    counter <- 8L

    out <- iSEE:::.increment_counter(counter, max = 10L)
    expect_identical(out, 9L)

    out <- iSEE:::.increment_counter(counter, max = 9L)
    expect_identical(out, 0L)

})

test_that(".safe_nonzero_range works", {

    out <- iSEE:::.safe_nonzero_range(range = c(3, 4), centered = FALSE)
    expect_identical(out, c(3, 4))

    out <- iSEE:::.safe_nonzero_range(range = c(2, 2), centered = FALSE)
    expect_identical(out, c(2, 3))

    out <- iSEE:::.safe_nonzero_range(range = c(2, 2), centered = TRUE)
    expect_identical(out, c(1, 3))

})
