# utils_colors.R ----
context("Color utilities")

test_that(".getPanelColor returns the expected colors", {

    x <- ColDataPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#DB0230")

    x <- ColStatTable()
    out <- .getPanelColor(x)
    expect_identical(out, "#B00258")

    x <- ComplexHeatmapPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#ABCDEF")

    x <- FeatAssayPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#7BB854")

    x <- RedDimPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#3565AA")

    x <- RowDataPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#F2B701")

    x <- RowStatTable()
    out <- .getPanelColor(x)
    expect_identical(out, "#E47E04")

    x <- SampAssayPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "#07A274")

    options(iSEE_panel_colors=c(RedDimPlot="dodgerblue"))

    x <- RedDimPlot()
    out <- .getPanelColor(x)
    expect_identical(out, "dodgerblue")
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

    pObjects$cached <- list(RedDimPlot1=cachedInfo)

    out <- .retrieveOutput("RedDimPlot1", sce, pObjects, rObjects)
    expect_identical(out, cachedInfo)
    expect_identical(out$commands, cachedInfo$commands)
    expect_identical(out$contents, cachedInfo$contents)

})

test_that(".requestUpdate updates rObjects", {

    rObjects <- new.env()

    rObjects$modified <- list()

    .requestUpdate("RedDimPlot1", rObjects)

    expect_identical(rObjects$modified, list(RedDimPlot1 = character(0)))
})

test_that(".requestCleanUpdate updates rObjects", {

    pObjects <- new.env()
    rObjects <- new.env()

    pObjects$memory <- list()

    x <- RedDimPlot()
    x[[iSEE:::.brushData]] <- list(xmin=1, xmax=2, ymin=1, ymax=2)
    x[[iSEE:::.multiSelectHistory]] <- list(list(xmin=1, xmax=2, ymin=1, ymax=2))
    pObjects$memory <- list(RedDimPlot1=x)

    .requestCleanUpdate("RedDimPlot1", pObjects, rObjects)

    expect_identical(pObjects$memory$RedDimPlot1[[iSEE:::.brushData]], list())
    expect_identical(pObjects$memory$RedDimPlot1[[iSEE:::.multiSelectHistory]], list())
})

test_that(".requestActiveSelectionUpdate updates rObjects", {

    rObjects <- new.env()

    rObjects$modified <- list()

    .requestActiveSelectionUpdate("RedDimPlot1", rObjects, update_output = TRUE)

    expect_identical(rObjects$RedDimPlot1_INTERNAL_multi_select, 2L)
    expect_identical(rObjects$modified, list(RedDimPlot1 = "Reactivated"))

    .requestActiveSelectionUpdate("RedDimPlot1", rObjects, update_output = FALSE)

    expect_identical(rObjects$RedDimPlot1_INTERNAL_multi_select, 3L)
    expect_identical(rObjects$modified, list(RedDimPlot1 = c("Reactivated", "Norender")))
})

test_that(".trackSingleSelection forces evaluation of .flagSingleSelect", {

    rObjects <- new.env()

    panel_name <- "RedDimPlot1"
    rObjects[[paste0(panel_name, "_", iSEE:::.flagSingleSelect)]] <- "forced output"

    out <- .trackSingleSelection("RedDimPlot1", rObjects)
    expect_identical(out, "forced output")
})

test_that(".trackMultiSelection forces evaluation of .flagMultiSelect", {

    rObjects <- new.env()

    panel_name <- "RedDimPlot1"
    rObjects[[paste0(panel_name, "_", iSEE:::.flagMultiSelect)]] <- "forced output"

    out <- .trackMultiSelection("RedDimPlot1", rObjects)
    expect_identical(out, "forced output")
})

test_that(".trackRelinkedSelection forces evaluation of .flagRelinkedSelect", {

    rObjects <- new.env()

    panel_name <- "RedDimPlot1"
    rObjects[[paste0(panel_name, "_", iSEE:::.flagRelinkedSelect)]] <- "forced output"

    out <- .trackRelinkedSelection("RedDimPlot1", rObjects)
    expect_identical(out, "forced output")
})

test_that(".safe_reactive_bump reset counter to 0 at a threhsold", {

    rObjects <- new.env()

    rObjects$counter <- 8L

    .safe_reactive_bump(rObjects, "counter", 10L)
    expect_identical(rObjects$counter, 9L)

    .safe_reactive_bump(rObjects, "counter", 10L)
    expect_identical(rObjects$counter, 0L)

})

test_that(".increment_counter", {

    counter <- 8L

    out <- .increment_counter(counter, max = 10L)
    expect_identical(out, 9L)

    out <- .increment_counter(counter, max = 9L)
    expect_identical(out, 0L)

})

# utils_class.R ----

context("Class utilities")

test_that(".single_string_error detects issues", {

    msg <- character(0)

    x <- RedDimPlot()
    x[[iSEE:::.colorByField]] <- character(0)

    out <- .single_string_error(msg, x, fields = iSEE:::.colorByField)
    expect_identical(out, "'ColorBy' should be a single string for 'RedDimPlot'")

})

test_that(".valid_logical_error detects issues", {

    msg <- character(0)

    x <- RedDimPlot()
    x[[iSEE:::.visualParamBoxOpen]] <- NA

    out <- .valid_logical_error(msg, x, fields = iSEE:::.visualParamBoxOpen)
    expect_identical(out, "'VisualBoxOpen' should be a non-NA logical scalar for 'RedDimPlot'")

})

test_that(".valid_string_error detects issues", {

    msg <- character(0)

    x <- RedDimPlot()
    x[[iSEE:::.colorByDefaultColor]] <- c("a", "b")

    out <- .valid_string_error(msg, x, fields = iSEE:::.colorByDefaultColor)
    expect_identical(out, "'ColorByDefaultColor' should be a non-NA string for 'RedDimPlot'")

})

test_that(".allowable_choice_error detects issues", {

    msg <- character(0)

    x <- RedDimPlot()
    x[[iSEE:::.selectEffect]] <- "other"

    out <- .allowable_choice_error(msg, x, .selectEffect, c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))
    expect_identical(out, "'SelectEffect' for 'RedDimPlot' should be one of 'Restrict', 'Color', 'Transparent'")

})

test_that(".multiple_choice_error detects issues", {

    msg <- character(0)

    x <- RedDimPlot()
    x[[iSEE:::.visualParamChoice]] <- "other"

    out <- .multiple_choice_error(msg, x, .visualParamChoice,
                                  c(.visualParamChoiceColorTitle, .visualParamChoiceShapeTitle, .visualParamChoicePointTitle,
                                    .visualParamChoiceFacetTitle, .visualParamChoiceOtherTitle))
    expect_identical(out, "values of 'VisualChoices' for 'RedDimPlot' should be in 'Color', 'Shape', 'Points', 'Facets', 'Other'")

})

test_that(".valid_number_error detects issues", {

    msg <- character(0)

    x <- RedDimPlot()
    x[[iSEE:::.selectTransAlpha]] <- 2

    out <- .valid_number_error(msg, x, .selectTransAlpha, lower=0, upper=1)
    expect_identical(out, "'SelectAlpha' for 'RedDimPlot' should be a numeric scalar in [0, 1]")

})
