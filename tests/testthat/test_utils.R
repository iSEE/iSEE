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
