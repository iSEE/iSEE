# This tests the various calls to handle class deprecation.
# library(testthat); library(iSEE); source("test_deprecated.R")

test_that("old facet constructor requests are honored", {
    expect_warning(x <- ColumnDataPlot(FacetByRow="WHEE", FacetByColumn="BLAH"), "deprecated")
    expect_identical(x[["FacetRowBy"]], "Column data")
    expect_identical(x[["FacetRowByColData"]], "WHEE")
    expect_identical(x[["FacetColumnBy"]], "Column data")
    expect_identical(x[["FacetColumnByColData"]], "BLAH")

    expect_warning(x <- RowDataPlot(FacetByRow="WHEE", FacetByColumn="BLAH"), "deprecated")
    expect_identical(x[["FacetRowBy"]], "Row data")
    expect_identical(x[["FacetRowByRowData"]], "WHEE")
    expect_identical(x[["FacetColumnBy"]], "Row data")
    expect_identical(x[["FacetColumnByRowData"]], "BLAH")
})

test_that("old facet getter requests are honored", {
    # For ColumnDotPlots:
    x <- ColumnDataPlot()
    expect_warning(out <- x[["FacetByRow"]], "deprecated")
    expect_identical(out, "---")

    x[["FacetRowBy"]] <- "Column data"
    x[["FacetRowByColData"]] <- "WHEE"
    expect_warning(out <- x[["FacetByRow"]], "deprecated")
    expect_identical(out, "WHEE")

    expect_warning(out <- x[["FacetByColumn"]], "deprecated")
    expect_identical(out, "---")

    x[["FacetColumnBy"]] <- "Column data"
    x[["FacetColumnByColData"]] <- "WHEE"
    expect_warning(out <- x[["FacetByColumn"]], "deprecated")
    expect_identical(out, "WHEE")

    # For RowDotPlots:
    x <- RowDataPlot()
    expect_warning(out <- x[["FacetByRow"]], "deprecated")
    expect_identical(out, "---")

    x[["FacetRowBy"]] <- "Row data"
    x[["FacetRowByRowData"]] <- "WHEE"
    expect_warning(out <- x[["FacetByRow"]], "deprecated")
    expect_identical(out, "WHEE")

    expect_warning(out <- x[["FacetByColumn"]], "deprecated")
    expect_identical(out, "---")

    x[["FacetColumnBy"]] <- "Row data"
    x[["FacetColumnByRowData"]] <- "WHEE"
    expect_warning(out <- x[["FacetByColumn"]], "deprecated")
    expect_identical(out, "WHEE")
})

test_that("old facet setter requests are honored", {
    # For ColumnDotPlots:
    x <- ColumnDataPlot()
    expect_warning(x[["FacetByRow"]] <- "WHEE", "deprecated")
    expect_identical(x[["FacetRowBy"]], "Column data")
    expect_identical(x[["FacetRowByColData"]], "WHEE")

    expect_warning(x[["FacetByRow"]] <- "---", "deprecated")
    expect_identical(x[["FacetRowBy"]], "None")

    expect_warning(x[["FacetByColumn"]] <- "WHEE", "deprecated")
    expect_identical(x[["FacetColumnBy"]], "Column data")
    expect_identical(x[["FacetColumnByColData"]], "WHEE")

    expect_warning(x[["FacetByColumn"]] <- "---", "deprecated")
    expect_identical(x[["FacetColumnBy"]], "None")

    # For RowDotPlots:
    x <- RowDataPlot()
    expect_warning(x[["FacetByRow"]] <- "WHEE", "deprecated")
    expect_identical(x[["FacetRowBy"]], "Row data")
    expect_identical(x[["FacetRowByRowData"]], "WHEE")

    expect_warning(x[["FacetByRow"]] <- "---", "deprecated")
    expect_identical(x[["FacetRowBy"]], "None")

    expect_warning(x[["FacetByColumn"]] <- "WHEE", "deprecated")
    expect_identical(x[["FacetColumnBy"]], "Row data")
    expect_identical(x[["FacetColumnByRowData"]], "WHEE")

    expect_warning(x[["FacetByColumn"]] <- "---", "deprecated")
    expect_identical(x[["FacetColumnBy"]], "None")
})

test_that("old selection constructor requests are honored", {
    expect_warning(ColumnDataPlot(RowSelectionType="Union"), "deprecated")
    expect_warning(ColumnDataPlot(RowSelectionSaved=1L), "deprecated")
    expect_warning(ColumnDataPlot(ColumnSelectionType="Union"), "deprecated")
    expect_warning(ColumnDataPlot(ColumnSelectionSaved=1L), "deprecated")

    expect_warning(X <- ColumnDataPlot(SelectionEffect="Restrict"), "deprecated")
    expect_true(X[["ColumnSelectionRestrict"]])
    expect_warning(ColumnDataPlot(SelectionColor="red"), "deprecated")

    expect_warning(X <- RowDataPlot(SelectionEffect="Restrict"), "deprecated")
    expect_true(X[["RowSelectionRestrict"]])
    expect_warning(RowDataPlot(SelectionColor="red"), "deprecated")
})

test_that("old basic selection getter requests are honored", {
    x <- ColumnDataPlot()
    expect_warning(out <- x[["RowSelectionType"]], "deprecated")
    expect_identical(out, NA_character_)

    expect_warning(out <- x[["ColumnSelectionType"]], "deprecated")
    expect_identical(out, NA_character_)

    expect_warning(out <- x[["RowSelectionSaved"]], "deprecated")
    expect_identical(out, NA_integer_)

    expect_warning(out <- x[["ColumnSelectionSaved"]], "deprecated")
    expect_identical(out, NA_integer_)
})

test_that("old basic selection setter requests are honored", {
    x <- ColumnDataPlot()
    expect_warning(x[["RowSelectionType"]] <- "Union", "deprecated")
    expect_identical(x, ColumnDataPlot())

    expect_warning(x[["ColumnSelectionType"]] <- "Union", "deprecated")
    expect_identical(x, ColumnDataPlot())

    expect_warning(x[["RowSelectionSaved"]] <- 1L, "deprecated")
    expect_identical(x, ColumnDataPlot())

    expect_warning(x[["ColumnSelectionSaved"]] <- 2L, "deprecated")
    expect_identical(x, ColumnDataPlot())
})

test_that("old visual selection getter requests are honored", {
    # For ColumnDotPlots
    x <- ColumnDataPlot()
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Transparent")

    expect_warning(out <- x[["SelectionColor"]], "deprecated")
    expect_identical(out, NA_character_)

    x <- ColumnDataPlot(ColumnSelectionRestrict=TRUE)
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Restrict")

    x <- ColumnDataPlot(ColorBy="Column selection")
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Color")

    # For RowDotPlots
    x <- RowDataPlot()
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Transparent")

    expect_warning(out <- x[["SelectionColor"]], "deprecated")
    expect_identical(out, NA_character_)

    x <- RowDataPlot(RowSelectionRestrict=TRUE)
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Restrict")

    x <- RowDataPlot(ColorBy="Row selection")
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Color")

    # For ComplexHeatmapPlots
    x <- ComplexHeatmapPlot()
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Color")

    expect_warning(out <- x[["SelectionColor"]], "deprecated")
    expect_identical(out, NA_character_)

    x <- ComplexHeatmapPlot(ColumnSelectionRestrict=TRUE)
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Restrict")

    x <- ComplexHeatmapPlot(ShowColumnSelection=FALSE)
    expect_warning(out <- x[["SelectionEffect"]], "deprecated")
    expect_identical(out, "Transparent")
})

test_that("old visual selection setter requests are honored", {
    # For ColumnDotPlots
    x <- ColumnDataPlot()
    expect_warning(x[["SelectionEffect"]] <- "Restrict", "deprecated")
    expect_true(x[["ColumnSelectionRestrict"]])

    expect_warning(x[["SelectionEffect"]] <- "Color", "deprecated")
    expect_false(x[["ColumnSelectionRestrict"]])

    x <- ColumnDataPlot()
    expect_warning(x[["SelectionColor"]] <- "red", "deprecated")
    expect_identical(x, ColumnDataPlot())

    # For RowDotPlots
    x <- RowDataPlot()
    expect_warning(x[["SelectionEffect"]] <- "Restrict", "deprecated")
    expect_true(x[["RowSelectionRestrict"]])

    expect_warning(x[["SelectionEffect"]] <- "Color", "deprecated")
    expect_false(x[["RowSelectionRestrict"]])

    x <- RowDataPlot()
    expect_warning(x[["SelectionColor"]] <- "red", "deprecated")
    expect_identical(x, RowDataPlot())

    # For ComplexHeatmapPlots
    x <- ComplexHeatmapPlot()
    expect_warning(x[["SelectionEffect"]] <- "Restrict", "deprecated")
    expect_true(x[["ColumnSelectionRestrict"]])
    expect_false(x[["ShowColumnSelection"]])

    expect_warning(x[["SelectionEffect"]] <- "Color", "deprecated")
    expect_false(x[["ColumnSelectionRestrict"]])
    expect_true(x[["ShowColumnSelection"]])

    x <- ComplexHeatmapPlot()
    expect_warning(x[["SelectionColor"]] <- "red", "deprecated")
    expect_identical(x, ComplexHeatmapPlot())
})
