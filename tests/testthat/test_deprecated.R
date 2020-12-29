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

test_that("old facet getter requests are honored", {
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
