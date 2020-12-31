# This tests the show method.
# library(testthat); library(iSEE); source("test_show.R")

test_that("show methods works as expected", {
    X <- ColumnDataPlot()
    expect_output(show(X), "ColumnDataPlot")

    X <- ColumnDataPlot(BrushData=list(A=1))
    expect_output(show(X), "list of length 1")

    X <- ColumnDataTable(HiddenColumns=LETTERS)
    expect_output(show(X), "+23 more")

    test <- setClass("Dummy", contains="Panel", slots=c(thing='SummarizedExperiment'))
    expect_output(show(test()), "SummarizedExperiment")
})
