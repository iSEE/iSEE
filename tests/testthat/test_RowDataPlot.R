# This tests the various class set-up methods.
# library(testthat); library(iSEE); source('setup_sce.R'); source('test_RowDataPlot.R')

test_that(".refineParameters identifies impossible RowDataPlot", {
    # Wiping out the metadata to trigger the NULL.
    copy <- sce
    rowData(copy) <- DataFrame(row.names = rownames(copy))

    x <- RowDataPlot()
    copy <- .cacheCommonInfo(x, copy)
    expect_warning(out <- .refineParameters(x, copy),
        "no valid y-axis 'rowData' fields for 'RowDataPlot'", fixed=TRUE)
    expect_null(out)

    # Making up a class to meet damn coverage targets.
    setClass("RowDataPlot342", contains="RowDataPlot")
    setMethod(".allowableXAxisChoices", "RowDataPlot342", function(x) character(0))
    x2 <- as(x, "RowDataPlot342")

    copy <- sce
    copy <- .cacheCommonInfo(x2, copy)
    expect_warning(out <- .refineParameters(x2, copy),
        "no valid x-axis 'rowData' fields for 'RowDataPlot342'", fixed=TRUE)
    expect_null(out)
})

test_that(".generateDotPlotData works with row selections on the x-axis", {
    x <- RowDataPlot()
    x[[iSEEslots$rowDataXAxis]] <- iSEE:::.rowDataXAxisSelectionsTitle

    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    # First trying empty.
    env <- new.env()
    env$se <- sce
    out <- .generateDotPlotData(x, env)
    expect_match(out$commands["x"], "multiSelectionToFactor.list") 
    expect_true(unique(env$plot.data$X)=="unselected")

    # Now trying with something inside.
    env$row_selected <- list(active=head(rownames(sce)))
    out <- .generateDotPlotData(x, env)
    expect_match(out$commands["x"], "multiSelectionToFactor.*row_selected") 
    expect_true(all(head(env$plot.data$X)=='active'))
    expect_true(all(tail(env$plot.data$X, -6)=='unselected'))
})

test_that(".multiSelectionInvalidated works correctly", {
    expect_false(.multiSelectionInvalidated(RowDataPlot()))
    expect_true(.multiSelectionInvalidated(RowDataPlot(XAxis="Row selection")))
    expect_true(.multiSelectionInvalidated(RowDataPlot(FacetRowBy="Row selection")))
    expect_true(.multiSelectionInvalidated(RowDataPlot(FacetColumnBy="Row selection")))
})
