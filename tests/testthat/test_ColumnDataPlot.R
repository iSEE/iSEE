# This tests the various class set-up methods.
# library(testthat); library(iSEE); source('setup_sce.R'); source('test_ColumnDataPlot.R')

test_that(".refineParameters identifies impossible ColumnDataPlot", {
    # Wiping out the metadata to trigger the NULL.
    copy <- sce
    colData(copy) <- DataFrame(row.names = colnames(copy))

    x <- ColumnDataPlot()
    copy <- .cacheCommonInfo(x, copy)
    expect_warning(out <- .refineParameters(x, copy),
        "no valid y-axis 'colData' fields for 'ColumnDataPlot'", fixed=TRUE)
    expect_null(out)

    # Making up a class to meet damn coverage targets.
    setClass("ColumnDataPlot342", contains="ColumnDataPlot")
    setMethod(".allowableXAxisChoices", "ColumnDataPlot342", function(x, se) character(0))
    x2 <- as(x, "ColumnDataPlot342")

    copy <- sce
    copy <- .cacheCommonInfo(x2, copy)
    expect_warning(out <- .refineParameters(x2, copy),
        "no valid x-axis 'colData' fields for 'ColumnDataPlot342'", fixed=TRUE)
    expect_null(out)
})

test_that(".generateDotPlotData works with column selections on the x-axis", {
    x <- ColumnDataPlot()
    x[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisSelectionsTitle

    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    # First trying empty.
    env <- new.env()
    env$se <- sce
    out <- .generateDotPlotData(x, env)
    expect_match(out$commands["x"], "multiSelectionToFactor.list") 
    expect_true(unique(env$plot.data$X)=="unselected")

    # Now trying with something inside.
    env$col_selected <- list(active=head(colnames(sce)))
    out <- .generateDotPlotData(x, env)
    expect_match(out$commands["x"], "multiSelectionToFactor.*col_selected") 
    expect_true(all(head(env$plot.data$X)=='active'))
    expect_true(all(tail(env$plot.data$X, -6)=='unselected'))
})

test_that(".multiSelectionInvalidated works correctly", {
    expect_false(.multiSelectionInvalidated(ColumnDataPlot()))
    expect_true(.multiSelectionInvalidated(ColumnDataPlot(XAxis="Column selection")))
    expect_true(.multiSelectionInvalidated(ColumnDataPlot(FacetRowBy="Column selection")))
    expect_true(.multiSelectionInvalidated(ColumnDataPlot(FacetColumnBy="Column selection")))
})
