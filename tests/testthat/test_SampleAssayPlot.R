# This tests the various class set-up methods.
# library(testthat); library(iSEE); source('setup_sce.R'); source('test_SampleAssayPlot.R')

test_that(".refineParameters identifies impossible SampleAssayPlot", {
    x <- SampleAssayPlot()

    sce0 <- sce[, 0]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(out <- .refineParameters(x, sce0),
        "no columns for plotting 'SampleAssayPlot'", fixed=TRUE)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(out <- .refineParameters(x, sce0),
        "no named 'assays' for plotting 'SampleAssayPlot'", fixed=TRUE)
    expect_null(out)

    sce0 <- sce
    rowData(sce0) <- rowData(sce0)[,0]
    x[[iSEE:::.sampAssayXAxis]] <- iSEE:::.sampAssayXAxisRowDataTitle
    sce0 <- .cacheCommonInfo(x, sce0)
    out <- .refineParameters(x, sce0)
    expect_identical(out[[iSEE:::.sampAssayXAxis]], iSEE:::.sampAssayXAxisNothingTitle)
})

test_that(".generateDotPlotData works with row selections on the x-axis", {
    x <- SampleAssayPlot()
    x[[iSEE:::.sampAssayXAxis]] <- iSEE:::.sampAssayXAxisSelectionsTitle

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
    expect_false(.multiSelectionInvalidated(SampleAssayPlot()))
    expect_true(.multiSelectionInvalidated(SampleAssayPlot(XAxis="Row selection")))

    # Dispatches correctly to the parent method.
    expect_true(.multiSelectionInvalidated(SampleAssayPlot(FacetRowBy="Row selection")))
})
