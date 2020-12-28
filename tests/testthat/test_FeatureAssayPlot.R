# This tests the various class set-up methods.
# library(testthat); library(iSEE); source('setup_sce.R'); source('test_FeatureAssayPlot.R')

test_that(".refineParameters identifies impossible FeatureAssayPlot", {
    x <- FeatureAssayPlot()

    sce0 <- sce[0, ]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(out <- .refineParameters(x, sce0),
        "no rows available for plotting 'FeatureAssayPlot'", fixed=TRUE)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(out <- .refineParameters(x, sce0),
        "no valid 'assays' for plotting 'FeatureAssayPlot'", fixed=TRUE)
    expect_null(out)

    sce0 <- sce
    colData(sce0) <- colData(sce0)[,0]
    x[[iSEE:::.featAssayXAxis]] <- iSEE:::.featAssayXAxisColDataTitle
    sce0 <- .cacheCommonInfo(x, sce0)
    out <- .refineParameters(x, sce0)
    expect_identical(out[[iSEE:::.featAssayXAxis]], iSEE:::.featAssayXAxisNothingTitle)
})

test_that(".generateDotPlotData works with column selections on the x-axis", {
    x <- FeatureAssayPlot()
    x[[iSEE:::.featAssayXAxis]] <- iSEE:::.featAssayXAxisSelectionsTitle

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
    expect_false(.multiSelectionInvalidated(FeatureAssayPlot()))
    expect_true(.multiSelectionInvalidated(FeatureAssayPlot(XAxis="Column selection")))

    # Dispatches correctly to the parent method.
    expect_true(.multiSelectionInvalidated(FeatureAssayPlot(FacetRowBy="Column selection")))
})
