# Tests for the data point hovering functionality.
# library(testthat); library(iSEE); source("setup_sce.R"); source("test_hover.R")

test_that(".getTooltipUI returns only colnames for ColumnDotPlot when not configured", {
    sce0 <- sce
    metadata(sce0) <- list()
    x <- ColumnDataPlot()
    
    out <- .getTooltipUI(x, sce0, "SRR2140028")
    expect_identical(out, "SRR2140028")
})

test_that(".getTooltipUI returns expected HTML tag for ColumnDotPlot when configured", {
    sce0 <- sce
    
    x <- ColumnDataPlot(
        TooltipColumnData = "NREADS"
    )
    
    out <- .getTooltipUI(x, sce0, "SRR2140028")
    expect_identical(out, HTML("<strong>SRR2140028</strong><br />NREADS: <i>13743900</i>"))
})

test_that(".getTooltipUI returns only rownames for RowDotPlot when not configured", {
    sce0 <- sce
    metadata(sce0) <- list()
    x <- SampleAssayPlot()
    
    out <- .getTooltipUI(x, sce0, "Lamp5")
    expect_identical(out, "Lamp5")
})

test_that(".getTooltipUI returns expected HTML tag for ColumnDotPlot when configured", {
    sce0 <- sce
    
    x <- SampleAssayPlot(
        TooltipRowData = "num_cells"
    )
    
    out <- .getTooltipUI(x, sce0, "Lamp5")
    expect_identical(out, HTML("<strong>Lamp5</strong><br />num_cells: <i>310</i>"))
})

test_that(".process_tooltip_field works for factor values", {
    out <- iSEE:::.process_tooltip_field(factor(c(dummy = "DUMMY")))
    expect_identical(out, "DUMMY")
})

test_that(".process_tooltip_field works for double values", {
    se <- SummarizedExperiment()
    out <- iSEE:::.process_tooltip_field(c(dummy = 1.1234567890))
    expect_identical(out, "1.12346(...)")
})

test_that(".process_tooltip_field works for character values", {
    se <- SummarizedExperiment()
    out <- iSEE:::.process_tooltip_field(c(dummy = "DUMMY"))
    expect_identical(out, "DUMMY")
})
