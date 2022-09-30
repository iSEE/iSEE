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
    expect_identical(out, HTML("<strong>SRR2140028</strong><br />NREADS: 13743900"))
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
    expect_identical(out, HTML("<strong>Lamp5</strong><br />num_cells: 310"))
})