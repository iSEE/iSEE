
test_that("list updater function works correctly", {
    blah <- new("DataFrame", nrows=20L)
    blah$A <- vector("list", 20)

    # Adding an element.
    out <- iSEE:::.update_list_element(blah, 2, "A", 1:5)
    expect_type(out$A, "list")
    tmp <- blah$A
    tmp[[2]] <- 1:5
    expect_identical(tmp, out$A)

    # Adding an element of a completely different type.
    out <- iSEE:::.update_list_element(out, 5, "A", list("X"))
    expect_type(out$A, "list")
    tmp[[5]] <- list("X")
    expect_identical(tmp, out$A)

    out <- iSEE:::.update_list_element(out, 2, "A", NULL)
    out <- iSEE:::.update_list_element(out, 5, "A", NULL)
    expect_identical(blah, out)
}) 

test_that("plot feasibility checks work correctly", {
    out <- iSEE:::.check_plot_feasibility(sce)
    expect_true(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)

    # No genes.
    out <- iSEE:::.check_plot_feasibility(sce[0,])
    expect_true(out$redDimPlot)
    expect_false(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_false(out$rowStatTable)

    # No samples.
    out <- iSEE:::.check_plot_feasibility(sce[,0])
    expect_false(out$redDimPlot)
    expect_false(out$featExprPlot)
    expect_false(out$colDataPlot)
    expect_true(out$rowStatTable)

    # No colDataPlot.
    sceX <- sce
    colData(sceX) <- colData(sceX)[,0]
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_false(out$colDataPlot)
    expect_true(out$rowStatTable)

    # No reduced dimensions.
    sceX <- sce
    reducedDims(sceX) <- SimpleList()
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_false(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)

    # No assays.
    sceX <- sce
    for (field in assayNames(sceX)) { 
        assay(sceX, field) <- NULL
    }
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_false(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
})

test_that("count incrementer works correctly", {
    expect_identical(iSEE:::.increment_counter(0L), 1L) 
    expect_identical(iSEE:::.increment_counter(9999L), 0L)
})
