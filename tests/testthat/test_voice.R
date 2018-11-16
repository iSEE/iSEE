
test_that("numbers can be digitalized from text", {

    # i.e. "Reduce dimension plot one"
    out <- .digitalizeNumbers("reduce dimension plus one")
    expect_identical(out, "reduce dimension plus 1")

})


test_that("vocal typos are handled", {

    out <- .nearestPanelType("reduce dimension plus")
    expect_identical(out, "redDimPlot")

    out <- .nearestPanelType("row statistics table")

    .nearestPanelType("row statistics plot")

})
