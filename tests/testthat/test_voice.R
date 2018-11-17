
test_that("numbers can be numeralized from text", {

    out <- .digitalizeNumbers("one")
    expect_identical(out, c("one"=1))

    out <- .digitalizeNumbers("two")
    expect_identical(out, c("two"=2))

    # allow some vocal typos
    out <- .digitalizeNumbers("to")
    expect_identical(out, c("to"=2))

    out <- .digitalizeNumbers("too")
    expect_identical(out, c("too"=2))

})


test_that("vocal typos are handled", {

    out <- .nearestPanelType("reduce dimension plus")
    expect_identical(out, c(redDimPlot="Reduced dimension plot"))

    out <- .nearestPanelType("row statistics table")
    expect_identical(out, c(rowStatTable = "Row statistics table"))

    out <- .nearestPanelType("row statistics plot")
    expect_identical(out, c(rowStatTable = "Row statistics table")) # NOTE: closer than row data plot
})
