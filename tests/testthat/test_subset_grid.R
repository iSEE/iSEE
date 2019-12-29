context("subset_points")

# This tests the subsetPointsByGrid function.
# library(testthat); library(iSEE); source("test_subset_points.R")

set.seed(110001)
test_that("subsetPointsByGrid works correctly", {
    x <- rnorm(20000)
    y <- rnorm(20000)
    chosen <- subsetPointsByGrid(x, y, resolution=200)
    expect_true(sum(chosen) < length(chosen))
    expect_true(sum(chosen) > 1L)

    # Checking the correctness of the result.
    xid <- as.integer(cut(x, 200))
    yid <- as.integer(cut(y, 200))
    combined <- sprintf("%i-%i", xid, yid)
    ref <- !duplicated(combined, fromLast=TRUE)
    expect_identical(ref, chosen)

    # Checking extremes.
    chosen.low <- subsetPointsByGrid(x, y, resolution=20)
    expect_true(sum(chosen) > sum(chosen.low))

    chosen.high <- subsetPointsByGrid(x, y, resolution=2000)
    expect_true(sum(chosen.high) > sum(chosen))

    # Checking silly inputs.
    expect_identical(suppressWarnings(subsetPointsByGrid(integer(0L), integer(0L))), logical(0L))
})
