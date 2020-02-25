context("subset_points")

# This tests the subsetPointsByGrid function.
# library(testthat); library(iSEE); source("test_subset_grid.R")

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
    expect_error(subsetPointsByGrid(1L, integer(0L)), "must be of the same length")
})

set.seed(110001)
test_that("subsetPointsByGrid works correctly with grouping", {
    x <- rnorm(20000)
    y <- rnorm(20000)
    g <- sample(letters[1:5], length(x), replace=TRUE)

    chosen <- subsetPointsByGrid(x, y, resolution=200, grouping=g)
    for (ug in unique(g)) {
        keep <- g==ug
        expect_identical(
            chosen[keep],
            subsetPointsByGrid(x[keep], y[keep], resolution=200)
        )
    }

    multires <- c(a=100, b=200, c=300, d=50, e=20)
    chosen <- subsetPointsByGrid(x, y, resolution=multires, grouping=g)
    for (ug in unique(g)) {
        keep <- g==ug
        expect_identical(
            chosen[keep],
            subsetPointsByGrid(x[keep], y[keep], resolution=multires[[ug]])
        )
    }

    expect_error(subsetPointsByGrid(x, y, grouping=1), "must be of the same length")
    expect_error(subsetPointsByGrid(x, y, grouping=g, resolution=c(A=1, B=2)), "named with all levels")
})

