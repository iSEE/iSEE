# library(testthat); library(iSEE); source("test_groupable.R")

context("groupable")

test_that("grouping evaluation functions work correctly", {
    X <- data.frame(A=LETTERS[1:10], B=1L, C=0.1, D=factor(letters[1:10]))
    expect_equivalent(iSEE:::.which_groupable(X), c(1L, 4L))
    expect_equivalent(iSEE:::.which_numeric(X), c(2L, 3L))

    # Too many levels to consider groupable.
    X <- data.frame(A=LETTERS, B=1L)
    expect_equivalent(iSEE:::.which_groupable(X), integer(0))
    expect_equivalent(iSEE:::.which_numeric(X), 2L)
})

test_that("grouping functions are robust in the absence of columns", {
    out <- iSEE:::.which_numeric(DataFrame())
    expect_equivalent(out, integer(0L))

    out <- iSEE:::.which_groupable(DataFrame())
    expect_equivalent(out, integer(0L))
})

test_that("finding atomic fields works correctly", {
    X <- DataFrame(A=LETTERS[1:10], B=1L, C=0.1, D=factor(letters[1:10]))
    expect_identical(iSEE:::.find_atomic_fields(X), colnames(X))

    X <- DataFrame(A=Rle(1:5), B=2)
    expect_identical(iSEE:::.find_atomic_fields(X), "B")
})
