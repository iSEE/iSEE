# This tests the default settings
# library(iSEE); library(testthat); source('test_defaults.R')

test_that("new_defaults works", {

    out <- iSEE:::new_defaults(list(init=TRUE))

    expect_named(out, c("get", "set", "merge", "restore"))
    expect_identical(out$get(), list(init=TRUE))

    # users can create non-default keys
    expect_null(out$set(a=1))
    expect_identical(out$get(), list(init=TRUE, a=1))

    # get() can return initial defaults, irrespective of any set()
    expect_identical(out$get(default = TRUE), list(init=TRUE))

    # get() can drop names when a single name is queried
    expect_identical(out$get("a", drop = TRUE), 1)

    # get() names values anyway if there are more than 1
    expect_identical(out$get(c("a", "init"), drop = TRUE), list(a=1, init=TRUE))

    # set() doesn't do anything if no argument is given
    expect_null(out$set())
    expect_identical(out$get(), list(init=TRUE, a=1))

    # set() can be given a named list
    expect_null(out$set(list(new=TRUE)))
    expect_identical(out$get(), list(init=TRUE, a=1, new=TRUE))

    # set() can be given an empty list
    expect_null(out$set(list()))
    expect_identical(out$get(), list(init=TRUE, a=1, new=TRUE))

    # merge returns a copy merging new values and overwriting existing ones
    expect_identical(out$merge(list(b=2)), list(init=TRUE, a=1, new=TRUE, b=2))
    expect_identical(out$merge(list(a=3)), list(init=TRUE, a=3, new=TRUE))

    # restore the initial defaults
    expect_identical(out$restore(), list(init=TRUE))
})
