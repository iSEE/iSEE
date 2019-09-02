context("groupable")

# .which_numeric ----

test_that(".which_numeric is robust in the absence of columns", {

    out <- .which_numeric(DataFrame())
    expect_identical(out, integer(0L))

})

