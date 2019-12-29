context("names")

test_that("", {

    x <- c("---", b=2, c=3)
    y <- c(c=3)

    out <- .setdiffWithNames(x, y)
    expect_identical(out, c("---", b = "2"))

})
