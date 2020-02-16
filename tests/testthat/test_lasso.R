# Tests for the lasso updating functionality.
# library(testthat); library(iSEE); source("test_lasso.R")

context("lasso")

DATA_POINTS <- data.frame(
    X=seq(1.05, 2.95, 0.1),
    Y=rep(1.5, 20)
)

stopifnot(
  require(mgcv)
)

# .update_lasso ----

test_that(".update_lasso works with a first click", {

    out <- iSEE:::.update_lasso(CLICK_WAYPOINT, NULL)

    expect_identical(out$closed, FALSE)

    expect_identical(out$coord, matrix(c(CLICK_WAYPOINT$x, CLICK_WAYPOINT$y), ncol=2))

})

test_that(".update_lasso works with closing click", {

    out <- iSEE:::.update_lasso(CLICK_CLOSING, LASSO_OPEN)

    expect_identical(out$closed, TRUE)

    expected_coord <- rbind(LASSO_OPEN$coord, matrix(c(CLICK_CLOSING$x, CLICK_CLOSING$y), ncol=2))
    expect_identical(out$coord, expected_coord)

})

test_that(".update_lasso works with a non-closing waypoint", {

    out <- iSEE:::.update_lasso(CLICK_WAYPOINT, LASSO_OPEN)

    expect_identical(out$closed, FALSE)

    expected_coord <- rbind(LASSO_OPEN$coord, matrix(c(CLICK_WAYPOINT$x, CLICK_WAYPOINT$y), ncol=2))
    expect_identical(out$coord, expected_coord)

})

# lassoPoints ----

test_that("lassoPoints throws an error for open lassos", {

    expect_error(
        lassoPoints(DATA_POINTS, LASSO_OPEN),
        "cannot find points in open lasso",
        fixed=TRUE
    )

})

test_that("lassoPoints detects points in or out of lassos", {

    out <- lassoPoints(DATA_POINTS, CLOSED_LASSO)

    mgcvOut <- in.out(CLOSED_LASSO$coord, as.matrix(DATA_POINTS))
    expect_identical(out, DATA_POINTS[mgcvOut, ])

})

test_that("lassoPoints detects points within lasso in selected facet", {

    CLOSED_LASSO$mapping$panelvar1 <- "FacetX"
    CLOSED_LASSO$mapping$panelvar2 <- "FacetY"
    CLOSED_LASSO$panelvar1 <- "inX"
    CLOSED_LASSO$panelvar2 <- "inY"
    # all data points are in the facet with lasso
    DATA_POINTS$FacetX <- "inX"
    DATA_POINTS$FacetY <- "inY"

    out <- lassoPoints(DATA_POINTS, CLOSED_LASSO)

    mgcvOut <- in.out(CLOSED_LASSO$coord, as.matrix(DATA_POINTS[, c("X", "Y")]))
    expect_identical(out, DATA_POINTS[mgcvOut, ])

})

test_that("lassoPoints ignores points within lasso in other facets", {

    CLOSED_LASSO$mapping$panelvar1 <- "FacetX"
    CLOSED_LASSO$mapping$panelvar2 <- "FacetY"
    CLOSED_LASSO$panelvar1 <- "inX"
    CLOSED_LASSO$panelvar2 <- "inY"
    # all data points are in a different facet than the one with lasso
    DATA_POINTS$FacetX <- "outX"
    DATA_POINTS$FacetY <- "inY"

    out <- lassoPoints(DATA_POINTS, CLOSED_LASSO)

    expect_identical(nrow(out), 0L)

})

test_that (".multiSelectionHasActive detects a closed lasso", {

    redDimArgs <- ReducedDimPlot(BrushData=LASSO_CLOSED)
    expect_true(iSEE:::.multiSelectionHasActive(redDimArgs))

    redDimArgs <- ReducedDimPlot(BrushData=LASSO_OPEN)
    expect_false(iSEE:::.multiSelectionHasActive(redDimArgs))

})
