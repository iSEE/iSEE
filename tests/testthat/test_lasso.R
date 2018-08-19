
stopifnot(
  require(mgcv)
)

LASSO_OPEN <- list(
    lasso=NULL,
    closed=FALSE,
    panelvar1=NULL, panelvar2=NULL,
    mapping=list(x="X", y="Y"),
    coord=matrix(c(1, 2, 2, 1, 1, 1, 2, 2), ncol=2))

CLOSING_CLICK <- list(
    x=1, y=1,
    mapping=list(x="X", y="Y"),
    domain=list(left=-14.1, right=10.9, bottom=-12, top=16.4),
    range=list(left=38.7, right=379, bottom=466, top=23.8),
    log=list(x=NULL, y=NULL)
)

WAYPOINT_CLICK <- list(
    x=3, y=4,
    mapping=list(x="X", y="Y"),
    domain=list(left=-14.1, right=10.9, bottom=-12, top=16.4),
    range=list(left=38.7, right=379, bottom=466, top=23.8),
    log=list(x=NULL, y=NULL)
)

DATA_POINTS <- data.frame(
    X=seq(1.05, 2.95, 0.1),
    Y=rep(1.5, 20)
)

# .update_lasso ----

test_that(".update_lasso works with a first click", {

    out <- iSEE:::.update_lasso(WAYPOINT_CLICK, NULL)

    expect_identical(out$closed, FALSE)

    expect_identical(out$coord, matrix(c(WAYPOINT_CLICK$x, WAYPOINT_CLICK$y), ncol=2))

})

test_that(".update_lasso works with closing click", {

    out <- iSEE:::.update_lasso(CLOSING_CLICK, LASSO_OPEN)

    expect_identical(out$closed, TRUE)

    expected_coord <- rbind(LASSO_OPEN$coord, matrix(c(CLOSING_CLICK$x, CLOSING_CLICK$y), ncol=2))
    expect_identical(out$coord, expected_coord)

})

test_that(".update_lasso works with a non-closing waypoint", {

    out <- iSEE:::.update_lasso(WAYPOINT_CLICK, LASSO_OPEN)

    expect_identical(out$closed, FALSE)

    expected_coord <- rbind(LASSO_OPEN$coord, matrix(c(WAYPOINT_CLICK$x, WAYPOINT_CLICK$y), ncol=2))
    expect_identical(out$coord, expected_coord)

})

# Run this _after_ the unit tests above
CLOSED_LASSO <- iSEE:::.update_lasso(CLOSING_CLICK, LASSO_OPEN)

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

    DATA_POINTS$FacetX <- "inX"
    DATA_POINTS$FacetY <- "inY"

    out <- lassoPoints(DATA_POINTS, CLOSED_LASSO)

    mgcvOut <- in.out(CLOSED_LASSO$coord, as.matrix(DATA_POINTS[, c("X", "Y")]))
    expect_identical(out, DATA_POINTS[mgcvOut, ])

})

test_that("lassoPoints ignores points within lasso in other facets", {

    CLOSED_LASSO$mapping$panelvar1 <- "FacetX"
    CLOSED_LASSO$mapping$panelvar2 <- "FacetY"
    CLOSED_LASSO$panelvar1 <- "selectedX"
    CLOSED_LASSO$panelvar2 <- "selectedY"

    DATA_POINTS$FacetX <- "outX"
    DATA_POINTS$FacetY <- "inY"

    out <- lassoPoints(DATA_POINTS, CLOSED_LASSO)

    expect_identical(nrow(out), 0L)

})
