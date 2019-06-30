context("jitter")

# Tests jitterViolinPoints, jitterSquarePoints and related functions.
# library(testthat); library(iSEE); source("test_jitter.R")

test_that(".define_groups works correctly with multi-factor groupings", {
    nvals <- 100
    X <- Y <- integer(nvals) # these values don't matter for this function.
    g <- list(U=sample(LETTERS[1:5], nvals, replace=TRUE), V=sample(3, nvals, replace=TRUE))

    out <- iSEE:::.define_groups(X, Y, grouping=g)
    ref <- paste(g$U, g$V, sep="|")
    ref <- split(seq_along(ref), ref)
    expect_identical(out, unname(ref))

    out <- iSEE:::.define_groups(X, Y, grouping=g[1])
    ref <- split(seq_along(g$U), g$U)
    expect_identical(out, unname(ref))

    out <- iSEE:::.define_groups(X, Y, grouping=g[2])
    ref <- split(seq_along(g$V), g$V)
    expect_identical(out, unname(ref))

    # Throws a variety of errors.
    expect_error(iSEE:::.define_groups(X[1], Y), "not TRUE")
    expect_error(iSEE:::.define_groups(X[1:10], Y[1:10], grouping=g), "not TRUE")
    expect_error(iSEE:::.define_groups(X, Y, grouping=unname(g)), "not TRUE")

    # Behaves correctly with empty inputs.
    out <- iSEE:::.define_groups(X[0], Y[0], grouping=lapply(g, "[", 0))
    expect_identical(out, list())
})

test_that("jitterViolinPoints works correctly", {
    nvals <- 100L
    Y <- runif(nvals)
    X <- factor(sample(letters[1:3], nvals, replace=TRUE))
    g <- list(U=sample(LETTERS[1:5], nvals, replace=TRUE), V=sample(3, nvals, replace=TRUE))

    set.seed(0)
    Xa <- jitterViolinPoints(X, Y)
    expect_identical(length(Xa), nvals)
    expect_equal(round(Xa), as.integer(X))

    # Testing with the grouping variable.
    set.seed(0)
    Xb <- jitterViolinPoints(X, Y, grouping=g)
    expect_identical(length(Xb), nvals)
    expect_equal(round(Xb), as.integer(X))

    expect_false(isTRUE(all.equal(Xa, Xb))) # grouping has an effect.

    # Points are directly returned on X when there is only one value.
    Z <- 1:10
    Xc <- jitterViolinPoints(factor(LETTERS[Z]), Z)
    expect_equal(Z, Xc)

    # Function behaves with silly inputs.
    expect_identical(jitterViolinPoints(X[0], Y[0]), numeric(0L))
    expect_error(jitterViolinPoints(1, 1), "'X' should be a factor")
    expect_error(jitterViolinPoints(X[1], "A"), "'Y' should be numeric")
})

test_that("jitterSquarePoints works correctly", {
    nvals <- 100L
    Y <- factor(sample(4, nvals, replace=TRUE))
    X <- factor(sample(letters[1:3], nvals, replace=TRUE))
    g <- list(FacetRow=sample(LETTERS[1:5], nvals, replace=TRUE), FacetColumn=sample(3, nvals, replace=TRUE))

    set.seed(0)
    out.a <- jitterSquarePoints(X, Y)
    checker <- function(out, refX, refY) {
        expect_identical(length(out$X), nvals)
        expect_equal(round(out$X), as.integer(refX))
        expect_identical(length(out$Y), nvals)
        expect_equal(round(out$Y), as.integer(refY))
        return(NULL)
    }
    checker(out.a, X, Y)

    combinations <- paste0(X, "|", Y)
    helper <- function(combinations, summary.data, to.combine=c("X", "Y")) {
        summary.combo <- do.call(paste, c(list(sep="|"), lapply(to.combine, function(i) summary.data[[i]])))
        expect_true(all(combinations %in% summary.combo))
        for (i in seq_along(summary.combo)) {
            expect_identical(summary.data$Freq[i], sum(combinations == summary.combo[i]))
        }
        return(NULL)
    }
    helper(combinations, out.a$summary)
    expect_equal(out.a$summary$XWidth, out.a$summary$YWidth)
    expect_identical(out.a$summary$XWidth == 0, out.a$summary$Freq == 0)
    expect_equal(0, mad(out.a$summary$XWidth^2/out.a$summary$Freq, na.rm=TRUE))

    # Correctly reverts to a bar plot if X axis is only one level.
    set.seed(0)
    X0 <- factor(rep(X[1], nvals))
    out.b <- jitterSquarePoints(X0, Y)
    checker(out.b, X0, Y)

    combinations <- paste0(X0, "|", Y)
    helper(combinations, out.b$summary)
    expect_identical(length(unique(out.b$summary$YWidth)), 1L)
    expect_identical(out.b$summary$XWidth == 0, out.b$summary$Freq == 0)
    expect_equal(0, mad(out.b$summary$XWidth/out.b$summary$Freq, na.rm=TRUE))

    # ... or if the Y axis only has one level.
    set.seed(0)
    Y0 <- factor(rep(Y[1], nvals))
    out.c <- jitterSquarePoints(X, Y0)
    checker(out.c, X, Y0)

    combinations <- paste0(X, "|", Y0)
    helper(combinations, out.c$summary)
    expect_identical(length(unique(out.c$summary$XWidth)), 1L)
    expect_identical(out.c$summary$YWidth == 0, out.c$summary$Freq == 0)
    expect_equal(0, mad(out.c$summary$YWidth/out.c$summary$Freq, na.rm=TRUE))

    # Testing with a grouping variable.
    set.seed(0)
    out.d <- jitterSquarePoints(X, Y, grouping=g)
    checker(out.d, X, Y)

    combinations <- paste0(X, "|", Y, "|", g$FacetRow, "|", g$FacetColumn)
    helper(combinations, out.d$summary, to.combine=c("X", "Y", "FacetRow", "FacetColumn"))
    expect_equal(out.d$summary$XWidth, out.d$summary$YWidth)
    expect_identical(out.d$summary$XWidth == 0, out.d$summary$Freq == 0)

    # Function behaves when input is empty.
    out <- jitterSquarePoints(X[0], Y[0])
    expect_identical(out$X, numeric(0))
    expect_identical(out$Y, numeric(0))
    expect_true(all(out$summary$Freq == 0))
    expect_true(all(out$summary$XWidth == 0))
    expect_true(all(out$summary$YWidth == 0))

    expect_error(jitterSquarePoints(1, Y[1]), "'X' should be a factor")
    expect_error(jitterSquarePoints(X[1], 1), "'Y' should be a factor")
})

