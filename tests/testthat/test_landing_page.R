# Tests jitterViolinPoints, jitterSquarePoints and related functions.
# library(testthat); library(iSEE); source("test_landing_page.R")

context("Landing page")

tempfile_sce <- tempfile(fileext = "_sce.rds")
saveRDS(sce, file = tempfile_sce)

thing <- list(ReducedDimensionPlot(), ReducedDimensionPlot())
tempfile_params <- tempfile(fileext = "_params.rds")
saveRDS(thing, file = tempfile_params)

test_that("createLandingPage returns a function ", {

    input <- new.env()
    output <- new.env()
    session <- new.env()

    # All NULL arguments
    out <- iSEE::createLandingPage()
    expect_is(out, "function")
    expect_named(formals(out), c("FUN", "input", "output", "session"))

    out2 <- out(FUN=print, input=input, output=output, session=session)
    expect_null(out2)
    expect_named(output, c("allPanels"))
})
