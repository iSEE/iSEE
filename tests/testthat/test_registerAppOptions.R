# This tests the registerAppOptions framework.
# library(testthat); library(iSEE); source("test_registerAppOptions.R")

test_that("registerAppOptions works as expected", {
    se <- SummarizedExperiment()
    se <- registerAppOptions(se, factor.maxlevels=10, color.maxlevels=20)

    expect_identical(getAppOption("factor.maxlevels", se), 10)
    expect_identical(getAppOption("color.maxlevels", se), 20)
    expect_null(getAppOption("random", se))
    expect_identical(getAppOption("random", se, default="A"), "A")

    expect_identical(getAllAppOptions(se), list(factor.maxlevels=10, color.maxlevels=20))
})

test_that("registerAppOptions works with the globals", {
    se <- SummarizedExperiment()
    se <- registerAppOptions(se, factor.maxlevels=10, color.maxlevels=20)

    .activateAppOptionRegistry(se)
    expect_identical(getAppOption("factor.maxlevels"), 10)
    expect_identical(getAppOption("color.maxlevels"), 20)
    expect_null(getAppOption("random"))
    expect_identical(getAppOption("random", default="A"), "A")

    expect_identical(getAllAppOptions(), list(factor.maxlevels=10, color.maxlevels=20))

    .deactivateAppOptionRegistry()
    expect_null(getAppOption("factor.maxlevels"))
    expect_null(getAppOption("color.maxlevels"))
    expect_identical(unname(getAllAppOptions()), list())
})

test_that("registerAppOptions works with iSEEOptions for back-compatibility's sake", {
    expect_null(getAppOption("panel.color"))
    expect_warning(iSEEOptions$set(panel.color="A"))
    expect_identical(getAppOption("panel.color"), "A")
    iSEEOptions$restore()
})

