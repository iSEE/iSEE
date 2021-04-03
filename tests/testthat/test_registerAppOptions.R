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

    # Replacement as a list works as expected.
    se <- registerAppOptions(se, list(factor.maxlevels=100, color.maxlevels=24))
    expect_identical(getAppOption("factor.maxlevels", se), 100)
    expect_identical(getAppOption("color.maxlevels", se), 24)
})

test_that("registerAppOptions properly appends", {
    se <- SummarizedExperiment()

    se <- registerAppOptions(se, factor.maxlevels=50)
    se <- registerAppOptions(se, color.maxlevels=20)
    se <- registerAppOptions(se, other.maxlevels=10)
    expect_identical(getAppOption("factor.maxlevels", se), 50)
    expect_identical(getAppOption("color.maxlevels", se), 20)
    expect_identical(getAppOption("other.maxlevels", se), 10)

    # Replacement of existing elements works correctly.
    se <- registerAppOptions(se, color.maxlevels=10, factor.maxlevels=100)
    expect_identical(getAppOption("color.maxlevels", se), 10)
    expect_identical(getAppOption("factor.maxlevels", se), 100)
    expect_identical(getAppOption("other.maxlevels", se), 10)

    # No arguments, no-op.
    se2 <- registerAppOptions(se)
    expect_identical(se2, se)

    # Unless we wipe.
    se2 <- registerAppOptions(se, list())
    expect_identical(unname(getAllAppOptions(se2)), list())
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

