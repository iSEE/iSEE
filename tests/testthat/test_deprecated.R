context("Deprecated")

redDimArgs <- redDimPlotDefaults(sce, 1)

test_that("iSEE converts old to new panels", {

    app <- iSEE(sce, redDimArgs = redDimArgs)
    expect_s3_class(app, "shiny.appobj")

})
