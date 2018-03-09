
test_that("iSEE main function runs", {
  
  app <- iSEE(sce, redDimArgs = NULL, colDataArgs = NULL, featExprArgs = NULL,
    rowStatArgs = NULL, rowDataArgs = NULL, heatMapArgs = NULL,
    redDimMax = 5, colDataMax = 5, featExprMax = 5, rowStatMax = 5,
    rowDataMax = 5, heatMapMax = 5, initialPanels = NULL, annotFun = NULL,
    colormap = ExperimentColorMap(), run_local = TRUE)

  expect_s3_class(app, "shiny.appobj")
  
})
