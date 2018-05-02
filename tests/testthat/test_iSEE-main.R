
test_that("iSEE main function runs", {
  
  app <- iSEE(sce, redDimArgs = NULL, colDataArgs = NULL, featAssayArgs = NULL,
    rowStatArgs = NULL, rowDataArgs = NULL, heatMapArgs = NULL,
    redDimMax = 5, colDataMax = 5, featAssayMax = 5, rowStatMax = 5,
    rowDataMax = 5, heatMapMax = 5, initialPanels = NULL, annotFun = NULL,
    colormap = ExperimentColorMap(), runLocal = TRUE)

  expect_s3_class(app, "shiny.appobj")
  
})

test_that("iSEE main function runs with empty rowData(sce)", {
  
  rowData(sce) <- NULL
  
  app <- iSEE(sce, redDimArgs = NULL, colDataArgs = NULL, featAssayArgs = NULL,
    rowStatArgs = NULL, rowDataArgs = NULL, heatMapArgs = NULL,
    redDimMax = 5, colDataMax = 5, featAssayMax = 5, rowStatMax = 5,
    rowDataMax = 5, heatMapMax = 5, initialPanels = NULL, annotFun = NULL,
    colormap = ExperimentColorMap(), runLocal = TRUE)

  expect_s3_class(app, "shiny.appobj")
  
})
