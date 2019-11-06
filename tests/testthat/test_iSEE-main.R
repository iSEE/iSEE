context("iSEE-main")

test_that("iSEE main function runs without object", {

    app <- iSEE()

    expect_s3_class(app, "shiny.appobj")

})

test_that("iSEE main function runs", {

    app <- iSEE(
        sce, redDimArgs=NULL, colDataArgs=NULL, featAssayArgs=NULL, sampAssayArgs=NULL,
        rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
        redDimMax=5, colDataMax=5, featAssayMax=5, sampAssayMax=5, rowStatMax=5,
        rowDataMax=5, heatMapMax=5, initialPanels=NULL, annotFun=NULL,
        colormap=ExperimentColorMap(), runLocal=TRUE)

    expect_s3_class(app, "shiny.appobj")

})

test_that("iSEE main function runs with empty rowData(sce)", {

    rowData(sce) <- NULL
    sce <- removeAltExps(sce)

    app <- iSEE(
        sce, redDimArgs=NULL, colDataArgs=NULL, featAssayArgs=NULL, sampAssayArgs=NULL,
        rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
        redDimMax=5, colDataMax=5, featAssayMax=5, sampAssayMax=5, rowStatMax=5,
        rowDataMax=5, heatMapMax=5, initialPanels=NULL, annotFun=NULL,
        colormap=ExperimentColorMap(), runLocal=TRUE)

    expect_s3_class(app, "shiny.appobj")

})

test_that("iSEE main function runs with empty colData(sce)", {

    colData(sce) <- DataFrame(row.names=colnames(sce))
    sizeFactors(sce) <- NULL

    app <- iSEE(
        sce, redDimArgs=NULL, colDataArgs=NULL, featAssayArgs=NULL, sampAssayArgs=NULL,
        rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
        redDimMax=0, colDataMax=1, featAssayMax=0, sampAssayMax=0, rowStatMax=0,
        rowDataMax=0, heatMapMax=0, initialPanels=NULL, annotFun=NULL,
        colormap=ExperimentColorMap(), runLocal=TRUE)

    expect_s3_class(app, "shiny.appobj")

})
