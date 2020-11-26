# Cursory tests for the main function to make sure that it creates the app.
# library(testthat); library(iSEE); source("setup_sce.R"); source("test_iSEE-main.R")

context("iSEE-main")

test_that("iSEE main function runs without object", {
    app <- iSEE()
    expect_s3_class(app, "shiny.appobj")
})

test_that("iSEE main function runs", {
    app <- iSEE(sce, runLocal=TRUE)
    expect_s3_class(app, "shiny.appobj")
})

test_that("iSEE main function runs with empty rowData(sce)", {
    rowData(sce) <- NULL
    sce <- removeAltExps(sce)
    app <- iSEE(sce, runLocal=TRUE)
    expect_s3_class(app, "shiny.appobj")

})

test_that("iSEE main function runs with empty colData(sce)", {
    colData(sce) <- DataFrame(row.names=colnames(sce))
    app <- iSEE(sce, runLocal=TRUE)
    expect_s3_class(app, "shiny.appobj")
})

test_that("iSEE runs correctly with saveState= specified", {
    app <- iSEE(sce, saveState=function(x) {
        withProgress(message="Saving!", saveRDS(x, file=tempfile(fileext='.rds')))
        showNotification("hooray, saved!", type="message")
    })
    expect_s3_class(app, "shiny.appobj")
})
