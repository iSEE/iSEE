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

test_that("iSEE main function runs with empty dimnames", {
    out <- .fill_se_dimnames(sce)
    expect_identical(out$se, sce)
    expect_identical(out$commands, character(0))
    
    dimnames(sce) <- NULL
    app <- iSEE(sce, runLocal=TRUE)
    expect_s3_class(app, "shiny.appobj")

    out <- .fill_se_dimnames(sce)
    expect_false(is.null(rownames(out$se)))
    expect_false(is.null(colnames(out$se)))
    expect_true(any(grepl("colnames.* <- ", out$commands)))
    expect_true(any(grepl("rownames.* <- ", out$commands)))
})
