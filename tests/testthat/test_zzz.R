context("zzz")

test_that("onLoad works", {

    iSEE:::.onLoad()

    directoryPath <- normalizePath(shiny:::.globals$resourcePaths[["iSEE"]][["path"]])
    expectedPath <- normalizePath(system.file("www", package="iSEE"))
    # expect_identical(directoryPath, expectedPath)

})
