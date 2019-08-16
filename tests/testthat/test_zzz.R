context("zzz")

test_that("onLoad works", {

    # Provide code coverage
    iSEE:::.onLoad()

    # To make an "expect_" statement, we would have to toy with shiny internals.
    
    # directoryPath <- normalizePath(shiny:::.globals$resourcePaths[["iSEE"]][["path"]])
    # expectedPath <- normalizePath(system.file("www", package="iSEE"))
    # expect_identical(directoryPath, expectedPath)

})
