
test_that("onLoad works", {

    iSEE:::.onLoad()

    directoryPath <- shiny:::.globals$resources[["iSEE"]][["directoryPath"]]
    expect_identical(directoryPath, system.file("www", package="iSEE"))

})
