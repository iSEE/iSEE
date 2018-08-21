
test_that("onLoad works", {

    .onLoad()

    directoryPath <- shiny:::.globals$resources[["iSEE"]][["directoryPath"]]
    expect_identical(directoryPath, system.file("www", package="iSEE"))

})
