context("Tables")

test_that(".multiSelectionCommands handles tables", {

    x <- ColStatTable()
    x[[iSEE:::.TableSearch]] <- "main search"
    x[[iSEE:::.TableColSearch]] <- c("column 1", "column 2")

    out <- .multiSelectionCommands(x, NA)
    expect_identical(out, 'selected <- rownames(contents)[iSEE::filterDT(contents, global="main search",\n    column=c("column 1", "column 2"))]')

})

test_that(".multiSelectionActive handles tables", {

    x <- ColStatTable()
    x[[iSEE:::.TableSearch]] <- "main search"
    x[[iSEE:::.TableColSearch]] <- c("column 1", "column 2")

    out <- .multiSelectionActive(x)
    expect_identical(out, list(Search = "main search", ColumnSearch = c("column 1", "column 2" )))

})

test_that(".createObservers populates rObjects for tables", {

    x <- ColStatTable(PanelId=1L)
    input <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    .createObservers(x, sce, input, NULL, pObjects, rObjects)

    expect_identical(rObjects$ColStatTable1, 1L)
    expect_identical(rObjects$ColStatTable1_INTERNAL_dimnames, 1L)
    expect_identical(rObjects$ColStatTable1_INTERNAL_multi_select, 1L)
    expect_identical(rObjects$ColStatTable1_INTERNAL_relinked_select, 1L)
    expect_identical(rObjects$ColStatTable1_INTERNAL_saved_choices, 1L)
    expect_identical(rObjects$ColStatTable1_INTERNAL_single_select, 1L)
})

test_that(".renderOutput populates output", {

    x <- ColStatTable(PanelId=1L)
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .renderOutput(x, sce, output = output, pObjects = pObjects, rObjects = rObjects)
    expect_null(out)
    expect_is(output$ColStatTable1, "shiny.render.function")
    expect_is(output$ColStatTable1_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
    expect_is(output$ColStatTable1_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
})
