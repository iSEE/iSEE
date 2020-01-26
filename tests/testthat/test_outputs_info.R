context("Output")

test_that(".create_selection_info_output works as expected", {

    output = new.env()
    pObjects = new.env()
    rObjects = new.env()

    out <- .create_selection_info_output("RedDimPlot", sce, output, pObjects, rObjects)
    expect_null(out)
    expect_is(output[["RedDimPlot_INTERNAL_PanelMultiSelectInfo"]], "shiny.render.function")

})

test_that(".create_link_info_output works as expected", {

    output = new.env()
    pObjects = new.env()
    rObjects = new.env()

    out <- .create_link_info_output("RedDimPlot", output, pObjects, rObjects)
    expect_null(out)
    expect_is(output[["RedDimPlot_INTERNAL_PanelSelectLinkInfo"]], "shiny.render.function")

})

test_that(".create_table_output works as expected", {

    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .create_table_output("RowStatTable1", sce, output, pObjects, rObjects)
    expect_null(out)
    expect_is(output[["RowStatTable1"]], "shiny.render.function")

})
