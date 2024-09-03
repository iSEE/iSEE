# library(iSEE); library(testthat); source("setup_sce.R"); source("test_outputs.R")
context("Info")

test_that(".create_selection_info_output works as expected", {

    output = new.env()
    pObjects = new.env()
    rObjects = new.env()

    out <- iSEE:::.create_selection_info_output("ReducedDimensionPlot", sce, output, pObjects, rObjects)
    expect_null(out)
    expect_is(output[["ReducedDimensionPlot_INTERNAL_PanelMultiSelectInfo"]], "shiny.render.function")

})

test_that(".create_link_info_output works as expected", {

    output = new.env()
    pObjects = new.env()
    rObjects = new.env()

    out <- iSEE:::.create_link_info_output("ReducedDimensionPlot", output, pObjects, rObjects)
    expect_null(out)
    expect_is(output[["ReducedDimensionPlot_INTERNAL_PanelSelectLinkInfo"]], "shiny.render.function")

})

# outputs_table.R ----
context("Table")

test_that(".create_table_output works as expected", {

    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- iSEE:::.create_table_output("RowDataTable1", sce, output, pObjects, rObjects)
    expect_null(out)
    expect_is(output[["RowDataTable1"]], "shiny.render.function")

})

# .create_general_output ----

test_that(".create_general_output works as expected", {

    input <- new.env()
    output <- new.env()
    session <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- iSEE:::.create_general_output(sce, input, output, session, pObjects, rObjects)

    expect_null(out)
    expect_named(output, c("iSEE_INTERNAL_export_content_download", "iSEE_INTERNAL_link_graph_plot",  "iSEE_INTERNAL_memory_export", "iSEE_INTERNAL_export_content_ui", "mdd"))

})

# .define_export_choices ----

test_that("define_export_choices supports empty memory", {

    memory <- list()

    out <- iSEE:::.define_export_choices(memory)
    expect_identical(out, character(0))
})

test_that("define_export_choices supports non-empty memory", {

    memory <- list(ReducedDimensionPlot(PanelId=1L))

    out <- iSEE:::.define_export_choices(memory)
    expect_identical(out, c("Reduced dimension plot 1" = "ReducedDimensionPlot1"))
})
