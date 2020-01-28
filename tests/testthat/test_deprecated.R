context("Deprecated")

redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
featAssayArgs <- featAssayPlotDefaults(sce, 1)
rowStatArgs <- rowStatTableDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 1)
colStatArgs <- colStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)

initialPanels <- DataFrame(
    Name = c("Reduced dimension plot 1",
             "Column data plot 1",
             "Feature assay plot 1",
             "Row statistics table 1",
             "Row data plot 1",
             "Sample assay plot 1",
             "Column statistics table 1",
             "Heat map 1",
             "Heat map 2"),
    Width = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
    Height = c(400L, 400L, 400L, 400L, 400L, 400L, 400L, 400L, 400L)
)

test_that("iSEE converts old to new panels", {

    app <- iSEE(sce, redDimArgs = redDimArgs, colDataArgs = colDataArgs,
                featAssayArgs = featAssayArgs, rowStatArgs = rowStatArgs,
                rowDataArgs = rowDataArgs, sampAssayArgs = sampAssayArgs,
                colStatArgs = colStatArgs, heatMapArgs = heatMapArgs, initialPanels = initialPanels)
    expect_s3_class(app, "shiny.appobj")

})

test_that("iSEE converts NULL args to new panels", {

    # All-NULL is a special case
    app <- iSEE(sce, redDimArgs = NULL, colDataArgs = NULL,
                featAssayArgs = NULL, rowStatArgs = NULL,
                rowDataArgs = NULL, sampAssayArgs = NULL,
                colStatArgs = NULL, heatMapArgs = NULL)
    expect_s3_class(app, "shiny.appobj")

    # Leave one out (to trigger all the other ones)
    app <- iSEE(sce, redDimArgs = redDimArgs, colDataArgs = NULL,
                featAssayArgs = NULL, rowStatArgs = NULL,
                rowDataArgs = NULL, sampAssayArgs = NULL,
                colStatArgs = NULL, heatMapArgs = NULL)
    expect_s3_class(app, "shiny.appobj")
    # Leave another one out (to trigger the previous one)
    app <- iSEE(sce, redDimArgs = NULL, colDataArgs = colDataArgs,
                featAssayArgs = NULL, rowStatArgs = NULL,
                rowDataArgs = NULL, sampAssayArgs = NULL,
                colStatArgs = NULL, heatMapArgs = NULL)
    expect_s3_class(app, "shiny.appobj")

})

test_that("default functions supports 0-zero DataFrames", {

    out <- redDimPlotDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- colDataPlotDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- featAssayPlotDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- rowStatTableDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- rowDataPlotDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- sampAssayPlotDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- colStatTableDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

    out <- heatMapPlotDefaults(sce, 0)
    expect_identical(nrow(out), 0L)

})

test_that(".set_default_assay returns the appropriate value in the absence of logcounts", {

    out <- .set_default_assay(sce)
    expect_identical(out, 2L)

    assayNames(sce) <- c("assay1", "assay2")
    out <- .set_default_assay(sce)
    expect_identical(out, 1L)

})

test_that(".translate_to_class handles NA values for faceting metadata", {

    redDimArgs[[iSEE:::.facetRowsByColData]] <- NA
    redDimArgs[[iSEE:::.facetColumnsByColData]] <- NA

    .translate_to_class(redDimArgs, RedDimPlot, sce, is_row = FALSE)

})

test_that(".translate_to_class properly converts panel names", {

    redDimArgs$SelectByPlot <- "Column data plot 1"

    out <- .translate_to_class(redDimArgs, RedDimPlot, sce, is_row = FALSE)
    expect_identical(out[[1]][["SelectColSource"]], "ColDataPlot1")

})

test_that(".convert_old_name_to_new works as expected", {

    out <- .convert_old_name_to_new("Column data plot 1")
    expect_identical(out, "ColDataPlot1")
})
