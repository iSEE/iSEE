context("voice")

initialPanels <- DataFrame(
    Name=c(
        paste(c(
            "Reduced dimension plot",
            "Column data plot",
            "Feature assay plot",
            "Row statistics table",
            "Row data plot",
            "Sample assay plot",
            "Column statistics table",
            "Custom data plot",
            "Custom statistics table",
            "Heat map"), 1),
        "Custom data plot 2"),
    Width=3
)

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
featAssayArgs <- featAssayPlotDefaults(sce, 1)
rowStatArgs <- rowStatTableDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 1)
colStatArgs <- colStatTableDefaults(sce, 1)
customDataArgs <- customDataPlotDefaults(sce, 2)
customStatArgs <- customStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)

#
customDataArgs[1, iSEE:::.customFun] <- "PCA2"
customDataArgs[2, iSEE:::.customFun] <- "PCA2"
customDataArgs[2, iSEE:::.customVisibleArgs] <- "arg1 test"
customDataArgs[2, iSEE:::.customArgs] <- "PCA2"
customStatArgs[1, iSEE:::.customFun] <- "DE"

# Adding row names to mimic .setup_memory().
# We don't actually want to run that function, though,
# as the number of customColPlots will be set to zero.
rownames(redDimArgs) <- sprintf("redDimPlot%i", seq_len(nrow(redDimArgs)))
rownames(colDataArgs) <- sprintf("colDataPlot%i", seq_len(nrow(colDataArgs)))
rownames(featAssayArgs) <- sprintf("featAssayPlot%i", seq_len(nrow(featAssayArgs)))
rownames(rowStatArgs) <- sprintf("rowStatTable%i", seq_len(nrow(rowStatArgs)))
rownames(rowDataArgs) <- sprintf("rowDataPlot%i", seq_len(nrow(rowDataArgs)))
rownames(sampAssayArgs) <- sprintf("sampAssayPlot%i", seq_len(nrow(sampAssayArgs)))
rownames(colStatArgs) <- sprintf("colStatTable%i", seq_len(nrow(colStatArgs)))
rownames(customDataArgs) <- sprintf("customDataPlot%i", seq_len(nrow(customDataArgs)))
rownames(customStatArgs) <- sprintf("customStatTable%i", seq_len(nrow(customStatArgs)))
rownames(heatMapArgs) <- sprintf("heatMapPlot%i", seq_len(nrow(heatMapArgs)))

# Setting up the memory.
memory <- list(
    redDimPlot=redDimArgs,
    colDataPlot=colDataArgs,
    featAssayPlot=featAssayArgs,
    rowStatTable=rowStatArgs,
    rowDataPlot=rowDataArgs,
    sampAssayPlot=sampAssayArgs,
    colStatTable=colStatArgs,
    customDataPlot=customDataArgs,
    customStatTable=customStatArgs,
    heatMapPlot=heatMapArgs)

# Set up alternative object.
se_out <- .sanitize_SE_input(sce)
sce <- se_out$object
sceX <- iSEE:::.precompute_UI_info(sce, list(PCA2=ggplot), list(DE=data.frame))
active_panels <- iSEE:::.setup_initial(initialPanels, memory)
memory <- iSEE:::.sanitize_memory(active_panels, memory)

tabs <- iSEE:::.spawn_table_links(memory)
selection_links <- .spawn_selection_chart(memory)

# prepareSpeechRecognition ----

test_that("prepareSpeechRecognition loads", {

    out <- prepareSpeechRecognition(use = TRUE)

    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

})

# .digitalizeNumbers ----

test_that("numbers can be numeralized from text", {

    out <- .digitalizeNumbers("one")
    expect_identical(out, 1)

    out <- .digitalizeNumbers("two")
    expect_identical(out, 2)

    # allow some vocal typos
    out <- .digitalizeNumbers("to")
    expect_identical(out, 2)

    out <- .digitalizeNumbers("too")
    expect_identical(out, 2)

})

# .isDigits ----

test_that(".isDigits works", {

    expect_true(.isDigits("123"))
    expect_false(.isDigits("oen hundred and three"))

})

# .showPanel ----

test_that(".showPanel adds a valid row to the table of active panels", {

    out <- .showPanel("redDimPlot", 6, active_panels, width=4L, height=400L)

    expect_identical(tail(out$Type, 1), "redDimPlot")
    expect_identical(tail(out$ID, 1), 6)
    expect_identical(tail(out$Width, 1), 4L)
    expect_identical(tail(out$Height, 1), 400L)

})

# .hidePanel ----

test_that(".hidePanel removes a row from active panels and associated table links", {

    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory
    pObjects$selection_links <- selection_links

    # Delete a plot panel
    out <- .hidePanel("redDimPlot", 1, active_panels, pObjects)
    expect_true(identical(nrow(out), nrow(active_panels)-1L))

    # Delete a table panel
    out <- .hidePanel("rowStatTable", 1, active_panels, pObjects)
    expect_true(identical(nrow(out), nrow(active_panels)-1L))

})

# .nearestPanelType ----

test_that(".nearestPanelType handles vocal typos", {

    out <- .nearestPanelType("definitely not a proper panel type")
    expect_identical(out, character(0L))

    out <- .nearestPanelType("reduce dimension plus")
    expect_identical(out, c(redDimPlot="Reduced dimension plot"))

    out <- .nearestPanelType("row statistics table")
    expect_identical(out, c(rowStatTable = "Row statistics table"))

    out <- .nearestPanelType("row statistics plot")
    expect_identical(out, c(rowStatTable = "Row statistics table"))
    # NOTE: closer than row data plot
})

# .nearestDecodedPanel ----

test_that(".nearestDecodedPanel handles vocal typos", {

    out <- .nearestDecodedPanel("definitely not a proper panel name")
    expect_null(out)

    out <- .nearestDecodedPanel("reduced dimension plot NotANumber", memory)
    expect_null(out)

    out <- .nearestDecodedPanel("reduced dimension plot 1", memory)
    expect_identical(out, "Reduced dimension plot 1")

})

# .nearestValidChoice ----

test_that(".nearestValidChoice handles vocal typos", {

    out <- .nearestValidChoice("input that cannot be matched", panelTypes)
    expect_identical(out, character(0L))

    out <- .nearestValidChoice("reduced dimension plot", panelTypes)
    expect_identical(out, "Reduced dimension plot")

})

# .nearestValidNamedChoice ----

test_that(".nearestValidNamedChoice handles vocal typos", {

    out <- .nearestValidNamedChoice("reddimplot", panelTypes)
    expect_identical(out, c(redDimPlot = "Reduced dimension plot"))

})

# .getValidParameterChoices ----

test_that(".getValidParameterChoices works for column plots", {

    out <- .getValidParameterChoices("ColorBy", "redDimPlot", sce)
    expect_identical(out, c("None", "Column data", "Feature name", "Sample name"))

    out <- .getValidParameterChoices("Not Supported", "redDimPlot", sce)
    expect_identical(out, character(0L))

})

test_that(".getValidParameterChoices works for row plots", {

    out <- .getValidParameterChoices("ColorBy", "rowDataPlot", sce)
    expect_identical(out, c("None", "Row data" , "Feature name", "Sample name"))

    out <- .getValidParameterChoices("Not Supported", "rowDataPlot", sce)
    expect_identical(out, character(0L))

})

# .colorByChoices ----

test_that(".colorByChoices works", {

    out <- .colorByChoices("None", sce)
    expect_identical(out, character(0L))

    out <- .colorByChoices("Column data", sce)
    expect_identical(out, colnames(colData(sce)))

    out <- .colorByChoices("Row data", sce)
    expect_identical(out, colnames(rowData(sce)))

    out <- .colorByChoices("Feature name", sce)
    expectedValue <- seq_len(nrow(sce))
    names(expectedValue) <- rownames(sce)
    expect_identical(out, expectedValue)

    out <- .colorByChoices("Sample name", sce)
    expectedValue <- seq_len(ncol(sce))
    names(expectedValue) <- colnames(sce)
    expect_identical(out, expectedValue)

})

