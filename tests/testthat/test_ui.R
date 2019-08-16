context("ui")

# Set up a custom plotting functions
CUSTOM <- function(se, columns, arg1=NULL) {
    stuff <- logcounts(se)[1:1000,columns]
    out <- prcomp(t(stuff), rank.=2)
    return(list(
        coordinates=data.frame(X=out$x[,1], Y=out$x[,2], row.names=columns),
        xlab="WHEE",
        ylab="YAY",
        title="HOORAY"))
}

CUSTOM_DE <- function(se, columns) {
    stuff <- logcounts(se)[ ,columns]
    # Completely ignore stuff and return depressing DE stats
    out <- data.frame(
        FC=rep(0, nrow(se)),
        p.value=rep(1, nrow(se)),
        row.names=rownames(se)
    )
    return(out)
}

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
sceX <- iSEE:::.precompute_UI_info(sce, list(PCA2=CUSTOM), list(DE=CUSTOM_DE))
active_panels <- iSEE:::.setup_initial(initialPanels, memory)
memory <- iSEE:::.sanitize_memory(active_panels, memory)

# .add_coldata_facet_UI_elements ----

test_that(".add_facet_UI_elements produces a valid tag list for column data plots", {

    rd <- redDimPlotDefaults(sce, 1)
    rd[["FacetByRow"]] <- TRUE
    rd[["FacetByColumn"]] <- TRUE

    groupable_colData <- colnames(colData(sce))[iSEE:::.which_groupable(colData(sce))]

    out <- iSEE:::.add_facet_UI_elements_for_column_plots("redDimPlot", 1, rd, groupable_colData)
    # TODO: expect_*(out)

    # Check a non default choice
    rd[["RowFacetColData"]] <- "Primary.Type"
    rd[["ColumnFacetColData"]] <- "Core.Type"
    out <- iSEE:::.add_facet_UI_elements_for_column_plots("redDimPlot", 1, rd, groupable_colData)

    # TODO: expect_*(out)
})

test_that(".add_facet_UI_elements produces a valid tag list for row data plots", {

    rd <- rowDataPlotDefaults(sce, 1)
    rd[["FacetByRow"]] <- TRUE
    rd[["FacetByColumn"]] <- TRUE

    groupable_colData <- colnames(colData(sce))[iSEE:::.which_groupable(rowData(sce))]

    out <- iSEE:::.add_facet_UI_elements_for_row_plots("rowDataPlot", 1, rd, groupable_colData)
    # TODO: expect_*(out)
})

# .panel_generation ----

test_that(".panel_generation works", {

    out <- iSEE:::.panel_generation(active_panels, memory, sceX)

    expect_is(out, "shiny.tag.list")
})


test_that(".panel_generation detects invalid panel modes", {

    active_panels <- rbind(active_panels, data.frame(
        Type="Whee!", ID=1, Width=3, Height=500, row.names="Whee!"
    ))

    expect_error(
        iSEE:::.panel_generation(active_panels, memory=memory, sceX)
    )

})

# .panel_organization ----

test_that(".panel_organization works", {

    out <- iSEE:::.panel_organization(active_panels)

    expect_is(out, "shiny.tag.list")
})

# .choose_links ----

test_that(".choose_links behaves as expected", {

    chosenValue <- "chosen"

    availableValues <- c(chosenValue, head(letters))

    # Return chosen value when available
    out <- iSEE:::.choose_link(chosenValue, availableValues, force_default=FALSE)
    expect_identical(out, chosenValue)

    availableValues <- head(letters)

    # Return empty character value if chosen is not available, and default is not forced
    out <- iSEE:::.choose_link(chosenValue, availableValues, force_default=FALSE)
    expect_identical(out, character(1L))

    # Return first available value if chosen is not available, and default is forced
    out <- iSEE:::.choose_link(chosenValue, availableValues, force_default=TRUE)
    expect_identical(out, availableValues[1])

})

# .precompute_UI_info ----

test_that(".precompute_UI_info generates missing sample names for internal metadata", {

    colnames(sce) <- NULL
    out <- iSEE:::.precompute_UI_info(sce, list(PCA2=CUSTOM), list(DE=CUSTOM_DE))

    expect_identical(
        int_metadata(out)[["iSEE"]][["sample_names"]],
        sprintf("Sample %i", seq_len(ncol(out)))
    )

})
