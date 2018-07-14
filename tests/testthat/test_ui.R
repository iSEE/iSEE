
# Set up a custom plotting functions
CUSTOM <- function(se, columns) {
    stuff <- logcounts(se)[1:1000,columns]
    out <- prcomp(t(stuff), rank.=2)
    return(list(coordinates=data.frame(X=out$x[,1], Y=out$x[,2], row.names=columns),
                xlab="WHEE", ylab="YAY", title="HOORAY"))
}

initialPanels <- DataFrame(
    Name = paste(c(
        "Reduced dimension plot",
        "Column data plot",
        "Feature assay plot",
        "Sample assay plot", 
        "Row statistics table",
        "Row data plot",
        "Custom column plot",
        "Heat map"),
        1),
    Width = 3
)

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
featAssayArgs <- featAssayPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 1)
rowStatArgs <- rowStatTableDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
customColArgs <- customColPlotDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)
customColArgs <- customColPlotDefaults(sce, 1)

#
customColArgs$Function <- "PCA2"

# Adding row names to mimic .setup_memory().
# We don't actually want to run that function, though, 
# as the number of customColPlots will be set to zero. 
rownames(redDimArgs) <- sprintf("redDimPlot%i", seq_len(nrow(redDimArgs)))
rownames(colDataArgs) <- sprintf("colDataPlot%i", seq_len(nrow(colDataArgs)))
rownames(featAssayArgs) <- sprintf("featAssayPlot%i", seq_len(nrow(featAssayArgs)))
rownames(sampAssayArgs) <- sprintf("sampAssayPlot%i", seq_len(nrow(sampAssayArgs)))
rownames(rowStatArgs) <- sprintf("rowStatTable%i", seq_len(nrow(rowStatArgs)))
rownames(rowDataArgs) <- sprintf("rowDataPlot%i", seq_len(nrow(rowDataArgs)))
rownames(customColArgs) <- sprintf("customColPlot%i", seq_len(nrow(customColArgs)))
rownames(heatMapArgs) <- sprintf("heatMapPlot%i", seq_len(nrow(heatMapArgs)))

# Setting up the memory.
memory <- list(redDimPlot=redDimArgs, 
               colDataPlot=colDataArgs, 
               featAssayPlot=featAssayArgs, 
               sampAssayPlot=sampAssayArgs,
               rowStatTable=rowStatArgs, 
               rowDataPlot=rowDataArgs, 
               customColPlot=customColArgs,
               heatMapPlot=heatMapArgs)

# Set up alternative object.
sceX <- iSEE:::.precompute_UI_info(sce, list(PCA2=CUSTOM))
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

test_that(".panel_generation works", {
    
    out <- iSEE:::.panel_generation(active_panels, memory, sceX)
    
    expect_is(out, "shiny.tag.list")
})

test_that(".panel_organization works", {
    
    out <- iSEE:::.panel_organization(active_panels)
    
    expect_is(out, "shiny.tag.list")
})
