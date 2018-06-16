# .add_coldata_facet_UI_elements ----

test_that(".add_facet_UI_elements produces a valid tag list for column data plots", {
    
    rd <- redDimPlotDefaults(sce, 1)
    rd[["FacetByRow"]] <- TRUE
    rd[["FacetByColumn"]] <- TRUE
    
    groupable_colData <- colnames(colData(sce))[iSEE:::.which_groupable(colData(sce))]
  
    out <- iSEE:::.add_facet_UI_elements_for_column_plots("redDimPlot", 1, rd, groupable_colData)
    
    # Check a non default choice
    rd[["RowFacetColData"]] <- "Primary.Type"
    rd[["ColumnFacetColData"]] <- "Core.Type"
    out <- iSEE:::.add_facet_UI_elements_for_column_plots("redDimPlot", 1, rd, groupable_colData)
    

})

test_that(".add_facet_UI_elements produces a valid tag list for row data plots", {
    
    rd <- rowDataPlotDefaults(sce, 1)
    rd[["FacetByRow"]] <- TRUE
    rd[["FacetByColumn"]] <- TRUE
    
    groupable_colData <- colnames(colData(sce))[iSEE:::.which_groupable(rowData(sce))]
    
    out <- iSEE:::.add_facet_UI_elements_for_row_plots("rowDataPlot", 1, rd, groupable_colData)
    
 

})
