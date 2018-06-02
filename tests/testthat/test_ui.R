# .add_coldata_facet_UI_elements ----

test_that(".add_facet_UI_elements produces a valid tag list for column data plots", {
    
    rd <- redDimPlotDefaults(sce, 1)
  
    # Default is '.' and '.' selected for row and column faceting, respectively
    out <- iSEE:::.add_facet_UI_elements("redDimPlot", 1, rd, colnames(colData(sce)))
    # Row
    expect_match(
        out[[1]]$children[[2]]$children[[1]]$attribs[[1]],
        "RowFacetColData"
    )
    expect_match(
        out[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\".\" selected>', fixed = TRUE
    )
    # Column
    expect_match(
        out[[2]]$children[[2]]$children[[1]]$attribs[[1]],
        "ColumnFacetColData"
    )
    expect_match(
        out[[2]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\".\" selected>', fixed = TRUE
    )
    
    # Check a non default choice
    
    rd[["RowFacetColData"]] <- "driver_1_s"
    rd[["ColumnFacetColData"]] <- "NREADS"
    out <- iSEE:::.add_facet_UI_elements("redDimPlot", 1, rd, colnames(colData(sce)))
    # Row facet
    expect_match(
        out[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"driver_1_s\" selected>', fixed = TRUE
    )
    # Column facet
    expect_match(
        out[[2]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"NREADS\" selected>', fixed = TRUE
    )

})

test_that(".add_facet_UI_elements produces a valid tag list for row data plots", {
    
    rd <- rowDataPlotDefaults(sce, 1)
  
    # Default is '.' and '.' selected for row and column faceting, respectively
    
    out <- iSEE:::.add_facet_UI_elements("rowDataPlot", 1, rd, colnames(rowData(sce)))
    # Row
    expect_match(
        out[[1]]$children[[2]]$children[[1]]$attribs[[1]],
        "RowFacetColData"
    )
    expect_match(
        out[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\".\" selected>', fixed = TRUE
    )
    # Column
    expect_match(
        out[[2]]$children[[2]]$children[[1]]$attribs[[1]],
        "ColumnFacetColData"
    )
    expect_match(
        out[[2]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\".\" selected>', fixed = TRUE
    )
    
    # Check a non default choice
    
    rd[["RowFacetColData"]] <- "mean_count"
    rd[["ColumnFacetColData"]] <- "num_cells"
    out <- iSEE:::.add_facet_UI_elements("rowDataPlot", 1, rd, colnames(rowData(sce)))
    # Row facet
    expect_match(
        out[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"mean_count\" selected>', fixed = TRUE
    )
    # Column facet
    expect_match(
        out[[2]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"num_cells\" selected>', fixed = TRUE
    )

})
