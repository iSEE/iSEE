# .add_coldata_facet_UI_elements ----

test_that(".add_facet_UI_elements produces a valid tag list for column data plots", {
    
    rd <- redDimPlotDefaults(sce, 1)
    rd[["FacetByRow"]] <- TRUE
    rd[["FacetByColumn"]] <- TRUE
  
    out <- iSEE:::.add_facet_UI_elements_for_column_plots("redDimPlot", 1, rd, colnames(colData(sce)))
    
    # Row checkbox is ticked
    expect_identical(
        out[[1]]$children[[1]]$children[[1]]$children[[1]]$attribs$id,
        "redDimPlot1_FacetByRow"
    )
    expect_identical(
        out[[1]]$children[[1]]$children[[1]]$children[[1]]$attribs$checked,
        "checked"
    )
    # Default covariate is selected
    expect_match(
        out[[2]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        "<option value=\"NREADS\" selected>", fixed = TRUE
    )

    # Column checkbox is ticked
    expect_identical(
        out[[3]]$children[[1]]$children[[1]]$children[[1]]$attribs$id,
        "redDimPlot1_FacetByColumn"
    )
    expect_identical(
        out[[3]]$children[[1]]$children[[1]]$children[[1]]$attribs$checked,
        "checked"
    )
    # Default covariate is selected
    expect_match(
        out[[4]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        "<option value=\"NREADS\" selected>", fixed = TRUE
    )
    
    # Check a non default choice
    
    rd[["RowFacetColData"]] <- "driver_1_s"
    rd[["ColumnFacetColData"]] <- "NREADS"
    out <- iSEE:::.add_facet_UI_elements_for_column_plots("redDimPlot", 1, rd, colnames(colData(sce)))
    
    # Row facet
    expect_match(
        out[[2]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"driver_1_s\" selected>', fixed = TRUE
    )
    
    # Column facet
    expect_match(
        out[[4]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"NREADS\" selected>', fixed = TRUE
    )

})

test_that(".add_facet_UI_elements produces a valid tag list for row data plots", {
    
    rd <- rowDataPlotDefaults(sce, 1)
    rd[["FacetByRow"]] <- TRUE
    rd[["FacetByColumn"]] <- TRUE
  
    # Default is '.' and '.' selected for row and column faceting, respectively
    
    out <- iSEE:::.add_facet_UI_elements_for_row_plots("rowDataPlot", 1, rd, colnames(rowData(sce)))
    
    # Row
    expect_identical(
        out[[1]]$children[[1]]$children[[1]]$children[[1]]$attribs$id,
        "rowDataPlot1_FacetByRow"
    )
    expect_identical(
        out[[1]]$children[[1]]$children[[1]]$children[[1]]$attribs$checked,
        "checked"
    )
    # Default covariate is selected
    expect_match(
        out[[2]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        "<option value=\"num_cells\" selected>", fixed = TRUE
    )
    
    # Column
    expect_identical(
        out[[3]]$children[[1]]$children[[1]]$children[[1]]$attribs$id,
        "rowDataPlot1_FacetByColumn"
    )
    expect_identical(
        out[[3]]$children[[1]]$children[[1]]$children[[1]]$attribs$checked,
        "checked"
    )
    expect_match(
        out[[4]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        "<option value=\"num_cells\" selected>", fixed = TRUE
    )
    
    # Check a non default choice
    
    rd[["RowFacetRowData"]] <- "mean_count"
    rd[["ColumnFacetRowData"]] <- "mean_count"
    out <- iSEE:::.add_facet_UI_elements_for_row_plots("rowDataPlot", 1, rd, colnames(rowData(sce)))
    # Row facet
    expect_match(
        out[[2]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"mean_count\" selected>', fixed = TRUE
    )
    # Column facet
    expect_match(
        out[[4]]$children[[1]]$children[[2]]$children[[1]]$children[[1]][[1]],
        '<option value=\"mean_count\" selected>', fixed = TRUE
    )

})
