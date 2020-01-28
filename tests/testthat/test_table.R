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

test_that(".generateTable handles RowStatTable", {
    
    table_env <- new.env()
    
    x <- RowStatTable()
    
    sce <- .cacheCommonInfo(x, sce)
    table_env$se <- sce
    
    # default
    out <- .generateTable(x, table_env)
    expect_identical(out, "tab <- as.data.frame(rowData(se));")
    
    # row_selected exists in the environment
    table_env$row_selected <- head(rownames(sce))
    out <- .generateTable(x, table_env)
    expect_identical(out, c(
        "tab <- as.data.frame(rowData(se));",
        "tab <- tab[unique(unlist(row_selected)),,drop=FALSE]"))
    
    # Some of the columns of rowData(se) are not valid atomic fields
    rowData(table_env$se)[["DataFrame"]] <- rowData(table_env$se)
    out <- .generateTable(x, table_env)
    expect_identical(out, c(
        "tab <- as.data.frame(rowData(se));",
        "tab <- tab[unique(unlist(row_selected)),,drop=FALSE]",
        "tab <- tab[,c(\"num_cells\", \"mean_count\", \"letters\"),drop=FALSE]"))
})

test_that(".generateTable handles ColStatTable", {
    
    table_env <- new.env()
    
    x <- ColStatTable()
    
    sce <- .cacheCommonInfo(x, sce)
    table_env$se <- sce
    
    # default
    out <- .generateTable(x, table_env)
    expect_identical(out, c(
        "tab <- as.data.frame(colData(se));",
        "tab <- tab[,c(\"NREADS\", \"NALIGNED\", \"RALIGN\", \"TOTAL_DUP\", \"PRIMER\", \"PCT_RIBOSOMAL_BASES\", \n     \"PCT_CODING_BASES\", \"PCT_UTR_BASES\", \"PCT_INTRONIC_BASES\", \"PCT_INTERGENIC_BASES\", \n     \"PCT_MRNA_BASES\", \"MEDIAN_CV_COVERAGE\", \"MEDIAN_5PRIME_BIAS\", \n     \"MEDIAN_3PRIME_BIAS\", \"MEDIAN_5PRIME_TO_3PRIME_BIAS\", \"driver_1_s\", \n     \"dissection_s\", \"Core.Type\", \"Primary.Type\", \"Secondary.Type\", \n     \"Animal.ID\", \"passes_qc_checks_s\"),drop=FALSE]"))
    
    # col_selected exists in the environment
    # Some of the columns of rowData(se) are not valid atomic fields
    table_env$col_selected <- head(colnames(sce))
    out <- .generateTable(x, table_env)
    expect_identical(out, c(
        "tab <- as.data.frame(colData(se));",
        "tab <- tab[unique(unlist(col_selected)),,drop=FALSE]",
        "tab <- tab[,c(\"NREADS\", \"NALIGNED\", \"RALIGN\", \"TOTAL_DUP\", \"PRIMER\", \"PCT_RIBOSOMAL_BASES\", \n     \"PCT_CODING_BASES\", \"PCT_UTR_BASES\", \"PCT_INTRONIC_BASES\", \"PCT_INTERGENIC_BASES\", \n     \"PCT_MRNA_BASES\", \"MEDIAN_CV_COVERAGE\", \"MEDIAN_5PRIME_BIAS\", \n     \"MEDIAN_3PRIME_BIAS\", \"MEDIAN_5PRIME_TO_3PRIME_BIAS\", \"driver_1_s\", \n     \"dissection_s\", \"Core.Type\", \"Primary.Type\", \"Secondary.Type\", \n     \"Animal.ID\", \"passes_qc_checks_s\"),drop=FALSE]" ))
    
})
