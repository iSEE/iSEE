# This tests the table output functions.
# library(iSEE); library(testthat); source('setup_sce.R'); source('test_table.R')

context("Tables")

test_that(".multiSelectionCommands handles tables", {
    
    x <- ColumnDataTable()
    x[[iSEE:::.TableSearch]] <- "main search"
    x[[iSEE:::.TableColSearch]] <- c("column 1", "column 2")
    
    out <- .multiSelectionCommands(x, NA)
    expect_identical(out, 'selected <- rownames(contents)[iSEE::filterDT(contents, global="main search",\n    column=c("column 1", "column 2"))]')
    
})

test_that(".multiSelectionActive handles tables", {
    
    x <- ColumnDataTable()
    x[[iSEE:::.TableSearch]] <- "main search"
    x[[iSEE:::.TableColSearch]] <- c("column 1", "column 2")
    
    out <- .multiSelectionActive(x)
    expect_identical(out, list(Search = "main search", ColumnSearch = c("column 1", "column 2" )))
    
})

test_that(".createObservers populates rObjects for tables", {
    
    x <- ColumnDataTable(PanelId=1L)
    input <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()
    
    .createObservers(x, sce, input, NULL, pObjects, rObjects)
    
    expect_identical(rObjects$ColumnDataTable1, 1L)
    expect_identical(rObjects$ColumnDataTable1_INTERNAL_dimnames, 1L)
    expect_identical(rObjects$ColumnDataTable1_INTERNAL_multi_select, 1L)
    expect_identical(rObjects$ColumnDataTable1_INTERNAL_relinked_select, 1L)
    expect_identical(rObjects$ColumnDataTable1_INTERNAL_saved_choices, 1L)
    expect_identical(rObjects$ColumnDataTable1_INTERNAL_single_select, 1L)
})

test_that(".renderOutput populates output", {
    
    x <- ColumnDataTable(PanelId=1L)
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()
    
    out <- .renderOutput(x, sce, output = output, pObjects = pObjects, rObjects = rObjects)
    expect_null(out)
    expect_is(output$ColumnDataTable1, "shiny.render.function")
    expect_is(output$ColumnDataTable1_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
    expect_is(output$ColumnDataTable1_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
})

test_that(".generateTable handles RowDataTable", {
    
    table_env <- new.env()
    
    x <- RowDataTable()
    
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
        "tab <- tab[rownames(tab) %in% unlist(row_selected),,drop=FALSE]"))
    
    # Some of the columns of rowData(se) are not valid atomic fields
    rowData(table_env$se)[["DataFrame"]] <- rowData(table_env$se)
    out <- .generateTable(x, table_env)
    expect_identical(out, c(
        "tab <- as.data.frame(rowData(se));",
        "tab <- tab[rownames(tab) %in% unlist(row_selected),,drop=FALSE]",
        "tab <- tab[,c(\"num_cells\", \"mean_count\", \"letters\"),drop=FALSE]"))
})

test_that(".generateTable handles ColumnDataTable", {
    
    table_env <- new.env()
    
    x <- ColumnDataTable()
    
    sce <- .cacheCommonInfo(x, sce)
    table_env$se <- sce
    
    # Some of the columns of rowData(se) are not valid atomic fields
    out <- .generateTable(x, table_env)
    expect_identical(out[1], "tab <- as.data.frame(colData(se));")
    expect_match(out[2], "tab <- tab\\[,c\\(.*\\),drop=FALSE\\]")

    # default
    table_env$se$nested <- NULL
    out <- .generateTable(x, table_env)
    expect_identical(out, "tab <- as.data.frame(colData(se));")

    # col_selected exists in the environment
    table_env$col_selected <- head(colnames(sce))
    out <- .generateTable(x, table_env)
    expect_identical(out, c(
        "tab <- as.data.frame(colData(se));",
        "tab <- tab[rownames(tab) %in% unlist(col_selected),,drop=FALSE]"
    ))
})

test_that(".showSelectionDetails works as expected", {
    se <- SummarizedExperiment()
    se <- registerAppOptions(se, ColumnTable.select.details=identity, RowTable.select.details=identity)

    x <- ColumnDataTable()
    expect_null(.showSelectionDetails(x))

    y <- RowDataTable(Selected="B")
    expect_null(.showSelectionDetails(y))

    .activateAppOptionRegistry(se)
    expect_identical(.showSelectionDetails(x), x[["Selected"]])
    expect_identical(.showSelectionDetails(x), x[["Selected"]])
    .deactivateAppOptionRegistry()
})
