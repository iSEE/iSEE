
test_that("list updater function works correctly", {
    blah <- new("DataFrame", nrows=20L)
    blah$A <- vector("list", 20)

    # Adding an element.
    out <- iSEE:::.update_list_element(blah, 2, "A", 1:5)
    expect_type(out$A, "list")
    tmp <- blah$A
    tmp[[2]] <- 1:5
    expect_identical(tmp, out$A)

    # Adding an element of a completely different type.
    out <- iSEE:::.update_list_element(out, 5, "A", list("X"))
    expect_type(out$A, "list")
    tmp[[5]] <- list("X")
    expect_identical(tmp, out$A)

    out <- iSEE:::.update_list_element(out, 2, "A", NULL)
    out <- iSEE:::.update_list_element(out, 5, "A", NULL)
    expect_identical(blah, out)
}) 

test_that("plot feasibility checks work correctly", {
    out <- iSEE:::.check_plot_feasibility(sce)
    expect_true(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No genes.
    out <- iSEE:::.check_plot_feasibility(sce[0,])
    expect_true(out$redDimPlot)
    expect_false(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_false(out$rowStatTable)
    expect_false(out$rowDataPlot)

    # No samples.
    out <- iSEE:::.check_plot_feasibility(sce[,0])
    expect_false(out$redDimPlot)
    expect_false(out$featExprPlot)
    expect_false(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No column data. 
    sceX <- sce
    colData(sceX) <- colData(sceX)[,0]
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_false(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No reduced dimensions.
    sceX <- sce
    reducedDims(sceX) <- SimpleList()
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_false(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No assays.
    sceX <- sce
    for (field in assayNames(sceX)) { 
        assay(sceX, field) <- NULL
    }
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_false(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No row data
    sceX <- sce
    rowData(sceX) <- rowData(sceX)[,0]
    out <- iSEE:::.check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_true(out$featExprPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_false(out$rowDataPlot)
})

test_that("count incrementer works correctly", {
    expect_identical(iSEE:::.increment_counter(0L), 1L) 
    expect_identical(iSEE:::.increment_counter(9999L), 0L)
})

test_that("memory setup works correctly", {
    rowData(sce)$whee <- 1       

    # Works correctly in the vanilla setting.
    memory <- iSEE:::.setup_memory(sce, redDimArgs=NULL, colDataArgs=NULL, featExprArgs=NULL, rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
                                   redDimMax=5, colDataMax=3, featExprMax=1, rowStatMax=2, rowDataMax=3, heatMapMax=2)
    expect_identical(nrow(memory$redDimPlot), 5L)
    expect_identical(nrow(memory$colDataPlot), 3L)
    expect_identical(nrow(memory$featExprPlot), 1L)
    expect_identical(nrow(memory$rowStatTable), 2L)
    expect_identical(nrow(memory$rowDataPlot), 3L)
    expect_identical(nrow(memory$heatMapPlot), 2L)

    # Works correctly when arguments are specified.
    memory <- iSEE:::.setup_memory(sce, 
                                   redDimArgs=DataFrame(Type=2L), 
                                   colDataArgs=DataFrame(XAxis="Column data"),
                                   featExprArgs=DataFrame(XAxis="Row table"),
                                   rowStatArgs=DataFrame(Selected=10L), 
                                   rowDataArgs=DataFrame(XAxis="Row data"),
                                   heatMapArgs=DataFrame(Assay=1L),
                                   redDimMax=5, colDataMax=3, featExprMax=1, rowStatMax=2, rowDataMax=3, heatMapMax=2)
    expect_identical(nrow(memory$redDimPlot), 5L)
    expect_identical(nrow(memory$colDataPlot), 3L)
    expect_identical(nrow(memory$featExprPlot), 1L)
    expect_identical(nrow(memory$rowStatTable), 2L)
    expect_identical(nrow(memory$rowDataPlot), 3L)
    expect_identical(nrow(memory$heatMapPlot), 2L)

    expect_identical(memory$redDimPlot$Type, rep(2:1, c(1,4))) # Checking arguments were actually replaced.
    expect_identical(memory$colDataPlot$XAxis, rep(c("Column data", "None"), c(1,2)))
    expect_identical(memory$featExprPlot$XAxis, "Row table")
    expect_identical(memory$rowStatTable$Selected, c(10L, 1L))
    expect_identical(memory$rowDataPlot$XAxis, rep(c("Row data", "None"), c(1,2)))
    expect_identical(memory$heatMapPlot$Assay, c(1L, 6L))

    # Works correctly when the number of arguments is greater than MAx.
    memory <- iSEE:::.setup_memory(sce, 
                                   redDimArgs=DataFrame(Type=2L), 
                                   colDataArgs=DataFrame(XAxis="Column data"),
                                   featExprArgs=DataFrame(XAxis="Row table"),
                                   rowStatArgs=DataFrame(Selected=10L), 
                                   rowDataArgs=DataFrame(XAxis="Row data"),
                                   heatMapArgs=DataFrame(Assay=1L),
                                   redDimMax=0, colDataMax=0, featExprMax=0, rowStatMax=0, rowDataMax=0, heatMapMax=0)
    expect_identical(nrow(memory$redDimPlot), 1L)
    expect_identical(nrow(memory$colDataPlot), 1L)
    expect_identical(nrow(memory$featExprPlot), 1L)
    expect_identical(nrow(memory$rowStatTable), 1L)
    expect_identical(nrow(memory$rowDataPlot), 1L)
    expect_identical(nrow(memory$heatMapPlot), 1L)

    expect_identical(memory$redDimPlot$Type, 2L) # Checking arguments were actually replaced.
    expect_identical(memory$colDataPlot$XAxis, "Column data")
    expect_identical(memory$featExprPlot$XAxis, "Row table")
    expect_identical(memory$rowStatTable$Selected, 10L)
    expect_identical(memory$rowDataPlot$XAxis, "Row data")
    expect_identical(memory$heatMapPlot$Assay, 1L)

    # Works correctly when nothing is feasible.
    sceX <- sce[0,0]
    colData(sceX) <- colData(sceX)[,0]
    reducedDims(sceX) <- SimpleList()
    for (field in assayNames(sceX)) { 
        assay(sceX, field) <- NULL
    }
    rowData(sceX) <- rowData(sceX)[,0]

    memory <- iSEE:::.setup_memory(sceX, redDimArgs=NULL, colDataArgs=NULL, featExprArgs=NULL, rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
                                   redDimMax=5, colDataMax=3, featExprMax=1, rowStatMax=2, rowDataMax=3, heatMapMax=2)
    expect_identical(nrow(memory$redDimPlot), 0L)
    expect_identical(nrow(memory$colDataPlot), 0L)
    expect_identical(nrow(memory$featExprPlot), 0L)
    expect_identical(nrow(memory$rowStatTable), 0L)
    expect_identical(nrow(memory$rowDataPlot), 0L)
    expect_identical(nrow(memory$heatMapPlot), 0L)
})

test_that("initialization of active panels works correctly", {
    memory <- iSEE:::.setup_memory(sce, redDimArgs=NULL, colDataArgs=NULL, featExprArgs=NULL, rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
                                   redDimMax=5, colDataMax=3, featExprMax=1, rowStatMax=2, rowDataMax=3, heatMapMax=2)
    out <- iSEE:::.setup_initial(NULL, memory)
    expect_identical(nrow(out), 6L)

    # Trying with actual specifications.
    out <- iSEE:::.setup_initial(DataFrame(Name=c("Feature expression plot 1", "Reduced dimension plot 2")), memory)
    expect_identical(out$Type, c("featExprPlot", "redDimPlot"))
    expect_identical(out$ID, 1:2)
    expect_identical(out$Width, rep(4L, 2))
    expect_identical(out$Height, rep(500L, 2))

    out <- iSEE:::.setup_initial(DataFrame(Name=c("Column data plot 3", "Row statistics table 2"), Width=c(6, 3), Height=c(600, 700)), memory)
    expect_identical(out$Type, c("colDataPlot", "rowStatTable"))
    expect_identical(out$ID, 3:2)
    expect_identical(out$Width, c(6L, 3L))
    expect_identical(out$Height, c(600L, 700L))

    # Width and height constraints work correctly.
    out <- iSEE:::.setup_initial(DataFrame(Name="Column data plot 3", Width=0L, Height=10), memory)
    expect_identical(out$Width, iSEE:::width_limits[1])
    expect_identical(out$Height, iSEE:::height_limits[1])

    out <- iSEE:::.setup_initial(DataFrame(Name="Column data plot 3", Width=100L, Height=1e6), memory)
    expect_identical(out$Width, iSEE:::width_limits[2])
    expect_identical(out$Height, iSEE:::height_limits[2])

    # Throws a message if you request an impossible feature.
    expect_message(out <- iSEE:::.setup_initial(DataFrame(Name=c("Reduced dimension plot 1", "Column data plot 3", "Feature expression plot 2")), memory),
                   "not available")
    expect_identical(out$Type, c("redDimPlot", "colDataPlot"))
    expect_identical(out$ID, c(1L, 3L))
})

test_that("sanitation of memory works correctly", {
    memory <- iSEE:::.setup_memory(sce, redDimArgs=NULL, colDataArgs=NULL, featExprArgs=NULL, rowStatArgs=NULL, rowDataArgs=NULL, heatMapArgs=NULL,
                                   redDimMax=5, colDataMax=3, featExprMax=2, rowStatMax=2, rowDataMax=3, heatMapMax=2)
    init_panels <- iSEE:::.setup_initial(NULL, memory)

    # No effect when there are no links.
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory)
    expect_identical(sanitized, memory)

    # Does NOT remove valid brushing or table links in active plots.
    memory2 <- memory
    memory2$redDimPlot[1, iSEE:::.brushByPlot] <- "Column data plot 1"
    memory2$colDataPlot[1, iSEE:::.colorByRowTable] <- "Row statistics table 1"
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Correctly removes valid brushing or table links in inactive plots.
    memory2 <- memory
    memory2$redDimPlot[2, iSEE:::.brushByPlot] <- "Column data plot 1"
    memory2$colDataPlot[2, iSEE:::.colorByRowTable] <- "Row statistics table 1"
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Correctly removes invalid brushing or table links.
    memory2 <- memory
    memory2$redDimPlot[1, iSEE:::.brushByPlot] <- "Column data plot 2"
    memory2$colDataPlot[1, iSEE:::.colorByRowTable] <- "Row statistics table 2"
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Repeating for the feature expression plots: retains valid table links in active plots.
    memory2 <- memory
    memory2$featExprPlot[1, iSEE:::.featExprXAxisRowTable] <- "Row statistics table 1"
    memory2$featExprPlot[1, iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Removes valid table links in inactive plots.
    memory2 <- memory
    memory2$featExprPlot[2, iSEE:::.featExprXAxisRowTable] <- "Row statistics table 1"
    memory2$featExprPlot[2, iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Removes inactive table links.
    memory2 <- memory
    memory2$featExprPlot[1, iSEE:::.featExprXAxisRowTable] <- "Row statistics table 2"
    memory2$featExprPlot[1, iSEE:::.featExprYAxisRowTable] <- "Row statistics table 2"
    sanitized <- iSEE:::.sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)
})
