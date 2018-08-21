# This tests miscellaneous functions in iSEE-extras, required to prepare data for shiny.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_misc.R")

test_that("list updater function works correctly", {
    blah <- new("DataFrame", nrows=20L)
    blah$A <- vector("list", 20)

    # Adding an element.
    out <- .update_list_element(blah, 2, "A", 1:5)
    expect_type(out$A, "list")
    tmp <- blah$A
    tmp[[2]] <- 1:5
    expect_identical(tmp, out$A)

    # Adding an element of a completely different type.
    out <- .update_list_element(out, 5, "A", list("X"))
    expect_type(out$A, "list")
    tmp[[5]] <- list("X")
    expect_identical(tmp, out$A)

    out <- .update_list_element(out, 2, "A", NULL)
    out <- .update_list_element(out, 5, "A", NULL)
    expect_identical(blah, out)
})

test_that("plot feasibility checks work correctly", {
    sce <- .precompute_UI_info(sce, NULL, NULL)

    out <- .check_plot_feasibility(sce)
    expect_true(out$redDimPlot)
    expect_true(out$featAssayPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No genes.
    out <- .check_plot_feasibility(sce[0,])
    expect_true(out$redDimPlot)
    expect_false(out$featAssayPlot)
    expect_true(out$colDataPlot)
    expect_false(out$rowStatTable)
    expect_false(out$rowDataPlot)

    # No samples.
    out <- .check_plot_feasibility(sce[,0])
    expect_false(out$redDimPlot)
    expect_false(out$featAssayPlot)
    expect_false(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No column data.
    sceX <- sce
    colData(sceX) <- colData(sceX)[,0]
    out <- .check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_true(out$featAssayPlot)
    expect_false(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No reduced dimensions.
    sceX <- sce
    reducedDims(sceX) <- SimpleList()
    out <- .check_plot_feasibility(sceX)
    expect_false(out$redDimPlot)
    expect_true(out$featAssayPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No assays.
    sceX <- sce
    for (field in assayNames(sceX)) {
        assay(sceX, field) <- NULL
    }
    out <- .check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_false(out$featAssayPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_true(out$rowDataPlot)

    # No row data
    sceX <- sce
    rowData(sceX) <- rowData(sceX)[,0]
    out <- .check_plot_feasibility(sceX)
    expect_true(out$redDimPlot)
    expect_true(out$featAssayPlot)
    expect_true(out$colDataPlot)
    expect_true(out$rowStatTable)
    expect_false(out$rowDataPlot)
})

test_that("count incrementer works correctly", {
    expect_identical(.increment_counter(0L), 1L)
    expect_identical(.increment_counter(9999L), 0L)
})

test_that("memory setup works correctly", {
    sce <- .precompute_UI_info(sce, list(PCA2="WHEE"), list(DE="WHOO"))  # Adding custom functions for .setup_memory to check.

    # Works correctly in the vanilla setting.
    memory <- .setup_memory(
        se=sce,
        redDimArgs=NULL,
        colDataArgs=NULL,
        featAssayArgs=NULL,
        rowStatArgs=NULL,
        rowDataArgs=NULL,
        sampAssayArgs=NULL,
        colStatArgs=NULL,
        customDataArgs=NULL,
        customStatArgs=NULL,
        heatMapArgs=NULL,
        redDimMax=5,
        colDataMax=3,
        featAssayMax=1,
        rowStatMax=2,
        rowDataMax=3,
        sampAssayMax=1,
        colStatMax=2,
        customDataMax=2,
        customStatMax=3,
        heatMapMax=2)
    expect_identical(nrow(memory$redDimPlot), 5L)
    expect_identical(nrow(memory$colDataPlot), 3L)
    expect_identical(nrow(memory$featAssayPlot), 1L)
    expect_identical(nrow(memory$rowStatTable), 2L)
    expect_identical(nrow(memory$rowDataPlot), 3L)
    expect_identical(nrow(memory$sampAssayPlot), 1L)
    expect_identical(nrow(memory$colStatTable), 2L)
    expect_identical(nrow(memory$customDataPlot), 2L)
    expect_identical(nrow(memory$customStatTable), 3L)
    expect_identical(nrow(memory$heatMapPlot), 2L)

    # Works correctly when arguments are specified.
    memory <- .setup_memory(
        se=sce,
        redDimArgs=DataFrame(Type=2L),
        colDataArgs=DataFrame(XAxis="Column data"),
        featAssayArgs=DataFrame(XAxis="Row table"),
        rowStatArgs=DataFrame(Selected=10L),
        rowDataArgs=DataFrame(XAxis="Row data"),
        sampAssayArgs=DataFrame(YAxisSampeName=3L),
        colStatArgs=DataFrame(Selected=5L),
        customDataArgs=DataFrame(Function="PCA2"),
        customStatArgs=DataFrame(Function="DE"),
        heatMapArgs=DataFrame(Assay=1L),
        redDimMax=5L,
        colDataMax=3L,
        featAssayMax=1L,
        rowStatMax=2L,
        rowDataMax=3L,
        sampAssayMax=1L,
        colStatMax=2L,
        customDataMax=2L,
        customStatMax=3L,
        heatMapMax=2L)
    expect_identical(nrow(memory$redDimPlot), 5L)
    expect_identical(nrow(memory$colDataPlot), 3L)
    expect_identical(nrow(memory$featAssayPlot), 1L)
    expect_identical(nrow(memory$rowStatTable), 2L)
    expect_identical(nrow(memory$rowDataPlot), 3L)
    expect_identical(nrow(memory$sampAssayPlot), 1L)
    expect_identical(nrow(memory$colStatTable), 2L)
    expect_identical(nrow(memory$customDataPlot), 2L)
    expect_identical(nrow(memory$customStatTable), 3L)
    expect_identical(nrow(memory$heatMapPlot), 2L)

    expect_identical(memory$redDimPlot$Type, rep(2:1, c(1, 4))) # Checking arguments were actually replaced.
    expect_identical(memory$colDataPlot$XAxis, rep(c("Column data", "None"), c(1, 2)))
    expect_identical(memory$featAssayPlot$XAxis, "Row table")
    expect_identical(memory$rowStatTable$Selected, c(10L, 1L))
    expect_identical(memory$rowDataPlot$XAxis, rep(c("Row data", "None"), c(1, 2)))
    expect_identical(memory$sampAssayPlot$YAxisSampeName, 3L)
    expect_identical(memory$colStatTable$Selected, c(5L, 1L))
    expect_identical(memory$customDataPlot$Function, c("PCA2", "---"))
    expect_identical(memory$customStatTable$Function, c("DE", "---", "---"))
    expect_identical(memory$heatMapPlot$Assay, c(1L, 6L))

    # Works correctly when the number of arguments is greater than max.
    memory <- .setup_memory(
        sce,
        redDimArgs=DataFrame(Type=2L),
        colDataArgs=DataFrame(XAxis="Column data"),
        featAssayArgs=DataFrame(XAxis="Row table"),
        rowStatArgs=DataFrame(Selected=10L),
        rowDataArgs=DataFrame(XAxis="Row data"),
        sampAssayArgs=DataFrame(YAxisSampeName=3L),
        colStatArgs=DataFrame(Selected=5L),
        customDataArgs=DataFrame(Function="PCA2"),
        customStatArgs=DataFrame(Function="DE"),
        heatMapArgs=DataFrame(Assay=1L),
        redDimMax=0,
        colDataMax=0,
        featAssayMax=0,
        rowStatMax=0,
        rowDataMax=0,
        sampAssayMax=0,
        colStatMax=0,
        customDataMax=0,
        customStatMax=0,
        heatMapMax=0)
    expect_identical(nrow(memory$redDimPlot), 1L)
    expect_identical(nrow(memory$colDataPlot), 1L)
    expect_identical(nrow(memory$featAssayPlot), 1L)
    expect_identical(nrow(memory$sampAssayPlot), 1L)
    expect_identical(nrow(memory$rowStatTable), 1L)
    expect_identical(nrow(memory$rowDataPlot), 1L)
    expect_identical(nrow(memory$colStatTable), 1L)
    expect_identical(nrow(memory$customDataPlot), 1L)
    expect_identical(nrow(memory$customStatTable), 1L)
    expect_identical(nrow(memory$heatMapPlot), 1L)

    expect_identical(memory$redDimPlot$Type, 2L) # Checking arguments were actually replaced.
    expect_identical(memory$colDataPlot$XAxis, "Column data")
    expect_identical(memory$featAssayPlot$XAxis, "Row table")
    expect_identical(memory$sampAssayPlot$YAxisSampeName, 3L)
    expect_identical(memory$rowStatTable$Selected, 10L)
    expect_identical(memory$rowDataPlot$XAxis, "Row data")
    expect_identical(memory$heatMapPlot$Assay, 1L)
    expect_identical(memory$customDataPlot$Function, "PCA2")
    expect_identical(memory$customStatTable$Function, "DE")

    # Works correctly when nothing is feasible.
    sceX <- sce[0,0]
    colData(sceX) <- colData(sceX)[,0]
    reducedDims(sceX) <- SimpleList()
    for (field in assayNames(sceX)) {
        assay(sceX, field) <- NULL
    }
    rowData(sceX) <- rowData(sceX)[,0]
    SingleCellExperiment:::int_metadata(sce) <- list()

    memory <- .setup_memory(
        se=sceX,
        redDimArgs=NULL,
        colDataArgs=NULL,
        featAssayArgs=NULL,
        rowStatArgs=NULL,
        rowDataArgs=NULL,
        sampAssayArgs=NULL,
        colStatArgs=NULL,
        customDataArgs=NULL,
        customStatArgs=NULL,
        heatMapArgs=NULL,
        redDimMax=5,
        colDataMax=3,
        featAssayMax=1,
        sampAssayMax=1,
        rowStatMax=2,
        rowDataMax=3,
        colStatMax=2,
        customDataMax=2,
        customStatMax=3,
        heatMapMax=2)
    expect_identical(nrow(memory$redDimPlot), 0L)
    expect_identical(nrow(memory$colDataPlot), 0L)
    expect_identical(nrow(memory$featAssayPlot), 0L)
    expect_identical(nrow(memory$sampAssayPlot), 0L)
    expect_identical(nrow(memory$rowStatTable), 0L)
    expect_identical(nrow(memory$rowDataPlot), 0L)
    expect_identical(nrow(memory$colStatTable), 0L)
    expect_identical(nrow(memory$customDataPlot), 0L)
    expect_identical(nrow(memory$customStatTable), 0L)
    expect_identical(nrow(memory$heatMapPlot), 0L)
})

test_that("name to index coercion works correctly", {
    set.seed(0)
    ix <- sample(length(LETTERS), 15)

    df <- DataFrame(WHEE=ix)
    out <- .name2index(df, "WHEE", LETTERS)
    expect_identical(df, out)

    df <- DataFrame(WHEE=LETTERS[ix])
    out <- .name2index(df, "WHEE", LETTERS)
    expect_identical(ix, out$WHEE)

    # Behaves sensibly in response to missing values.
    df <- DataFrame(WHEE=LETTERS[ix])
    chosen <- sample(nrow(df), 5)
    df$WHEE[chosen] <- tolower(df$WHEE[chosen])
    out <- .name2index(df, "WHEE", LETTERS)

    ref <- ix
    ref[chosen] <- 1L
    expect_identical(ref, out$WHEE)

    # Handles lists correctly.
    input <- lapply(1:5*4, function(x) { sample(length(LETTERS), x) })
    df <- DataFrame(YAY=I(input))
    out <- .name2index(df, "YAY", LETTERS)
    expect_identical(out, df)

    input2 <- lapply(input, function(x) { LETTERS[x] })
    df2 <- DataFrame(YAY=I(input2))
    out <- .name2index(df2, "YAY", LETTERS)
    expect_identical(out, df)

    df2$YAY <- SimpleList(df2$YAY)
    out <- .name2index(df2, "YAY", LETTERS)
    expect_identical(out, df)
})

test_that("initialization of active panels works correctly", {
    sce <- .precompute_UI_info(sce, list(PCA2="WHEE"), list(DE="WHOO")) # Adding custom functions for .setup_memory to check.

    memory <- .setup_memory(
        se=sce,
        redDimArgs=NULL,
        colDataArgs=NULL,
        rowStatArgs=NULL,
        rowDataArgs=NULL,
        featAssayArgs=NULL,
        sampAssayArgs=NULL,
        colStatArgs=NULL,
        customDataArgs=NULL,
        customStatArgs=NULL,
        heatMapArgs=NULL,
        redDimMax=5L,
        colDataMax=3L,
        rowStatMax=2L,
        rowDataMax=3L,
        featAssayMax=1L,
        sampAssayMax=1L,
        colStatMax=2L,
        customDataMax=2L,
        customStatMax=3L,
        heatMapMax=2L)
    out <- .setup_initial(NULL, memory)
    expect_identical(out$ID, rep(1L, nrow(out)))
    expect_identical(out$Type, unname(rev.translation))

    # Trying with actual specifications.
    out <- .setup_initial(DataFrame(Name=c("Feature assay plot 1", "Reduced dimension plot 2")), memory)
    expect_identical(out$Type, c("featAssayPlot", "redDimPlot"))
    expect_identical(out$ID, 1:2)
    expect_identical(out$Width, rep(4L, 2))
    expect_identical(out$Height, rep(500L, 2))

    out <- .setup_initial(DataFrame(Name=c("Column data plot 3", "Row statistics table 2"), Width=c(6, 3), Height=c(600, 700)), memory)
    expect_identical(out$Type, c("colDataPlot", "rowStatTable"))
    expect_identical(out$ID, 3:2)
    expect_identical(out$Width, c(6L, 3L))
    expect_identical(out$Height, c(600L, 700L))

    # Width and height constraints work correctly.
    out <- .setup_initial(DataFrame(Name="Column data plot 3", Width=0L, Height=10), memory)
    expect_identical(out$Width, width_limits[1])
    expect_identical(out$Height, height_limits[1])

    out <- .setup_initial(DataFrame(Name="Column data plot 3", Width=100L, Height=1e6), memory)
    expect_identical(out$Width, width_limits[2])
    expect_identical(out$Height, height_limits[2])

    # Throws a message if you request an impossible feature.
    expect_message(out <- .setup_initial(DataFrame(Name=c("Reduced dimension plot 1", "Column data plot 3", "Feature assay plot 2")), memory),
                   "not available")
    expect_identical(out$Type, c("redDimPlot", "colDataPlot"))
    expect_identical(out$ID, c(1L, 3L))
})

test_that("sanitation of memory works correctly", {
    sce <- .precompute_UI_info(sce, list(a=1), list(b=1))

    memory <- .setup_memory(
        se=sce,
        redDimArgs=NULL,
        colDataArgs=NULL,
        featAssayArgs=NULL,
        rowStatArgs=NULL,
        rowDataArgs=NULL,
        sampAssayArgs=NULL,
        colStatArgs=NULL,
        customDataArgs=NULL,
        customStatArgs=NULL,
        heatMapArgs=NULL,
        redDimMax=5L,
        colDataMax=3L,
        featAssayMax=2L,
        rowStatMax=2L,
        rowDataMax=3L,
        sampAssayMax=2L,
        colStatMax=2L,
        customDataMax=2L,
        customStatMax=3L,
        heatMapMax=2L
        )
    init_panels <- .setup_initial(NULL, memory)

    # No effect when there are no links.
    sanitized <- .sanitize_memory(init_panels, memory)
    expect_identical(sanitized, memory)

    ##############
    # Does NOT remove valid selecting or table links in active plots.
    memory2 <- memory
    memory2$redDimPlot[1, .selectByPlot] <- "Column data plot 1"
    memory2$colDataPlot[1, .colorByRowTable] <- "Row statistics table 1"
    memory2$rowDataPlot[1, .colorByColTable] <- "Column statistics table 1"
    memory2$rowDataPlot[1, .selectByPlot] <- "Sample assay plot 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Correctly removes valid selecting or table links in inactive plots.
    memory2 <- memory
    memory2$redDimPlot[2, .selectByPlot] <- "Column data plot 1"
    memory2$colDataPlot[2, .colorByRowTable] <- "Row statistics table 1"
    memory2$rowDataPlot[2, .colorByColTable] <- "Column statistics table 1"
    memory2$rowDataPlot[2, .selectByPlot] <- "Sample assay plot 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Correctly removes invalid selecting or table links.
    memory2 <- memory
    memory2$redDimPlot[1, .selectByPlot] <- "Column data plot 2"
    memory2$colDataPlot[1, .colorByRowTable] <- "Row statistics table 2"
    memory2$rowDataPlot[1, .colorByColTable] <- "Column statistics table 2"
    memory2$rowDataPlot[1, .selectByPlot] <- "Sample assay plot 2"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    ##############
    # Repeating for the feature assay plots: retains valid table links in active plots.
    memory2 <- memory
    memory2$featAssayPlot[1, .featAssayXAxisRowTable] <- "Row statistics table 1"
    memory2$featAssayPlot[1, .featAssayYAxisRowTable] <- "Row statistics table 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Removes valid table links in inactive plots.
    memory2 <- memory
    memory2$featAssayPlot[2, .featAssayXAxisRowTable] <- "Row statistics table 1"
    memory2$featAssayPlot[2, .featAssayYAxisRowTable] <- "Row statistics table 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Removes inactive table links.
    memory2 <- memory
    memory2$featAssayPlot[1, .featAssayXAxisRowTable] <- "Row statistics table 2"
    memory2$featAssayPlot[1, .featAssayYAxisRowTable] <- "Row statistics table 2"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    ##############
    # Repeating for the sample assay plots: retains valid table links in active plots.
    memory2 <- memory
    memory2$sampAssayPlot[1, .sampAssayXAxisColTable] <- "Column statistics table 1"
    memory2$sampAssayPlot[1, .sampAssayYAxisColTable] <- "Column statistics table 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Removes valid table links in inactive plots.
    memory2 <- memory
    memory2$sampAssayPlot[2, .sampAssayXAxisColTable] <- "Column statistics table 1"
    memory2$sampAssayPlot[2, .sampAssayYAxisColTable] <- "Column statistics table 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Removes inactive table links.
    memory2 <- memory
    memory2$sampAssayPlot[1, .sampAssayXAxisColTable] <- "Column statistics table 2"
    memory2$sampAssayPlot[1, .sampAssayYAxisColTable] <- "Column statistics table 2"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    ##############
    # Repeating for the heatmaps.
    memory2 <- memory
    memory2$heatMapPlot[1, .heatMapImportSource] <- "Row statistics table 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Removes valid plot/table links in inactive plots.
    memory2 <- memory
    memory2$heatMapPlot[2, .heatMapImportSource] <- "Row statistics table 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Removes inactive plot/table links.
    memory2 <- memory
    memory2$heatMapPlot[1, .heatMapImportSource] <- "Row statistics table 2"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    ##############
    # Repeating for the custom plots. 
    memory2 <- memory
    memory2$customDataPlot[1, .customRowSource] <- "Row data plot 1"
    memory2$customDataPlot[1, .customColSource] <- "Column data plot 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory2)

    # Removes valid plot/table links in inactive plots.
    memory2 <- memory
    memory2$customDataPlot[2, .customRowSource] <- "Row data plot 1"
    memory2$customDataPlot[2, .customColSource] <- "Column data plot 1"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)

    # Removes inactive plot/table links.
    memory2 <- memory
    memory2$customDataPlot[1, .customRowSource] <- "Row data plot 2"
    memory2$customDataPlot[1, .customColSource] <- "Column data plot 2"
    sanitized <- .sanitize_memory(init_panels, memory2)
    expect_identical(sanitized, memory)
})

# grouping ----

test_that("groupability detection functions work", {

    df <- DataFrame()

    # No column returns an empty vector
    expect_identical(
        .which_groupable(df),
        integer()
    )

    max_groupable <-  getOption("iSEE.maxlevels", 24)

    df <- DataFrame(
        groupable1 = factor(rep(seq_len(max_groupable), 2)),
        not_groupable = factor(seq_len(max_groupable * 2)),
        groupable2 = factor(rep(seq_len(max_groupable/2), 4))
    )

    expect_identical(
        .which_groupable(df),
        c(groupable1 = 1L, groupable2 = 3L)
    )


})
