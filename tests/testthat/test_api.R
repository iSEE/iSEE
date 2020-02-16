# .refineParameters ----
context(".refineParameters")

test_that(".refineParameters handles NULL x", {

    x_classes <- c("ColumnDotPlot", "ColumnTable", "DotPlot", "Panel",
        "RowDotPlot", "RowTable", "Table", "ColumnDataPlot", "ColumnDataTable",
        "ComplexHeatmapPlot", "FeatureAssayPlot", "ReducedDimPlot", "RowDataPlot",
        "RowDataTable", "SampleAssayPlot"
    )

    for (x_class in x_classes) {
        FUN <- selectMethod(".refineParameters", signature=x_class)
        out <- FUN(NULL, sce)
        expect_null(out, NULL)
    }

})

test_that(".refineParameters identifies impossible ColumnDataPlot", {

    x <- ColumnDataPlot()

    colData(sce) <- DataFrame(row.names = colnames(sce))
    sce <- .cacheCommonInfo(x, sce)
    expect_warning(.refineParameters(x, sce),
        "no valid 'colData' fields for 'ColumnDataPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce)
    expect_null(out)

})

test_that(".refineParameters identifies impossible RowDataPlot", {

    x <- RowDataPlot()

    rowData(sce) <- DataFrame(row.names = rownames(sce))
    sce <- .cacheCommonInfo(x, sce)
    expect_warning(.refineParameters(x, sce),
        "no atomic 'rowData' fields for 'RowDataPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce)
    expect_null(out)

})

test_that(".refineParameters identifies impossible SampleAssayPlot", {

    x <- SampleAssayPlot()

    sce0 <- sce[, 0]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no columns for plotting 'SampleAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no named 'assays' for plotting 'SampleAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

})

test_that(".refineParameters identifies impossible FeatureAssayPlot", {

    x <- FeatureAssayPlot()

    sce0 <- sce[0, ]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no rows available for plotting 'FeatureAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no valid 'assays' for plotting 'FeatureAssayPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

})

test_that(".refineParameters identifies impossible ComplexHeatmapPlot", {

    x <- ComplexHeatmapPlot()

    sce0 <- sce[0, ]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no rows available for plotting 'ComplexHeatmapPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(.refineParameters(x, sce0),
        "no valid 'assays' for plotting 'ComplexHeatmapPlot'", fixed=TRUE)
    out <- .refineParameters(x, sce0)
    expect_null(out)

})

# .colorDotPlot ----
context(".colorDotPlot")

test_that(".colorDotPlot returns NULL when coloring DotPlot by nothing", {

    x <- ColumnDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByNothingTitle
    out <- .colorDotPlot(x, LETTERS)
    expect_null(out)

    x <- RowDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByNothingTitle
    out <- .colorDotPlot(x, LETTERS)
    expect_null(out)

})

# .cacheCommonInfo ----
context(".cacheCommonInfo")
test_that(".cacheCommonInfo identifies valid reduced dimension names for ReducedDimPlot", {

    x <- ReducedDimPlot()

    reducedDim(sce, "empty") <- matrix(numeric(0), nrow = ncol(sce), ncol = 0)
    out <- .cacheCommonInfo(x, sce)
    expect_false("empty" %in% .get_common_info(out, "ReducedDimPlot")[["valid.reducedDim.names"]])

    se <- as(sce, "SummarizedExperiment")
    out <- .cacheCommonInfo(x, se)
    expect_identical(.get_common_info(out, "ReducedDimPlot")[["valid.reducedDim.names"]], character(0))

})

test_that(".cacheCommonInfo detects earlier cache", {

    x_classes <- c("ColumnDataPlot", "ColumnDataTable", "ComplexHeatmapPlot",
        "FeatureAssayPlot", "ReducedDimPlot", "RowDataPlot", "RowDataTable", "SampleAssayPlot"
    )

    for (x_class in x_classes) {
        x_instance <- new(x_class)
        for (i in seq_len(2)) {
            sce <- .cacheCommonInfo(x_instance, sce)
            # Run again to trigger !is.null(.get_common_info(se, "CLASS"))
            sce <- .cacheCommonInfo(x_instance, sce)
        }
    }

})

# .renderOutput ----
context(".renderOutput")

test_that(".renderOutput populates output for ComplexHeatmapPlot", {

    x <- ComplexHeatmapPlot(PanelId=1L)
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .renderOutput(x, sce, output = output, pObjects = pObjects, rObjects = rObjects)
    expect_null(out)
    expect_is(output$ComplexHeatmapPlot1, "shiny.render.function")
    expect_is(output$ComplexHeatmapPlot1_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
    expect_is(output$ComplexHeatmapPlot1_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
})

test_that(".renderOutput populates output for DotPlot", {

    x <- ReducedDimPlot(PanelId=1L)
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .renderOutput(x, sce, output = output, pObjects = pObjects, rObjects = rObjects)
    expect_null(out)
    expect_is(output$ReducedDimPlot1, "shiny.render.function")
    expect_is(output$ReducedDimPlot1_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
    expect_is(output$ReducedDimPlot1_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
})

# .addDotPlotDataSelected ----
context(".addDotPlotDataSelected")

test_that(".addDotPlotDataSelected handles RowDotPlot", {

    plot_env <- new.env()

    x <- SampleAssayPlot()

    # no row_selected in plot_env
    out <- .addDotPlotDataSelected(x, plot_env)
    expect_null(out)

    # row_selected exists in plot_env
    plot_env$row_selected <- head(letters, 3)
    plot_env$plot.data <- data.frame(row.names = letters)
    out <- .addDotPlotDataSelected(x, plot_env)
    expect_identical(out, c(
        header1 = "",
        header2 = "# Receiving row point selection",
        SelectBy = "plot.data$SelectBy <- rownames(plot.data) %in% unlist(row_selected);",
        footer = ""))

    # row_selected exists in plot_env with effect Restrict
    x[[iSEE:::.selectEffect]] <- iSEE:::.selectRestrictTitle
    out <- .addDotPlotDataSelected(x, plot_env)
    expect_identical(out, c(
        header1 = "",
        header2 = "# Receiving row point selection",
        SelectBy = "plot.data$SelectBy <- rownames(plot.data) %in% unlist(row_selected);",
        saved = "plot.data.all <- plot.data;",
        subset = "plot.data <- subset(plot.data, SelectBy);",
        footer = ""))

})

# .multiSelectionRestricted ----
context(".multiSelectionRestricted")

test_that(".multiSelectionRestricted handles DotPlot", {

    x <- ReducedDimPlot()

    out <- .multiSelectionRestricted(x)
    expect_false(out)

    x[[iSEE:::.selectEffect]] <- iSEE:::.selectRestrictTitle
    out <- .multiSelectionRestricted(x)
    expect_true(out)
})

test_that(".multiSelectionRestricted handles Panel", {

    x <- new("PanelChildClass")

    out <- .multiSelectionRestricted(x)
    expect_true(out)
})

# .multiSelectionClear ----
context(".multiSelectionClear")

test_that(".multiSelectionClear handles DotPlot", {

    x <- ReducedDimPlot()

    x[[iSEE:::.brushData]] <- list(anything=1L)

    out <- .multiSelectionClear(x)
    expect_identical(out[[iSEE:::.brushData]], list())
})

test_that(".multiSelectionClear handles Panel", {

    x <- new("PanelChildClass")

    out <- .multiSelectionClear(x)
    expect_identical(out, x)
})

# .singleSelectionValue ----
context(".singleSelectionValue")

test_that(".singleSelectionValue handles DotPlot", {

    x <- ReducedDimPlot(PanelId=1L)
    pObjects <- new.env()
    pObjects$contents[["ReducedDimPlot1"]] <- data.frame(X=1, Y=seq_len(100), row.names = paste0("X", seq_len(100)))

    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 1, ymax = 50,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "ReducedDimPlot1_Brush",
        outputId = "ReducedDimPlot1")

    out <- .singleSelectionValue(x, pObjects)
    expect_identical(out, "X1")

    # Brush does not include any data point
    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 1000, ymax = 2000,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "ReducedDimPlot1_Brush",
        outputId = "ReducedDimPlot1")

    out <- .singleSelectionValue(x, pObjects)
    expect_null(out)
})

# multiSelectionInvalidated ----
context(".multiSelectionInvalidated")

test_that(".multiSelectionInvalidated handles Panel", {

    x <- new("PanelChildClass")

    out <- .multiSelectionInvalidated(x)
    expect_false(out)

})

# .multiSelectionAvailable ----
context(".multiSelectionAvailable")

test_that(".multiSelectionAvailable handles Panel", {

    x <- new("PanelChildClass")
    contents <- data.frame(row.names = letters)

    out <- .multiSelectionAvailable(x, contents)
    expect_identical(out, length(letters))

})

# .exportOutput ----
context(".exportOutput")

test_that(".exportOutput handles DotPlot", {

    ReducedDimPlot1 <- ReducedDimPlot(PanelId=1L)
    sce <- .cacheCommonInfo(ReducedDimPlot1, sce)
    ReducedDimPlot1 <- .refineParameters(ReducedDimPlot1, sce)
    memory <- list(ReducedDimPlot1=ReducedDimPlot1)
    pObjects <- mimic_live_app(sce, memory)
    metadata(sce)$colormap <- ExperimentColorMap()

    out <- .exportOutput(memory$ReducedDimPlot1, sce, memory, pObjects$contents)
    expect_identical(out, "ReducedDimPlot1.pdf")

})

test_that(".exportOutput handles Table", {

    ColumnDataTable1 <- ColumnDataTable(PanelId=1L)
    sce <- .cacheCommonInfo(ColumnDataTable1, sce)
    ColumnDataTable1 <- .refineParameters(ColumnDataTable1, sce)
    memory <- list(ColumnDataTable1=ColumnDataTable1)
    pObjects <- mimic_live_app(sce, memory)
    metadata(sce)$colormap <- ExperimentColorMap()

    out <- .exportOutput(memory$ColumnDataTable1, sce, memory, pObjects$contents)
    expect_identical(out, "ColumnDataTable1.csv")

})

test_that(".exportOutput handles Panel", {

    ComplexHeatmapPlot1 <- ComplexHeatmapPlot(PanelId=1L)
    sce <- .cacheCommonInfo(ComplexHeatmapPlot1, sce)
    ComplexHeatmapPlot1 <- .refineParameters(ComplexHeatmapPlot1, sce)
    memory <- list(ComplexHeatmapPlot1=ComplexHeatmapPlot1)
    pObjects <- mimic_live_app(sce, memory)
    metadata(sce)$colormap <- ExperimentColorMap()

    out <- .exportOutput(memory$ComplexHeatmapPlot1, sce, memory, pObjects$contents)
    expect_identical(out, character(0))

})
