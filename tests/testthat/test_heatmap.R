context("heatmap")

memory <- list(
    ComplexHeatmapPlot1=ComplexHeatmapPlot(SelectRowSource="SampAssayPlot1", SelectColSource="FeatAssayPlot1"),
    SampAssayPlot1=SampAssayPlot(),
    FeatAssayPlot1=FeatAssayPlot()
)

pObjects <- mimic_live_app(sce, memory)
metadata(sce)$colormap <- ExperimentColorMap()

test_that(".process_heatmap_assay_colormap handles discrete assays", {

    plot_env <- new.env()

    assay(sce, "letters") <- matrix(sample(letters[1:3], prod(dim(sce)), TRUE), nrow = nrow(sce), ncol = ncol(sce))
    plot_env$plot.data <- assay(sce, "letters")[1:3, 1:3]

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapAssay]] <- "letters"

    out <- .process_heatmap_assay_colormap(x, sce, plot_env)
    expect_identical(out, c(
        ".col_values <- as.vector(plot.data)",
        ".col_values <- setdiff(.col_values, NA)",
        '.col_colors <- colDataColorMap(colormap, "letters", discrete=TRUE)(length(unique(.col_values)))',
        "if (is.null(names(.col_colors))) { names(.col_colors) <- unique(.col_values) }",
        "heatmap_col <- .col_colors"))
})

test_that(".process_heatmap_column_annotations handles column selections", {

    plot_env <- new.env()
    plot_env$col_selected <- head(colnames(sce))

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    plot_env$se <- sce

    out <- .process_heatmap_column_annotations(x, sce, plot_env)
    expect_true(any(out == 'column_col[["Selected points"]] <- c("TRUE"="red", "FALSE"="white")'))

})

test_that(".process_heatmap_column_annotations handles column annotations", {

    plot_env <- new.env()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapColData]] <- c("driver_1_s", "NREADS")

    plot_env$se <- sce

    out <- .process_heatmap_column_annotations(x, sce, plot_env)
    expect_true(any(out == '.col_values <- column_data[["driver_1_s"]]'))
    expect_true(any(out == '.col_values <- column_data[["NREADS"]]'))
    expect_true(any(out == '.column_annot_order <- with(column_data, order(driver_1_s, NREADS))'))
    expect_true(any(out == 'column_data <- column_data[.column_annot_order, , drop=FALSE]'))
})

test_that(".process_heatmap_row_annotations handles row annotations", {

    rowData(sce)[["letters"]] <- sample(letters[1:3], nrow(sce), TRUE)

    plot_env <- new.env()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapRowData]] <- c("letters", "num_cells")

    plot_env$se <- sce

    out <- .process_heatmap_row_annotations(x, sce, plot_env)
    expect_true(any(out == '.col_values <- row_data[["letters"]]'))
    expect_true(any(out == '.col_values <- row_data[["num_cells"]]'))
    expect_true(any(out == 'row_data <- row_data[.heatmap.rows, , drop=FALSE]'))
})

test_that(".process_heatmap_continuous_annotation handles continuous scale for all-identical values", {

    plot_env <- new.env()
    plot_env$.col_values <- rep(1, 2)

    out <- .process_heatmap_continuous_annotation(plot_env)
    expect_identical(out, ".col_FUN <- colorRamp2(breaks = seq(1, 2, length.out = 21L), colors = .col_colors)")
})
