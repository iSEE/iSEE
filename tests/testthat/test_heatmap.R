context("heatmap")

memory <- list(
    ComplexHeatmapPlot1=ComplexHeatmapPlot(PanelId=1L),
    SampAssayPlot1=SampAssayPlot(PanelId=1L),
    FeatAssayPlot1=FeatAssayPlot(PanelId=1L)
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

test_that(".generateOutput detects col_selected and row_selected", {

    pObjects <- new.env()

    x <- memory$FeatAssayPlot1
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 1000, ymax = 2000,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "FeatAssayPlot1_Brush",
        outputId = "FeatAssayPlot1")
    memory$FeatAssayPlot1 <- x
    out <- .generateOutput(memory$FeatAssayPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    pObjects$contents[["FeatAssayPlot1"]] <- out$contents

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.selectColSource]] <- "FeatAssayPlot1"
    x[[iSEE:::.selectEffect]] <- iSEE:::.selectRestrictTitle
    x[[iSEE:::.heatMapCustomFeatNames]] <- TRUE
    x[[iSEE:::.heatMapFeatNameText]] <- paste0(head(rownames(sce), 2), collapse = "\n")
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_identical(out$commands$columns, ".heatmap.columns <- intersect(colnames(se), unlist(col_selected));")
    expect_identical(out$commands$rows, '.heatmap.rows <- c("0610007P14Rik", "0610009B22Rik");')
})

test_that(".generateOutput handles row_selected when not using custom feature names", {

    pObjects <- new.env()

    x <- memory$SampAssayPlot1
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 25000, ymax = 50000,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "SampAssayPlot1_Brush",
        outputId = "SampAssayPlot1")
    memory$SampAssayPlot1 <- x
    out <- .generateOutput(memory$SampAssayPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    pObjects$contents[["SampAssayPlot1"]] <- out$contents

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.selectRowSource]] <- "SampAssayPlot1"
    x[[iSEE:::.heatMapCustomFeatNames]] <- FALSE
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_identical(out$commands$rows, ".heatmap.rows <- intersect(rownames(se), unlist(row_selected));")
})

test_that(".generateOutput handles row annotations", {

    pObjects <- new.env()

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapRowData]] <- c("mean_count", "letters")
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_true(grepl("left_annotation=row_annot", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl('row_data <- rowData(se)[, c(\"mean_count\", \"letters\"), drop=FALSE]', out$commands$rowdata, fixed = TRUE))

})

test_that(".generateOutput handles column annotations", {

    pObjects <- new.env()

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapColData]] <- c("driver_1_s", "NREADS")
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_true(grepl("top_annotation=column_annot", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl('column_data <- colData(se)[, c(\"driver_1_s\", \"NREADS\"), drop=FALSE]', out$commands$coldata, fixed = TRUE))
    expect_true(grepl("plot.data <- plot.data[, .column_annot_order, drop=FALSE]", out$commands$order_columns, fixed = TRUE))
})


test_that(".generateOutput handles clustering", {

    pObjects <- new.env()

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapClusterFeatures]] <- TRUE
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_true(grepl("cluster_rows=TRUE", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl("clustering_distance_rows=\"spearman\"", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl("clustering_method_rows=\"ward.D2\"", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl("clustering_distance_rows=\"spearman\"", out$commands$heatmap, fixed = TRUE))
})
