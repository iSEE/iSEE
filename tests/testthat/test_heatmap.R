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

    plot_env$plot.data <- assay(sce, "letters")[1:3, 1:3]
    plot_env$colormap <- ExperimentColorMap()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapAssay]] <- "letters"

    out <- iSEE:::.process_heatmap_assay_colormap(x, sce, plot_env)
    expect_identical(out, c(
        ".assay_values <- unique(as.vector(plot.data))",
        ".assay_values <- setdiff(.assay_values, NA)",
        '.assay_colors <- colDataColorMap(colormap, "letters", discrete=TRUE)(length(.assay_values))',
        "names(.assay_colors) <- .assay_values"))
})

test_that(".process_heatmap_assay_colormap handles centered values", {

    plot_env <- new.env()

    plot_env$plot.data <- matrix(seq_len(10), 5, 2)

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.assayCenterRows]] <- TRUE

    out <- iSEE:::.process_heatmap_assay_colormap(x, sce, plot_env)
    expect_identical(out, c(
        '.assay_colors <- c("purple", "black", "yellow")',
        ".assay_colors <- circlize::colorRamp2(breaks = c(1, 0, 10), colors = .assay_colors)" ))
})

test_that(".process_heatmap_column_annotations_colorscale handles column selections", {

    plot_env <- new.env()
    plot_env$col_selected <- head(colnames(sce))

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    plot_env$se <- sce
    plot_env$.heatmap.columns <- head(colnames(sce))

    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_true(any(out == '.column_col[["Selected points"]] <- c("TRUE"="red", "FALSE"="white")'))

})

test_that(".process_heatmap_column_annotations_colorscale handles column annotations", {

    plot_env <- new.env()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapColData]] <- c("driver_1_s", "NREADS")

    plot_env$se <- sce
    plot_env$colormap <- ExperimentColorMap()
    plot_env$plot.data <- assay(sce)[1, , drop=FALSE]
    plot_env$.heatmap.columns <- head(colnames(sce))

    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_true(any(out == '.color_values <- .column_data[[\"driver_1_s\"]]'))
    expect_true(any(out == '.color_values <- .column_data[["NREADS"]]'))
    expect_true(any(out == '.column_annot_order <- with(.column_data, order(driver_1_s, NREADS))'))
    expect_true(any(out == '.column_data <- .column_data[.column_annot_order, , drop=FALSE]'))
})

test_that(".process_heatmap_row_annotations_colorscale handles row annotations", {

    rowData(sce)[["letters"]] <- sample(letters[1:3], nrow(sce), TRUE)

    plot_env <- new.env()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapRowData]] <- c("letters", "num_cells")

    plot_env$se <- sce
    plot_env$colormap <- ExperimentColorMap()
    plot_env$.heatmap.rows <- head(rownames(sce))

    out <- iSEE:::.process_heatmap_row_annotations_colorscale(x, sce, plot_env)
    expect_true(any(out == '.color_values <- .row_data[["letters"]]'))
    expect_true(any(out == '.color_values <- .row_data[["num_cells"]]'))
    expect_true(any(out == '.row_data <- .row_data[.heatmap.rows, , drop=FALSE]'))
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
    expect_identical(out$commands$assay[["rows"]], '.heatmap.rows <- c("0610007P14Rik", "0610009B22Rik");')
    expect_identical(out$commands$assay[["columns"]], '.heatmap.columns <- intersect(colnames(se), unlist(col_selected));')
    expect_identical(out$commands$assay[["columns"]], '.heatmap.columns <- intersect(colnames(se), unlist(col_selected));')
    expect_identical(out$commands$assay[["data"]], 'plot.data <- assay(se, "tophat_counts")[.heatmap.rows, .heatmap.columns, drop=FALSE]\nplot.data <- as.matrix(plot.data);')
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
    expect_identical(out$commands$assay[["rows"]], ".heatmap.rows <- intersect(rownames(se), unlist(row_selected));")
})

test_that(".generateOutput handles row annotations", {

    pObjects <- new.env()

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapRowData]] <- c("mean_count", "letters")
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_true(grepl("left_annotation=.row_annot", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl('.row_data <- rowData(se)[, c("mean_count", "letters"), drop=FALSE]', out$commands$row_annotations, fixed = TRUE))

})

test_that(".generateOutput handles column annotations", {

    pObjects <- new.env()

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapColData]] <- c("driver_1_s", "NREADS")
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_true(grepl("top_annotation=.column_annot", out$commands$heatmap, fixed = TRUE))
    expect_true(grepl('.column_data <- colData(se)[, c("driver_1_s", "NREADS"), drop=FALSE]', out$commands$column_annotations, fixed = TRUE))
    expect_true(grepl("plot.data <- plot.data[, .column_annot_order, drop=FALSE]", out$commands$column_annotations, fixed = TRUE))
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

test_that(".generateOutput handles centering and scaling", {

    pObjects <- new.env()

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_identical(out$commands$transform, c(
        "plot.data <- plot.data - rowMeans(plot.data)",
        "plot.data <- plot.data / apply(plot.data, 1, sd)"))
})

test_that("process_heatmap_assay_row_transformations handles row centering and scaling", {

    envir <- new.env()
    envir$plot.data <- assay(sce, "tophat_counts")[1, , drop=FALSE]

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    x[[.heatMapAssay]] <- "tophat_counts"
    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE

    out <- .process_heatmap_assay_row_transformations(x, sce, envir)
    expect_identical(out, c(
        "plot.data <- plot.data - rowMeans(plot.data)",
        "plot.data <- plot.data / apply(plot.data, 1, sd)" ))
})

test_that(".create_visual_box_for_complexheatmap handles continuous and discrete assays", {

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    out <- .create_visual_box_for_complexheatmap(x, sce)
    expect_false(any(grepl("shinyjs-disabled", unlist(out)))) # none of the UI are disabled

    x[[iSEE:::.heatMapAssay]] <- "letters"

    out <- .create_visual_box_for_complexheatmap(x, sce)
    expect_true(any(grepl("shinyjs-disabled", unlist(out)))) # some of the UI are disabled

})

test_that(".defineDataInterface handles continuous and discrete assays", {

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    out <- .defineDataInterface(x, sce)
    expect_false(any(grepl("shinyjs-disabled", unlist(out)))) # none of the UI are disabled

    x[[iSEE:::.heatMapAssay]] <- "letters"

    out <- .defineDataInterface(x, sce)
    expect_true(any(grepl("shinyjs-disabled", unlist(out)))) # some of the UI are disabled

})

test_that(".build_heatmap_assay_legend_title handles centering and scaling", {

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    x[[iSEE:::.heatMapAssay]] <- "letters"

    out <- .build_heatmap_assay_legend_title(x, discrete = TRUE)
    expect_identical(out, "letters")

    x[[iSEE:::.heatMapAssay]] <- "tophat_counts"

    out <- .build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- FALSE
    out <- .build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts (centered)")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE
    out <- .build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts (centered, scaled)")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- FALSE
    x[[iSEE:::.plotLegendDirection]] <- "Vertical"
    out <- .build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts\n(centered)")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE
    x[[iSEE:::.plotLegendDirection]] <- "Vertical"
    out <- .build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts\n(centered, scaled)")

})
