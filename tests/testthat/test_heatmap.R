# This tests the various heatmap-related functions.
# library(iSEE); library(testthat); source("setup_sce.R"); source("setup_mimic_live_app.R"); source("test_heatmap.R")

context("heatmap")

memory <- list(
    ComplexHeatmapPlot1=ComplexHeatmapPlot(PanelId=1L),
    SampleAssayPlot1=SampleAssayPlot(PanelId=1L),
    FeatureAssayPlot1=FeatureAssayPlot(PanelId=1L)
)

pObjects <- mimic_live_app(sce, memory)
sce <- iSEE:::.set_colormap(sce, ExperimentColorMap())

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

test_that(".process_heatmap_assay_colormap handles custom bounds", {

    plot_env <- new.env()

    plot_env$plot.data <- assay(sce, "tophat_counts")[1:3, 1:3]
    plot_env$colormap <- ExperimentColorMap()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapCustomAssayBounds]] <- TRUE
    x[[iSEE:::.assayLowerBound]] <- NA_real_
    x[[iSEE:::.assayUpperBound]] <- NA_real_

    out <- iSEE:::.process_heatmap_assay_colormap(x, sce, plot_env)
    expect_identical(out, c(
        '.assay_colors <- assayColorMap(colormap, "logcounts", discrete=FALSE)(21L)',
        ".assay_colors <- circlize::colorRamp2(breaks = seq(0, 2581, length.out = 21L), colors = .assay_colors)" ))
})

test_that(".process_heatmap_column_annotations_colorscale handles column selections", {
    plot_env <- new.env()
    plot_env$col_selected <- head(colnames(sce))

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    plot_env$se <- sce
    plot_env$colormap <- ExperimentColorMap()
    plot_env$plot.data <- assay(sce)[1:10,1:10]

    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_true(any(grepl('.column_col[["Selected points"]] <- iSEE::columnSelectionColorMap', out, fixed=TRUE)))

    # What happens when we turn off column selections?
    x[[iSEE:::.heatMapShowSelection]] <- FALSE
    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_false(any(grepl('.column_col[["Selected points"]] <- iSEE::columnSelectionColorMap', out, fixed=TRUE)))
})

test_that(".process_heatmap_column_annotations_colorscale handles existing 'Selected points' column", {
    plot_env <- new.env()
    plot_env$col_selected <- head(colnames(sce))

    x <- memory[["ComplexHeatmapPlot1"]]

    sce$`Selected points` <- "A"
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    plot_env$se <- sce
    plot_env$colormap <- ExperimentColorMap()
    plot_env$plot.data <- assay(sce)[1:10,1:10]

    # Handles existing 'Selected points' in the coldata.
    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_false(any(grepl('.column_col[["Selected points (1)"]] <- iSEE::columnSelectionColorMap', out, fixed=TRUE)))

    x[[iSEE:::.heatMapColData]] <- c("Selected points")
    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_true(any(grepl('.column_col[["Selected points (1)"]] <- iSEE::columnSelectionColorMap', out, fixed=TRUE)))
})

test_that(".process_heatmap_column_annotations_colorscale handles other column annotations", {
    plot_env <- new.env()

    x <- memory[["ComplexHeatmapPlot1"]]
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.heatMapColData]] <- c("driver_1_s", "NREADS")

    plot_env$se <- sce
    plot_env$colormap <- ExperimentColorMap()
    plot_env$plot.data <- assay(sce)[1, , drop=FALSE]

    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_true(any(out == '.color_values <- .column_data[[\"driver_1_s\"]]'))
    expect_true(any(out == '.color_values <- .column_data[["NREADS"]]'))
    expect_true(any(out == '.column_annot_order <- order(.column_data[["Selected points"]], .column_data[["driver_1_s"]], .column_data[["NREADS"]])'))
    expect_true(any(out == '.column_data <- .column_data[.column_annot_order, , drop=FALSE]'))

    # What happens when we turn off column selections?
    x[[iSEE:::.heatMapOrderSelection]] <- FALSE
    out <- iSEE:::.process_heatmap_column_annotations_colorscale(x, sce, plot_env)
    expect_true(any(out == '.column_annot_order <- order(.column_data[["driver_1_s"]], .column_data[["NREADS"]])'))
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
    plot_env$plot.data <- assay(sce)[1:10,1:10]

    out <- iSEE:::.process_heatmap_row_annotations_colorscale(x, sce, plot_env)
    expect_true(any(out == '.color_values <- .row_data[["letters"]]'))
    expect_true(any(out == '.color_values <- .row_data[["num_cells"]]'))
    expect_true(any(out == '.row_data <- .row_data[rownames(plot.data), , drop=FALSE]'))
})

test_that(".generateOutput detects col_selected and row_selected", {

    pObjects <- new.env()

    x <- memory$FeatureAssayPlot1
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 1000, ymax = 2000,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "FeatureAssayPlot1_Brush",
        outputId = "FeatureAssayPlot1")
    memory$FeatureAssayPlot1 <- x
    out <- .generateOutput(memory$FeatureAssayPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    pObjects$contents[["FeatureAssayPlot1"]] <- out$contents

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.selectColumnSource]] <- "FeatureAssayPlot1"
    x[[iSEE:::.selectColumnRestrict]] <- TRUE
    x[[iSEE:::.heatMapCustomFeatNames]] <- TRUE
    x[[iSEE:::.heatMapFeatNameText]] <- paste0(head(rownames(sce), 2), collapse = "\n")
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_identical(out$commands$assay[["rows"]], '.chosen.rows <- c("Lamp5", "Fam19a1");')
    expect_identical(out$commands$assay[["columns"]], '.chosen.columns <- intersect(colnames(se), unlist(col_selected));')
    expect_identical(out$commands$assay[["data"]], 'plot.data <- assay(se, "logcounts")[.chosen.rows, .chosen.columns, drop=FALSE];\nplot.data <- as.matrix(plot.data);')
})

test_that(".generateOutput handles row_selected when not using custom feature names", {

    pObjects <- new.env()

    x <- memory$SampleAssayPlot1
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 25000, ymax = 50000,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "SampleAssayPlot1_Brush",
        outputId = "SampleAssayPlot1")
    memory$SampleAssayPlot1 <- x
    out <- .generateOutput(memory$SampleAssayPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    pObjects$contents[["SampleAssayPlot1"]] <- out$contents

    x <- ComplexHeatmapPlot(CapRowSelection=0L) # skip the capping for the time being.
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)
    x[[iSEE:::.selectRowSource]] <- "SampleAssayPlot1"
    x[[iSEE:::.heatMapCustomFeatNames]] <- FALSE
    memory$ComplexHeatmapPlot1 <- x

    out <- .generateOutput(memory$ComplexHeatmapPlot1, sce, all_memory = memory, all_contents = pObjects$contents)
    expect_identical(out$commands$assay[["rows"]], ".chosen.rows <- intersect(rownames(se), unlist(row_selected));")
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

    x[[iSEE:::.heatMapAssay]] <- "tophat_counts"
    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE

    out <- iSEE:::.process_heatmap_assay_row_transformations(x, sce, envir)
    expect_identical(out, c(
        "plot.data <- plot.data - rowMeans(plot.data)",
        "plot.data <- plot.data / apply(plot.data, 1, sd)" ))
})

test_that(".create_visual_box_for_complexheatmap handles continuous and discrete assays", {

    x <- ComplexHeatmapPlot()
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    out <- iSEE:::.create_visual_box_for_complexheatmap(x, sce)
    expect_false(any(grepl("shinyjs-disabled", unlist(out)))) # none of the UI are disabled

    x[[iSEE:::.heatMapAssay]] <- "letters"

    out <- iSEE:::.create_visual_box_for_complexheatmap(x, sce)
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

    out <- iSEE:::.build_heatmap_assay_legend_title(x, discrete = TRUE)
    expect_identical(out, "letters")

    x[[iSEE:::.heatMapAssay]] <- "tophat_counts"

    out <- iSEE:::.build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- FALSE
    out <- iSEE:::.build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts (centered)")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE
    out <- iSEE:::.build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts (centered, scaled)")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- FALSE
    x[[iSEE:::.plotLegendDirection]] <- "Vertical"
    out <- iSEE:::.build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts\n(centered)")

    x[[iSEE:::.assayCenterRows]] <- TRUE
    x[[iSEE:::.assayScaleRows]] <- TRUE
    x[[iSEE:::.plotLegendDirection]] <- "Vertical"
    out <- iSEE:::.build_heatmap_assay_legend_title(x, discrete = FALSE)
    expect_identical(out, "tophat_counts\n(centered, scaled)")

})

test_that("constructor concatenates newlines in text", {

    com <- ComplexHeatmapPlot(PanelId=1L, CustomRowsText=LETTERS)
    stored <- com[["CustomRowsText"]]
    expect_identical(stored, paste(LETTERS, collapse="\n"))

})

test_that(".extractAssaySubmatrix works without coercion to matrix", {

    evalenv <- new.env()
    evalenv$se <- sce
    evalenv$row_selected <- head(rownames(sce))
    evalenv$col_selected <- head(colnames(sce))

    x <- ComplexHeatmapPlot(CustomRows=FALSE, ColumnSelectionRestrict=TRUE)
    extracted <- .extractAssaySubmatrix(x, sce, evalenv, 
        use_custom_row_slot=iSEE:::.heatMapCustomFeatNames,
        custom_row_text_slot=iSEE:::.heatMapFeatNameText,
        as_matrix=FALSE
    )

    expect_identical(extracted[["data"]], 'plot.data <- assay(se, "logcounts")[.chosen.rows, .chosen.columns, drop=FALSE];')
    expect_identical(dim(evalenv$plot.data), c(length(evalenv$row_selected), length(evalenv$col_selected)))
})


test_that(".extractAssaySubmatrix works with a cap on the row selections", {

    evalenv <- new.env()
    evalenv$se <- sce
    evalenv$row_selected <- head(rownames(sce))
    evalenv$col_selected <- head(colnames(sce))

    x <- ComplexHeatmapPlot(CustomRows=FALSE, ColumnSelectionRestrict=TRUE, CapRowSelection=2L)
    extracted <- .extractAssaySubmatrix(x, sce, evalenv, 
        use_custom_row_slot=iSEE:::.heatMapCustomFeatNames,
        custom_row_text_slot=iSEE:::.heatMapFeatNameText,
        cap_row_selection_slot=iSEE:::.heatMapCapRowSelection,
        as_matrix=FALSE
    )

    expect_identical(extracted[["data"]], 'plot.data <- assay(se, "logcounts")[.chosen.rows, .chosen.columns, drop=FALSE];')
    expect_identical(dim(evalenv$plot.data), c(2L, length(evalenv$col_selected)))
})
