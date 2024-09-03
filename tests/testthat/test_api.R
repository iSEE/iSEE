# This tests the various class set-up methods.
# library(testthat); library(iSEE); source('setup_sce.R'); source("setup_mimic_live_app.R"); source('setup_classes.R'); source('test_api.R')

# .refineParameters ----
context(".refineParameters")

test_that(".refineParameters handles NULL x", {

    x_classes <- c("ColumnDotPlot", "ColumnTable", "DotPlot", "Panel",
        "RowDotPlot", "RowTable", "Table", "ColumnDataPlot", "ColumnDataTable",
        "ComplexHeatmapPlot", "FeatureAssayPlot", "ReducedDimensionPlot", "RowDataPlot",
        "RowDataTable", "SampleAssayPlot"
    )

    for (x_class in x_classes) {
        FUN <- selectMethod(".refineParameters", signature=x_class)
        out <- FUN(NULL, sce)
        expect_null(out)
    }

})

test_that(".refineParameters identifies impossible ComplexHeatmapPlot", {

    x <- ComplexHeatmapPlot()

    sce0 <- sce[0, ]
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(out <- .refineParameters(x, sce0),
        "no rows available for plotting 'ComplexHeatmapPlot'", fixed=TRUE)
    expect_null(out)

    sce0 <- sce
    assays(sce0) <- List()
    sce0 <- .cacheCommonInfo(x, sce0)
    expect_warning(out <- .refineParameters(x, sce0),
        "no valid 'assays' for plotting 'ComplexHeatmapPlot'", fixed=TRUE)
    expect_null(out)

})

# .cacheCommonInfo ----
context(".cacheCommonInfo")
test_that(".cacheCommonInfo identifies valid reduced dimension names for ReducedDimensionPlot", {

    x <- ReducedDimensionPlot()

    reducedDim(sce, "empty") <- matrix(numeric(0), nrow = ncol(sce), ncol = 0)
    out <- .cacheCommonInfo(x, sce)
    expect_false("empty" %in% .getCachedCommonInfo(out, "ReducedDimensionPlot")[["valid.reducedDim.names"]])

    se <- as(sce, "SummarizedExperiment")
    out <- .cacheCommonInfo(x, se)
    expect_identical(.getCachedCommonInfo(out, "ReducedDimensionPlot")[["valid.reducedDim.names"]], character(0))

})

test_that(".cacheCommonInfo detects earlier cache", {

    x_classes <- c("ColumnDataPlot", "ColumnDataTable", "ComplexHeatmapPlot",
        "FeatureAssayPlot", "ReducedDimensionPlot", "RowDataPlot", "RowDataTable", "SampleAssayPlot"
    )

    for (x_class in x_classes) {
        x_instance <- new(x_class)
        copy <- sce
        expect_null(metadata(copy)$iSEE)

        copy <- .cacheCommonInfo(x_instance, copy)
        cached <- .getCachedCommonInfo(copy, x_class)
        expect_false(is.null(metadata(copy)$iSEE))
        metadata(copy)$iSEE[[x_class]] <- "DONE"

        # Run again: this will not wipe 'status'. 
        copy <- .cacheCommonInfo(x_instance, copy)
        expect_identical(metadata(copy)$iSEE[[x_class]], "DONE")
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

    x <- ReducedDimensionPlot(PanelId=1L)
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- .renderOutput(x, sce, output = output, pObjects = pObjects, rObjects = rObjects)
    expect_null(out)
    expect_is(output$ReducedDimensionPlot1, "shiny.render.function")
    expect_is(output$ReducedDimensionPlot1_INTERNAL_PanelMultiSelectInfo, "shiny.render.function")
    expect_is(output$ReducedDimensionPlot1_INTERNAL_PanelSelectLinkInfo, "shiny.render.function")
})

# .multiSelectionRestricted ----
context(".multiSelectionRestricted")

test_that(".multiSelectionRestricted handles ColumnDotPlot", {
    x <- ReducedDimensionPlot()
    expect_false(.multiSelectionRestricted(x))

    x[[iSEE:::.selectColumnRestrict]] <- TRUE
    expect_true(.multiSelectionRestricted(x))
})

test_that(".multiSelectionRestricted handles RowDotPlot", {
    x <- SampleAssayPlot()
    expect_false(.multiSelectionRestricted(x))

    x[[iSEE:::.selectRowRestrict]] <- TRUE
    expect_true(.multiSelectionRestricted(x))
})

test_that(".multiSelectionRestricted handles Panel", {
    x <- new("PanelChildClass")
    out <- .multiSelectionRestricted(x)
    expect_true(out)
})

test_that(".multiSelectionRestricted handles Tables", {
    x <- ColumnDataTable()
    out <- .multiSelectionRestricted(x)
    expect_true(out)
})

test_that(".multiSelectionRestricted handles ComplexHeatmapPlot", {
    x <- ComplexHeatmapPlot()
    out <- .multiSelectionRestricted(x)
    expect_true(out)
})

# .multiSelectionClear ----
context(".multiSelectionClear")

test_that(".multiSelectionClear handles DotPlot", {

    x <- ReducedDimensionPlot()

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

    x <- ReducedDimensionPlot(PanelId=1L)
    contents <- data.frame(X=1, Y=seq_len(100), row.names = paste0("X", seq_len(100)))

    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 1, ymax = 50,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "ReducedDimensionPlot1_Brush",
        outputId = "ReducedDimensionPlot1")

    out <- .singleSelectionValue(x, contents)
    expect_identical(out, "X1")

    # Brush does not include any data point
    x[[iSEE:::.brushData]] <- list(
        xmin = 0.7, xmax = 1.3, ymin = 1000, ymax = 2000,
        mapping = list(x = "X", y = "Y"),
        log = list(x = NULL, y = NULL), direction = "xy",
        brushId = "ReducedDimensionPlot1_Brush",
        outputId = "ReducedDimensionPlot1")

    out <- .singleSelectionValue(x, contents)
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

# .multiSelectionRestricted ----
context(".multiSelectionRestricted")

test_that(".multiSelectionRestricted handles ColumnDotPlot", {
    
    x <- ColumnDataPlot()
    out <- .multiSelectionResponsive(x, "column")
    expect_true(out)
    
    x <- ColumnDataPlot()
    out <- .multiSelectionResponsive(x, "row")
    expect_false(out)
    
})

test_that(".multiSelectionRestricted handles RowDotPlot", {
    
    x <- RowDataPlot()
    out <- .multiSelectionResponsive(x, "row")
    expect_true(out)
    
    x <- RowDataPlot()
    out <- .multiSelectionResponsive(x, "column")
    expect_false(out)
    
})

test_that(".multiSelectionRestricted handles ColumnTable", {
    
    x <- ColumnDataTable()
    out <- .multiSelectionResponsive(x, "column")
    expect_true(out)
    
    x <- ColumnDataTable()
    out <- .multiSelectionResponsive(x, "row")
    expect_false(out)
    
})

test_that(".multiSelectionRestricted handles RowTable", {
    
    x <- RowDataTable()
    out <- .multiSelectionResponsive(x, "row")
    expect_true(out)
    
    x <- RowDataTable()
    out <- .multiSelectionResponsive(x, "column")
    expect_false(out)
    
})

test_that(".multiSelectionRestricted handles ComplexHeatmapPlot", {
    
    # ignore all incoming selections
    x <- ComplexHeatmapPlot(
        CustomRows = TRUE,
        ShowColumnSelection = FALSE,
        ColumnSelectionRestrict = FALSE,
        RowSelectionRestrict = FALSE
    )
    out <- .multiSelectionResponsive(x, c("row", "column"))
    expect_false(out)
    
    # enable incoming row selection
    x <- ComplexHeatmapPlot(
        CustomRows = FALSE,
        ShowColumnSelection = FALSE,
        ColumnSelectionRestrict = FALSE,
        RowSelectionRestrict = FALSE
    )
    out <- .multiSelectionResponsive(x, "row")
    expect_true(out)
    out <- .multiSelectionResponsive(x, "column")
    expect_false(out)
    
    # enable incoming column selection
    x <- ComplexHeatmapPlot(
        CustomRows = TRUE,
        ShowColumnSelection = TRUE,
        ColumnSelectionRestrict = FALSE,
        RowSelectionRestrict = FALSE
    )
    out <- .multiSelectionResponsive(x, "column")
    expect_true(out)
    out <- .multiSelectionResponsive(x, "row")
    expect_false(out)
    
})

# .exportOutput ----
context(".exportOutput")

test_that(".exportOutput handles DotPlot", {

    ReducedDimensionPlot1 <- ReducedDimensionPlot(PanelId=1L)
    sce <- .cacheCommonInfo(ReducedDimensionPlot1, sce)
    ReducedDimensionPlot1 <- .refineParameters(ReducedDimensionPlot1, sce)
    memory <- list(ReducedDimensionPlot1=ReducedDimensionPlot1)
    pObjects <- mimic_live_app(sce, memory)
    sce <- iSEE:::.set_colormap(sce, ExperimentColorMap())

    out <- .exportOutput(memory$ReducedDimensionPlot1, sce, memory, pObjects$contents)
    expect_identical(out, "ReducedDimensionPlot1.pdf")

})

test_that(".exportOutput handles Table", {

    ColumnDataTable1 <- ColumnDataTable(PanelId=1L)
    sce <- .cacheCommonInfo(ColumnDataTable1, sce)
    ColumnDataTable1 <- .refineParameters(ColumnDataTable1, sce)
    memory <- list(ColumnDataTable1=ColumnDataTable1)
    pObjects <- mimic_live_app(sce, memory)
    sce <- iSEE:::.set_colormap(sce, ExperimentColorMap())

    out <- .exportOutput(memory$ColumnDataTable1, sce, memory, pObjects$contents)
    expect_identical(out, "ColumnDataTable1.csv")

})

test_that(".exportOutput handles ComplexHeatmapPlot", {

    ComplexHeatmapPlot1 <- ComplexHeatmapPlot(PanelId=1L)
    sce <- .cacheCommonInfo(ComplexHeatmapPlot1, sce)
    ComplexHeatmapPlot1 <- .refineParameters(ComplexHeatmapPlot1, sce)
    memory <- list(ComplexHeatmapPlot1=ComplexHeatmapPlot1)
    pObjects <- mimic_live_app(sce, memory)
    sce <- iSEE:::.set_colormap(sce, ExperimentColorMap())

    out <- .exportOutput(memory$ComplexHeatmapPlot1, sce, memory, pObjects$contents)
    expect_identical(out, "ComplexHeatmapPlot1.pdf")

})

test_that(".exportOutput handles Panel", {

    panel1 <- new("PanelChildClass")

    out <- .exportOutput(panel1, sce, list(), list())
    expect_identical(out, character(0))

})

# .defineVisualShapeInterface ----
context(".defineVisualShapeInterface")

test_that(".defineVisualShapeInterface returns NULL if there are no discrete covariate", {

    # Note that at this point there is no discrete covariate cached, even if such a covariate exists
    expect_null(.defineVisualShapeInterface(ColumnDataPlot(), sce))

    # Note that at this point there is no discrete covariate cached, even if such a covariate exists
    expect_null(.defineVisualShapeInterface(RowDataPlot(), sce))
})

test_that(".defineVisualFacetInterface avoids column data if there are no discrete covariate", {
    # Note that at this point there is no discrete covariate cached, even if such a covariate exists
    expect_false(grepl("FacetRowByColData", .defineVisualFacetInterface(ColumnDataPlot(), sce)))

    expect_false(grepl("FacetRowByRowData", .defineVisualFacetInterface(RowDataPlot(), sce)))

    sce <- .cacheCommonInfo(ColumnDataPlot(), sce)
    sce <- .cacheCommonInfo(RowDataPlot(), sce)

    expect_true(grepl("FacetRowByColData", .defineVisualFacetInterface(ColumnDataPlot(), sce)))

    expect_true(grepl("FacetRowByRowData", .defineVisualFacetInterface(RowDataPlot(), sce)))
})
