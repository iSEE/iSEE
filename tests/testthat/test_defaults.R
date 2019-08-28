context("defaults")

# redDimPlotDefaults ----

test_that("redDimPlotDefaults supports objects without reduced dimensions", {
    sce0 <- SingleCellExperiment()

    out <- redDimPlotDefaults(sce0, 0)
    expect_identical(nrow(out), 0L)
})

# .set_default_assay ----

test_that(".set_default_assay returns 1L there is no logcounts assay", {
    sce_noLogCount <- sce
    keep <- which(assayNames(sce_noLogCount) != "logcounts")
    assays(sce_noLogCount) <- assays(sce_noLogCount)[keep]

    expect_identical(iSEE:::.set_default_assay(sce_noLogCount), 1L)
})

# .override_defaults ----

test_that(".override_defaults warns about unexpected settings", {
    redDimArgsExtraField <- redDimPlotDefaults(sce, 1)
    redDimArgsExtraField$DummyExtraField <- NA_character_

    expect_warning(
        iSEE:::.override_defaults(
            redDimPlotDefaults(sce, nrow(redDimArgsExtraField)),
            redDimArgsExtraField),
        "unknown field .* in user-specified settings"
    )

    # Correctly overwrites default fields.
    ref <- redDimPlotDefaults(sce, 5)
    out <- iSEE:::.override_defaults(ref, DataFrame(Type = 2L))
    expect_identical(out$Type, rep(2:1, c(1, 4)))

    out <- iSEE:::.override_defaults(ref, DataFrame(Type = c(2L, 2L)))
    expect_identical(out$Type, rep(2:1, c(2, 3)))

    # Correctly handles list inputs.
    zoomed <- c(xmin = 1, xmax = 2, ymin = 5, ymax = 5)
    out <- iSEE:::.override_defaults(ref, DataFrame(ZoomData = I(list(zoomed, zoomed))))
    expect_identical(out$ZoomData, list(zoomed, zoomed, NULL, NULL, NULL))
})


test_that(
    ".add_general_parameters_for_*_plots functions correctly respond to internal fields",
    {
        sce <- iSEE:::.precompute_UI_info(sce, NULL, NULL)

        out <- iSEE:::.add_general_parameters_for_column_plots(DataFrame(a = 1), sce)
        expect_identical(out[[iSEE:::.facetRowsByColData]], iSEE:::.get_internal_info(sce, "column_groupable")[1])
        expect_identical(out[[iSEE:::.facetColumnsByColData]], iSEE:::.get_internal_info(sce, "column_groupable")[1])

        SingleCellExperiment:::int_metadata(sce)$iSEE["column_groupable"] <- "YAY"
        out <- iSEE:::.add_general_parameters_for_column_plots(DataFrame(a = 1), sce)
        expect_identical(out[[iSEE:::.facetRowsByColData]], "YAY")
        expect_identical(out[[iSEE:::.facetColumnsByColData]], "YAY")

        out <- iSEE:::.add_general_parameters_for_row_plots(DataFrame(b = 1), sce)
        expect_identical(out[[iSEE:::.facetRowsByRowData]], iSEE:::.get_internal_info(sce, "row_groupable")[1])
        expect_identical(out[[iSEE:::.facetColumnsByRowData]], iSEE:::.get_internal_info(sce, "row_groupable")[1])

        SingleCellExperiment:::int_metadata(sce)$iSEE["row_groupable"] <- "WHEE"
        out <- iSEE:::.add_general_parameters_for_row_plots(DataFrame(b = 1), sce)
        expect_identical(out[[iSEE:::.facetRowsByRowData]], "WHEE")
        expect_identical(out[[iSEE:::.facetColumnsByRowData]], "WHEE")
    }
)
