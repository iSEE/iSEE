
# .set_default_assay ----

test_that(".set_default_assay returns 1L there is no logcounts assay", {

  sce_noLogCount <- sce
  assays(sce_noLogCount) <- assays(sce_noLogCount)["counts"]

  expect_identical(
    .set_default_assay(sce_noLogCount),
    1L
  )

})

# .override_defaults ----

test_that(".override_defaults warns about unexpected settings", {

  redDimArgsExtraField <- redDimPlotDefaults(sce, 1)
  redDimArgsExtraField$DummyExtraField <- NA_character_

  expect_warning(
    .override_defaults(
      redDimPlotDefaults(sce, nrow(redDimArgsExtraField)),
      redDimArgsExtraField
    ),
    "unknown field .* in user-specified settings"
  )

  # Correctly overwrites default fields.
  ref <- redDimPlotDefaults(sce, 5)
  out <- .override_defaults(ref, DataFrame(Type=2L))
  expect_identical(out$Type, rep(2:1, c(1,4)))

  out <- .override_defaults(ref, DataFrame(Type=c(2L, 2L)))
  expect_identical(out$Type, rep(2:1, c(2,3)))

  # Correctly handles list inputs.
  zoomed <- c(xmin=1, xmax=2, ymin=5, ymax=5)
  out <- .override_defaults(ref, DataFrame(ZoomData=I(list(zoomed, zoomed))))
  expect_identical(out$ZoomData, list(zoomed, zoomed, NULL, NULL, NULL))
})


test_that(".add_general_parameters_for_*_plots functions correctly respond to internal fields", {
    sce <- .precompute_UI_info(sce, NULL, NULL)

    out <- .add_general_parameters_for_column_plots(DataFrame(a=1), sce)
    expect_identical(out[[.facetRowsByColData]], .get_internal_info(sce, "column_groupable")[1])
    expect_identical(out[[.facetColumnsByColData]], .get_internal_info(sce, "column_groupable")[1])

    SingleCellExperiment:::int_metadata(sce)$iSEE["column_groupable"] <- "YAY"
    out <- .add_general_parameters_for_column_plots(DataFrame(a=1), sce)
    expect_identical(out[[.facetRowsByColData]], "YAY")
    expect_identical(out[[.facetColumnsByColData]], "YAY")

    out <- .add_general_parameters_for_row_plots(DataFrame(b=1), sce)
    expect_identical(out[[.facetRowsByRowData]], .get_internal_info(sce, "row_groupable")[1])
    expect_identical(out[[.facetColumnsByRowData]], .get_internal_info(sce, "row_groupable")[1])

    SingleCellExperiment:::int_metadata(sce)$iSEE["row_groupable"] <- "WHEE"
    out <- .add_general_parameters_for_row_plots(DataFrame(b=1), sce)
    expect_identical(out[[.facetRowsByRowData]], "WHEE")
    expect_identical(out[[.facetColumnsByRowData]], "WHEE")
})
