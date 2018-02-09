
# .set_default_assay ----

test_that(".set_default_assay returns 1L there is no logcounts assay", {
  
  sce_noLogCount <- sce
  assays(sce_noLogCount) <- assays(sce_noLogCount)["counts"]
  
  expect_identical(
    iSEE:::.set_default_assay(sce_noLogCount),
    1L
  )
  
})

# .override_defaults ----

test_that(".override_defaults warns about unexpected settings", {
  
  redDimArgsExtraField <- redDimPlotDefaults(sce, 1)
  redDimArgsExtraField$DummyExtraField <- NA_character_

  expect_warning(
    iSEE:::.override_defaults(
      redDimPlotDefaults(sce, nrow(redDimArgsExtraField)),
      redDimArgsExtraField
    ),
    "unknown field .* in user-specified settings"
  )

  # Correctly overwrites default fields.
  ref <- redDimPlotDefaults(sce, 5)
  out <- iSEE:::.override_defaults(ref, DataFrame(Type=2L))
  expect_identical(out$Type, rep(2:1, c(1,4)))

  out <- iSEE:::.override_defaults(ref, DataFrame(Type=c(2L, 2L)))
  expect_identical(out$Type, rep(2:1, c(2,3)))

  # Correctly handles list inputs.
  zoomed <- c(xmin=1, xmax=2, ymin=5, ymax=5)
  out <- iSEE:::.override_defaults(ref, DataFrame(ZoomData=I(list(zoomed, zoomed))))
  expect_identical(out$ZoomData, list(zoomed, zoomed, NULL, NULL, NULL))
})
