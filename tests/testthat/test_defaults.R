
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

test_that(".override_defaults warns about unexpected settings", 

  expect_warning(
    iSEE:::.override_defaults(
      redDimPlotDefaults(sce, nrow(redDimArgsExtraField)),
      redDimArgsExtraField
    ),
    "unknown field .* in user-specified settings"
  )
  
)
