
# Constructors ----

test_that("Constructor produce a valid object",{

  expect_s4_class(
    ExperimentColorMap(
      assays = list(
        counts = count_colors,
        tophat_counts = count_colors,
        cufflinks_fpkm = fpkm_colors,
        cufflinks_fpkm = fpkm_colors,
        rsem_tpm = tpm_colors
      ),
      colData = list(
        passes_qc_checks_s = qc_color_fun
      ),
      global_continuous = assay_continuous_colours
    ),
    "ExperimentColorMap"
  )

})

# Accessor ----

test_that("colDataColorMap returns appropriate values",{

  # specific > (discrete) all > global > .defaultDiscreteColorMap
  expect_identical(
    colDataColorMap(ecm, "test", discrete = TRUE)(21L),
    .defaultDiscreteColorMap(21L)
  )
  
  # specific > (continuous) all > global
  expect_identical(
      colDataColorMap(ecm, "test", discrete = FALSE)(21L),
      assay_continuous_colours(21L)
    )

})

test_that("colDataColorMap returns appropriate values",{

  # specific
  expect_equal(
    assayColorMap(ecm, "counts")(21L),
    count_colors(21L)
  )
  
  # specific > (continuous) all > global
  expect_equal(
    assayColorMap(ecm, "undefined", discrete = FALSE)(21L),
    assay_continuous_colours(21L)
  )

})

# Validity method ----

test_that("Invalid objects are not allowed to be created", {
  
  # color maps must be functions
  expect_error(
    ExperimentColorMap(
      assays = list(
        dummy1 = 'a'
      )
    ),
    "not a function"
  )
  expect_error(
    ExperimentColorMap(
     colData = list(
        dummy2 = NULL
      )
    ),
    "not a function"
  )
  expect_error(
    ExperimentColorMap(
     rowData = list(
        dummy2 = NULL
      )
    ),
    "not a function"
  )
  
  # colData and rowData color maps must be named
  expect_error(
    ExperimentColorMap(
      colData = list(
        dummy1 = function(x){NULL},
        function(x){NULL} # unnamed
      )
    ),
    "must be named"
  )
  expect_error(
    ExperimentColorMap(
      rowData = list(
        dummy1 = function(x){NULL},
        function(x){NULL} # unnamed
      )
    ),
    "must be named"
  )
  
  # all_* slots have specific names
  expect_error(
    ExperimentColorMap(
      all_discrete = list(a = function(x){NULL}),
      all_continuous = list(assays = NULL, b = NULL, rowData = NULL)
    )
  )
  
})

# isColorMapCompatible ----

test_that("Discrepancies between ECM and SE objects are detected", {
  
  expect_error(
    iSEE:::isColorMapCompatible(ecm_manyAssays, sce, error = TRUE),
    "More assays in color map"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(ecm_manyAssays, sce, error = FALSE),
    FALSE
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(nullECM, sce, error = TRUE),
    "assay `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(nullECM, sce, error = FALSE),
    FALSE
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(nullECM, sce, error = TRUE),
    "assay `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(nullECM, sce, error = FALSE),
    FALSE
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(missingColData, sce, error = TRUE),
    "colData `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(missingColData, sce, error = FALSE),
    FALSE
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(missingRowData, sce, error = TRUE),
    "rowData `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(missingRowData, sce, error = FALSE),
    FALSE
  )
  
  expect_identical(
    iSEE:::isColorMapCompatible(ecm, sce, error = FALSE),
    TRUE
  )
  
})
