
# Constructors ----

test_that("Constructor produce a valid object",{
  
  ecm <- ExperimentColorMap(
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
  )

  expect_s4_class(
    ecm,
    "ExperimentColorMap"
  )

})

# assays ----

test_that("assays returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  expect_identical(
    assays(ecm),
    ecm@assays
  )
  
})

test_that("assays<- sets appropriate values",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  new_value <- list()
  assays(ecm) <- new_value
  
  expect_identical(
    assays(ecm),
    new_value
  )
  
})

# colData ----

test_that("colData returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    colData = list(
        passes_qc_checks_s = qc_color_fun
    )
  )
  
  expect_identical(
    colData(ecm),
    ecm@colData
  )
  
})

test_that("colData<- sets appropriate values",{
  
  ecm <- ExperimentColorMap(
    colData = list(
        passes_qc_checks_s = qc_color_fun
    )
  )
  
  new_value <- list(
    new_coldata <- function(n){return("blue")}
  )
  colData(ecm) <- new_value
  
  expect_identical(
    colData(ecm),
    new_value
  )
  
})

# rowData ----

test_that("rowData returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    rowData = list(
        passes_qc_checks_s = qc_color_fun
    )
  )
  
  expect_identical(
    rowData(ecm),
    ecm@rowData
  )
  
})

test_that("rowData<- sets appropriate values",{
  
  logical_colormap <- function(n){
      logical_colors <- c("forestgreen", "firebrick1")
      names(logical_colors) <- c("TRUE", "FALSE")
      return(logical_colors)
  }
  
  ecm <- ExperimentColorMap(
    rowData = list(
        is_MT = logical_colormap
    )
  )
  
  new_value <- list(
    new_rowData = function(n){return("blue")}
  )
  rowData(ecm) <- new_value
  
  expect_identical(
    rowData(ecm),
    new_value
  )
  
})

# assay ----

test_that("assay returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  # character
  expect_identical(
    assay(ecm, "counts"),
    ecm@assays$counts
  )
  
  # numeric
  expect_identical(
    assay(ecm, 1),
    ecm@assays[[1]]
  )
  
})

test_that("assay<- sets appropriate values with character indexing",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  new_value <- function(n){return("red")}
  assay(ecm, "counts") <- new_value
  
  # character
  expect_identical(
    assay(ecm, "counts"),
    new_value
  )
  
})

test_that("assay<- sets appropriate values with numeric indexing",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  new_value <- function(n){return("red")}
  assay(ecm, 1) <- new_value
  
  # character
  expect_identical(
    assay(ecm, 1),
    new_value
  )
  
})

# assayColorMap ----

test_that("assayColorMap returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )

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

test_that("assay<- sets appropriate values with character indexing",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  new_value <- function(n){return("red")}
  assay(ecm, "counts") <- new_value
  
  # character
  expect_identical(
    assay(ecm, "counts"),
    new_value
  )
  
})

test_that("assay<- sets appropriate values with numeric indexing",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  new_value <- function(n){return("red")}
  assay(ecm, 1) <- new_value
  
  # character
  expect_identical(
    assay(ecm, 1),
    new_value
  )
  
})

# colDataColorMap ----

test_that("colDataColorMap returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
 
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

test_that("colDataColorMap<- sets appropriate values with character indexing",{
  
  ecm <- ExperimentColorMap(
    colData = list(
      passes_qc_checks_s = qc_color_fun
    )
  )
  
  new_value <- function(n){return("red")}
  colDataColorMap(ecm, "passes_qc_checks_s") <- new_value
  
  # character
  expect_identical(
    colDataColorMap(ecm, "passes_qc_checks_s"),
    new_value
  )
  
})

# rowDataColorMap ----

test_that("rowDataColorMap returns appropriate values",{
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
 
  # specific > (discrete) all > global > .defaultDiscreteColorMap
  expect_identical(
    rowDataColorMap(ecm, "test", discrete = TRUE)(21L),
    .defaultDiscreteColorMap(21L)
  )
  
  # specific > (continuous) all > global
  expect_identical(
      rowDataColorMap(ecm, "test", discrete = FALSE)(21L),
      assay_continuous_colours(21L)
    )

})

test_that("rowDataColorMap<- sets appropriate values with character indexing",{
  
  ecm <- ExperimentColorMap(
    rowData = list(
      passes_qc_checks_s = qc_color_fun
    )
  )
  
  new_value <- function(n){return("red")}
  rowDataColorMap(ecm, "passes_qc_checks_s") <- new_value
  
  # character
  expect_identical(
    rowDataColorMap(ecm, "passes_qc_checks_s"),
    new_value
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

# isColorMapCompatible (many assays) ----

test_that("isColorMapCompatible catches too many assays color maps", {
  
  ecm_manyAssays <- ExperimentColorMap(
    assays = list(
      counts = count_colors,
      tophat_counts = count_colors,
      cufflinks_fpkm = fpkm_colors,
      cufflinks_fpkm = fpkm_colors,
      rsem_tpm = tpm_colors,
      another = tpm_colors,
      yet_another = tpm_colors,
      last_one_i_promise = tpm_colors,
      oh_well = tpm_colors
    )
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(ecm_manyAssays, sce, error = TRUE),
    "More assays in color map"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(ecm_manyAssays, sce, error = FALSE),
    FALSE
  )
  
})

# isColorMapCompatible (superfluous assays) ----

test_that("isColorMapCompatible catches superfluous assays color map", {
  
  nullECM <- ExperimentColorMap(
    assays = list(
      dummy1 = function(x){NULL}
    )
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(nullECM, sce, error = TRUE),
    "assay `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(nullECM, sce, error = FALSE),
    FALSE
  )
  
})

# isColorMapCompatible (superfluous colData) ----

test_that("isColorMapCompatible catches superfluous colData color map", {
  
  missingColData <- ExperimentColorMap(
  colData = list(
    dummy2 = function(x){NULL}
  )
)
  
  expect_error(
    iSEE:::isColorMapCompatible(missingColData, sce, error = TRUE),
    "colData `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(missingColData, sce, error = FALSE),
    FALSE
  )
  
})

# isColorMapCompatible (superfluous rowData) ----


test_that("isColorMapCompatible catches superfluous rowData color map", {
  
  missingRowData <- ExperimentColorMap(
    rowData = list(
      dummy2 = function(x){NULL}
    )
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(missingRowData, sce, error = TRUE),
    "rowData `.*` in color map missing in experiment"
  )
  expect_identical(
    iSEE:::isColorMapCompatible(missingRowData, sce, error = FALSE),
    FALSE
  )
  
})

# isColorMapCompatible (valid) ----

test_that("isColorMapCompatible accepts compatible color map", {
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors
    ),
    global_continuous = assay_continuous_colours
  )
  
  expect_identical(
    iSEE:::isColorMapCompatible(ecm, sce, error = FALSE),
    TRUE
  )
  
})

# synchronizeAssays ----

test_that("synchronizeAssays works for fully named assays", {
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors,
      tophat_counts = count_colors,
      cufflinks_fpkm = fpkm_colors,
      rsem_tpm = fpkm_colors,
      orphan = count_colors,
      orphan2 = count_colors,
      count_colors,
      tpm_colors
    )
  )
  
  ecm_expected <- ExperimentColorMap(
    assays = list(
      tophat_counts = count_colors,
      cufflinks_fpkm = fpkm_colors,
      rsem_counts = iSEE:::.defaultContinuousColorMap,
      rsem_tpm = fpkm_colors,
      counts = count_colors,
      logcounts = iSEE:::.defaultContinuousColorMap
    )
  )
  
  expect_warning(
    synchronizeAssays(ecm, sce),
    "Unused assays dropped from ecm"
  )
  
  ecm_sync <- synchronizeAssays(ecm, sce)
  
  expect_identical(
    ecm_sync,
    ecm_expected
  )
  
  # The returned ECM must have named in the same order as SCE
  expect_identical(
    assayNames(sce),
    assayNames(ecm_sync)
  )
  
})

test_that("synchronizeAssays requires same number of unnamed assays", {
  
  sce_unnamed <- sce
  assayNames(sce_unnamed) <- rep("", length(assays(sce_unnamed)))
  
  # Different number of un/named colormap
  ecm_unmatched <- ExperimentColorMap(
    assays = list(
      count_colors,
      test = count_colors,
      fpkm_colors
    )
  )
  
  expect_error(
    synchronizeAssays(ecm_unmatched, sce_unnamed),
    "Cannot synchronize assays"
  )
  
})


test_that("synchronizeAssays works for fully _un_named assays", {
  
  sce_unnamed <- sce
  assayNames(sce_unnamed) <- rep("", length(assays(sce_unnamed)))
  
  # same number of un/named colormaps
  ecm_matched <- ExperimentColorMap(
    assays = list(
      dummy = count_colors,
      tophat_counts = count_colors,
      fpkm_colors,
      fpkm_colors,
      count_colors,
      count_colors
    )
  )

  ecm_sync <- synchronizeAssays(ecm_matched, sce_unnamed)
  
  # Expect the input ExperimentColorMap returned as is
  expect_identical(
    ecm_sync,
    ecm_matched
  )
  
  # assayNames may differ, if the input ExperimentColorMap had names
  expect_identical(
    length(assayNames(sce_unnamed)),
    length(assayNames(ecm_sync))
  )
  
})


test_that("synchronizeAssays works for partially named assays", {
  
  sce_some_names <- sce
  assayNames(sce_some_names)[1:3] <- ""
  
  ecm <- ExperimentColorMap(
    assays = list(
      counts = count_colors,
      tophat_counts = count_colors,
      cufflinks_fpkm = fpkm_colors,
      rsem_tpm = fpkm_colors, # missing colormap
      orphan = count_colors,
      orphan2 = count_colors,
      count_colors,
      tpm_colors
    )
  )

  ecm_sync <- synchronizeAssays(ecm, sce_some_names)
  
  ecm_expected <- ExperimentColorMap(
    assays = list(
      iSEE:::.defaultContinuousColorMap,
      iSEE:::.defaultContinuousColorMap,
      iSEE:::.defaultContinuousColorMap,
      rsem_tpm = fpkm_colors,
      counts = count_colors,
      logcounts = iSEE:::.defaultContinuousColorMap
    )
  )
  
  # Expect:
  # - unnamed assays to be assigned default continuous colormap
  # - named assays matched to have the appropriate colormap
  # - named assays unmatched to have the default continuous colormap
  expect_identical(
    ecm_sync,
    ecm_expected
  )
  
  # The returned ECM must have named in the same order as SCE
  expect_identical(
    assayNames(sce_some_names),
    assayNames(ecm_sync)
  )
  
})

