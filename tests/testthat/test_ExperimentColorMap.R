
# Default values ----

# default continuous colormap
.defaultContinuousColorMap <- viridis::viridis # function(n)
# default discrete colormap
.defaultDiscreteColorMap <- function(n) {
  # Credit: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Example values ----

count_colors <- function(n){
  c("black","brown","red","orange","yellow")
}

fpkm_colors <- viridis::inferno

tpm_colors <- viridis::plasma

assay_continuous_colours <- function(n){
  c("black","purple","yellow")
}

qc_color_fun <- function(n){
  qc_colors <- c("forestgreen", "firebrick1")
  names(qc_colors) <- c("Y", "N")
  return(qc_colors)
}

stopifnot(
  require(scRNAseq),
  require(scater)
)
data(allen)
sce <- as(allen, "SingleCellExperiment")

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

# ECM test objects ----

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

nullECM <- ExperimentColorMap(
  assays = list(
    dummy1 = function(x){NULL}
  ),
  colData = list(
    dummy2 = function(x){NULL}
  ),
  rowData = list(
    dummy3 = function(x){NULL}
  ),
  all_discrete = list(
    assays = function(x){NULL}
  ),
  all_continuous = list(
    colData = function(x){NULL}
  ),
  global_discrete = function(x){NULL},
  global_continuous = function(x){NULL}
)

missingColData <- ExperimentColorMap(
  colData = list(
    dummy2 = function(x){NULL}
  )
)

missingRowData <- ExperimentColorMap(
  rowData = list(
    dummy2 = function(x){NULL}
  )
)

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
    iSEE:::isColorMapCompatible(ecm, sce, error = TRUE),
    "More assays in color map"
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(nullECM, sce, error = TRUE),
    "assay `.*` in color map missing in experiment"
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(nullECM, sce, error = TRUE),
    "assay `.*` in color map missing in experiment"
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(missingColData, sce, error = TRUE),
    "colData `.*` in color map missing in experiment"
  )
  
  expect_error(
    iSEE:::isColorMapCompatible(missingRowData, sce, error = TRUE),
    "rowData `.*` in color map missing in experiment"
  )
  
  
})
