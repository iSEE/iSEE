
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

# Accessor ----

test_that("colDataColorMap returns appropriate values",{

  # specific > (discrete) all > global > .defaultDiscreteColorMap
  expect_equal(
    colDataColorMap(ecm, "test", discrete = TRUE),
    .defaultDiscreteColorMap
  )
  
  # specific > (continuous) all > global
  expect_identical(
      colDataColorMap(ecm, "test", discrete = FALSE),
      assay_continuous_colours
    )

})

test_that("colDataColorMap returns appropriate values",{

  # specific
  expect_equal(
    assayColorMap(ecm, "counts"),
    count_colors
  )
  
  # specific > (continuous) all > global
  expect_equal(
    assayColorMap(ecm, "undefined", discrete = FALSE),
    assay_continuous_colours
  )

})
