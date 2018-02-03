
# Default color maps ----

# default continuous colormap
.defaultContinuousColorMap <- iSEE:::.defaultContinuousColorMap
# default discrete colormap
.defaultDiscreteColorMap <- iSEE:::.defaultDiscreteColorMap

# Example custom color maps ----

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

# Exmple ExperimentColorMap objects ----

# probably need to give it a more memorable name if we create other ones
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
