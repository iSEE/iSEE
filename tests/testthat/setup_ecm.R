
# Example custom color maps ----

COUNT_COLORS <- function(n){
  c("black","brown","red","orange","yellow")
}

FPKM_COLORS <- viridis::inferno

TPM_COLORS <- viridis::plasma

ASSAY_CONTINUOUS_COLORS <- function(n){
  c("black","purple","yellow")
}

QC_COLOR_FUN <- function(n){
  qc_colors <- c("forestgreen", "firebrick1")
  names(qc_colors) <- c("Y", "N")
  return(qc_colors)
}
