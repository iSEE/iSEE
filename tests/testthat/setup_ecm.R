
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
