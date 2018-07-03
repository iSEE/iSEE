#' iSEE: interactive SummarizedExperiment/SingleCellExperiment Explorer
#'
#' \code{iSEE} is a Bioconductor package that provides an interactive Shiny-based
#'  graphical user interface for exploring data stored in \code{SummarizedExperiment}
#'  objects, including row- and column-level metadata. 
#'  Particular attention is given to single-cell data in a \code{SingleCellExperiment}
#'  object with visualization of dimensionality reduction results, e.g., from principal
#'  components analysis (PCA) or t-distributed stochastic neighbour embedding (t-SNE)
#'
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importMethodsFrom BiocGenerics ncol nrow
#' @importFrom methods as new
#' @importFrom reshape2 melt
#' @importFrom methods new slot is
#' @importFrom igraph topo_sort V delete.vertices 
#' set_vertex_attr add_vertices
#' @importFrom graphics plot
#' @importFrom grDevices hcl col2rgb rgb
#' @importFrom viridisLite viridis
#' @importFrom cowplot plot_grid
#' @importFrom scales rescale
#' @author Aaron Lun \email{alun@@wehi.edu.au}
#' @author Charlotte Soneson \email{charlotte.soneson@@uzh.ch}
#' @author Federico Marini \email{marinif@@uni-mainz.de}
#' @author Kevin Rue-Albrecht \email{kevin.rue-albrecht@@kennedy.ox.ac.uk}
#' @name iSEE-pkg
#' @docType package
NULL
