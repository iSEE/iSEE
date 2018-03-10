#' Check compatibility between ExperimentColorMap and SummarizedExperiment
#' objects
#' 
#' This function compares a pair of \linkS4class{ExperimentColorMap} and
#' \linkS4class{SingleCellExperiment} objects, and examines whether
#' all of the \code{assays}, \code{colData}, and \code{rowData} defined
#' in the ExperimentColorMap object exist in the SingleCellExperiment object.
#'
#' @param ecm An \linkS4class{ExperimentColorMap}.
#' @param se A \linkS4class{SingleCellExperiment}.
#' @param error A logical value that indicates whether an informative error
#' should be thrown, describing why the two objects are not compatible.
#'
#' @return A logical value that indicates whether a given pair of
#' ExperimentColorMap and SummarizedExperiment objects are compatible.
#' If \code{error=TRUE}, an informative error is thrown,
#' rather than returing \code{FALSE}.
#' 
#' @export 
#'
#' @examples
#' 
#' # Example color maps ----
#'
#' count_colors <- function(n){
#'   c("black","brown","red","orange","yellow")
#' }
#'
#' qc_color_fun <- function(n){
#'   qc_colors <- c("forestgreen", "firebrick1")
#'   names(qc_colors) <- c("Y", "N")
#'   return(qc_colors)
#' }
#'
#' ecm <- ExperimentColorMap(
#'     assays = list(
#'         tophat_counts = count_colors
#'     ),
#'     colData = list(
#'         passes_qc_checks_s = qc_color_fun
#'     )
#' )
#'
#' # Example SingleCellExperiment ----
#' 
#' library(scRNAseq)
#' data(allen)
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' 
#' # Test for compatibility ----
#' 
#' isColorMapCompatible(ecm, sce)
#' 
isColorMapCompatible <- function(ecm, se, error = FALSE){
  
  # The count of color maps cannot exceed the count of assays
  num_assay_maps <- length(ecm@assays)
  num_assay_se <- length(se@assays)
  if (num_assay_maps > num_assay_se){
    if (error){
      stop(
        "More assays in color map (",
        num_assay_maps,
        ") than experiment (",
        num_assay_se,
        ")")
    } else {
      return(FALSE)
    }
  }
  
  # Named color maps must map to existing data in the experiment
  names_assays_maps <- names(ecm@assays)
  names_coldata_maps <- names(ecm@colData)
  names_rowdata_maps <- names(ecm@rowData)
  
  names_assays_se <- assayNames(se)
  names_coldata_se <- names(colData(se))
  names_rowdata_se <- names(rowData(se))
  
  # process assays
  names_assays_maps <- names_assays_maps[names_assays_maps != ""]
  check_assay_names <- names_assays_maps %in% names_assays_se
  if (!all(check_assay_names)){
    if (error){
      stop(
        sprintf(
          "assay `%s` in color map missing in experiment",
          names_assays_maps[!check_assay_names]
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  # process colData
  check_coldata_names <- names_coldata_maps %in% names_coldata_se
  if (!all(check_coldata_names)){
    if (error){
      stop(
        sprintf(
          "colData `%s` in color map missing in experiment",
          names_coldata_maps[!check_coldata_names]
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  # process rowData
  check_rowdata_names <- names_rowdata_maps %in% names_rowdata_se
  if (!all(check_rowdata_names)){
    if (error){
      stop(
        sprintf(
          "rowData `%s` in color map missing in experiment",
          names_rowdata_maps[!check_rowdata_names]
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#' Synchronise assay color maps to match those in a SummarizedExperiment
#'
#' @param ecm An \linkS4class{ExperimentColorMap}.
#' @param se A \linkS4class{SingleCellExperiment}.
#'
#' @return An \linkS4class{ExperimentColorMap} with color maps in the
#' \code{assay} slot synchronised to match the position of the corresponding
#' assay in the \linkS4class{SingleCellExperiment}.
#' 
#' @export
#'
#' @examples
#' 
#' # Example ExperimentColorMap ----
#'
#' count_colors <- function(n){
#'   c("black","brown","red","orange","yellow")
#' }
#' fpkm_colors <- viridis::inferno
#' 
#' ecm <- ExperimentColorMap(
#'     assays = list(
#'         counts = count_colors,
#'         tophat_counts = count_colors,
#'         cufflinks_fpkm = fpkm_colors,
#'         rsem_counts = count_colors,
#'         orphan = count_colors,
#'         orphan2 = count_colors,
#'         count_colors,
#'         fpkm_colors
#'     )
#' )
#' 
#' # Example SingleCellExperiment ----
#'
#' library(scRNAseq)
#' data(allen)
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#' sce <- runPCA(sce)
#' sce <- runTSNE(sce)
#' 
#' # Example ----
#' 
#' ecm_sync <- synchronizeAssays(ecm, sce)
#' 
synchronizeAssays <- function(ecm, se){
  stopifnot(is(ecm, "ExperimentColorMap"))
  stopifnot(inherits(se, "SummarizedExperiment"))
  
  se_assay_names <- assayNames(se)
  ecm_assay_names <- assayNames(ecm)
  
  # Prepare a warning message for unused color maps
  unnamed_ecm <- which(ecm_assay_names == "")
  unnamed_warning <- ifelse(
    length(unnamed_ecm) == 0,
    "",
    sprintf("unnamed [%s]", paste(unnamed_ecm, collapse = ","))
  )
  
  orphan_ecm <- setdiff(ecm_assay_names, se_assay_names)
  orphan_ecm <- setdiff(orphan_ecm, "")
  orphan_warning <- ifelse(
    length(unnamed_ecm) == 0,
    "",
    sprintf("named [%s]", paste(orphan_ecm, collapse = ","))
  )
  
  ecm_warning <- paste(unnamed_warning, orphan_warning, sep = ", ")
  
  if (all(se_assay_names != "")){
      # Drop assays from ECM that are absent in se
      if (length(orphan_ecm) > 0){
        warning(
              "Unused assays dropped from ecm: ",
              ecm_warning)
      }
    new_ecm_assays <- sapply(
        se_assay_names,
        function(x){assayColorMap(ecm, x)})
  }
  assays(ecm) <- new_ecm_assays
  return(ecm)
}
