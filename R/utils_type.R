#' Identify numeric assays
#'
#' @param i Index or name of an assay in \code{se}.
#' @param se A SummarizedExperiment object.
#'
#' @return A logical scalar indicating whether the assay is numeric.
#'
#' @details
#' The \code{as.matrix} is necessary to account for non-ordinary matrices,
#' e.g., sparse or file-backed matrices that remain as S4 instances after subsetting.
#' 
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_is_assay_numeric
.is_assay_numeric <- function(i, se) {
    is.numeric(as.matrix(assay(se, i)[0, 0]))
}
