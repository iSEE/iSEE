#' Identify numeric assays
#'
#' @param i Index or name of an assay in \code{se}.
#' @param se A SummarizedExperiment object.
#'
#' @return A logical scalar indicating whether the assay is numeric.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_is_assay_numeric
.is_assay_numeric <- function(i, se){
    is.numeric(assay(se, i)[0, 0])
}
