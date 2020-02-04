#' Identify numeric assays
#'
#' @param name Index or name of an assay in \code{se}.
#' @param se A SummarizedExperiment object.
#'
#' @return A logical scalar indicating whether the assay is numeric.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_is_assay_numeric
.is_assay_numeric <- function(name, se){
    is.numeric(assay(se, name)[0, 0])
}
