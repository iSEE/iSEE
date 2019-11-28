#' @importFrom S4Vectors metadata metadata<-
.set_common_info <- function(se, mode, info) {
    if (!is.null(metadata(se)$iSEE)) {
        metadata(se)$iSEE <- list()
    }
    metadata(se)$iSEE[[mode]] <- info
    se
}

#' @importFrom S4Vectors metadata
.get_common_info <- function(se, mode) {
    metadata(se)$iSEE[[mode]]
}

.empty_default <- function(x, field, newdef=NA) {
    if (length(x[[field]])==0L) {
        x[[field]] <- as(newdef, type(x[[field]]))
    }
    x
}
