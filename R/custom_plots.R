#' Get/set custom column functions
#'
#' Get or set a list of custom column functions using the internal metadata in a SingleCellExperiment.
#'
#' @param se A SingleCellExperiment object.
#' @param fun_list A named list of custom column functions.
#' 
#' @details
#' These functions use the internal metadata of the SingleCellExperiment to store the custom functions.
#' This means that we do not have to add an extra argument to many other internal functions in \pkg{iSEE}.
#'
#' @return
#' \code{.set_custom_col_fun} will store the list of custom column functions in the internal metadata,
#' and return the modified \code{se}.
#'
#' \code{.get_custom_col_fun} will return the named list of custom column functions.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_custom_col_fun
.set_custom_col_fun <- function(se, fun_list) {
    out <-  SingleCellExperiment:::int_metadata(se)$iSEE
    if (is.null(out)) {
        out <- list()
    }
    out$custom_col_fun <- fun_list 
    SingleCellExperiment:::int_metadata(se)$iSEE <- out
    return(se)
}

#' @rdname INTERNAL_custom_col_fun
.get_custom_col_fun <- function(se) {
    SingleCellExperiment:::int_metadata(se)$iSEE$custom_col_fun # AL: will fix this.
}
