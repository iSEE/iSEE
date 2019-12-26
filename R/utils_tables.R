#' Generate table filter
#'
#' Generate R commands to filter a data.frame by the global and column search strings used in the \code{\link{datatable}}.
#'
#' @param x An instance of a \linkS4class{Table} class.
#' @param varname String containing the name of the data.frame variable to use in the output commands.
#'
#' @return String containing an R expression to produce a logical vector indicating which rows of the data.frame to retain.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_generate_table_filter
.generate_table_filter <- function(panel, varname="tab") {
    filters <- NULL

    search <- panel[[.TableSearch]]
    if (search!="") {
        filters <- c(filters,
            sprintf("Reduce('|', lapply(%s, FUN=grepl, pattern=%s))", 
                varname, deparse(search)))
    }

    searchcols <- panel[[.TableColSearch]]
    involved <- which(searchcols!="")
    if (length(involved)) {
        filters <- c(filters, sprintf("iSEE::filterDTColumn(%s[[%i]], %s)", 
            varname, involved, vapply(searchcols[involved], deparse, "")))
    }

    if (!is.null(filters)) {
        filters <- paste(filters, collapse="\n    & ")
    }
    filters
}


