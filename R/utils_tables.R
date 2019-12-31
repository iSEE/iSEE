#' Generate table filter
#'
#' Generate R commands to filter a data.frame by the global and column search strings used in the \code{\link{datatable}}.
#' This is done using a combination of \code{\link{grepl}} and \code{\link{filterDT}}.
#'
#' @param x An instance of a \linkS4class{Table} class.
#' @param varname String containing the name of the data.frame variable to use in the output commands.
#'
#' @return String containing an R expression to produce a logical vector indicating which rows of the data.frame to retain.
#' Alternatively, if no filters are present, \code{NULL} is returned.
#'
#' @seealso
#' \code{\link{filterDT}}, on which this function is based.
#' @author Aaron Lun
#' @rdname INTERNAL_generate_table_filter
.generate_table_filter <- function(x, varname="tab") {
    search <- x[[.TableSearch]]
    searchcols <- x[[.TableColSearch]]
    if (search!="" || any(searchcols!="")) {
        sprintf("iSEE::filterDT(%s, global=%s,\n    column=%s)", 
            varname, 
            deparse(search),
            .deparse_for_viewing(searchcols, indent=2))
    } else {
        NULL
    }
}
