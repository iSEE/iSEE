#' Add data parameter box
#'
#' Add a data parameter box to all panels,
#' to be filled with elements according to \code{\link{.defineDataInterface}}.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param select_info A list containing the selection information for single/multiple selections of rows/columns.
#' See \code{?\link{.defineDataInterface}} for more details.
#'
#' @return
#' A HTML tag object containing a \code{\link{collapseBox}} with UI elements to modify data parameters.
#' 
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_data_param_box
.create_data_param_box <- function(x, se, select_info) {
    do.call(.collapseBoxHidden,
        c(
            list(x=x, field=.dataParamBoxOpen, title="Data parameters"),
            open=slot(x, .dataParamBoxOpen),
            .defineDataInterface(x, se, select_info)
        )
    )
}
