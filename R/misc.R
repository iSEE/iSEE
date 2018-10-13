
#' Available panel types
#' 
#' Displays a data.frame that presents the list of available panel types,
#' with their decoded and encoded names.
#' 
#' Decoded names are used to define the panels visible at initialization.
#' Refer to the documentation of the \code{iSEE} function for more information.
#' 
#' Encoded names are used to name the individual HTML elements in the UI.
#' Those names may be used to anchor tour steps at HTML elements using the \code{rintrojs} package.
#'
#' @param row.names Which column should be set as \code{row.names}.
#' The default \code{NA} does not set rownames.
#'
#' @return A data.frame.
#' @export
#' 
#' @author Kevin Rue-Albrecht
#' 
#' @seealso \code{\link{iSEE}}
#'
#' @examples
#' availablePanelTypes()
availablePanelTypes <- function(row.names=c(NA, "encoded", "decoded")){
    
    row.names <- match.arg(row.names)
    
    df <- data.frame(
        decoded=panelTypes,
        encoded=names(panelTypes),
        row.names=NULL,
        stringsAsFactors=FALSE
    )
    
    if (!is.na(row.names)) {
        rownames(df) <- df[, row.names]
    }
    
    df
}
