
#' Create UI element to select an assay
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param field Field identifier.
#' @param assay_names Character vector of available assay names.
#'
#' @return A \code{\link{selectInput}} widget.
.create_assay_selectize_ui <- function(x, field, assay_names) {
    selectInput(paste0(.getEncodedName(x), "_", field), label=NULL,
            choices=assay_names, selected=x[[field]])
}
