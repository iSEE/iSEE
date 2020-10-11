#' Remove comment and whitespace from text
#'
#' @param txt A single character text input.
#'
#' @return A character vector representing valid lines in the tex input.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_convert_text_to_names
.convert_text_to_names <- function(txt) {
    rn <- strsplit(txt, split="\n")[[1]]
    rn <- sub("#.*", "", rn)
    rn <- sub("^ +", "", rn)
    sub(" +$", "", rn)
}

#' Add a step to the tour
#'
#' Utility to add a step to the panel-specific \pkg{rintrojs} tour, generating the \code{element} tag automatically.
#' 
#' @param x A \linkS4class{Panel} object to be toured.
#' @param field String containing the name of the slot of \code{x}, itself corresponding to an interface element to highlight.
#' @param text String containing the text to show in the corresponding step of the tour.
#' @param is_selectize Logical scalar indicating whether \code{field} corresponds to a selectize element.
#'
#' @return Character vector of length two.
#' The first entry contains the \code{element} tag to identify the interface element to highlight,
#' while the second entry contains the \code{text}.
#'
#' Alternatively, \code{NULL} may be returned if \code{\link{.hideInterface}(x, field)} indicates that the corresponding interface element has been hidden.
#'
#' @author Aaron Lun
#'
#' @export
#' @rdname addTourStep
.addTourStep <- function(x, field, text, is_selectize=FALSE) {
    if (.hideInterface(x, field)) {
        return(NULL)
    }

    element <- paste0("#", .getEncodedName(x), "_", field) 
    if (is_selectize) {
        element <- paste(element, "+ .selectize-control")
    }

    c(element, text)
}
