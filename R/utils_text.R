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
