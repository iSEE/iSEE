#' Generate Text in Tooltip
#'
#' @param name Identifier of the data point.
#' Typically, from the \code{rownames} or \code{colnames}.
#' @param fields Named list of fields to include in the tooltip text.
#' See \emph{Details}.
#' @param value Scalar representing one value in \code{fields}.
#' 
#' @details 
#' Every item in the argument \code{fields} is added as a separate line in the tooltip.
#' The line is formatted as \dQuote{\bold{name:} value}.
#'
#' @return 
#' \code{.generate_tooltip_html} returns an \code{\link{HTML}} element
#' that will be displayed in the tooltip.
#' 
#' \code{.process_tooltip_field} converts individual fields to a character
#' representation suitable for display. For instance, this may include trimming
#' double-precision scalars to a set number of significant digits.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_generate_tooltip_html
.generate_tooltip_html <- function(name, fields) {
    fields <- sapply(fields, function(x) .process_tooltip_field(x), USE.NAMES = TRUE)
    HTML(
        paste0(c(
            sprintf("<strong>%s</strong>", name),
            sprintf("%s: <i>%s</i>", names(fields), fields)
            ), collapse = "<br />")
        )
}

#' @rdname INTERNAL_generate_tooltip_html
.process_tooltip_field <- function(value) {
    original <- value
    if (is.double(value)) {
        value <- signif(value, digits = getAppOption("tooltip.signif", default = 6))
        value <- ifelse(
            identical(value, original),
            as.character(value),
            paste0(as.character(value), "(...)")
            )
    } else if (is.factor(value)) {
        value <- as.character(value)
    } else {
        value <- as.character(value)
    }
    value
}
