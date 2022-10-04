#' Generate Text in Tooltip
#'
#' @param name Identifier of the data point.
#' Typically, from the \code{rownames} or \code{colnames}.
#' @param fields Named list of fields to include in the tooltip text.
#' See \emph{Details}.
#' 
#' @details 
#' Every item in the argument \code{fields} is added as a new line in the tooltip.
#' The line is formatted as \dQuote{\bold{name:} value}.
#'
#' @return An \code{\link{HTML}} element that will be displayed in the tooltip.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_generate_tooltip_html
.generate_tooltip_html <- function(name, fields) {
    fields <- sapply(fields, function(x) as.character(x))
    print(name)
    print(fields)
    HTML(
        paste0(c(
            sprintf("<strong>%s</strong>", name),
            sprintf("%s: %s", names(fields), fields)
            ), collapse = "<br />")
        )
}
