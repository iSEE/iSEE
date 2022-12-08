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
.generate_tooltip_html <- function(query, metadata) {
    name <- rownames(metadata)
    fields <- sapply(query, .process_tooltip_field, metadata = metadata, USE.NAMES = TRUE)
    HTML(
        paste0(c(
            sprintf("<strong>%s</strong>", name),
            sprintf("%s: <i>%s</i>", names(fields), fields)
            ), collapse = "<br />")
        )
}

#' @rdname INTERNAL_generate_tooltip_html
.process_tooltip_field <- function(query, metadata) {
    label <- names(query)
    cmd <- paste0("metadata", paste0(sprintf("[['%s']]", query), collapse = ''))
    value <- eval(parse(text = cmd))
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
    names(value) <- label
    value
}

# x: DataFrame
# i: vector of nested indices
extractNestedColumn <- function(x, i) {
  if (identical(length(i), 1L)) {
    x[[i]]
  } else {
    getNestedColumn(x[[1]], i[-1])
  }
}
