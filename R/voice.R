#' Nearest panel type
#'
#' Identify the panel type name that is the smallest edit distance from a recorded voice input.
#'
#' @param x Character string expected to match a panel type.
#' @param max.mismatches Maximal number of mismatches allowed.
#'
#' @return Encoded name of the matched panel type.
#'
#' @rdname INTERNAL_nearest_panel_type
.nearestPanelType <- function(x, max.mismatches=5) {
    distances <- adist(x, y = panelTypes, partial = FALSE, ignore.case = TRUE)[1, ]

    nearEnough <- distances[which(distances <= max.mismatches)]

    if (length(nearEnough) == 0L){
        return(character(0))
    }

    nearestMatch <- panelTypes[names(which.min(nearEnough))]

    nearestMatch
}

# Note: consider package 'english' if we ever need to convert digital numbers to text
.numbersText <- c(
    "one"=1,
    "two"=2, "to"=2, "too"=2, # allow common vocal typos
    "three"=3, "four"=4, "five"=5, "six"=6, "seven"=7, "eight"=8, "nine"=9)

#' Substitute numbers from words to numerals
#'
#' @description
#' \code{.wordIsDigits} tests whether inputs are entirely composed of digits.
#'
#' \code{.digitalizeNumbers} substitutes words from "one" to "ten" into the corresponding numeral.
#'
#' @param x Character string.
#'
#' @return
#'
#' \code{.wordIsDigits} returns \code{TRUE} for each input entirely composed of digits.
#'
#' \code{.digitalizeNumbers} returns the substituted \code{character} string.
#' @rdname INTERNAL_digitalize_numbers
#' @author Kevin Rue-Albrecht
.digitalizeNumbers <- function(x) {
    .numbersText[match(x, names(.numbersText))]
}

#' @rdname INTERNAL_digitalize_numbers
.wordIsDigits <- function(x) {
    grepl("^[[:digit:]]+$", x)
}
