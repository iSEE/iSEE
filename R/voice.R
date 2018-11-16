
# Note: consider package 'english' if we ever need to convert digital numbers to text
.numbersText <- c(
    "zero"=0, "one"=1, "two"=2, "three"=3, "four"=4, "five"=5, "six"=6, "seven"=7, "eight"=8, "nine"=9,
    "ten"=10, "eleven"=11, "twelve"=12, "thirteen"=13, "fourteen"=14, "fifteen"=15, "sixteen"=16, "seventeen"=17, "eighteen"=18, "nineteen"=19, "twenty"=20)

# Voice add panel:
# Voice add panel: reduce dimension panel one
# Voice add panel: reduced dimension panel one

#' Nearest panel type
#'
#' @param x Character string expected to match a panel type.
#' @param max.mismatches Maximal number of mismatches allowed.
#'
#' @return Encoded name of the matched panel type.
.nearestPanelType <- function(x, max.mismatches=5) {
    distances <- adist(x, y = panelTypes, partial = FALSE, ignore.case = TRUE)[1, ]

    nearEnough <- distances[which(distances <= max.mismatches)]

    if (length(nearEnough) == 0L){
        return(character(0))
    }

    nearestMatch <- panelTypes[names(which.min(nearEnough))]

    nearestMatch
}

#' Substitute numbers from words to numerals
#'
#' @param x Character string.
#'
#' @return Character string
.digitalizeNumbers <- function(x) {
    for (text in names(.numbersText)) {
        x <- gsub(text, as.character(.numbersText[text]), x, ignore.case=TRUE)
    }
    x
}

