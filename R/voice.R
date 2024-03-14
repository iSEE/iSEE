#' Prepare speech recognition
#'
#' @param use Whether speech recognition should be enabled.
#'
#' @return A list of HTML content to include in the user interface.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_prepare_speech_recognition
#' @importFrom shiny singleton includeScript
prepareSpeechRecognition <- function(use=FALSE) {
    if (!use) {
        return(list())
    }
    singleton(tags$head(
        tags$script(src="iSEE/annyang.min.js"),
        includeScript(system.file(package = "iSEE", "www", "voice.js"))
    ))
}

#' Nearest panel
#'
#' @param x Character string, typically representing a voice input.
#' @param panels A list of \code{Panel} objects.
#' @param max.edits Maximal number of mismatches allowed.
#'
#' @description
#'
#' \code{.nearestPanelByType} identifies the panel with the type that is the smallest edit distance from \code{x}.
#'
#' \code{.nearestPanelByName} identifies the panel with the full name that is the smallest edit distance from \code{x}.
#'
#' @details
#' A panel full name is composed of a panel type and an identifier.
#' The panel type is returned by \code{\link{.fullName}}, (e.g., "Reduced dimension plot").
#' The panel identifier is an integer value set during the initialization of each panel, uniquely identifying each individual panel within each panel type.
#'
#' @return Integer index of the matched panel, or \code{NULL} if all matches exceed \code{max.edits}.
#'
#' @rdname INTERNAL_nearest_panel
#'
#' @author Kevin Rue-Albrecht
.nearestPanelByType <- function(x, panels, max.edits=Inf) {

    available_types <- vapply(panels, .fullName, character(1))

    .nearestMatch(x, available_types, max.edits=max.edits)
}

#' @rdname INTERNAL_nearest_panel
.nearestPanelByName <- function(x, panels, max.edits=Inf) {

    available_names <- vapply(panels, .getFullName, character(1))

    .nearestMatch(x, available_names, max.edits=max.edits)
}

#' Nearest match
#'
#' @param x Character string, typically representing a voice input.
#' @param y Character vector of available choices (candidates for match).
#' @param max.edits Maximal number of mismatches allowed.
#'
#' @description
#'
#' \code{.nearestMatch} identifies the index of the value in \code{y} that is the smallest edit distance from \code{x}, within the allowed edit distance.
#'
#' \code{.nearestValidChoice} is intended for unnamed character vectors.
#' It identifies the character value in \code{y} that is the smallest edit distance from \code{x}, within the allowed edit distance.
#'
#' \code{.nearestValidNamedChoice} is intended for named vectors.
#' It identifies the character value in \code{y} with the name that is the smallest edit distance from \code{x}, within the allowed edit distance.
#'
#' @return
#'
#' \code{.nearestMatch} returns the integer index of the nearest match in \code{y}, or an empty integer vector if all matches exceed \code{max.edits}.
#'
#' \code{.nearestValidChoice} and \code{.nearestValidNamedChoice} return the closest match as a (named) character value.
#'
#'
#' @rdname INTERNAL_nearestValidChoice
#' @importFrom utils adist
#' @author Kevin Rue-Albrecht
.nearestMatch <- function(x, y, max.edits=Inf) {
    distances <- adist(x, y, partial=FALSE, ignore.case=TRUE)
    distances <- distances[1, ]

    nearEnough <- distances[which(distances <= max.edits)]

    which.min(nearEnough)
}

#' @rdname INTERNAL_nearestValidChoice
#' @importFrom utils adist
.nearestValidChoice <- function(x, y, max.edits=Inf) {
    idx <- .nearestMatch(x, y, max.edits)
    y[idx]
}

#' @rdname INTERNAL_nearestValidChoice
.nearestValidNamedChoice <- function(x, y, max.edits=Inf) {
    nearestMatch <- .nearestValidChoice(x, names(y), max.edits)
    y[nearestMatch]
}

#' List valid coloring choices
#'
#' @rdname INTERNAL_colorByChoices
#'
#' @param colorby_title A character value representing one of the coloring modes.
#' @param se A SingleCellExperiment object.
#'
#' @return A character vector of valid coloring choices.
#' @author Kevin Rue-Albrecht
.colorByChoices <- function(colorby_title, se) {

    if (colorby_title == .colorByNothingTitle) {
        choices <- character(0)
    } else if (colorby_title == .colorByColDataTitle) {
        choices <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
    } else if (colorby_title == .colorByRowDataTitle) {
        choices <- .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
    } else if (colorby_title == .colorByFeatNameTitle) {
        choices <- seq_len(nrow(se))
        names(choices) <- rownames(se)
    } else if (colorby_title == .colorBySampNameTitle) {
        choices <- seq_len(ncol(se))
        names(choices) <- colnames(se)
    }
    choices
}

# .numbers and text ----

# Note: consider package 'english' if we ever need to convert digital numbers to text
# allow common vocal typos
.numbersText <- c(
    "one"=1, "on"=1,
    "two"=2, "to"=2, "too"=2, "tour"=2, "tool"=2, "tooth"=2,
    "three"=3, "free"=3, "freed"=3,
    "four"=4, "for"=4, "fore"=4, "fall"=4,
    "five"=5, "hive"=5,
    "six"=6,
    "seven"=7,
    "eight"=8, "ate"=8, "late"=8,
    "nine"=9, "dine"=9, "line"=9)

#' Substitute numbers from words to numerals
#'
#' @rdname INTERNAL_digitalizeText
#'
#' @description
#' \code{.allDigits} tests whether inputs are entirely composed of digits.
#'
#' \code{.digitalizeText} substitutes words from "one" to "ten" into the corresponding numeral.
#' Selected near-matches are also converted (e.g., "too" is substituted to "2").
#'
#' @param x Character string.
#'
#' @return
#'
#' \code{.allDigits} returns \code{TRUE} for each input entirely composed of digits.
#'
#' \code{.digitalizeText} returns the substituted \code{character} string as a numeric scalar.
#'
#' @author Kevin Rue-Albrecht
.digitalizeText <- function(x) {
    ifelse(
        .allDigits(x),
        as.numeric(x),
        as.numeric(.numbersText[match(tolower(x), names(.numbersText))]))
}

#' @rdname INTERNAL_digitalizeText
.allDigits <- function(x) {
    grepl("^[[:digit:]]+$", x)
}
