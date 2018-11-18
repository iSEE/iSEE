prepareVoiceRecognition <- function(use=FALSE) {
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
#' Identify the decoded panel name that is the smallest edit distance from a recorded voice input.
#'
#' @param x Character string expected to match a decoded panel identifier.
#' See Details.
#' @param max.edits Maximal number of mismatches allowed.
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#'
#' @details
#' A panel identifier is composed of:
#' \itemize{
#' \item a panel type (see \code{\link{panelTypes}}).
#' \item a numeral identifier within panels of that type.
#' }
#'
#' @return Decoded name of the matched panel identifier.
#'
#' @rdname INTERNAL_nearest_decoded_panel
.nearestDecodedPanel <- function(x, memory, max.edits=5) {
    # Split the last word apart: it should be a number (e.g. "one" or "22")
    voiceType <- gsub(" [[:alnum:]]+$", "", x)
    voiceId <- gsub(".* ([[:alnum:]]+)$", "\\1", x)

    decodedType <- .nearestPanelType(voiceType, max.edits=5)
    if (length(decodedType) != 1L) {
        return(NULL)
    }
    encodedType <- panelCodes[[decodedType]]

    # Then identify the numeral index of the requested panel amongst the available ones
    maxPanels <- nrow(memory[[encodedType]])
    # Coerce the recorded word to a numeral
    # Numbers that would take more than two words to pronounce are already recorded as digits
    voiceId <- ifelse(.isDigits(voiceId), as.numeric(voiceId), .digitalizeNumbers(voiceId))
    if (is.na(voiceId)) {
        return(NULL)
    }

    if (voiceId > maxPanels) {
        showNotification(sprintf("'%s' max is %i", decodedType, maxPanels), type="error")
        return(NULL)
    }

    decodedPanel <- paste (decodedType, voiceId)
    decodedPanel
}

#' Nearest panel type
#'
#' Identify the panel type name that is the smallest edit distance from a recorded voice input.
#'
#' @param x Character string expected to match a panel type.
#' @param max.edits Maximal number of mismatches allowed.
#'
#' @return Encoded name of the matched panel type.
#'
#' @rdname INTERNAL_nearest_panel_type
.nearestPanelType <- function(x, max.edits=5) {
    distances <- adist(x, y = panelTypes, partial=FALSE, ignore.case=TRUE)[1, ]

    # we don't want the "closest" at any cost (it can still be very far)
    nearEnough <- distances[which(distances <= max.edits)]
    if (length(nearEnough) == 0L){
        return(character(0))
    }

    nearestMatch <- panelTypes[names(which.min(nearEnough))]
    nearestMatch
}

.nearestValidChoice <- function(x, choices, max.edits=5) {
    names(choices) <- choices
    distances <- adist(x, y = choices, partial=FALSE, ignore.case=TRUE)[1, ]

    # we don't want the "closest" at any cost (it can still be very far)
    nearEnough <- distances[which(distances <= max.edits)]
    if (length(nearEnough) == 0L){
        return(character(0))
    }

    nearestMatch <- names(nearEnough)[which.min(nearEnough)]
    nearestMatch
}

.getValidParameterChoices <- function(parameterName, mode, se){
    if (parameterName == "ColorBy") {
        if (mode %in% row_point_plot_types) {
            create_FUN <- .create_visual_box_for_row_plots
        } else {
            create_FUN <- .define_color_options_for_column_plots
        }
    } else {
        message(sprintf("Parameter '%s' not supported yet", parameterName))
        return(character(0))
    }
    choices <- create_FUN(se)
    choices
}

# .numbersText ----
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
    as.numeric(.numbersText[match(tolower(x), names(.numbersText))])
}

#' @rdname INTERNAL_digitalize_numbers
.isDigits <- function(x) {
    grepl("^[[:digit:]]+$", x)
}
