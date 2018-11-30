#' Prepare speech recognition
#'
#' @param use Whether speech recognition should be enabled.
#'
#' @return A list of HTML content to include in the user interface.
#' @author Kevin Rue-Albrecht
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
#' @author Kevin Rue-Albrecht
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
        #nocov start
        showNotification(sprintf("'%s' max is %i", decodedType, maxPanels), type="error")
        return(NULL)
        #nocov end
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
#' @importFrom utils adist
#' @author Kevin Rue-Albrecht
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

#' Nearest valid choice
#' 
#' @description 
#' \code{.nearestValidChoice} is intended for unnamed character vectors.
#' It returns the closest match to the given character value.
#' 
#' \code{.nearestValidNamedChoice} is intended for named vectors.
#' It returns the value whose name is closest to the given character value.
#' 
#' @rdname INTERNAL_nearestValidChoice
#'
#' @param x Character value.
#' @param choices Character vector of valid choices.
#' @param max.edits Allowed edit distance.
#'
#' @return Nearest match amongst the choices within the allowed edit distance.
#' 
#' @importFrom utils adist
#' @author Kevin Rue-Albrecht
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

#' @rdname INTERNAL_nearestValidChoice
.nearestValidNamedChoice <- function(x, choices, max.edits=5) {
    nearestMatch <- .nearestValidChoice(x, names(choices), max.edits)
    choices[nearestMatch]
}

#' List valid parameter choices
#' 
#' @rdname INTERNAL_getValidParameterChoices
#'
#' @param parameterName A column name in a \code{DataFrame} stored in \code{memory}.
#' @param mode String specifying the encoded panel type of the current plot.
#' @param se A SingleCellExperiment object.
#'
#' @return A character vector of valid parameter choices.
#' @author Kevin Rue-Albecht
.getValidParameterChoices <- function(parameterName, mode, se){
    if (parameterName == "ColorBy") {
        if (mode %in% row_point_plot_types) {
            create_FUN <- .define_color_options_for_row_plots
        } else {
            create_FUN <- .define_color_options_for_column_plots
        }
    } else {
        warning(sprintf("Parameter '%s' not supported yet", parameterName))
        return(character(0))
    }
    choices <- create_FUN(se)
    choices
}

#' List valid coloring choices
#' 
#' @rdname INTERNAL_colorByChoices
#'
#' @param colorby_title A character value representing one of the coloring modes.
#' @param se A SingleCellExperiment object.
#'
#' @return A character vector of valid coloring choices.
#' @author Kevin Rue-Albecht
.colorByChoices <- function(colorby_title, se) {
    
    if (colorby_title == .colorByNothingTitle) {
        choices <- character(0)
    } else if (colorby_title == .colorByColDataTitle) {
        choices <- colnames(colData(se))
    } else if (colorby_title == .colorByRowDataTitle) {
        choices <- colnames(rowData(se))
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
#' @rdname INTERNAL_digitalize_numbers
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
#' 
#' @author Kevin Rue-Albrecht
.digitalizeNumbers <- function(x) {
    as.numeric(.numbersText[match(tolower(x), names(.numbersText))])
}

#' @rdname INTERNAL_digitalize_numbers
.isDigits <- function(x) {
    grepl("^[[:digit:]]+$", x)
}
