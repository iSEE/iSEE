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
#' @param x Character string expected to match a panel type.
#' @param panels A list of \code{Panel} objects.
#' @param max.edits Maximal number of mismatches allowed.
#'
#' @return Decoded name of the matched panel identifier.
#'
#' @rdname INTERNAL_nearest_decoded_panel
#' @author Kevin Rue-Albrecht
.nearestPanelType <- function(x, panels, max.edits=Inf) {

    available_types <- vapply(panels, .fullName, character(1))

    matched_index <- .nearest_match(x, available_types, max.edits=max.edits)
    panels[[matched_index]]
}

#' Nearest panel type
#'
#' Identify the panel type name that is the smallest edit distance from a recorded voice input.
#'
#' @param x Character string expected to match a panel type.
#' @param y Character vector of available panel types (candidates for match).
#' @param max.edits Maximal number of mismatches allowed.
#'
#' @return Index of the nearest match in \code{y}.
#'
#' @rdname INTERNAL_nearest_panel_type
#' @importFrom utils adist
#' @author Kevin Rue-Albrecht
.nearest_match <- function(x, y, max.edits=Inf) {
    distances <- adist(x, y, partial=FALSE, ignore.case=TRUE)
    distances <- distances[1, ]

    # refuse the nearest match if excessively different
    nearEnough <- distances[which(distances <= max.edits)]
    if (length(nearEnough) == 0L){
        return(character(0))
    }

    which.min(nearEnough)
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
.nearestValidNamedChoice <- function(x, choices, max.edits=Inf) {
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
#' \code{.allDigits} tests whether inputs are entirely composed of digits.
#'
#' \code{.digitalizeText} substitutes words from "one" to "ten" into the corresponding numeral.
#' Selected near-matches are also converted (e.g., "too" is substituted to "2").
#'
#' @param x Character string.
#'
#' @return
#'
#' \code{.allDigits} returns \code{TRUE} for each input entirely composed of digits
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
