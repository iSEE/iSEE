.hide_this_thing <- function(x, field, FUN, ...) {
    if (.hideInterface(x, field)) {
        NULL        
    } else {
        FUN(paste0(.getEncodedName(x), "_", field), ...)
    }
}

#' @importFrom shiny selectInput
.selectInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, selectInput, ...)
}

.collapseBoxHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, collapseBox, ...)
}

#' @importFrom shiny radioButtons
.radioButtonsHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, radioButtons, ...)
}

#' @importFrom shiny selectizeInput
.selectizeInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, selectizeInput, ...)
}
