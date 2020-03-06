#' Hidden interface elements
#'
#' Returns an interface element or \code{NULL}, depending on whether the element is hidden based on \code{\link{.hideInterface}}.
#' This allows panels to hide interface elements that are provided by parent classes but are not needed in the subclass.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param field String specifying the name of the suffix of the ID of the interface element.
#' @param FUN Function to create an HTML widget for a given interface element.
#' @param ... Further arguments to pass to \code{FUN} (in \code{.hide_this_thing}),
#' or whichever widget is being used in the specialized functions.
#'
#' @return
#' The output of \code{FUN(id, ..)} is returned where \code{id} is defined by concatenating \code{\link{.getEncodedName}(x)} and \code{field} (separated by an underscore).
#'
#' If \code{.hideInterface(x, field)} is \code{TRUE}, the output is wrapped inside a \code{\link{hidden}} call.
#'
#' @details
#' Wrapping the output inside a \code{\link{hidden}} call is intended to avoid problems with conditional elements.
#' Consider a situation where \code{\link{.conditional_on_radio}} depends on the value of a UI element;
#' one needs the upstream element to exist for the conditional element to have correct behavior.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{.hideInterface}}, which determines whether an interface element should be hidden.
#'
#' \code{\link{.defineInterface}}, where these functions are typically used.
#'
#' \code{\link{.create_selection_param_box}}, for a specific usage example.
#'
#' @rdname INTERNAL_hidden_elements
#' @importFrom shinyjs hidden
.hide_this_thing <- function(x, field, FUN, ...) {
    element <- FUN(paste0(.getEncodedName(x), "_", field), ...)
    if (.hideInterface(x, field)) {
        hidden(element)
    } else {
        element
    }
}

#' @importFrom shiny selectInput
#' @rdname INTERNAL_hidden_elements
.selectInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, selectInput, ...)
}

#' @rdname INTERNAL_hidden_elements
.collapseBoxHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, collapseBox, ...)
}

#' @rdname INTERNAL_hidden_elements
#' @importFrom shiny radioButtons
.radioButtonsHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, radioButtons, ...)
}

#' @rdname INTERNAL_hidden_elements
#' @importFrom shiny radioButtons
checkboxInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, checkboxInput, ...)
}

#' @rdname INTERNAL_hidden_elements
#' @importFrom shiny selectizeInput
.selectizeInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, selectizeInput, ...)
}

#' Conditional elements on radio or checkbox selection
#'
#' Creates a conditional UI element that appears upon a certain choice in a radio button or checkbox group selection.
#'
#' @param id String containing the id of the UI element for the radio buttons or checkbox group.
#' @param choice String containing the choice on which to show the conditional elements.
#' @param on_select Logical scalar specifying whether the conditional element should be shown upon selection in a check box, or upon de-selection (if \code{FALSE}).
#' @param ... UI elements to show conditionally.
#'
#' @return
#' A HTML object containing elements that only appear when \code{choice} is selected in the UI element for \code{id}.
#'
#' @details
#' This function is useful for hiding options that are irrelevant when a different radio button is selected
#' or when the corresponding checkbox element is unselected.
#' In this manner, we can avoid cluttering the UI.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_conditional_elements
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{.create_selection_param_box}},
#' \code{\link{.create_visual_box}}
#'
#' @importFrom shiny conditionalPanel
.conditional_on_radio <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', id, choice), ...)
}

#' @rdname INTERNAL_conditional_elements
#' @importFrom shiny conditionalPanel
.conditional_on_check_solo <- function(id, on_select=TRUE, ...) {
    choice <- ifelse(on_select, 'true', 'false')
    conditionalPanel(condition=sprintf('(input["%s"] == %s)', id, choice), ...)
}

#' @rdname INTERNAL_conditional_elements
#' @importFrom shiny conditionalPanel
.conditional_on_check_group <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(typeof input["%s"] !== "undefined" && input["%s"].includes("%s"))', id, id, choice), ...)
}
