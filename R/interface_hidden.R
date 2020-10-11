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
#' Consider a situation where \code{\link{.conditionalOnRadio}} depends on the value of a UI element;
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
.checkboxInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, checkboxInput, ...)
}

#' @rdname INTERNAL_hidden_elements
#' @importFrom shiny selectizeInput
.selectizeInputHidden <- function(x, field, ...) {
    .hide_this_thing(x, field, selectizeInput, ...)
}

#' Conditional elements on radio or checkbox selection
#'
#' Creates a conditional UI element that appears when the user picks a certain choice in a radio button, single checkbox or checkbox group interface element.
#'
#' @param id String containing the ID of the UI element controlling the relevant choice.
#' @param choice String containing the choice for the radio button or checkbox group on which to show the conditional element(s).
#' @param on_select Logical scalar specifying whether the conditional element should be shown upon selection in a check box, or upon de-selection (if \code{FALSE}).
#' @param ... UI elements to show conditionally.
#'
#' @return
#' A HTML object containing interface elements in \code{...} that only appear when the relevant condition is satisfied.
#'
#' @details
#' These functions are just wrappers around \code{\link{conditionalPanel}},
#' with the added value coming from the pre-written conditional expressions in Javascript.
#' They are useful for hiding elements that are only relevant when the right radio button or checkbox is selected.
#' This means that we avoid cluttering the UI with options that are not immediately useful to the user.
#'
#' @author Aaron Lun
#' @rdname conditional-utils 
#' @seealso
#' \code{\link{conditionalPanel}}, which is used under the hood.
#'
#' @export
#' @importFrom shiny conditionalPanel
.conditionalOnRadio <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(input["%s"] == "%s")', id, choice), ...)
}

#' @export
#' @rdname conditional-utils 
#' @importFrom shiny conditionalPanel
.conditionalOnCheckSolo <- function(id, on_select=TRUE, ...) {
    choice <- ifelse(on_select, 'true', 'false')
    conditionalPanel(condition=sprintf('(input["%s"] == %s)', id, choice), ...)
}

#' @export
#' @rdname conditional-utils 
#' @importFrom shiny conditionalPanel
.conditionalOnCheckGroup <- function(id, choice, ...) {
    conditionalPanel(condition=sprintf('(typeof input["%s"] !== "undefined" && input["%s"].includes("%s"))', id, id, choice), ...)
}
