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

#' Hidden interface elements
#'
#' Returns an interface element with or without the CSS class \code{shinyjs-hide}, depending on whether the element is hidden based on \code{\link{.hideInterface}}.
#' This allows panels to hide interface elements that are provided by parent classes but are not needed in the subclass.
#' 
#' \code{.selectInputHidden(x, field, ...)} produces a Shiny \code{\link{selectInput}} element that will be hidden if \code{.hideInterface(x)} is \code{TRUE}.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param field String specifying the name of the suffix of the ID of the interface element.
#' @param ... Further arguments to pass to the Shiny function responsible for producing the interface element.
#'
#' @return
#' The output of \code{FUN(id, ..)} is returned where \code{id} is defined by concatenating \code{\link{.getEncodedName}(x)} and \code{field} (separated by an underscore).
#' 
#' @export
#' @importFrom shiny selectInput
#' @name hidden-inputs
#' @author Kevin Rue-Albrecht
#'
#' @examples
#' .selectInputHidden(ComplexHeatmapPlot(), "SelectionHistory",
#'   choices = c(1, 2, 3), label = "Saved selection (hidden)")
#'   
#' .selectInputHidden(ReducedDimensionPlot(), "Type",
#'   choices = c("UMAP", "PCA"), label = "Reduced dimension type (hidden)")
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

.hide_this_thing2 <- function(x, field, element) {
    if (.hideInterface(x, field)) {
        hidden(element)
    } else {
        element
    }
}

#' \pkg{iSEE} UI element wrappers
#'
#' Wrapper functions to create the standard \pkg{shiny} user interface elements,
#' accompanied by an optional help icon that opens an interactive tour describing the purpose of the element.
#' Also responds to requests to hide a particular element via \code{\link{.hideInterface}}.
#'
#' @param x A \linkS4class{Panel} object for which to construct an interface element.
#' @param field String containing the name of the parameter controlled by the interface element.
#' @param label String specifying the label to be shown.
#' @param ... Further arguments to be passed to the corresponding \pkg{shiny} function.
#' @param help Logical scalar indicating whether a help icon should be added to the label.
#'
#' @return
#' The output of \code{FUN(id, ..)} is returned where \code{FUN} is set the corresponding \pkg{shiny} function, e.g., \code{\link{selectInput}} for \code{.selectInput.iSEE}.
#' \code{id} is defined by concatenating \code{\link{.getEncodedName}(x)} and \code{field} (separated by an underscore).
#'
#' If \code{.hideInterface(x, field)} is \code{TRUE}, the output is wrapped inside a \code{\link{hidden}} call.
#'
#' @author Aaron Lun
#'
#' @name interface-wrappers
NULL

#' @importFrom shiny span HTML
.specific_help <- function(id) {
    span(id=paste0(id, "_specific_help"), HTML("<sup>?</sup>"))
}

#' @importFrom shiny HTML
.label_with_help <- function(text, id) {
    HTML(paste(text, as.character(.specific_help(id))))
}

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny selectInput
.selectInput.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)

    ui <- selectInput(element, label=label, ...)
    if (help) {
        helper <- .specific_help(element)
        ui$children <- c(ui$children[1], list(helper), ui$children[-1])
    }

    .hide_this_thing2(x, field, ui)
}

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny selectizeInput
.selectizeInput.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)

    ui <- selectizeInput(element, label=label, ...)
    if (help) {
        helper <- .specific_help(element)
        ui$children <- c(ui$children[1], list(helper), ui$children[-1])
    }

    .hide_this_thing2(x, field, ui)
}

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny HTML span checkboxInput
.checkboxInput.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)
    ui <- checkboxInput(element, label=label, ...)

    if (help) {
        helper <- .specific_help(element)
        ui$children[[1]]$children <- c(ui$children[[1]]$children, list(helper))
    }

    .hide_this_thing2(x, field, ui)
}

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny HTML span checkboxGroupInput
.checkboxGroupInput.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)
    ui <- checkboxGroupInput(element, label=label, ...)

    if (help) {
        helper <- .specific_help(element)
        ui$children[[1]]$children <- c(ui$children[[1]]$children, list(helper))
    }

    .hide_this_thing2(x, field, ui)
}

.slider_extra <- "_help_anchor"

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny sliderInput
.sliderInput.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)
    if (help) {
        label <- .label_with_help(label, element)
    }

    ui <- sliderInput(element, label=label, ...)

    if (help) {
        ui$attribs$id <- paste0(element, .slider_extra)
    }

    .hide_this_thing2(x, field, ui)
}

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny numericInput
.numericInput.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)
    if (help) {
        label <- .label_with_help(label, element)
    }

    ui <- numericInput(element, label=label, ...)

    .hide_this_thing2(x, field, ui)
}

#' @export
#' @rdname interface-wrappers
#' @importFrom shiny radioButtons
.radioButtons.iSEE <- function(x, field, label, ..., help=TRUE) {
    element <- paste0(.getEncodedName(x), "_", field)
    if (help) {
        label <- .label_with_help(label, element)
    }
    ui <- radioButtons(element, label=label, ...)
    .hide_this_thing2(x, field, ui)
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
