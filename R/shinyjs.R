# nocov start
#' Toggle a Shiny actionButton on a condition
#'
#' If \code{condition} is met, the button is disabled and its label is set to \code{inactiveLabel}.
#' Otherwise, the button is enabled and its label is set to \code{activeLabel}.
#'
#' @param id The \code{input} slot that used to access the value.
#' @param condition The condition that disables the action button.
#' @param inactiveLabel Label of the button if inactive.
#' @param activeLabel Label of the button if active.
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#'
#' @return
#' The status of the button in the current \code{session} is changed, and a \code{NULL} is invisibly returned.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_disableButtonIf
#'
#' @importFrom shinyjs disable enable
#' @importFrom shiny updateActionButton
.disableButtonIf <- function(id, condition, inactiveLabel, activeLabel, session) {
    if (condition) {
        disable(id)
        updateActionButton(session, id, inactiveLabel)
    } else {
        enable(id)
        updateActionButton(session, id, activeLabel)
    }
    invisible(NULL)
}
# nocov end
