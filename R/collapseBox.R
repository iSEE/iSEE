#' A collapsible box
#'
#' A custom collapsible box with Shiny inputs upon collapse, more or less stolen from \pkg{shinyBS}.
#'
#' @param id String specifying the identifier for this object, to use as a field of the Shiny input.
#' @param title String specifying the title of the box for use in the UI.
#' @param ... Additional UI elements to show inside the box.
#' @param open Logical scalar indicating whether this box should be open upon initialization.
#' @param style String specifying the box style, defaults to \code{"default"}.
#'
#' @return A HTML tag object containing a collapsible box.
#'
#' @details
#' Collapsible boxes are used to hold parameters in the \dQuote{parameter boxes} described in \code{\link{.defineInterface}}.
#' It is recommended to format the \code{id} as \code{PANEL_SLOT} where \code{PANEL} is the name of the panel associated with the box and \code{SLOT} is the name of the slot that specifies whether this box should be open or not at initialization.
#' (See \linkS4class{Panel} for some examples with \code{DataBoxOpen}.)
#'
#' Do not confuse these boxes with the \code{shinydashboard::box}es, which are used to hold the plot and table panels.
#' Adding to the nomenclature confusion is the fact that our collapsible boxes are implemented in Javascript using the Bootstrap \dQuote{panel} classes, which in turn has nothing to do with our \linkS4class{Panel} classes.
#'
#' @section Comments on \pkg{shinyBS}:
#' We would have preferred to use \code{bsCollapse} from \pkg{shinyBS}.
#' However, that package does not seem to be under active maintenance, and there are several aspects that make it difficult to use.
#' Specifically, it does not seem to behave well with conditional elements inside the box,
#' and it also does needs a \code{Depends:} relationship with \pkg{shinyBS}.
#'
#' For these reasons, we created our own collapsible box, taking code from \code{shinyBS} where appropriate.
#' The underlying Javascript code for this object is present in \code{inst/www} and is attached to the search path for Shiny resources upon loading \pkg{iSEE}.
#'
#' @author Aaron Lun
#' @seealso
#' \pkg{shinyBS}, from which the Javascript code was derived.
#'
#' \code{\link{.defineInterface}}, which should return a list of these collapsible boxes.
#'
#' @examples
#' library(shiny)
#' collapseBox("SomePanelType1_ParamBoxOpen",
#'     title="Custom parameters",
#'     open=FALSE,
#'     selectInput("SomePanelType1_Thing",
#'         label="What thing?",
#'         choices=LETTERS, selected="A"
#'     )
#' )
#'
#' @export
#' @importFrom shiny tagList tags singleton
collapseBox <- function(id, title, ..., open = FALSE, style = NULL) {
    if(is.null(style)) {
        style <- "default"
    }

    sub.id <- paste0("collapse_", id)
    bsTag <- tags$div(class = paste0("isee-collapse-box panel panel-", style),
        id=id,
        value = title,
        tags$div(class = "panel-heading",
            role = "tab",
            id = paste0("heading_", sub.id),
            tags$h4(class = "panel-title",
                tags$a("data-toggle" = "collapse",
                    href = paste0("#", sub.id),
                    title
                )
            )
        ),
        tags$div(
            id = sub.id,
            class = sprintf("panel-collapse %s", ifelse(open, "collapse in", "collapse")),
            role = "tabpanel",
            tags$div(class = "panel-body", list(...))
        )
    )

    tagList(singleton(tags$head(tags$script(src = "iSEE/collapseBox.js"))), bsTag)
}
