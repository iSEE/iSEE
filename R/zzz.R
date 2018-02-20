#' @importFrom shiny addResourcePath
.onLoad <- function(...) {
    # Create link to javascript files.
    shiny::addResourcePath("iSEE", system.file("www", package="iSEE"))
}

