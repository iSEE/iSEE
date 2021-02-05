#' @importFrom shiny HTML
.label_with_help <- function(text, id) {
    HTML(paste0(text, " <span id='", id, "_specific_help'><sup>?</sup></span>"))
}

#' @importFrom shiny div HTML
.checkbox_with_help <- function(checkui, id) {
    helper <- span(id=paste0(id, "_specific_help"), HTML("<sup>?</sup>"))
    checkui$children[[1]]$children <- c(checkui$children[[1]]$children, list(helper))
    checkui
}

.slider_extra <- "_help_anchor"

.slider_with_help <- function(sliderui, id) {
    sliderui$attribs$id <- paste0(id, .slider_extra)
    sliderui
}

.global_tour_env <- new.env()
.global_tour_env$tours <- list()

.addSpecificTour <- function(cls, field, fun) {
    if (is.null(.global_tour_env$tours[[cls]][[field]])) {
        if (is.null(.global_tour_env$tours[[cls]])) {
            .global_tour_env$tours[[cls]] <- list()
        }
        .global_tour_env$tours[[cls]][[field]] <- fun
    }
    invisible(NULL)
}

.getSpecificTour <- function(cls, field) {
    .global_tour_env$tours[[cls]][[field]] 
}

.clearSpecificTours <- function() {
    .global_tour_env$tours <- list()
    invisible(NULL)
}
