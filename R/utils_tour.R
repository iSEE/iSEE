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
