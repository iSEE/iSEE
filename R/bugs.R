
#' Prepare Bugs Easter Egg
#'
#' Credits to https://github.com/Auz/Bug
#'
#' @param use Use \code{FALSE} to disable (default),
#' \code{TRUE} to enable 3 bugs and 1 spider,
#' or an integer vector named \code{c('bugs', 'spiders')}
#' to declare the respective number of bugs to spawn.
#'
#' @return A list of HTML content to include in the user interface.
#'
#' @rdname INTERNAL_nearest_decoded_panel
#' @author Kevin Rue-Albrecht
.prepareBugsEasterEgg <- function(use=FALSE) {
    defaults <- c(bugs=3, spiders=1)
    if (isFALSE(use)) {
        return(list())
    } else if (isTRUE(use)) {
        use <- defaults
    } else {
        if (!identical(names(use), c('bugs', 'spiders'))) {
            stop("'use' must be TRUE, FALSE, or an integer vector named c('bugs', 'spiders')")
        }
    }

    singleton(tags$head(
        tags$script(src="iSEE/bug-min.js"),
        tags$script(HTML(sprintf(
            "new BugController({'minBugs':%i, 'maxBugs':%i});",
            use["bugs"], use["bugs"]
        ))),
        tags$script(HTML(sprintf(
            "new SpiderController({'minBugs':%i, 'maxBugs':%i});",
            use["spiders"], use["spiders"]
        )))
    ))
}
