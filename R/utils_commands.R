#' Manage commands to be evaluated
#'
#' Functions to manage commands to be evaluated.
#'
#' @param cmd A character vector containing commands to be executed.
#' @param envir An environment in which to execute the commands.
#'
#' @return
#' \code{.textEval} returns the output of \code{eval(parse(text=cmd), envir)},
#' unless \code{cmd} is empty in which case it returns \code{NULL}.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#' @name manage_commands
#'
#' @export
#' @examples
#' myenv <- new.env()
#' myenv$x <- "Hello world!"
#' .textEval("print(x)", myenv)
.textEval <- function(cmd, envir) {
    if (length(cmd)) {
        return(eval(parse(text=cmd), envir))
    } else {
        return(NULL)
    }
}
