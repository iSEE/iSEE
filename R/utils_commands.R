#' Manage commands to be evaluated
#'
#' Functions to manage pending and processed commands to be evaluated.
#'
#' @param cmd A character vector containing commands to be executed.
#' @param envir An environment in which to execute the commands.
#'
#' @return
#' \code{.text_eval} returns the output of \code{eval(parse(text=cmd), envir)}, 
#' unless \code{cmd} is empty in which case it returns \code{NULL}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_manage_commands
.text_eval <- function(cmd, envir) {
    if (length(cmd)) {
        return(eval(parse(text=cmd), envir))
    } else {
        return(NULL)
    }
}
