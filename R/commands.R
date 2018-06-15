#' Manage commands to be evaluated
#' 
#' Functions to manage pending and processed commands to be evaluated.
#'
#' @param obj A command store produced by \code{.initialize_cmd_store}.
#' @param cmd A character vector containing commands to be executed at some point in time.
#' @param envir An environment in which to execute the commands.
#' 
#' @return
#' A command store, i.e., a list of two character vectors \code{pending} and \code{processed}.
#'
#' \code{.initialize_cmd_store} will return a command store with two empty vectors.
#'
#' \code{.add_command} will append \code{cmd} to \code{pending}.
#'
#' \code{.evaluate_commands} will evaluate \code{pending} in \code{envir}, and then move them to \code{processed}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_manage_commands
.initialize_cmd_store <- function() {
    return(list(pending=character(0), processed=character(0)))
}

#' @rdname INTERNAL_manage_commands
.add_command <- function(obj, cmd) {
    if (is.list(cmd)) cmd <- unlist(cmd)
    obj$pending <- c(obj$pending, cmd)
    return(obj)
}

#' @rdname INTERNAL_manage_commands
.evaluate_commands <- function(obj, envir) {
    eval(parse(text=obj$pending), envir=envir)
    obj$processed <- c(obj$processed, obj$pending)
    obj$pending <- character(0)
    return(obj)
}

