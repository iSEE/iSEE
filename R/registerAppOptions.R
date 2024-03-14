#' Set and get app-level options
#'
#' Set and get global options for the \code{\link{iSEE}} application.
#' These are options that do not correspond to any \linkS4class{Panel} slot and cannot be changed by the user after initialization.
#'
#' @param se The \linkS4class{SummarizedExperiment} object to be supplied to \code{\link{iSEE}}.
#' @param ... Named options to register.
#' Alternatively a single named list containing the options to register.
#' @param name String containing the name of the option to retrieve.
#' @param default Value to return if \code{name} is not present in the available options.
#'
#' @return
#' \code{registerAppOptions} will return \code{se}, modified with the application-level options.
#'
#' \code{getAppOption} will return the value of the specified option, or \code{default} if that option is not available.
#'
#' \code{getAllAppOptions} will return a named list of all registered options.
#' 
#' @details
#' \code{registerAppOptions} provides an alternative mechanism for setting global options, separate from \code{\link{panelDefaults}}.
#' The primary difference is that \code{registerAppOptions} allows tuning of options that do not have a corresponding slot in any \linkS4class{Panel} subclass.
#' This makes it useful for parameters that the user should not or cannot change within the application,
#' as well as for fine-tuning parameters that are too rarely used to have their own interface elements.
#'
#' Known options include:
#' \itemize{
#' \item{\code{panel.color}}{Named character vector of colors.
#' The names of the vector should be set to the name of class to be overridden; if a class is not named here, its default color is used.
#' It is highly recommended to define colors as hex color codes (e.g., \code{"#1e90ff"}), for full compatibility with both HTML elements and R plots.}
#' \item{\code{color.maxlevels}}{Maximum number of levels for a categorical variable used for coloring.
#' Variables with more levels are coerced to numeric to avoid problems with an overly-large legend.
#' Defaults to 24.}
#' \item{\code{factor.maxlevels}}{Maximum number of levels for a categorical variable to be used anywhere in the app.
#' Variables with more levels are coerced to numeric to avoid rendering delays.
#' Defaults to 100.}
#' \item{\code{RowTable.select.details}}{A function that takes a string containing the name of a feature (i.e., the current selection in the \linkS4class{RowTable}) and returns a HTML element with more details.} 
#' \item{\code{ColumnTable.select.details}}{A function that takes a string containing the name of a sample (i.e., the current selection in the \linkS4class{ColumnTable}) and returns a HTML element with more details.}
#' \item{\code{tooltip.signif}}{Number of \emph{significant} digits to display in the tooltip. Defaults to 6.}
#' }
#'
#' The registered options are stored in the SummarizedExperiment to ensure that we can recover the application state with the combination of the SummarizedExperiment and list of Panel settings.
#' By comparison, if we had used a global cache as in \code{\link{panelDefaults}}, we would need to save them separately to ensure that we can recover a particular application state.
#'
#' By default, \code{registerAppOptions} will add or replace individual arguments specified by \code{...}.
#' This means that users can call the function multiple times to accumulate registered options in \code{se}.
#' The exception is if \code{...} contains a single list, in which case the entire set of options is directly replaced by that list.
#' For example, one could supply a single empty list to clear \code{se} of all existing options.
#'
#' @section For developers:
#' Developers of Panel subclasses can add arbitrary options to \code{...} to help control the behavior of their Panel instances.
#' We recommend prefixing any options with the name of the package in the form of \code{<PACKAGE>_<OPTION>},
#' so as to avoid conflicts with other options (in the base classes, or in other downstream packages) that have the same name.
#'
#' For calls to \code{\link{getAppOption}} that occur after the \code{\link{iSEE}} app has started, it is not actually necessary to supply \code{se}.
#' The options in \code{se} are transferred to a global option store when the app starts, allowing us to call \code{\link{getAppOption}} without \code{se} in various Panel methods.
#' This is useful for some generics where \code{se} is not part of the function signature.
#' Developers can mimic this state (e.g., for unit testing) by calling \code{\link{.activateAppOptionRegistry}} on the SummarizedExperiment produced by \code{\link{registerAppOptions}}.
#' Conversely, calling \code{\link{.deactivateAppOptionRegistry}} will reset the global option store.
#'
#' @author Aaron Lun
#' @examples
#' se <- SummarizedExperiment()
#' se <- registerAppOptions(se, factor.maxlevels=10, color.maxlevels=10)
#'
#' getAppOption("factor.maxlevels", se)
#' getAppOption("color.maxlevels", se)
#' getAppOption("random.other.thing", se, default=10)
#'
#' getAllAppOptions(se)
#'
#' # For developers: you don't actually need to pass 'se' to the getters
#' # if they are being called inside Panel methods.
#' .activateAppOptionRegistry(se)
#' getAppOption("factor.maxlevels")
#' getAppOption("color.maxlevels")
#' .deactivateAppOptionRegistry()
#' 
#' # Wiping out all options.
#' se <- registerAppOptions(se, list())
#' getAllAppOptions(se)
#' @aliases
#' .activateAppOptionRegistry
#' .deactivateAppOptionRegistry
#' @export
#' @importFrom S4Vectors metadata metadata<-
registerAppOptions <- function(se, ...) {
    current <- list(...)

    if (.is_options_list(current)) {
        latest <- current[[1]]
    } else {
        existing <- getAllAppOptions(se)
        existing[names(current)] <- current 
        latest <- existing
    }

    metadata(se) <- .set_nested_list(metadata(se), c("iSEE", "options"), latest)

    se
}

#' @export
#' @rdname registerAppOptions
getAppOption <- function(name, se, default=NULL) {
    # For back-compatibility, for the time being.
    back.comp <- iSEEOptions$get(name)
    if (!is.null(back.comp)) {
        return(back.comp)
    }

    available <- getAllAppOptions(se)
    if (name %in% names(available)) {
        available[[name]]
    } else {
        default
    }
}

.empty_named_list <- list()
names(.empty_named_list) <- character(0)

#' @export
#' @rdname registerAppOptions
#' @importFrom S4Vectors metadata
getAllAppOptions <- function(se) {
    if (missing(se)) {
        global.opt.env$options
    } else {
        existing <- metadata(se)[["iSEE"]][["options"]]
        if (is.null(existing)) {
            existing <- .empty_named_list
        }
        existing
    }
}

global.opt.env <- new.env()
global.opt.env$options <- list()

#' @export
#' @importFrom S4Vectors metadata
.activateAppOptionRegistry <- function(se) {
    global.opt.env$options <- getAllAppOptions(se)
    invisible(NULL)
}

#' @export
.deactivateAppOptionRegistry <- function() {
    global.opt.env$options <- .empty_named_list
    invisible(NULL)
}

.get_factor_maxlevels <- function() {
    getAppOption("factor.maxlevels", default=100L)
}

.get_color_maxlevels <- function() {
    getAppOption("color.maxlevels", default=24L)
}
