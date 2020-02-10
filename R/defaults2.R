# Adapted from knitr
# https://github.com/yihui/knitr/blob/670a530b53d6cc5002797d54034ec8b07b74702c/R/defaults.R
new_defaults = function(value = list()) {
    defaults = value

    get = function(name, default = FALSE, drop = TRUE) {
        if (default) defaults = value  # this is only a local version
        if (missing(name)) defaults else {
            if (drop && length(name) == 1) defaults[[name]] else {
                setNames(defaults[name], name)
            }
        }
    }
    resolve = function(...) {
        dots = list(...)
        if (length(dots) == 0) return()
        if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
            if (length(dots <- dots[[1]]) == 0) return()
        dots
    }
    set = function(...) {
        dots = resolve(...)
        if (!all(names(dots) %in% names(defaults))) return()
        if (length(dots)) defaults <<- merge(dots)
        invisible(NULL)
    }
    merge = function(values) merge_list(defaults, values)
    restore = function(target = value) defaults <<- target

    list(
        get = get, set = set,
        merge = merge, restore = restore
    )
}

#' Default panel options
#'
#' Set global options using \code{opts_panel$set()}, so that all panels will use these default values during initialization.
#'
#' See \code{str(knitr::opts_panel$get())} for a list of default panel options.
#'
#' Note that \code{opts_panel$restore()} can be used to reset the global options to the package default values.
#'
#' @export
#' @examples opts_chunk$get('downsample'); opts_chunk$get('selected.color')
opts_panel <- new_defaults(list(

    point.color = "black",
    point.size = 1,
    point.alpha = 1,

    downsample = FALSE,
    downsample.res = 200,

    selected.color = "red",
    selected.alpha = 0.1,

    contour.color = "blue",

    font.size = 1,
    legend.position = .plotLegendBottomTitle,
    legend.direction = .plotLegendHorizontalTitle
))

# merge elements of y into x with the same names
# credit: knitr
merge_list = function(x, y) {
  x[names(y)] = y
  x
}
