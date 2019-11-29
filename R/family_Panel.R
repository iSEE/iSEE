#' @export
setMethod("initialize", "Panel", function(.Object, ...) {
    .Object <- .empty_default(.Object, .selectParamBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .selectByPlot, .noSelection)
    .Object
})

setValidity2("Panel", function(object) {
    msg <- character(0)
    msg <- .valid_logical_error(msg, object, .selectParamBoxOpen)
    msg <- .single_string_error(msg, object, .selectByPlot)

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "Panel", function(x, i, j, ...) {
    slot(x, i)        
})

#' @export
setReplaceMethod("[[", "Panel", function(x, i, j, ..., value) {
    slot(x, i) <- value
    x
})

#' @export
setMethod("[", "Panel", function(x, i, j, ..., drop=FALSE) {
    x[[i]]
})

#' @export
setReplaceMethod("[", "Panel", function(x, i, j, ..., value) {
    x[[i]] <- value
    x
})

#' @export
setMethod(".refineParameters", "Panel", function(x, se) {
    x
})

#' @export
setMethod(".cacheCommonInfo", "Panel", function(x, se) {
    se
})
