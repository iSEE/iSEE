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
