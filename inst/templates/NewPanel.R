setClass("__ENCODED__", contains="__PARENT__")

#' Panel name
#'
#' Panel description
#'
#' @section Constructor:
#' \code{__ENCODED__()} creates an instance of a __ENCODED__ class.
#'
#' @author Author name
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- __ENCODED__()
#'
#' ##################
#' # For developers #
#' ##################
#'
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' # Spits out a NULL and a warning if no reducedDims are available.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' sce <- runPCA(sce)
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases __ENCODED__ __ENCODED__-class
#' .defineParamInterface,__ENCODED__-method
#' .createParamObservers,__ENCODED__-method
#' @name __ENCODED__
NULL

#' @export
__ENCODED__ <- function() {
    new("__ENCODED__")
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "__ENCODED__", function(.Object, ...) {
    # TODO: Package `hexbin` required for `stat_binhex`
    stopifnot(require(hexbin))
    .Object <- callNextMethod(.Object, ...)
    .Object
})

#' @export
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "__ENCODED__", function(x, se) {
    callNextMethod()
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "__ENCODED__", function(x, se) {
    x <- callNextMethod()
    x
})

#' @importFrom S4Vectors setValidity2
setValidity2("__ENCODED__", function(object) {
    msg <- character(0)

    if (length(msg)>0) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".defineParamInterface", "__ENCODED__", function(x, se, active_panels) {
    callNextMethod()
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "__ENCODED__", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()
})

#' @export
setMethod(".get__ENCODED__", "__ENCODED__", function(x) "__ENCODED__")

#' @export
setMethod(".getFullName", "__ENCODED__", function(x) "__DECODED__")

#' @export
setMethod(".getCommandsDataXY", "__ENCODED__", function(x, param_choices) {
    callNextMethod()
})

#' @export
setMethod(".getCommandsPlot", "__ENCODED__", function(x, param_choices, plot_data, plot_type, labs, is_subsetted, is_downsampled) {
    callNextMethod()
})
