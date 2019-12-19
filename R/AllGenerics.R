
setGeneric(
    "assayColorMap",
    function(x, i, ..., discrete=FALSE) standardGeneric("assayColorMap")
)

setGeneric("assayColorMap<-", signature=c("x", "i"),
    function(x, i, ..., value) standardGeneric("assayColorMap<-"))

setGeneric(
    "colDataColorMap",
    function(x, i, ..., discrete=FALSE) standardGeneric("colDataColorMap"))

setGeneric("colDataColorMap<-", signature=c("x", "i"),
    function(x, i, ..., value) standardGeneric("colDataColorMap<-"))

setGeneric(
    "rowDataColorMap",
    function(x, i, ..., discrete=FALSE) standardGeneric("rowDataColorMap"))

setGeneric("rowDataColorMap<-", signature=c("x", "i"),
    function(x, i, ..., value) standardGeneric("rowDataColorMap<-"))

###############################################

#' Panel generics
#'
#' The Panel S4 hierarchy enables arbitrary extensions of the \pkg{iSEE} visualization to new panel types.
#' New panels must follow a few rules in order to interact correctly with the Shiny architecture inside \code{\link{iSEE}}.
#'
#' @section Defining the parameter interface:
#' The \code{.defineParamInterface} generic takes the following arguments:
#' \itemize{
#' \item \code{x}, an instance of a Panel subclass.
#' \item \code{id}, integer scalar specifying the identity of the panel.
#' \item \code{param_choices}, a \linkS4class{DataFrame} with one row containing the parameter choices for the current plot.
#' \item \code{se}, a \linkS4class{SingleCellExperiment} object with precomputed UI information from \code{\link{.precompute_UI_info}}.
#' \item \code{active_panels}, a data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#' }
#' Each method is expected to return a list of \code{\link{collapseBox}} elements,
#' where each parameter box can contain arbitrary numbers of UI elements for interactive setting of various parameters.
#'
#' @section Creating parameter observers:
#' The \code{.createParamObservers} generic takes the following arguments:
#' \itemize{
#' \item \code{x}, an instance of a Panel subclass.
#' \item \code{id}, integer scalar specifying the index of the current panel.
#' \item \code{se}, a \linkS4class{SingleCellExperiment} object with precomputed UI information from \code{\link{.precompute_UI_info}}.
#' \item \code{input}, the Shiny input object from the server function.
#' \item \code{session}, the Shiny session object from the server function.
#' \item \code{pObjects}, an environment containing global parameters generated in the \code{\link{iSEE}} app.
#' \item \code{rObjects}, a reactive list of values generated in the \code{\link{iSEE}} app.
#' }
#' It is expected to set up all observers required to respond to input changes in the UI elements set up by \code{\link{.defineParamInterface}}.
#'
#' @section Defining the output element:
#' The \code{.defineOutputElement} generic takes the following arguments:
#' \itemize{
#' \item \code{x}, an instance of a Panel subclass.
#' \item \code{id}, integer scalar specifying the index of the current panel.
#' \item \code{...}, further arguments that may be used by specific methods.
#' }
#' It is expected to return an output element like \code{\link{plotOutput}}.
#'
#' @section Defining the rendered output:
#' The \code{.defineRenderedOutput} generic takes the following arguments:
#' \itemize{
#' \item \code{x}, an instance of a Panel subclass.
#' \item \code{id}, integer scalar specifying the index of the current panel.
#' \item \code{input}, the Shiny output object from the server function.
#' \item \code{...}, further arguments that may be used by specific methods.
#' }
#' It is expected to attach a reactive expression to \code{output} to render the output element.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases .defineParamInterface .createParamObservers
#' .defineOutputElement .createRenderedOutput
#' Panel-class
#' @name Panel
NULL

#' @export
setGeneric(".defineParamInterface", function(x, se, active_panels) {
    standardGeneric(".defineParamInterface")
})

#' @export
setGeneric(".hideInterfaceElement", function(x, field) standardGeneric(".hideInterfaceElement"))

#' @export
setGeneric(".createParamObservers", function(x, se, input, session, pObjects, rObjects) {
    standardGeneric(".createParamObservers")
})

#' @export
setGeneric(".defineOutputElement", function(x, ...) {
    standardGeneric(".defineOutputElement")
})

#' @export
setGeneric(".createRenderedOutput", function(x, se, ..., output, pObjects, rObjects) {
    standardGeneric(".createRenderedOutput")
})

##########################

#' @export
setGeneric(".getEncodedName", function(x) standardGeneric(".getEncodedName"))

#' @export
setGeneric(".getFullName", function(x) standardGeneric(".getFullName"))

##########################

setGeneric(".getPanelPlottingFunction", function(x) standardGeneric(".getPanelPlottingFunction"))

#' @export
setGeneric(".getCommandsDataXY", function(x) standardGeneric(".getCommandsDataXY"))

#' @export
setGeneric(".getCommandsPlot", function(x, plot_data, plot_type, labs, is_subsetted, is_downsampled) standardGeneric(".getCommandsPlot"))

setGeneric(".getCommandsDataColor", function(x, se) standardGeneric(".getCommandsDataColor"))

setGeneric(".getCommandsDataShape", function(x, se) standardGeneric(".getCommandsDataShape"))

setGeneric(".getCommandsDataSize", function(x, se) standardGeneric(".getCommandsDataSize"))

setGeneric(".getCommandsDataFacets", function(x, se) standardGeneric(".getCommandsDataFacets"))

###########################

#' @export
setGeneric(".getTableFunction", function(x) standardGeneric(".getTableFunction"))

#' @export
setGeneric(".refineParameters", function(x, se) standardGeneric(".refineParameters"))

#' @export
setGeneric(".cacheCommonInfo", function(x, se) standardGeneric(".cacheCommonInfo"))

###########################

#' @export
setGeneric(".restrictsSelection", function(x) standardGeneric(".restrictsSelection"))

#' @export
setGeneric(".transmittedDimension", function(x) standardGeneric(".transmittedDimension"))

#' @export
setGeneric(".processTransmission", function(x, index) standardGeneric(".processTransmission"))

#' @export
setGeneric(".hasActiveSelection", function(x) standardGeneric(".hasActiveSelection"))

#' @param
setGeneric(".selectionToSave", function(x) standardGeneric(".selectionToSave")) 
