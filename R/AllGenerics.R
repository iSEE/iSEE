
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

#' Generics for the panel interface
#'
#' An overview of the generics for defining the user interface (UI) for each panel as well as some recommendations on their implementation.
#'
#' @section Defining the parameter interface:
#' In \code{.defineInterface(x, se, select_info)}, the required arguments are:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, a \linkS4class{SummarizedExperiment} object containing the current dataset.
#' This can be assumed to have been produced by running \code{\link{.refineParameters}(x, se)}.
#' \item \code{select_info}, a list of two lists, \code{single} and \code{multiple},
#' each of which contains the character vectors \code{row} and \code{column}.
#' This specifies the panels available for transmitting single/multiple selections on the rows/columns,
#' see \code{?\link{.multiSelectionDimension}} and \code{?\link{.singleSelectionDimension}} for more details.
#' }
#'
#' Methods for this generic are expected to return a list of \code{\link{collapseBox}} elements.
#' Each parameter box can contain arbitrary numbers of additional UI elements, 
#' each of which is expected to modify one slot of \code{x} upon user interaction.
#'
#' The ID of each interface element should follow the form of \code{PANEL_SLOT} where \code{PANEL} is the panel name and \code{SLOT} is the name of the slot modified by the interface element, e.g., \code{"redDimPlot1_RedDimType"}.
#' Each interface element should have an equivalent observer in \code{\link{.createObservers}} unless they are hidden by \code{\link{.hideInterface}} (see below).
#'
#' It is the developer's responsibility to call \code{\link{callNextMethod}} to obtain interface elements for parent classes.
#' Refer to the contract for each \linkS4class{Panel} class to determine what is already provided by each parent.
#'
#' @section Hiding interface elements:
#' In \code{.hideInterface(x, field)}, the required arguments are:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{field}, string containing the name of a slot of \code{x}.
#' }
#'
#' Methods for this generic are expected to return a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden from the user.
#' This is useful for hiding UI elements that cannot be changed or have no effect.
#' 
#' It is the developer's responsibility to call \code{\link{callNextMethod}} to hide the same interface elements as parent classes.
#' This is not strictly required if one wishes to expose previously hidden elements.
#' Refer to the contract for each \linkS4class{Panel} class to determine what is already provided by each parent.
#'
#' @docType methods
#' @aliases .defineInterface .hideInterface
#' @name interface-generics
#' @author Aaron Lun
NULL

#' @export
setGeneric(".defineInterface", function(x, se, select_info) {
    standardGeneric(".defineInterface")
})

#' @export
setGeneric(".hideInterface", function(x, field) standardGeneric(".hideInterface"))

#' Generic for the panel observers
#'
#' An overview of the generic for defining the panel observers, along with recommendations on its implementation.
#' 
#' @section Creating parameter observers:
#' In \code{.createObservers(x, se, input, session, pObjects, rObjects)}, the required arguments are:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, a \linkS4class{SummarizedExperiment} object containing the current dataset.
#' This can be assumed to have been produced by running \code{\link{.refineParameters}(x, se)}.
#' \item \code{input}, the Shiny input object from the server function.
#' \item \code{session}, the Shiny session object from the server function.
#' \item \code{pObjects}, an environment containing global parameters generated in the \code{\link{iSEE}} app.
#' \item \code{rObjects}, a reactive list of values generated in the \code{\link{iSEE}} app.
#' }
#'
#' Methods for this generic are expected to set up all observers required to respond to changes in the interface elements set up by \code{\link{.defineInterface}}.
#' Recall that each interface element has an ID of the form of \code{PANEL_SLOT}, where \code{PANEL} is the panel name and \code{SLOT} is the name of the slot modified by the interface element; so observers should respond to those names in \code{input}.
#' The return value of this generic is not used; only the side-effect of observer set-up is relevant.
#'
#' It is the developer's responsibility to call \code{\link{callNextMethod}} to set up the observers required by the parent class.
#' This is best done by calling \code{\link{callNextMethod}} at the top of the method before defining up additional observers.
#' Each parent class should implement observers for its slots, so it is usually only necessary to implement observers for any newly added slots in a particular class.
#'
#' @section Modifying the memory:
#' Consider an observer for an interface element that modifies a slot of \code{x}.
#' The code within this observer is expected to modify the \dQuote{memory} of the app state in \code{pObjects}, via:
#' \preformatted{
#' new_value <- input[[paste0(PANEL, "_", SLOT)]]
#' pObjects$memory[[PANEL]][[SLOT]] <- new_value
#' }
#'
#' This enables \pkg{iSEE} to keep a record of the current state of the application.
#' In fact, any changes must go through \code{pObjects$memory} before they change the output in \code{\link{.createOutput}};
#' there is no direct interaction between \code{input} and \code{output} in this framework.
#' 
#' @section Triggering re-rendering:
#' To trigger re-rendering of an output, observers should call \code{\link{.refreshPanel}(panel_name, rObjects)}, 
#' where \code{panel_name} is the name of the current panel.
#' This will simply re-render the output with no additional side effects and is most useful for responding to aesthetic parameters.
#'
#' In the specific case of \linkS4class{DotPlot} subclasses, changes to some parameters may invalidate brushes or lassos,
#' e.g., if the variable on the axes are altered.
#' Observers responding to such changes should instead call \code{\link{.refreshPlot}(panel_name, rObjects)},
#' which will destroy all brushes and lassos to avoid misleading conclusions.
#'
#' @aliases .createObservers
#' @author Aaron Lun
#' @name observer-generics
NULL

#' Generics for Panel outputs
#'
#' An overview of the generics for defining the panel outputs, along with recommendations on their implementation.
#'
#' @section Defining the output element:
#' In \code{.defineOutput(x, ...)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a Panel subclass.
#' \item \code{...}, further arguments that are not currently used.
#' }
#'
#' Methods for this generic are expected to return an output element for inclusion into the \pkg{iSEE} interface, such as the output of \code{\link{plotOutput}}.
#' Multiple elements can be provided via a \code{\link{tagList}}.
#' 
#' The IDs of the output elements are expected to be prefixed with \code{PANEL} (the panel name) and an underscore.
#' One of the output elements may simply have the ID set to \code{PANEL} alone;
#' this is usually the case for simple panels with one primary output like a \linkS4class{DotPlot}.
#'
#' @section Defining the rendered output:
#' In \code{.renderOutput(x, se, ..., output, pObjects, rObjects)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, a \linkS4class{SummarizedExperiment} object containing the current dataset.
#' \item \code{...}, further arguments that may be used by specific methods.
#' \item \code{output}, the Shiny output object from the server function.
#' \item \code{pObjects}, an environment containing global parameters generated in the \code{\link{iSEE}} app.
#' \item \code{rObjects}, a reactive list of values generated in the \code{\link{iSEE}} app.
#' }
#' 
#' It is expected to attach a reactive expression to \code{output} to render the output elements created by \code{.defineOutput}.
#' The return value of this generic is not used; only the side-effect of the output set-up is relevant.
#' 
#' Within the rendering expression for each output, developers should call \code{force(rObjects[[PANEL]])} where \code{PANEL} is the panel name.
#' This ensures that the output is rerendered upon changes to the appropriate reactive variable, which is itself modified by \code{\link{.renderPanel}} and related functions in \code{\link{.createObservers}}.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases .renderOutput .defineOutput
#' @name output-generics
NULL

#' @export
setGeneric(".createObservers", function(x, se, input, session, pObjects, rObjects) {
    standardGeneric(".createObservers")
})

#' @export
setGeneric(".defineOutput", function(x, ...) {
    standardGeneric(".defineOutput")
})

#' @export
setGeneric(".renderOutput", function(x, se, ..., output, pObjects, rObjects) {
    standardGeneric(".renderOutput")
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

setGeneric(".getCommandsDataSelect", function(x, envir) standardGeneric(".getCommandsDataSelect"))

setGeneric(".getCommandsPlotColor", function(x, colorby, x_aes="X", y_aes="Y") standardGeneric(".getCommandsPlotColor"))

###########################

#' @export
setGeneric(".getTableFunction", function(x) standardGeneric(".getTableFunction"))

#' @export
setGeneric(".refineParameters", function(x, se) standardGeneric(".refineParameters"))

#' @export
setGeneric(".cacheCommonInfo", function(x, se) standardGeneric(".cacheCommonInfo"))

###########################

#' @export
setGeneric(".multiSelectionCommands", function(x, index) standardGeneric(".multiSelectionCommands"))

#' @export
setGeneric(".multiSelectionRestricted", function(x) standardGeneric(".multiSelectionRestricted"))

#' @export
setGeneric(".multiSelectionDimension", function(x) standardGeneric(".multiSelectionDimension"))

#' @export
setGeneric(".multiSelectionHasActive", function(x) standardGeneric(".multiSelectionHasActive"))

#' @export
setGeneric(".multiSelectionStructure", function(x) standardGeneric(".multiSelectionStructure"))

#' @export
setGeneric(".singleSelectionDimension", function(x) standardGeneric(".singleSelectionDimension"))

#' @export
setGeneric(".singleSelectionValue", function(x, pObjects) standardGeneric(".singleSelectionValue"))
