
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

#' @export
setGeneric(".createObservers", function(x, se, input, session, pObjects, rObjects) {
    standardGeneric(".createObservers")
})

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
#' @section Additional rendering obligations:
#' The rendering expression defined in \code{\link{.renderOutput}} is also expected to:
#' \itemize{
#' \item Fill \code{pObjects$contents[[PANEL]]} with some content related to the displayed output.
#' This will be used in other generics like \code{\link{.multiSelectionCommands}} and \code{\link{.singleSelectionValue}} to determine the identity of the selected point(s).
#' \item Fill \code{pObjects$commands[[PANEL]]} with a character vector of commands required to produce the displayed output.
#' This will minimally include the commands required to generate \code{pObjects$contents[[PANEL]]};
#' for plotting panels, the vector should also include code to create the plot.
#' }
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases .renderOutput .defineOutput
#' @name output-generics
NULL

#' @export
setGeneric(".defineOutput", function(x, ...) {
    standardGeneric(".defineOutput")
})

#' @export
setGeneric(".renderOutput", function(x, se, ..., output, pObjects, rObjects) {
    standardGeneric(".renderOutput")
})

#' @export
#' @rdname getEncodedName
setGeneric(".fullName", function(x) standardGeneric(".fullName"))

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

#' Generics for setting up parameters
#'
#' These generics are related to the iniial setup of the \pkg{iSEE} application.
#' 
#' @section Caching common information:
#' In \code{.cacheCommonInfo(x, se)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset.
#' }
#' 
#' It is expected to return \code{se} with (optionally) extra fields added to \code{metadata(se)$iSEE}.
#' Each field should be named according to the class name and contain some common information that is constant for all instances of the class of \code{x}.
#' The goal is to avoid repeated recomputation of required values when creating user interface elements or observers.
#' 
#' Methods for this generic should start by checking whether the metadata already contains the class name, and returning \code{se} without modification if this is the case.
#' Otherwise, it should \code{\link{callNextMethod}} to fill in the cache values from the parent classes, before adding cached values under the class name for \code{x}.
#'
#' Remember, the cache is strictly for use in defining interface elements and in observers.
#' Developers should not expect to be able to retrieve cached values when rendering the output for a panel,
#' as the code tracker does not capture the code used to construct the cache.
#' 
#' @section Refining parameters:
#' In \code{.refineParameters(x, se)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset.
#' }
#' 
#' Methods for this generic should return a copy of \code{x} 
#' where slots with invalid values are replaced with appropriate entries from \code{se}.
#' This is necessary because the constructor and validity methods for \code{x} does not know about \code{se};
#' thus, certain slots (e.g., for the row/column names) cannot be set to a reasonable default or checked at that point.
#'
#' We generally recommend specializing \code{\link{initialize}} to fill any yet-to-be-determined slots with \code{NA} defaults.
#' \code{\link{.refineParameters}} can then be used to sweep across these slots and replace them with appropriate entries.
#' Of course, any slots that are not \code{se}-dependent should be set at construction and checked by the validity method.
#'
#' It is also possible for this generic to return \code{NULL}, which is used as an indicator that \code{se} does not contain information to meaningfully show any instance of the class of \code{x} in the \pkg{iSEE} app.
#' For example, the method for \linkS4class{RedDimPlot} will return \code{NULL} if \code{se} is not a \linkS4class{SingleCellExperiment} containing some dimensionality reduction results.
#' 
#' @author Aaron Lun
#' @name setup-generics
#' @aliases
#' .refineParameters
#' .cacheCommonInfo
NULL

#' @export
setGeneric(".refineParameters", function(x, se) standardGeneric(".refineParameters"))

#' @export
setGeneric(".cacheCommonInfo", function(x, se) standardGeneric(".cacheCommonInfo"))

#' Generics for controlling multiple selections
#'
#' A panel can create a multiple selection on either the rows or columns 
#' and transmit this selection to another panel to affect its set of displayed points.
#' For example, users can brush on a \linkS4class{DotPlot}s to select a set of points,
#' and then the panel can transmit the identities of those points to another panel for highlighting.
#' This suite of generics controls the behavior of these multiple selections.
#'
#' @section Specifying the nature of the selection:
#' In all of the code chunks shown below, \code{x} is assumed to be an instance of the \linkS4class{Panel} class.
#' \itemize{
#' \item \code{.multiSelectionDimension(x)} should return a string specifying whether the selection contains rows (\code{"row"}), columns (\code{"column"}) or if the Panel in \code{x} does not perform multiple selections at all (\code{"none"}).
#' The output should be constant for all instances of \code{x}.
#' \item \code{.multiSelectionActive(x)} should return some structure containing all the parameters required to identify all points in the active multiple selection of \code{x}.
#' For example, for \linkS4class{DotPlot}s, this would be the contents of the \code{BrushData} slot.
#' If \code{NULL}, \code{x} is assumed to have no active multiple selection.
#' This is primarily used to save multiple selections into the \code{MultiSelectHistory} slot.
#' \item \code{.multiSelectionRestricted(x)} should return a logical scalar indicating whether \code{x}'s output will be restricted to the selection transmitted from another panel.
#' This is used to determine whether child panels of \code{x} need to be re-rendered when \code{x}'s transmitter changes its multiple selection.
#' }
#'
#' @section Performing the selection:
#' \code{.multiSelectionCommands(x, index)} is expected to return a character vector of commands to generate a character vector of row or column names in the desired multiple selection.
#' If \code{index=NA}, the desired selection is the currently active one;
#' otherwise, for an integer \code{index}, it refers to the corresponding saved selection in the \code{MultiSelectHistory}.
#'
#' The commands will be evaluated in an environment containing:
#' \itemize{
#' \item \code{select}, a variable of the same type as returned by \code{\link{.multiSelectionStructure}(x)}.
#' This will contain the active selection if \code{index=NA} and one of the saved selections otherwise.
#' For example, for \linkS4class{DotPlot}s, \code{select} will be either a Shiny brush or a lasso structure.
#' \item \code{contents}, some arbitrary content saved by the rendering expression in \code{\link{.renderOutput}(x)}.
#' This content should have some sensible interaction with the panel's multiple selection mechanism.
#' For example, a data.frame of coordinates is stored by \linkS4class{DotPlot}s to identify the points selected by a brush/lasso.
#' }
#' 
#' The command is expected to produce a character vector named \code{selected} in the evaluation environment.
#' All internal variables should be prefixed with \code{.} to avoid name clashes.
#' 
#' @author Aaron Lun
#' @name multi-select-generics
#' @aliases .multiSelectionDimension
#' .multiSelectionRestricted
#' .multiSelectionActive
#' .multiSelectionCommands
NULL

#' @export
setGeneric(".multiSelectionCommands", function(x, index) standardGeneric(".multiSelectionCommands"))

#' @export
setGeneric(".multiSelectionRestricted", function(x) standardGeneric(".multiSelectionRestricted"))

#' @export
setGeneric(".multiSelectionDimension", function(x) standardGeneric(".multiSelectionDimension"))

#' @export
setGeneric(".multiSelectionActive", function(x) standardGeneric(".multiSelectionActive"))

#' Generics for controlling single selections
#'
#' A panel can create a single selection on either the rows or columns
#' and transmit this selection to another panel for use as an aesthetic parameter.
#' For example, users can click on a \linkS4class{RowTable} to select a gene of interest,
#' and then the panel can transmit the identities of that row to another panel for coloring by that selected gene's expression.
#' This suite of generics controls the behavior of these single selections.
#'
#' @section Specifying the nature of the selection:
#' Given an instance of the \linkS4class{Panel} class \code{x}, \code{.singleSelectionDimension(x)} should return a string 
#' specifying whether the selection contains a row (\code{"row"}), 
#' column (\code{"column"}) or if the Panel in \code{x} does not perform single selections at all (\code{"none"}).
#' The output should be constant for all instances of \code{x}.
#'
#' @section Obtaining the selected element:
#' \code{.singleSelectionValue}(x, contents) should return a string specifying the selected row or column.
#' If no row or column is selected, it should return \code{NULL}.
#'
#' \code{contents} is any arbitrary structure set by the rendering expression in \code{\link{.renderOutput}} for \code{x}.
#' This should contain all of the information necessary to determine the name of the selected row/column.
#' For example, a data.frame of coordinates is stored by \linkS4class{DotPlot}s to identify the point selected by a brush/lasso.
#'
#' @author Aaron Lun
#' @name single-select-generics
#' @aliases .singleSelectionDimension
#' .singleSelectionValue
NULL

#' @export
setGeneric(".singleSelectionDimension", function(x) standardGeneric(".singleSelectionDimension"))

#' @export
setGeneric(".singleSelectionValue", function(x, pObjects) standardGeneric(".singleSelectionValue"))
