
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
#' The ID of each interface element should follow the form of \code{PANEL_SLOT} where \code{PANEL} is the panel name (from \code{\link{.getEncodedName}(x)}) and \code{SLOT} is the name of the slot modified by the interface element,
#' e.g., \code{"ReducedDimensionPlot1_Type"}.
#' Each interface element should have an equivalent observer in \code{\link{.createObservers}} unless they are hidden by \code{\link{.hideInterface}} (see below).
#'
#' It is the developer's responsibility to call \code{\link{callNextMethod}} to obtain interface elements for parent classes.
#' Refer to the contract for each \linkS4class{Panel} class to determine what is already provided by each parent.
#'
#' @section Defining the data parameter interface:
#' In \code{.defineDataInterface(x, se, select_info)}, the required arguments are the same as those for \code{.defineInterface}.
#' Methods for this generic are expected to return a list of UI elements for altering data-related parameters,
#' which are automatically placed inside the \dQuote{Data parameters} collapsible box.
#' Each element's ID should still follow the \code{PANEL_SLOT} pattern described above.
#'
#' This method aims to provide a simpler alternative to specializing \code{.defineInterface} for the most common use case,
#' where new panels wish to add their own interface elements for altering the contents of the panel.
#' In fact, \code{\link{.defineInterface,Panel-method}} will simply call \code{.defineDataInterface} to populate the data parameter box.
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
#' @aliases .defineInterface .defineDataInterface .hideInterface
#' @name interface-generics
#' @author Aaron Lun
NULL

#' @export
setGeneric(".defineInterface", function(x, se, select_info) standardGeneric(".defineInterface"))

#' @export
setGeneric(".defineDataInterface", function(x, se, select_info) standardGeneric(".defineDataInterface"))

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
#' In fact, any changes must go through \code{pObjects$memory} before they change the output in \code{\link{.renderOutput}};
#' there is no direct interaction between \code{input} and \code{output} in this framework.
#'
#' @section Triggering re-rendering:
#' To trigger re-rendering of an output, observers should call \code{\link{.requestUpdate}(PANEL, rObjects)},
#' where \code{PANEL} is the name of the current panel.
#' This will request a re-rendering of the output with no additional side effects and is most useful for responding to aesthetic parameters.
#'
#' In some cases, changes to some parameters may invalidate existing multiple selections,
#' e.g., brushes and lassos are no longer valid if the variable on the axes are altered.
#' Observers responding to such changes should instead call \code{\link{.requestCleanUpdate}},
#' which will destroy all existing selections in order to avoid misleading conclusions.
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
#' The IDs of the output elements are expected to be prefixed with the panel name from \code{\link{.getEncodedName}(x)} and an underscore, e.g., \code{"ReducedDimensionPlot1_someOutput"}.
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
#' The rendering expression defined in \code{\link{.renderOutput}} is also expected to:
#' \enumerate{
#' \item Call \code{force(rObjects[[PANEL]])}, where \code{PANEL} is the output of \code{\link{.getEncodedName}(x)}.
#' This ensures that the output is rerendered upon requesting changes in \code{\link{.requestUpdate}}.
#' \item Fill \code{pObjects$contents[[PANEL]]} with some content related to the displayed output that allows cross-referencing with single/multiple selection structures.
#' This will be used in other generics like \code{\link{.multiSelectionCommands}} and \code{\link{.singleSelectionValue}} to determine the identity of the selected point(s).
#' As a result, it is only strictly necessary if the panel is a potential transmitter, as determined by the return value of \code{\link{.multiSelectionDimension}}.
#' \item Fill \code{pObjects$commands[[PANEL]]} with a character vector of commands required to produce the displayed output.
#' This should minimally include the commands required to generate \code{pObjects$contents[[PANEL]]};
#' for plotting panels, the vector should also include code to create the plot.
#' \item Fill \code{pObjects$varname[[PANEL]]} with a string containing the R expression in \code{pObjects$commands[[PANEL]]} that holds the contents stored in \code{pObjects$contents[[PANEL]]}.
#' This is used for code reporting, and again, is only strictly necessary if the panel is a potential transmitter.
#' }
#'
#' We strongly recommend calling \code{\link{.retrieveOutput}} within the rendering expression,
#' which will automatically perform tasks 1-3 above, rather than calling \code{\link{.generateOutput}} manually.
#' This means that the only extra work required in the implementation of \code{\link{.renderOutput}} is to perform task 4 and
#' to choose an appropriate rendering function.
#'
#' @section Generating content:
#' In \code{.generateOutput(x, se, all_memory, all_contents)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, a \linkS4class{SummarizedExperiment} object containing the current dataset.
#' \item \code{all_memory}, a named list containing \linkS4class{Panel} objects parameterizing the current state of the app.
#' \item \code{all_contents}, a named list containing the contents of each panel.
#' }
#'
#' Methods for this generic should return a list containing:
#' \itemize{
#' \item \code{contents}, some arbitrary content for the panel (usually a data.frame).
#' This is used during app initialization to ensure that \code{pObjects$contents} of transmitter panels is filled before rendering their children.
#' It should be of a structure that allows it to be cross-referenced against the output of \code{\link{.multiSelectionActive}} to determine the selected rows/columns.
#' \item \code{commands}, a list of character vectors of R commands that, when executed, produces the contents of the panel and any displayed output (e.g., a \link{ggplot} object).
#' Developers should write these commands as if the evaluation environment only contains the SummarizedExperiment \code{se} and ExperimentColorMap \code{colormap}.
#' }
#' The output list may contain any number of other fields that will be ignored.
#'
#' We suggest implementing this method using \code{\link{eval}(\link{parse}(text=...))} calls,
#' which enables easy construction and evaluation of the commands and contents at the same time.
#' Developers should consider using the \code{\link{.processMultiSelections}} function for easily processing the multiple selection parameters.
#'
#' @section Exporting content:
#' In \code{.exportOutput(x, se, all_memory, all_contents)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{Panel} class.
#' \item \code{se}, a \linkS4class{SummarizedExperiment} object containing the current dataset.
#' \item \code{all_memory}, a named list containing \linkS4class{Panel} objects parameterizing the current state of the app.
#' \item \code{all_contents}, a named list containing the contents of each panel.
#' }
#'
#' Methods for this generic should generate appropriate files containing the content of \code{x}.
#' (For example, plots may create PDFs while tables may create CSV files.)
#' All files should be created in the working directory at the time of the function call, possibly in further subdirectories.
#' Each file name should be prefixed with the \code{\link{.getEncodedName}}.
#' The method itself should return a character vector containing \emph{relative} paths to all newly created files.
#' 
#' To implement this method, we suggest simply passing all arguments onto \code{\link{.generateOutput}}
#' and then handling the contents appropriately.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases .renderOutput .defineOutput .generateOutput .exportOutput
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
setGeneric(".generateOutput", function(x, se, ..., all_memory, all_contents) {
    standardGeneric(".generateOutput")
})

#' @export
setGeneric(".exportOutput", function(x, se, all_memory, all_contents) {
    standardGeneric(".exportOutput")
})

#' @export
#' @rdname getEncodedName
setGeneric(".fullName", function(x) standardGeneric(".fullName"))

#' @export
#' @rdname getPanelColor
setGeneric(".panelColor", function(x) standardGeneric(".panelColor"))

#' Plotting generics
#'
#' A series of generics for controlling how plotting is performed in \linkS4class{DotPlot} panels.
#' \linkS4class{DotPlot} subclasses can specialize one or more of them to modify the behavior of \code{\link{.generateOutput}}.
#'
#' @section Generating plotting data:
#' In \code{.generateDotPlotData(x, envir)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{DotPlot} subclass.
#' \item \code{envir}, the evaluation environment in which the data.frame is to be constructed.
#' This can be assumed to have \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset;
#' possibly \code{col_selected}, if a multiple column selection is being transmitted to \code{x};
#' and possibly \code{row_selected}, if a multiple row selection is being transmitted to \code{x}.
#' }
#'
#' The method should add a \code{plot.data} variable in \code{envir} containing a data.frame with columns named \code{"X"} and \code{"Y"}, denoting the variables to show on the x- and y-axes respectively.
#' It should return a list with \code{commands}, a character vector of commands that produces \code{plot.data} when evaluated in \code{envir};
#' and \code{labels}, a list of strings containing labels for the x-axis (named \code{"X"}), y-axis (\code{"Y"}) and plot (\code{"title"}).
#'
#' Each row of the \code{plot.data} data.frame should correspond to one row or column in the SummarizedExperiment \code{envir$se} for \linkS4class{RowDotPlot}s and \linkS4class{ColumnDotPlot}s respectively.
#' Note that, even if only a subset of rows/columns in the SummarizedExperiment are to be shown, there must always be one row in the data.frame per row/column of the SummarizedExperiment, and in the same order.
#' All other rows of the data.frame should be filled in with \code{NA}s rather than omitted entirely.
#' This is necessary for correct interactions with later methods that add other variables to \code{plot.data}.
#'
#' Any internal variables that are generated by the commands should be prefixed with \code{.} to avoid potential clashes with reserved variable names in the rest of the application.
#'
#' @section Generating the ggplot object:
#' In \code{.generateDotPlot(x, labels, envir)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{DotPlot} subclass.
#' \item \code{labels}, a list of labels corresponding to the columns of \code{plot.data}.
#' This is typically used to define axis or legend labels in the plot.
#' \item \code{envir}, the evaluation environment in which the \link{ggplot} object is to be constructed.
#' This can be assumed to have \code{plot.data}, a data.frame of plotting data.
#'
#' Note that \code{se}, \code{row_selected} and \code{col_selected} will still be present in \code{envir},
#' but it is simplest to only use information that has already been incorporated into \code{plot.data} where possible.
#' This is because the order and number of rows in \code{plot.data} may have changed since \code{\link{.generateDotPlotData}}.
#' }
#'
#' The method should return a list with \code{plot}, a \link{ggplot} object;
#' and \code{commands}, a character vector of commands to produce that object when evaluated inside \code{envir}.
#' This plot will subsequently be the rendered output in \code{\link{.renderOutput}}.
#'
#' The \code{plot.data} has at least the fields \code{"X"} and \code{"Y"} from \code{\link{.generateDotPlotData}}.
#' Depending on the parameters of \code{x}, it may also have the following columns:
#' \itemize{
#' \item \code{"ColorBy"}, the values of the covariate to ue to color each point.
#' \item \code{"ShapeBy"}, the values of the covariate to use for shaping each point.
#' This is guaranteed to be categorical.
#' \item \code{"SizeBy"}, the values of the covariate to use for sizing each point.
#' This is guaranteed to be continuous.
#' \item \code{"FacetRow"}, the values of the covariate to use to create row facets.
#' This is guaranteed to be categorical.
#' \item \code{"FacetColumn"}, the values of the covariate to use to create column facets.
#' This is guaranteed to be categorical.
#' \item \code{"SelectBy"}, a logical field indicating whether the point was included in a multiple selection
#' (i.e., transmitted from another plot with \code{x} as the receiver).
#' Note that if \code{SelectionEffect="Restrict"}, \code{plot.data} will already have been subsetted
#' to only retain \code{TRUE} values of this field.
#' }
#'
#' \code{envir} may also contain the following variables:
#' \itemize{
#' \item \code{plot.data.all}, present when a multiple selection is transmitted to \code{x}
#' and \code{SelectionEffect="Restrict"}.
#' This is a data.frame that contains all points prior to subsetting and is useful for defining the boundaries of the plot
#' such that they do not change when the transmitted multiple selection changes.
#' \item \code{plot.data.pre}, present when downsampling is turned on.
#' This is a data.frame that contains all points prior to downsampling (but after subsetting, if that was performed)
#' and is again mainly used to fix the boundaries of the plot.
#' }
#'
#' Developers may wish to use the \code{\link{.addMultiSelectionPlotCommands}} utility to draw brushes and lassos of \code{x}.
#' Note that this refers to the brushes and lassos made on \code{x} itself, not those transmitted from another panel to \code{x}.
#'
#' It would be very unwise for methods to alter the x-axis, y-axis or faceting values in \code{plot.data}.
#' This will lead to uninuitive discrepancies between apparent visual selections for a brush/lasso
#' and the actual multiple selection that is evaluated by downstream functions like \code{\link{.processMultiSelections}}.
#'
#' @section Prioritizing points:
#' In \code{.prioritizeDotPlotData(x, envir)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a \linkS4class{DotPlot} subclass.
#' \item \code{envir}, the evaluation environment in which the \link{ggplot} object is to be constructed.
#' This can be assumed to have \code{plot.data}, a data.frame of plotting data.
#'
#' Again, note that \code{se}, \code{row_selected} and \code{col_selected} will still be present in \code{envir},
#' but it is simplest to only use information that has already been incorporated into \code{plot.data} where possible.
#' This is because the order and number of rows in \code{plot.data} may have changed since \code{\link{.generateDotPlotData}}.
#' }
#'
#' Methods for this generic are expected to generate a \code{.priority} variable in \code{envir},
#' an ordered factor of length equal to \code{nrow(plot.data)} indicating the priority of each point.
#' They may also generate a \code{.rescaled} variable,
#' a named numeric vector containing the scaling factor to apply to the downsampling resolution for each level of \code{.priority}.
#'
#' The method itself should return a list containing \code{commands}, 
#' a character vector of R commands required to generate these variables;
#' and \code{rescaled}, a logical scalar indicating whether a \code{.rescaled} variable was produced.
#'
#' Points assigned the highest level in \code{.priority} are regarded as having the highest visual importance.
#' Such points will be shown on top of other points if there are overlaps on the plot,
#' allowing developers to specify that, e.g., DE genes should be shown on top of non-DE genes. 
#' Scaling of the resolution enables developers to peform more aggressive downsampling for unimportant points.
#'
#' Methods for this generic may also return \code{NULL}, in which case no special action is taken.
#'
#' @section Controlling the \dQuote{None} color scale:
#' In some cases, it is desirable to insert a default scale when \code{ColorBy="None"}.
#' This is useful for highlighting points in a manner that is integral to the nature of the plot,
#' e.g., up- or down-regulated genes in a MA plot. 
#' We provide a few generics to help control which points are highlighted and how they are colored.
#'
#' \code{.colorByNoneDotPlotField(x)} expects \code{x}, an instance of a \linkS4class{DotPlot} subclass,
#' and returns a string containing a name of a column in \code{plot.data} to use for coloring in the \code{ggplot} mapping.
#' This assumes that the relevant field was added to \code{plot.data} by a method for \code{\link{.generateDotPlotData}}.
#'
#' \code{.colorByNoneDotPlotScale(x)} expects \code{x}, an instance of a \linkS4class{DotPlot} subclass,
#' and returns a string containing a \pkg{ggplot2} \code{scale_color_*} call, e.g., \code{\link{scale_color_manual}}.
#' This string should end with a \code{"+"} operator as additional \pkg{ggplot2} layers will be added by \pkg{iSEE}.
#' 
#' @author Kevin \dQuote{K-pop} Rue-Albrecht, Aaron \dQuote{A-bomb} Lun
#'
#' @name plot-generics
#' @aliases .generateDotPlotData
#' .generateDotPlot
#' .prioritizeDotPlotData
#' .colorByNoneDotPlotField
#' .colorByNoneDotPlotScale
NULL

#' @export
setGeneric(".generateDotPlotData", function(x, envir) standardGeneric(".generateDotPlotData"))

#' @export
setGeneric(".generateDotPlot", function(x, labels, envir) standardGeneric(".generateDotPlot"))

#' Internal generics for \linkS4class{DotPlot} plotting
#'
#' These functions are implemented as generics so as to enable differences in behavior between
#' \linkS4class{RowDotPlot}s and \linkS4class{ColumnDotPlot}s.
#' They will modify \code{plot.data} in the environment while returning the commands used to perform that modification.
#'
#' In the following code snippets, \code{x} is an instance of a \linkS4class{DotPlot} class,
#' and \code{envir} is an environment in which \code{plot.data} can be expected to be present.
#'
#' @section Adding aesthetic parameters:
#' \code{.addDotPlotDataColor(x, envir)} will add a \code{ColorBy} field to \code{plot.data} in \code{envir},
#' representing the covariate used to color each point.
#'
#' \code{.addDotPlotDataShape(x, envir)} will add a \code{ShapeBy} field to \code{plot.data} in \code{envir},
#' representing the covariate used to determine the shape of each point.
#'
#' \code{.addDotPlotDataSize(x, envir)} will add a \code{SizeBy} field to \code{plot.data} in \code{envir},
#' representing the covariate used to determine the size of each point.
#'
#' \code{.addDotPlotDataFacets(x, envir)} will add \code{FacetRow} and/or \code{FacetColumn} fields to \code{plot.data},
#' representing the covariate used for faceting by row and/or column respectively.
#'
#' All methods should return a list containing:
#' \itemize{
#' \item \code{commands}, a character vector of R commands used to modify \code{plot.data}.
#' \item \code{labels}, a named list of strings where each name corresponds to the newly added column of \code{plot.data}
#' and each string corresponds to a label that might be used in the legend for that column.
#' }
#'
#' If \code{plot.data} is not modified, methods should return \code{NULL}.
#'
#' @section Adding multiple selection information:
#' \code{.addDotPlotDataSelected(x, envir)} will add a \code{SelectBy} field to \code{plot.data},
#' representing the identity of points in a multiple selection transmitted to \code{x}.
#' \code{plot.data} may also be subsetted if \code{x} is restricting its points to the transmitted multiple selection.
#'
#' The method will return a character vector of commands used to modify \code{plot.data}.
#' If no modification is performed, it will return \code{NULL}.
#'
#' @section Controlling the color scale:
#' \code{.colorDotPlot(x, colorby, x_aes="X", y_aes="Y")} returns a character vector of \pkg{ggplot2}
#' commands to modify the color scale, given:
#' \itemize{
#' \item \code{colorby}, a vector (usually \code{plot.data$ColorBy}) with which colors are decided for each point.
#' \item \code{x_aes}, a string specifying the column of \code{plot.data} containing the x-axis coordinates.
#' \item \code{y_aes}, a string specifying the column of \code{plot.data} containing the y-axis coordinates.
#' }
#' The last two are used in case of coloring based on the identity of a single point.
#'
#' @section Design note:
#' We ask the modification to occur inside the method,
#' rather than evaluating it ourselves in \code{\link{.add_extra_aesthetic_columns}},
#' so as to enable concurrent construction and evaluation of commands.
#' This enables more flexible construction of commands that can change based on intermediate variables;
#' it also avoids potential double evaluation by making it clear that evaluation is the method's responsibility.
#' The same philosophy applies to \code{\link{.generateDotPlotData}} and \code{\link{.generateDotPlot}}.
#'
#' @aliases
#' .addDotPlotDataColor
#' .addDotPlotDataShape
#' .addDotPlotDataSize
#' .addDotPlotDataFacets
#' .addDotPlotDataSelected
#' .colorDotPlot
#' .addDotPlotDataColor,ColumnDotPlot-method
#' .addDotPlotDataShape,ColumnDotPlot-method
#' .addDotPlotDataSize,ColumnDotPlot-method
#' .addDotPlotDataFacets,ColumnDotPlot-method
#' .addDotPlotDataSelected,ColumnDotPlot-method
#' .colorDotPlot,ColumnDotPlot-method
#' .addDotPlotDataColor,RowDotPlot-method
#' .addDotPlotDataShape,RowDotPlot-method
#' .addDotPlotDataSize,RowDotPlot-method
#' .addDotPlotDataFacets,RowDotPlot-method
#' .addDotPlotDataSelected,RowDotPlot-method
#' .colorDotPlot,RowDotPlot-method
#'
#' @author Kevin Rue-Albrecht
#'
#' @name INTERNAL_addDotPlotData
NULL

setGeneric(".addDotPlotDataColor", function(x, envir) standardGeneric(".addDotPlotDataColor"))

setGeneric(".addDotPlotDataShape", function(x, envir) standardGeneric(".addDotPlotDataShape"))

setGeneric(".addDotPlotDataSize", function(x, envir) standardGeneric(".addDotPlotDataSize"))

setGeneric(".addDotPlotDataFacets", function(x, envir) standardGeneric(".addDotPlotDataFacets"))

setGeneric(".addDotPlotDataSelected", function(x, envir) standardGeneric(".addDotPlotDataSelected"))

setGeneric(".colorDotPlot", function(x, colorby, x_aes="X", y_aes="Y") standardGeneric(".colorDotPlot"))

setGeneric(".prioritizeDotPlotData", function(x, envir) standardGeneric(".prioritizeDotPlotData"))

setGeneric(".colorByNoneDotPlotField", function(x) standardGeneric(".colorByNoneDotPlotField"))

setGeneric(".colorByNoneDotPlotScale", function(x) standardGeneric(".colorByNoneDotPlotScale"))

###########################

#' Generics for table construction
#'
#' Generic to control the creation of a data.frame to show in the \code{\link{datatable}} widget of a \linkS4class{Table} panel.
#' \linkS4class{Table} subclasses can specialize methods to modify the behavior of \code{\link{.generateOutput}}.
#'
#' @section Constructing the table:
#' In \code{.generateTable(x, envir)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a Table subclass.
#' \item \code{envir}, the evaluation environment in which the data.frame is to be constructed.
#' This can be assumed to have \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset;
#' possibly \code{col_selected}, if a multiple column selection is being transmitted to \code{x};
#' and possibly \code{row_selected}, if a multiple row selection is being transmitted to \code{x}.
#' }
#'
#' In return, the method should add a \code{tab} variable in \code{envir} containing the relevant data.frame.
#' This will automatically be passed to the \code{\link{datatable}} widget as well as being stored in \code{pObjects$contents}.
#' The return value should be a character vector of commands that produces \code{tab} when evaluated in \code{envir}.
#'
#' Each row of the \code{tab} data.frame should correspond to one row or column in the SummarizedExperiment \code{envir$se} for \linkS4class{RowTable}s and \linkS4class{ColumnTable}s respectively.
#' Unlike \code{\link{.generateDotPlotData}}, it is not necessary for all rows or columns to be represented in this data.frame.
#'
#' Ideally, the number and names of the columns of the data.frame should be fixed for all calls to \code{.generateTable}.
#' Violating this principle may result in unpredictable interactions with existing values in the \code{SearchColumns} slot.
#' Nonetheless, the app will be robust to mismatches, see \code{\link{filterDT}} for more details.
#'
#' Any internal variables that are generated by the commands should be prefixed with \code{.} to avoid potential clashes with reserved variable names in the rest of the application.
#'
#' @author Aaron Lun
#'
#' @aliases .generateTable
#' @name table-generics
NULL

#' @export
setGeneric(".generateTable", function(x, envir) standardGeneric(".generateTable"))

###########################

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
#' Each field should be named according to the class name and contain some common information that is constant for all instances of the class of \code{x} - see \code{\link{.setCachedCommonInfo}} for an appropriate setter utility.
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
#' Methods for this generic should return a copy of \code{x} where slots with invalid values are replaced with appropriate entries from \code{se}.
#' This is necessary because the constructor and validity methods for \code{x} does not know about \code{se};
#' thus, certain slots (e.g., for the row/column names) cannot be set to a reasonable default or checked at that point.
#'
#' We recommend specializing \code{\link{initialize}} to fill any yet-to-be-determined slots with \code{NA} defaults.
#' \code{\link{.refineParameters}} can then be used to sweep across these slots and replace them with appropriate entries,
#' typically by using \code{\link{.getCachedCommonInfo}} to extract previously cached valid values.
#' Of course, any slots that are not \code{se}-dependent should be set at construction and checked by the validity method.
#'
#' It is also possible for this generic to return \code{NULL}, which is used as an indicator that \code{se} does not contain information to meaningfully show any instance of the class of \code{x} in the \pkg{iSEE} app.
#' For example, the method for \linkS4class{ReducedDimensionPlot} will return \code{NULL} if \code{se} is not a \linkS4class{SingleCellExperiment} containing some dimensionality reduction results.
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

#' @title Generics for controlling multiple selections
#'
#' @description
#' A panel can create a multiple selection on either the rows or columns
#' and transmit this selection to another panel to affect its set of displayed points.
#' For example, users can brush on a \linkS4class{DotPlot}s to select a set of points,
#' and then the panel can transmit the identities of those points to another panel for highlighting.
#'
#' This suite of generics controls the behavior of these multiple selections.
#' In all of the code chunks shown below, \code{x} is assumed to be an instance of the \linkS4class{Panel} class.
#'
#' @section Specifying the dimension:
#' \code{.multiSelectionDimension(x)} should return a string specifying whether the selection contains rows (\code{"row"}), columns (\code{"column"}) or if the Panel in \code{x} does not perform multiple selections at all (\code{"none"}).
#' The output should be constant for all instances of \code{x} and is used to govern the interface choices for the selection parameters.
#'
#' @section Specifying the active selection:
#' \code{.multiSelectionActive(x)} should return some structure containing all parameters required to identify all points in the active multiple selection of \code{x}.
#' If \code{.multiSelectionActive(x)} returns \code{NULL}, \code{x} is assumed to have no active multiple selection.
#'
#' The active selection is considered to be the one that can be directly changed by the user, as compared to saved selections that are not modifiable (other than being deleted on a first-in-last-out basis).
#' This generic is primarily used to bundle up selection parameters to be stored in the \code{SelectionHistory} slot when the user saves the current active selection.
#'
#' As an example, in \linkS4class{DotPlot}s, the method for this generic would return the contents of the \code{BrushData} slot.
#'
#' @section Evaluating the selection:
#' \code{.multiSelectionCommands(x, index)} is expected to return a character vector of commands to generate a character vector of row or column names in the desired multiple selection of \code{x}.
#' If \code{index=NA}, the desired selection is the currently active one;
#' developers can assume that \code{.multiSelectionActive(x)} returns a non-\code{NULL} value in this case.
#' Otherwise, for an integer \code{index}, it refers to the corresponding saved selection in the \code{SelectionHistory}.
#'
#' The commands will be evaluated in an environment containing:
#' \itemize{
#' \item \code{select}, a variable of the same type as returned by \code{\link{.multiSelectionActive}(x)}.
#' This will contain the active selection if \code{index=NA} and one of the saved selections otherwise.
#' For example, for \linkS4class{DotPlot}s, \code{select} will be either a Shiny brush or a lasso structure.
#' \item \code{contents}, some arbitrary content saved by the rendering expression in \code{\link{.renderOutput}(x)}.
#' This is most often a data.frame but can be anything as long as \code{.multiSelectionCommands} knows how to process it.
#' For example, a data.frame of coordinates is stored by \linkS4class{DotPlot}s to identify the points selected by a brush/lasso.
#' \item \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset.
#' }
#'
#' The output commands are expected to produce a character vector named \code{selected} in the evaluation environment.
#' All other variables generated by the commands should be prefixed with \code{.} to avoid name clashes.
#'
#' @section Determining the available points for selection:
#' \code{.multiSelectionAvailable(x, contents)} is expected to return an integer scalar specifying the number of points available for selection in the the current instance of the panel \code{x}.
#' This defaults to \code{nrow(contents)} for all \linkS4class{Panel} subclasses,
#' assuming that \code{contents} is a data.frame where each row represents a point.
#' If not, this method needs to be specialized in order to return an accurate total of available points
#' (to be used to compute the percentage selected in the multiple selection information panels).
#'
#' @section Destroying selections:
#' \code{.multiSelectionClear(x)} should return \code{x} after removing the active selection, i.e., so that nothing is selected.
#' This is used internally to remove multiple selections that do not make sense after protected parameters have changed.
#' For example, a brush or lasso made on a PCA plot in \linkS4class{ReducedDimensionPlot}s would not make sense after switching to t-SNE coordinates, so the application will automatically erase those selections to avoid misleading conclusions.
#'
#' @section Responding to selections:
#' These generics pertain to how \code{x} responds to a transmitted selection, not how \code{x} itself transmits selections.
#'
#' \code{.multiSelectionRestricted(x)} should return a logical scalar indicating whether \code{x}'s displayed contents will be restricted to the selection transmitted from \emph{another panel}.
#' This is used to determine whether child panels of \code{x} need to be re-rendered when \code{x}'s transmitter changes its multiple selection.
#' For example, in \linkS4class{DotPlot}s, the method for this generic would return \code{TRUE} if \code{SelectionEffect="Restrict"}.
#' Otherwise, it would be \code{FALSE} as the transmitted selection is only used for aesthetics, not for changing the identity of the displayed points.
#'
#' \code{.multiSelectionInvalidated(x)} should return a logical scalar indicating whether a transmission of a multiple selection to \code{x} invalidates \code{x}'s own existing selections.
#' This should only be \code{TRUE} in special circumstances, e.g., if receipt of a new multiple selection causes recalculation of coordinates in a \linkS4class{DotPlot}.
#'
#' @author Aaron Lun
#' @name multi-select-generics
#' @aliases .multiSelectionDimension
#' .multiSelectionRestricted
#' .multiSelectionActive
#' .multiSelectionCommands
#' .multiSelectionClear
#' .multiSelectionInvalidated
#' .multiSelectionAvailable
NULL

#' @export
setGeneric(".multiSelectionCommands", function(x, index) standardGeneric(".multiSelectionCommands"))

#' @export
setGeneric(".multiSelectionRestricted", function(x) standardGeneric(".multiSelectionRestricted"))

#' @export
setGeneric(".multiSelectionDimension", function(x) standardGeneric(".multiSelectionDimension"))

#' @export
setGeneric(".multiSelectionActive", function(x) standardGeneric(".multiSelectionActive"))

#' @export
setGeneric(".multiSelectionClear", function(x) standardGeneric(".multiSelectionClear"))

#' @export
setGeneric(".multiSelectionInvalidated", function(x) standardGeneric(".multiSelectionInvalidated"))

#' @export
setGeneric(".multiSelectionAvailable", function(x, contents) standardGeneric(".multiSelectionAvailable"))

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
#' \code{.singleSelectionValue(x, contents)} should return a string specifying the selected row or column.
#' If no row or column is selected, it should return \code{NULL}.
#'
#' \code{contents} is any arbitrary structure set by the rendering expression in \code{\link{.renderOutput}} for \code{x}.
#' This should contain all of the information necessary to determine the name of the selected row/column.
#' For example, a data.frame of coordinates is stored by \linkS4class{DotPlot}s to identify the point selected by a brush/lasso.
#'
#' @section Indicating the receiving slots:
#' \code{.singleSelectionSlots(x)} should return a list with one internal list for each slot in \code{x} that might respond to a single selection from a transmitting panel.
#' This internal list should contain at least entries with the following names:
#' \itemize{
#' \item \code{param}, the name of the slot of \code{x} that can potentially respond to a single selection in a transmitting panel,
#' e.g., \code{ColorByFeatureName} in \linkS4class{DotPlot}s.
#' \item \code{source}, the name of the slot of \code{x} that indicates which transmitting panel to respond to,
#' e.g., \code{ColorByFeatureSource} in \linkS4class{DotPlot}s.
#' }
#'
#' The paradigm here is that the interface will contain two \code{\link{selectInput}} elements, one for each of the \code{param} and \code{source} slots.
#' It is possible for users to manually alter the choice in the \code{param}'s \code{selectInput};
#' it is also possible for users to specify a transmitting panel via the \code{source}'s \code{selectInput},
#' which will then trigger automatic updates to the chosen entry in the \code{param}'s \code{selectInput} when the transmitter's single selection changes.
#'
#' Developers are strongly recommended to follow the above paradigm.
#' In fact, the observers to perform these updates are automatically set up by \code{\link{.createObservers,Panel-method}}
#' if the internal list also contains the following named entries:
#' \itemize{
#' \item \code{dimension}, either \code{"row"} or \code{"column"}.
#' This specifies whether the slot specified by \code{param} contains row or column names;
#' if this is not present, no observers will be set up.
#' \item \code{use_mode}, the name of the slot of \code{x} containing the current usage mode,
#' in cases where there are multiple aesthetic choices.
#' An example would be \code{ColorBy} in \linkS4class{DotPlot}s where coloring by feature name is only one of many options,
#' such that the panel should only respond to transmitted single selections when the user intends to color by feature name.
#' If this is \code{NA}, the usage mode is assumed to be such that the panel should always respond to transmissions.
#' \item \code{use_value}, a string containing the relevant value of the slot specified by \code{use_mode} in order for the panel to respond to transmitted single selections.
#' An example would be \code{"Feature name"} in \linkS4class{DotPlot}s.
#' \item \code{protected}, a logical scalar indicating whether the slot specified by \code{param} is \dQuote{protected},
#' i.e., changing this value will cause all existing selections to be invalidated
#' and will trigger re-rendering of the children receiving multiple selections.
#' This is \code{FALSE} for purely aesthetic parameters (e.g., coloring) and \code{TRUE} for data-related parameters
#' (e.g., \code{XAxisFeatureName} in \linkS4class{FeatureAssayPlot}).
#' }
#'
#' @author Aaron Lun
#' @name single-select-generics
#' @aliases .singleSelectionDimension
#' .singleSelectionValue
#' .singleSelectionSlots
NULL

#' @export
setGeneric(".singleSelectionDimension", function(x) standardGeneric(".singleSelectionDimension"))

#' @export
setGeneric(".singleSelectionValue", function(x, pObjects) standardGeneric(".singleSelectionValue"))

#' @export
setGeneric(".singleSelectionSlots", function(x) standardGeneric(".singleSelectionSlots"))

###########################

#' Generics for row/column metadata plots
#'
#' These generics allow subclasses to refine the choices of allowable variables on the x- and y-axes
#' of a \linkS4class{ColumnDataPlot} or \linkS4class{RowDataPlot}.
#' This is most useful for restricting the visualization to a subset of variables, e.g.,
#' only taking log-fold changes in a y-axis of a MA plot.
#'
#' @section Allowable y-axis choices:
#' \code{.allowableYAxisChoices(x, se)} takes \code{x}, a \linkS4class{Panel} instance,
#' and \code{se}, the \linkS4class{SummarizedExperiment} object.
#' It is expected to return a character vector containing the names of acceptable variables to show on the y-axis.
#' For \linkS4class{ColumnDataPlot}s, these should be a subset of the variables in \code{\link{colData}(se)},
#' while for \linkS4class{RowDataPlot}s, these should be a subset of the variables in \code{\link{rowData}(se)}.
#'
#' In practice, it is a good idea to make use of information precomputed by \code{\link{.cacheCommonInfo}}.
#' For example, \code{\link{.cacheCommonInfo,ColumnDotPlot-method}} will add vectors specifying whether a variable in the \code{\link{colData}} is valid and discrete or continuous.
#' This can be intersected with additional requirements in this function.
#'
#' @section Allowable x-axis choices:
#' \code{.allowableXAxisChoices(x, se)} is the same as above but for the variables to show on the x-axis.
#' This need not return the same subset of variables as \code{.allowableYAxisChoices}.
#' 
#' @author Aaron Lun
#'
#' @name metadata-plot-generics
#' @aliases .allowableYAxisChoices
#' .allowableXAxisChoices
NULL

#' @export
setGeneric(".allowableYAxisChoices", function(x, se) standardGeneric(".allowableYAxisChoices"))

#' @export
setGeneric(".allowableXAxisChoices", function(x, se) standardGeneric(".allowableXAxisChoices"))
