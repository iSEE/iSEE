#' The ColumnDataPlot panel
#'
#' The ColumnDataPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents a variable from the \code{\link{colData}} of a \linkS4class{SummarizedExperiment} object.
#' It provides slots and methods for specifying which variable to use on the y-axis (and, optionally, also the x-axis), as well as a method to create the data.frame in preparation for plotting.
#'
#' @section Slot overview:
#' The following slots control the column data information that is used:
#' \itemize{
#' \item \code{YAxis}, a string specifying the column of the \code{\link{colData}} to show on the y-axis.
#' If \code{NA}, defaults to the first valid field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"}).
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Column data"} or \code{"Column selection"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColumnData}, string specifying the column of the \code{\link{colData}} to show on the x-axis.
#' If \code{NA}, defaults to the first valid field.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{ColumnDataPlot(...)} creates an instance of a ColumnDataPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColumnDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after replacing any \code{NA} value in \code{YAxis} or \code{XAxisColumnData} with the name of the first valid \code{\link{colData}} variable.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no valid column metadata variables are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.allowableXAxisChoices}(x, se)} returns a character vector specifying the acceptable variables in \code{\link{colData}(se)} that can be used as choices for the x-axis. 
#' This consists of all variables with atomic values.
#' \item \code{\link{.allowableYAxisChoices}(x, se)} returns a character vector specifying the acceptable variables in \code{\link{colData}(se)} that can be used as choices for the y-axis. 
#' This consists of all variables with atomic values.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionInvalidated}(x)} returns \code{TRUE} if the x-axis uses multiple column selections,
#' such that the point coordinates may change upon updates to upstream selections in transmitting panels.
#' Otherwise, it dispatches to the \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Column data plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of column metadata variables in \code{envir}.
#' It will return the commands required to do so as well as a list of labels.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing a panel-specific tour.
#' }
#'
#' @section Subclass expectations:
#' Subclasses do not have to provide any methods, as this is a concrete class.
#' 
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{ColumnDotPlot}, for the immediate parent class.
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- ColumnDataPlot()
#' x[["XAxis"]]
#' x[["XAxis"]] <- "Column data"
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' old_cd <- colData(sce)
#' colData(sce) <- NULL
#'
#' # Spits out a NULL and a warning if there is nothing to plot.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' colData(sce) <- old_cd
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases ColumnDataPlot ColumnDataPlot-class
#' initialize,ColumnDataPlot-method
#' .refineParameters,ColumnDataPlot-method
#' .defineDataInterface,ColumnDataPlot-method
#' .createObservers,ColumnDataPlot-method
#' .fullName,ColumnDataPlot-method
#' .panelColor,ColumnDataPlot-method
#' .generateDotPlotData,ColumnDataPlot-method
#' .multiSelectionInvalidated,ColumnDataPlot-method
#' .allowableXAxisChoices,ColumnDataPlot-method
#' .allowableYAxisChoices,ColumnDataPlot-method
#' .definePanelTour,ColumnDataPlot-method
#'
#' @name ColumnDataPlot-class
NULL

#' @export
ColumnDataPlot <- function(...) {
    new("ColumnDataPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnDataPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .colDataXAxis, .colDataXAxisNothingTitle)
    args <- .emptyDefault(args, .colDataXAxisColData, NA_character_)
    args <- .emptyDefault(args, .colDataYAxis, NA_character_)
    do.call(callNextMethod, c(list(.Object), args))
})

.colDataXAxisNothingTitle <- "None"
.colDataXAxisColDataTitle <- "Column data"
.colDataXAxisSelectionsTitle <- "Column selection"

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "ColumnDataPlot", function(x, se) {
    x <- callNextMethod() # Do this first to trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    yaxis <- .allowableYAxisChoices(x, se)
    if (length(yaxis)==0L) {
        warning(sprintf("no valid y-axis 'colData' fields for '%s'", class(x)[1]))
        return(NULL)
    }

    xaxis <- .allowableXAxisChoices(x, se)
    if (length(xaxis)==0L) {
        warning(sprintf("no valid x-axis 'colData' fields for '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, .colDataYAxis, yaxis)
    x <- .replaceMissingWithFirst(x, .colDataXAxisColData, xaxis)

    x
})

#' @importFrom S4Vectors setValidity2
setValidity2("ColumnDataPlot", function(object) {
    msg <- character(0)

    msg <- .allowableChoiceError(msg, object, .colDataXAxis,
        c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle, .colDataXAxisSelectionsTitle))

    msg <- .singleStringError(msg, object,
        c(.colDataXAxisColData, .colDataYAxis))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "ColumnDataPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    .addSpecificTour(class(x)[1], .colDataYAxis, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .colDataYAxis, " + .selectize-control"),
                    intro="Here, we can specify the field of the <code>colData</code> to show on the y-axis."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .colDataXAxis, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .colDataXAxis),
                    intro="Here, we choose what to show on the x-axis. If we were to <strong>click on <em>Column data</em></strong>..." 
                ),
                c(
                    element=paste0("#", plot_name, "_", .colDataXAxisColData, " + .selectize-control"),
                    intro="We can choose the field of the <code>colData</code> to show on the x-axis."
                ),
                c(
                    element=paste0("#", plot_name, "_", .colDataXAxis),
                    intro="The <em>Column selections</em> choice is a bit more exotic.
If this panel is receiving a multiple column selection from another panel,
we can stratify points on the x-axis according to whether they are part of that selection or not.
For example, if we made a brush on another panel containing a scatter plot, we could show two violin plots in this panel;
one containing data for columns corresponding to points inside the brush, and another containing the points outside the brush.
If any saved selections are present, these would show up as additional violins."
                )
            )
        )
    })

    list(
        .selectInput.iSEE(x, .colDataYAxis,
            label="Column of interest (Y-axis):",
            choices=.allowableYAxisChoices(x, se),
            selected=slot(x, .colDataYAxis)),
        .radioButtons.iSEE(x, .colDataXAxis, 
            label="X-axis:", inline=TRUE,
            choices=c(.colDataXAxisNothingTitle, .colDataXAxisColDataTitle, .colDataXAxisSelectionsTitle),
            selected=slot(x, .colDataXAxis)),
        .conditionalOnRadio(.input_FUN(.colDataXAxis),
            .colDataXAxisColDataTitle,
            .selectInput.iSEE(x, .colDataXAxisColData,
                label="Column of interest (X-axis):",
                choices=.allowableXAxisChoices(x, se),
                selected=slot(x, .colDataXAxisColData),
                help=FALSE
            )
        )
    )
})

#' @export
setMethod(".allowableXAxisChoices", "ColumnDataPlot", function(x, se) {
    .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
})

#' @export
setMethod(".allowableYAxisChoices", "ColumnDataPlot", function(x, se) {
    .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
})

#' @export
#' @importFrom shiny updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "ColumnDataPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionInvalidated", "ColumnDataPlot", function(x) {
    slot(x, .colDataXAxis) == .colDataXAxisSelectionsTitle || callNextMethod()
})

#' @export
setMethod(".fullName", "ColumnDataPlot", function(x) "Column data plot")

#' @export
setMethod(".panelColor", "ColumnDataPlot", function(x) "#DB0230")

#' @export
setMethod(".generateDotPlotData", "ColumnDataPlot", function(x, envir) {
    data_cmds <- list()

    y_lab <- slot(x, .colDataYAxis)
    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=colData(se)[, %s], row.names=colnames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    x_choice <- slot(x, .colDataXAxis)
    if (x_choice == .colDataXAxisNothingTitle) {
        x_title <- x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"

    } else if (x_choice == .colDataXAxisSelectionsTitle) {
        x_lab <- "Column selection"
        x_title <- "vs column selection"
        
        if (exists("col_selected", envir=envir, inherits=FALSE)) {
            target <- "col_selected"
        } else {
            target <- "list()"
        }
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- iSEE::multiSelectionToFactor(%s, colnames(se));", 
            target
        )

    } else {
        x_lab <- slot(x, .colDataXAxisColData)
        x_title <- sprintf("vs %s", x_lab)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- colData(se)[, %s];",
            deparse(x_lab)
        )
    }

    plot_title <- sprintf("%s %s", y_lab, x_title)

    data_cmds <- unlist(data_cmds)
    .textEval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})

#' @export
setMethod(".definePanelTour", "ColumnDataPlot", function(x) {
    collated <- character(0)

    collated <- rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Column data plot</font> panel shows variables from the column metadata (i.e., <code>colData</code>) of a <code>SummarizedExperiment</code> object or one of its subclasses. Here, each point corresponds to a column (usually a sample) of the <code>SummarizedExperiment</code>, and the y-axis represents a chosen variable.", .getPanelColor(x))),
        .addTourStep(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .addTourStep(x, .colDataYAxis, "We can manually choose the variable to show on the y-axis.", is_selectize=TRUE),
        .addTourStep(x, .colDataXAxis, sprintf("We can also specify what should be shown on the x-axis.<br/><br/><strong>Action:</strong> click on <i>Column data</i> to stratify values by a column metadata field.", .getPanelColor(x))),
        .addTourStep(x, .colDataXAxisColData, "This exposes a new interface element that can be used that can be used to choose a covariate to show on the x-axis.", is_selectize=TRUE)
    )

    rbind(
        data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE),
        callNextMethod()
    )
})
