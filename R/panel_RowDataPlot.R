#' The RowDataPlot panel
#'
#' The RowDataPlot is a panel class for creating a \linkS4class{RowDotPlot} where the y-axis represents a variable from the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment} object.
#' It provides slots and methods for specifying which variable to use on the y-axis (and, optionally, also the x-axis), as well as a method to create the data.frame in preparation for plotting.
#'
#' @section Slot overview:
#' The following slots control the variables to be shown:
#' \itemize{
#' \item \code{YAxis}, a string specifying the row of the \code{\link{rowData}} to show on the y-axis.
#' If \code{NA}, defaults to the first valid field (see \code{?"\link{.refineParameters,RowDotPlot-method}"}).
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Row data"} and \code{"Row selection"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisRowData}, string specifying the row of the \code{\link{rowData}} to show on the x-axis.
#' If \code{NA}, defaults to the first valid field.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{RowDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{RowDataPlot(...)} creates an instance of a RowDataPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after replacing any \code{NA} value in \code{YAxis} or \code{XAxisRowData} with the name of the first valid \code{\link{rowData}} variable.
#' This will also call the equivalent \linkS4class{RowDotPlot} method for further refinements to \code{x}.
#' If no valid row metadata variables are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.allowableXAxisChoices}(x, se)} returns a character vector specifying the acceptable variables in \code{\link{rowData}(se)} that can be used as choices for the x-axis. 
#' This consists of all variables with atomic values.
#' \item \code{\link{.allowableYAxisChoices}(x, se)} returns a character vector specifying the acceptable variables in \code{\link{rowData}(se)} that can be used as choices for the y-axis. 
#' This consists of all variables with atomic values.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{RowDotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionInvalidated}(x)} returns \code{TRUE} if the x-axis uses multiple row selections,
#' such that the point coordinates may change upon updates to upstream selections in transmitting panels.
#' Otherwise, it dispatches to the \linkS4class{RowDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Row data plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of row metadata variables in \code{envir}.
#' It will return the commands required to do so as well as a list of labels.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing a panel-specific tour.
#' \item \code{\link{.getSpecificTour}(x)} returns a character vector of all fields that have their own documention.
#' This triggers a tour specific to a particular UI element.
#' }
#'
#' @section Subclass expectations:
#' Subclasses do not have to provide any methods, as this is a concrete class.
#' 
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{RowDotPlot}, for the immediate parent class.
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- RowDataPlot()
#' x[["XAxis"]]
#' x[["XAxis"]] <- "Row data"
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' # Spits out a NULL and a warning if is nothing to plot.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' rowData(sce)$Stuff <- runif(nrow(sce))
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases RowDataPlot RowDataPlot-class
#' initialize,RowDataPlot-method
#' .refineParameters,RowDataPlot-method
#' .defineDataInterface,RowDataPlot-method
#' .createObservers,RowDataPlot-method
#' .fullName,RowDataPlot-method
#' .panelColor,RowDataPlot-method
#' .multiSelectionInvalidated,RowDataPlot-method
#' .generateDotPlotData,RowDataPlot-method
#' .allowableXAxisChoices,RowDataPlot-method
#' .allowableYAxisChoices,RowDataPlot-method
#' .definePanelTour,RowDataPlot-method
#' .getSpecificTour,RowDataPlot-method
#'
#' @name RowDataPlot-class
NULL

#' @export
RowDataPlot <- function(...) {
    new("RowDataPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowDataPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .rowDataXAxis, .rowDataXAxisNothingTitle)
    args <- .emptyDefault(args, .rowDataXAxisRowData, NA_character_)
    args <- .emptyDefault(args, .rowDataYAxis, NA_character_)
    do.call(callNextMethod, c(list(.Object), args))
})

.rowDataXAxisNothingTitle <- "None"
.rowDataXAxisRowDataTitle <- "Row data"
.rowDataXAxisSelectionsTitle <- "Row selection"

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "RowDataPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    yaxis <- .allowableYAxisChoices(x, se)
    if (length(yaxis)==0L) {
        warning(sprintf("no valid y-axis 'rowData' fields for '%s'", class(x)[1]))
        return(NULL)
    }

    xaxis <- .allowableXAxisChoices(x, se)
    if (length(xaxis)==0L) {
        warning(sprintf("no valid x-axis 'rowData' fields for '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, .rowDataYAxis, yaxis)
    x <- .replaceMissingWithFirst(x, .rowDataXAxisRowData, xaxis)

    x
})

#' @importFrom S4Vectors setValidity2
setValidity2("RowDataPlot", function(object) {
    msg <- character(0)

    msg <- .allowableChoiceError(msg, object, .rowDataXAxis,
        c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle, .rowDataXAxisSelectionsTitle))

    msg <- .singleStringError(msg, object, c(.rowDataXAxisRowData, .rowDataYAxis))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "RowDataPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    .addSpecificTour(class(x)[1], .rowDataYAxis, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .rowDataYAxis, " + .selectize-control"),
                    intro="Here, we can specify the field of the <code>rowData</code> to show on the y-axis."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .rowDataXAxis, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .rowDataXAxis),
                    intro="Here, we choose what to show on the x-axis. If we were to <strong>click on <em>Row data</em></strong>..." 
                ),
                c(
                    element=paste0("#", plot_name, "_", .rowDataXAxisRowData, " + .selectize-control"),
                    intro="We can choose the field of the <code>rowData</code> to show on the x-axis."
                ),
                c(
                    element=paste0("#", plot_name, "_", .rowDataXAxis),
                    intro="The <em>Row selections</em> choice is a bit more exotic.
If this panel is receiving a multiple row selection from another panel,
we can stratify points on the x-axis according to whether they are part of that selection or not.
For example, if we made a brush on another panel containing a scatter plot, we could show two violin plots in this panel;
one containing data for rows corresponding to points inside the brush, and another for the points outside the brush.
If any saved selections are present, these would show up as additional violins."
                )
            )
        )
    })

    list(
        .selectInput.iSEE(x, .rowDataYAxis,
            label="Column of interest (Y-axis):",
            choices=.allowableYAxisChoices(x, se), 
            selected=slot(x, .rowDataYAxis)),
        .radioButtons.iSEE(x, .rowDataXAxis, 
            label="X-axis:", inline=TRUE,
            choices=c(.rowDataXAxisNothingTitle, .rowDataXAxisRowDataTitle, .rowDataXAxisSelectionsTitle),
            selected=slot(x, .rowDataXAxis)),
        .conditionalOnRadio(.input_FUN(.rowDataXAxis),
            .rowDataXAxisRowDataTitle,
            .selectInputHidden(x, .rowDataXAxisRowData,
                label="Column of interest (X-axis):",
                choices=.allowableXAxisChoices(x, se), 
                selected=slot(x, .rowDataXAxisRowData)))
    )
})

#' @export
setMethod(".allowableXAxisChoices", "RowDataPlot", function(x, se) {
    .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
})

#' @export
setMethod(".allowableYAxisChoices", "RowDataPlot", function(x, se) {
    .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
})

#' @export
#' @importFrom shiny updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "RowDataPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionInvalidated", "RowDataPlot", function(x) {
    slot(x, .rowDataXAxis) == .rowDataXAxisSelectionsTitle || callNextMethod()
})

#' @export
setMethod(".fullName", "RowDataPlot", function(x) "Row data plot")

#' @export
setMethod(".panelColor", "RowDataPlot", function(x) "#F2B701")

#' @export
setMethod(".generateDotPlotData", "RowDataPlot", function(x, envir) {
    data_cmds <- list()

    y_lab <- slot(x, .rowDataYAxis)

    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=rowData(se)[, %s], row.names=rownames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    x_choice <- slot(x, .rowDataXAxis)
    if (x_choice == .rowDataXAxisNothingTitle) {
        x_title <- x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)))"

    } else if (x_choice == .rowDataXAxisSelectionsTitle) {
        x_lab <- "Row selection"
        x_title <- "vs row selection"

        if (exists("row_selected", envir=envir, inherits=FALSE)) {
            target <- "row_selected"
        } else {
            target <- "list()"
        }
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- iSEE::multiSelectionToFactor(%s, rownames(se));", 
            target
        )

    } else {
        x_lab <- slot(x, .rowDataXAxisRowData)
        x_title <- sprintf("vs %s", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))
    }

    plot_title <- sprintf("%s %s", y_lab, x_title)

    data_cmds <- unlist(data_cmds)
    .textEval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})

#' @export
setMethod(".getSpecificHelp", "RowDataPlot", function(x) {
    c(callNextMethod(), .rowDataYAxis, .rowDataXAxis)
})

#' @export
setMethod(".definePanelTour", "RowDataPlot", function(x) {
    collated <- character(0)

    collated <- rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Row data plot</font> panel shows variables from the row metadata (i.e., <code>rowData</code>) of a <code>SummarizedExperiment</code> object or one of its subclasses. Here, each point corresponds to a row (usually a feature) of the <code>SummarizedExperiment</code> object, and the y-axis represents a chosen variable.", .getPanelColor(x))),
        .addTourStep(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .addTourStep(x, .rowDataYAxis, "We can manually choose the variable to show on the y-axis.", is_selectize=TRUE),
        .addTourStep(x, .rowDataXAxis, "We can also specify what should be shown on the x-axis.<br/><br/><strong>Action:</strong> click on <i>Row data</i> to stratify values by a row metadata field."),
        .addTourStep(x, .rowDataXAxisRowData, "This exposes a new interface element that can be used that can be used to choose a covariate to show on the x-axis.", is_selectize=TRUE)
    )

    rbind(
        data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE),
        callNextMethod()
    )
})
