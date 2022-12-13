#' The SampleAssayPlot panel
#'
#' The SampleAssayPlot is a panel class for creating a \linkS4class{RowDotPlot} where the y-axis represents the expression of a sample of interest, using the \code{\link{assay}} values of the \linkS4class{SummarizedExperiment}.
#' It provides slots and methods for specifying the sample and what to plot on the x-axis, as well as a method to actually create a data.frame containing those pieces of data in preparation for plotting.
#'
#' @section Slot overview:
#' The following slots control the values on the y-axis:
#' \itemize{
#' \item \code{YAxisSampleName}, a string specifying the name of the sample to plot on the y-axis.
#' If \code{NA}, defaults to the first column name of the SummarizedExperiment object.
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to \code{"logcounts"} in \code{\link{getPanelDefault}}, falling back to the name of the first valid assay
#' (see \code{?"\link{.cacheCommonInfo,DotPlot-method}"} for the definition of validity).
#' \item \code{YAxisSampleSource}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{YAxisSampleName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{YAxisSampleDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for the y-axis.
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
#' }
#'
#' The following slots control the values on the x-axis:
#' \itemize{
#' \item \code{XAxis}, string specifying what should be plotted on the x-axis.
#' This can be any one of \code{"None"}, \code{"Sample name"}, \code{"Row data"} or \code{"Row selection"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColumnData}, string specifying which column of the \code{\link{colData}} should be shown on the x-axis,
#' if \code{XAxis="Column data"}.
#' Defaults to the first valid \code{\link{colData}} field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"} for details).
#' \item \code{XAaxisSampleName}, string specifying the name of the sample to plot on the x-axis,
#' if \code{XAxis="Sample name"}.
#' Defaults to the first column name.
#' \item \code{XAxisSampleSource}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{XAxisSampleName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{XAxisSampleDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for the x-axis.
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{SampleAssayPlot(...)} creates an instance of a SampleAssayPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{SampleAssayPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces any \code{NA} values in \code{XAxisSampleName} and \code{YAxisSampleName} with the first column name; any \code{NA} value in \code{Assay} with the first valid assay name; and any \code{NA} value in \code{XAxisColumnData} with the first valid column metadata field.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no columns or assays are present, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Sample assay plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of sample assay values in \code{envir}.
#' It will return the commands required to do so as well as a list of labels.
#' }
#'
#' For managing selections:
#' \itemize{
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, 
#' mostly related to the choice of sample on the x- and y-axes.
#' This includes the output of the method for the parent \linkS4class{RowDotPlot} class.
#' \item \code{\link{.multiSelectionInvalidated}(x)} returns \code{TRUE} if the x-axis uses multiple row selections,
#' such that the point coordinates may change upon updates to upstream selections in transmitting panels.
#' Otherwise, it dispatches to the \linkS4class{RowDotPlot} method.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing a panel-specific tour.
#' }
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
#' x <- SampleAssayPlot()
#' x[["XAxis"]]
#' x[["Assay"]] <- "logcounts"
#' x[["XAxisRowData"]] <- "stuff"
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' old_assay_names <- assayNames(sce)
#' assayNames(sce) <- character(length(old_assay_names))
#'
#' # Spits out a NULL and a warning if no assays are named.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' assayNames(sce) <- old_assay_names
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases SampleAssayPlot SampleAssayPlot-class
#' initialize,SampleAssayPlot-method
#' .refineParameters,SampleAssayPlot-method
#' .defineDataInterface,SampleAssayPlot-method
#' .createObservers,SampleAssayPlot-method
#' .singleSelectionSlots,SampleAssayPlot-method
#' .multiSelectionInvalidated,SampleAssayPlot-method
#' .fullName,SampleAssayPlot-method
#' .panelColor,SampleAssayPlot-method
#' .generateDotPlotData,SampleAssayPlot-method
#' .definePanelTour,SampleAssayPlot-method
#'
#' @name SampleAssayPlot-class
NULL

#' @export
SampleAssayPlot <- function(...) {
    new("SampleAssayPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "SampleAssayPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, iSEEslots$sampAssayAssay, getPanelDefault(iSEEslots$sampAssayAssay))
    args <- .emptyDefault(args, iSEEslots$sampAssayXAxis, .sampAssayXAxisNothingTitle)
    args <- .emptyDefault(args, iSEEslots$sampAssayXAxisRowData, NA_character_)

    args <- .emptyDefault(args, iSEEslots$sampAssayXAxisColTable, iSEEconstants$noSelection)
    args <- .emptyDefault(args, iSEEslots$sampAssayXAxisSampName, NA_character_)
    args <- .emptyDefault(args, iSEEslots$sampAssayXAxisSampDynamic, getPanelDefault("SingleSelectionDynamicSource"))

    args <- .emptyDefault(args, iSEEslots$sampAssayYAxisColTable, iSEEconstants$noSelection)
    args <- .emptyDefault(args, iSEEslots$sampAssayYAxisSampName, NA_character_)
    args <- .emptyDefault(args, iSEEslots$sampAssayYAxisSampDynamic, getPanelDefault("SingleSelectionDynamicSource"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "SampleAssayPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (ncol(se)==0L) {
        warning(sprintf("no columns for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no named 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, iSEEslots$sampAssayAssay, all_assays)

    for (field in c(iSEEslots$sampAssayXAxisSampName, iSEEslots$sampAssayYAxisSampName)) {
        x <- .replaceMissingWithFirst(x, field, colnames(se))
    }

    row_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
    if (length(row_covariates)==0L) {
        if (slot(x, iSEEslots$sampAssayXAxis)==.sampAssayXAxisRowDataTitle) {
            slot(x, iSEEslots$sampAssayXAxis) <- .sampAssayXAxisNothingTitle
        }
    } else {
        x <- .replaceMissingWithFirst(x, iSEEslots$sampAssayXAxisRowData, row_covariates)
    }

    x
})

.sampAssayXAxisNothingTitle <- "None"
.sampAssayXAxisRowDataTitle <- "Row data"
.sampAssayXAxisSampNameTitle <- "Sample name"
.sampAssayXAxisSelectionsTitle <- "Row selection"

#' @importFrom S4Vectors setValidity2
setValidity2("SampleAssayPlot", function(object) {
    msg <- character(0)

    msg <- .allowableChoiceError(msg, object, iSEEslots$sampAssayXAxis,
        c(
            .sampAssayXAxisNothingTitle,
            .sampAssayXAxisRowDataTitle,
            .sampAssayXAxisSampNameTitle,
            .sampAssayXAxisSelectionsTitle))

    msg <- .singleStringError(msg, object,
        c(
            iSEEslots$sampAssayAssay,
            iSEEslots$sampAssayXAxisRowData,
            iSEEslots$sampAssayXAxisColTable,
            iSEEslots$sampAssayXAxisSampName,
            iSEEslots$sampAssayYAxisColTable,
            iSEEslots$sampAssayYAxisSampName))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "SampleAssayPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    row_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
    tab_by_col <- select_info$single$sample

    xaxis_choices <- c(.sampAssayXAxisNothingTitle)
    if (length(row_covariates)) { # As it is possible for this plot to be _feasible_ but for no row data to exist.
        xaxis_choices <- c(xaxis_choices, .sampAssayXAxisRowDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .sampAssayXAxisSampNameTitle, .sampAssayXAxisSelectionsTitle)

    .addSpecificTour(class(x)[1], iSEEslots$sampAssayYAxisSampName, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayYAxisSampName, " + .selectize-control"),
                    intro="Here, we choose the sample to show on the y-axis.
This is based on the column names of the input <code>SummarizedExperiment</code>."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayAssay, " + .selectize-control"),
                    intro="This specifies the assay values to be shown."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayYAxisColTable, " + .selectize-control"),
                    intro="We can configure the plot so that the sample on the y-axis automatically changes based on a sample selection in another panel.
A common use case is to configure this panel so that we receive a selection from a <em>Column Data Table</em>,
such that users browsing the table can immediately examine the assay values for a sample of interest."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayYAxisSampDynamic),
                    intro="And in fact, we don't have to even specify the \"other panel\" ourselves.
If this box is checked, any column-based selection in any other panel of the <strong>iSEE</strong> application will be used to specify the sample on the y-axis in this panel.
This is achieved by dynamically changing the identity of the designated panel from which we receive the selection."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], iSEEslots$sampAssayXAxis, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxis),
                    intro="Here, we can choose what to show on the x-axis."
                ),
                if (length(row_covariates)) {
                    rbind(
                        c(
                            element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxis),
                            intro="If we <strong>select <em>Row data</em></strong>..."
                        ),
                        c(
                            element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxisRowData, " + .selectize-control"),
                            intro="... we can stratify points on the x-axis based on a field of interest in the <code>rowData</code>." 
                        )
                    )
                },
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxis),
                    intro="If we <strong>select <em>Sample name</em></strong>..."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxisSampName, " + .selectize-control"),
                    intro="... we can show the assay values of another sample of interest on the x-axis.
In other words, plotting one sample against another for the same set of assay values."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxisColTable, " + .selectize-control"),
                    intro="Just like the sample on the y-axis, the x-axis sample can automatically change in response to a sample selection made in another panel.
We can either choose the \"other panel\" manually with this dropdown..."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxisSampDynamic),
                    intro="... or we can dynamically change the identity of the other panel. 
If this box is checked, a column-based selection in any other panel of the <strong>iSEE</strong> application will be used to specify the sample on the x-axis in this panel."
                ),
                c(
                    element=paste0("#", plot_name, "_", iSEEslots$sampAssayXAxis),
                    intro="Finally, we can stratify points based on whether they are included in a multiple row selection made in another panel.
For example, if our \"other panel\" is a row-based plot containing a brush, we would see two violin plots in this panel;
one corresponding to the selected points inside the brush, and another corresponding to the unselected points."
                )
            )
        )
    })


    list(
        .selectizeInput.iSEE(
            x, iSEEslots$sampAssayYAxisSampName,
            label="Sample of interest (Y-axis):",
            choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(
            .input_FUN(iSEEslots$sampAssayYAxisColTable), label=NULL, choices=tab_by_col,
            selected=.choose_link(slot(x, iSEEslots$sampAssayYAxisColTable), tab_by_col)),
        checkboxInput(.input_FUN(iSEEslots$sampAssayYAxisSampDynamic),
            label="Use dynamic sample selection for the y-axis",
            value=slot(x, iSEEslots$sampAssayYAxisSampDynamic)),

        selectInput(paste0(.getEncodedName(x), "_", iSEEslots$sampAssayAssay), label=NULL,
            choices=all_assays, selected=slot(x, iSEEslots$sampAssayAssay)),

        .radioButtons.iSEE(
            x, iSEEslots$sampAssayXAxis, label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=slot(x, iSEEslots$sampAssayXAxis)),

        .conditionalOnRadio(
            .input_FUN(iSEEslots$sampAssayXAxis),
            .sampAssayXAxisRowDataTitle,
            selectInput(
                .input_FUN(iSEEslots$sampAssayXAxisRowData),
                label="Row data of interest (X-axis):",
                choices=row_covariates, selected=slot(x, iSEEslots$sampAssayXAxisRowData))),

        .conditionalOnRadio(
            .input_FUN(iSEEslots$sampAssayXAxis),
            .sampAssayXAxisSampNameTitle,
            selectizeInput(
                .input_FUN(iSEEslots$sampAssayXAxisSampName),
                label="Sample of interest (X-axis):",
                choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(iSEEslots$sampAssayXAxisColTable), label=NULL,
                choices=tab_by_col, selected=slot(x, iSEEslots$sampAssayXAxisColTable)),
            checkboxInput(.input_FUN(iSEEslots$sampAssayXAxisSampDynamic),
                label="Use dynamic sample selection for the x-axis",
                value=slot(x, iSEEslots$sampAssayXAxisSampDynamic))
        )
    )
})

#' @export
#' @importFrom shiny updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "SampleAssayPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(iSEEslots$sampAssayAssay, iSEEslots$sampAssayXAxisRowData),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".singleSelectionSlots", "SampleAssayPlot", function(x) {
    c(callNextMethod(),
        list(
            list(
                parameter=iSEEslots$sampAssayXAxisSampName,
                source=iSEEslots$sampAssayXAxisColTable,
                dimension="sample",
                dynamic=iSEEslots$sampAssayXAxisSampDynamic,
                use_mode=iSEEslots$sampAssayXAxis,
                use_value=.sampAssayXAxisSampNameTitle,
                protected=TRUE
            ),
            list(
                parameter=iSEEslots$sampAssayYAxisSampName,
                source=iSEEslots$sampAssayYAxisColTable,
                dimension="sample",
                dynamic=iSEEslots$sampAssayYAxisSampDynamic,
                use_mode=NA,
                use_value=NA,
                protected=TRUE
            )
        )
    )
})

#' @export
setMethod(".multiSelectionInvalidated", "SampleAssayPlot", function(x) {
    slot(x, iSEEslots$sampAssayXAxis) == .sampAssayXAxisSelectionsTitle || callNextMethod()
})

#' @export
setMethod(".fullName", "SampleAssayPlot", function(x) "Sample assay plot")

#' @export
setMethod(".panelColor", "SampleAssayPlot", function(x) "#07A274")

#' @export
setMethod(".generateDotPlotData", "SampleAssayPlot", function(x, envir) {
    data_cmds <- list()

    samp_selected_y <- slot(x, iSEEslots$sampAssayYAxisSampName)
    assay_choice <- slot(x, iSEEslots$sampAssayAssay)

    plot_title <- samp_selected_y
    y_lab <- sprintf("%s (%s)", samp_selected_y, assay_choice)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %s)[,%s], row.names=rownames(se));",
        deparse(assay_choice), deparse(samp_selected_y)
    )

    # Prepare X-axis data.
    x_choice <- slot(x, iSEEslots$sampAssayXAxis)

    if (x_choice == .sampAssayXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)));"

    } else if (x_choice == .sampAssayXAxisRowDataTitle) {
        x_lab <- slot(x, iSEEslots$sampAssayXAxisRowData)
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))

    } else if (x_choice == .sampAssayXAxisSelectionsTitle) {
        x_lab <- "Row selection"
        plot_title <- paste(plot_title, "vs row selection")
        
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
        samp_selected_x <- slot(x, iSEEslots$sampAssayXAxisSampName)
        plot_title <- paste(plot_title, "vs", samp_selected_x)
        x_lab <- sprintf("%s (%s)", samp_selected_x, assay_choice)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %s)[, %s];",
            deparse(assay_choice), deparse(samp_selected_x)
        )
    }

    data_cmds <- unlist(data_cmds)
    .textEval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})

#' @export
setMethod(".definePanelTour", "SampleAssayPlot", function(x) {
    collated <- rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Sample assay plot</font> panel shows assay values for a particular sample (i.e., column) of a <code>SummarizedExperiment</code> object or one of its subclasses. Here, each point corresponds to a row (usually a feature) of the <code>SummarizedExperiment</code> object, and the y-axis represents the assay values.", .getPanelColor(x))),
        .addTourStep(x, iSEEslots$dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options.")
    )

    rbind(
        data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE),
        callNextMethod()
    )
})
