#' The FeatureAssayPlot panel
#'
#' The FeatureAssayPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents the expression of a feature of interest, using the \code{\link{assay}} values of the \linkS4class{SummarizedExperiment}.
#' It provides slots and methods to specify the feature and what to plot on the x-axis, as well as a method to actually create a data.frame containing those pieces of data in preparation for plotting.
#'
#' @section Slot overview:
#' The following slots control the values on the y-axis:
#' \itemize{
#' \item \code{YAxisFeatureName}, a string specifying the name of the feature to plot on the y-axis.
#' If \code{NA}, defaults to the first row name of the SummarizedExperiment object.
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{YAxisFeatureSource}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{YAxisFeatureName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{YAxisFeatureDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for the y-axis.
#' Defaults to \code{FALSE}.
#' }
#'
#' The following slots control the values on the x-axis:
#' \itemize{ 
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Feature name"}, \code{"Column data"} or \code{"Column selection"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColumnData}, string specifying which column of the \code{\link{colData}} should be shown on the x-axis,
#' if \code{XAxis="Column data"}.
#' Defaults to the first valid \code{\link{colData}} field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"} for details).
#' \item \code{XAaxisFeatureName}, string specifying the name of the feature to plot on the x-axis,
#' if \code{XAxis="Feature name"}.
#' Defaults to the first row name.
#' \item \code{XAxisFeatureSource}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{XAxisFeatureName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{XAxisFeatureDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for the x-axis.
#' Defaults to \code{FALSE}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{FeatureAssayPlot(...)} creates an instance of a FeatureAssayPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{FeatureAssayPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces any \code{NA} values in \code{XAxisFeatureName} and \code{YAxisFeatureName} with the first row name; any \code{NA} value in \code{Assay} with the first valid assay name; and any \code{NA} value in \code{XAxisColumnData} with the first valid column metadata field.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no rows or assays are present, \code{NULL} is returned instead.
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
#' \item \code{\link{.fullName}(x)} will return \code{"Feature assay plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of feature expression values in \code{envir}.
#' It will return the commands required to do so as well as a list of labels.
#' }
#'
#' For managing selections:
#' \itemize{
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, 
#' mostly related to the choice of feature on the x- and y-axes.
#' This includes the output of the method for the parent \linkS4class{ColumnDotPlot} class.
#' \item \code{\link{.multiSelectionInvalidated}(x)} returns \code{TRUE} if the x-axis uses multiple column selections,
#' such that the point coordinates may change upon updates to upstream selections in transmitting panels.
#' Otherwise, it dispatches to the \linkS4class{ColumnDotPlot} method.
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
#' x <- FeatureAssayPlot()
#' x[["XAxis"]]
#' x[["Assay"]] <- "logcounts"
#' x[["XAxisColumnData"]] <- "stuff"
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
#' @aliases FeatureAssayPlot FeatureAssayPlot-class
#' initialize,FeatureAssayPlot-method
#' .refineParameters,FeatureAssayPlot-method
#' .defineDataInterface,FeatureAssayPlot-method
#' .createObservers,FeatureAssayPlot-method
#' .singleSelectionSlots,FeatureAssayPlot-method
#' .multiSelectionInvalidated,FeatureAssayPlot-method
#' .fullName,FeatureAssayPlot-method
#' .panelColor,FeatureAssayPlot-method
#' .generateDotPlotData,FeatureAssayPlot-method
#' .definePanelTour,FeatureAssayPlot-method
#'
#' @name FeatureAssayPlot-class
NULL

#' @export
FeatureAssayPlot <- function(...) {
    new("FeatureAssayPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "FeatureAssayPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .featAssayAssay, NA_character_)
    args <- .emptyDefault(args, .featAssayXAxis, .featAssayXAxisNothingTitle)
    args <- .emptyDefault(args, .featAssayXAxisColData, NA_character_)

    args <- .emptyDefault(args, .featAssayXAxisRowTable, .noSelection)
    args <- .emptyDefault(args, .featAssayXAxisFeatName, NA_character_)
    args <- .emptyDefault(args, .featAssayXAxisFeatDynamic, iSEEOptions$get("selection.dynamic.single"))

    args <- .emptyDefault(args, .featAssayYAxisRowTable, .noSelection)
    args <- .emptyDefault(args, .featAssayYAxisFeatName, NA_character_)
    args <- .emptyDefault(args, .featAssayYAxisFeatDynamic, iSEEOptions$get("selection.dynamic.single"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "FeatureAssayPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (nrow(se)==0L) {
        warning(sprintf("no rows available for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- c(intersect(iSEEOptions$get("assay"), all_assays), all_assays)
    x <- .replaceMissingWithFirst(x, .featAssayAssay, all_assays)

    for (field in c(.featAssayXAxisFeatName, .featAssayYAxisFeatName)) {
        x <- .replaceMissingWithFirst(x, field, rownames(se))
    }

    column_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
    if (length(column_covariates)==0L) {
        if (slot(x, .featAssayXAxis) == .featAssayXAxisColDataTitle) {
            slot(x, .featAssayXAxis) <- .featAssayXAxisNothingTitle
        }
    } else {
        x <- .replaceMissingWithFirst(x, .featAssayXAxisColData, column_covariates)
    }

    x
})

.featAssayXAxisNothingTitle <- "None"
.featAssayXAxisColDataTitle <- "Column data"
.featAssayXAxisFeatNameTitle <- "Feature name"
.featAssayXAxisSelectionsTitle <- "Column selection"

#' @importFrom S4Vectors setValidity2
setValidity2("FeatureAssayPlot", function(object) {
    msg <- character(0)

    msg <- .allowableChoiceError(msg, object, .featAssayXAxis,
        c(.featAssayXAxisNothingTitle, .featAssayXAxisColDataTitle, .featAssayXAxisFeatNameTitle, .featAssayXAxisSelectionsTitle))

    msg <- .singleStringError(msg, object,
        c(.featAssayAssay, .featAssayXAxisColData, .featAssayXAxisRowTable,
        .featAssayXAxisFeatName, .featAssayYAxisRowTable, .featAssayYAxisFeatName))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "FeatureAssayPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
    column_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
    tab_by_row <- select_info$single$feature

    xaxis_choices <- c(.featAssayXAxisNothingTitle)
    if (length(column_covariates)) { # As it is possible for this plot to be _feasible_ but for no column data to exist.
        xaxis_choices <- c(xaxis_choices, .featAssayXAxisColDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .featAssayXAxisFeatNameTitle, .featAssayXAxisSelectionsTitle)

    .addSpecificTour(class(x)[1], .featAssayYAxisFeatName, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .featAssayYAxisFeatName, " + .selectize-control"),
                    intro="Here, we choose the feature to show on the y-axis.
This is based on the row names of the input <code>SummarizedExperiment</code>."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayAssay, " + .selectize-control"),
                    intro="This specifies the assay values to be shown."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayYAxisRowTable, " + .selectize-control"),
                    intro="We can configure the plot so that the feature on the y-axis automatically changes based on a feature selection in another panel.
A common use case is to configure this panel so that we receive a selection from a <em>Row Data Table</em>,
such that users browsing the table can immediately examine the assay values for a gene of interest."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayYAxisFeatDynamic),
                    intro="And in fact, we don't have to even specify the \"other panel\" ourselves.
If this box is checked, any row-based selection in any other panel of the <strong>iSEE</strong> application will be used to specify the feature on the y-axis in this panel.
This is achieved by dynamically changing the identity of the designated panel from which we receive the selection."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .featAssayXAxis, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .featAssayXAxis),
                    intro="Here, we can choose what to show on the x-axis."
                ),
                if (length(column_covariates)) {
                    rbind(
                        c(
                            element=paste0("#", plot_name, "_", .featAssayXAxis),
                            intro="If we <strong>select <em>Column data</em></strong>..."
                        ),
                        c(
                            element=paste0("#", plot_name, "_", .featAssayXAxisColData, " + .selectize-control"),
                            intro="... we can stratify points on the x-axis based on a field of interest in the <code>colData</code>." 
                        )
                    )
                },
                c(
                    element=paste0("#", plot_name, "_", .featAssayXAxis),
                    intro="If we <strong>select <em>Feature name</em></strong>..."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayXAxisFeatName, " + .selectize-control"),
                    intro="... we can show the assay values of another feature of interest on the x-axis.
In other words, plotting one feature against another for the same set of assay values."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayXAxisRowTable, " + .selectize-control"),
                    intro="Just like the feature on the y-axis, the x-axis feature can automatically change in response to a feature selection made in another panel.
We can either choose the \"other panel\" manually with this dropdown..."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayXAxisFeatDynamic),
                    intro="... or we can dynamically change the identity of the other panel. 
If this box is checked, any feature selection in any other panel of the <strong>iSEE</strong> application will be used to specify the feature on the x-axis in this panel."
                ),
                c(
                    element=paste0("#", plot_name, "_", .featAssayXAxis),
                    intro="Finally, we can stratify points based on whether they are included in a multiple column selection made in another panel.
For example, if our \"other panel\" is a column-based plot containing a brush, we would see two violin plots in this panel;
one corresponding to the selected points inside the brush, and another corresponding to the unselected points."
                )
            )
        )
    })

    list(
        .selectizeInput.iSEE(x, .featAssayYAxisFeatName,
            label="Y-axis feature:", 
            choices=NULL, 
            selected=NULL, 
            multiple=FALSE),
        selectInput(.input_FUN(.featAssayYAxisRowTable), label=NULL, choices=tab_by_row,
            selected=.choose_link(slot(x, .featAssayYAxisRowTable), tab_by_row)),
        checkboxInput(.input_FUN(.featAssayYAxisFeatDynamic),
            label="Use dynamic feature selection for the y-axis",
            value=slot(x, .featAssayYAxisFeatDynamic)),

        selectInput(paste0(.getEncodedName(x), "_", .featAssayAssay), label=NULL,
            choices=all_assays, selected=slot(x, .featAssayAssay)),

        .radioButtons.iSEE(x, .featAssayXAxis, 
            label="X-axis:", 
            inline=TRUE,
            choices=xaxis_choices, 
            selected=slot(x, .featAssayXAxis)),

        .conditionalOnRadio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisColDataTitle,
            selectInput(.input_FUN(.featAssayXAxisColData),
                label="X-axis column data:",
                choices=column_covariates, selected=slot(x, .featAssayXAxisColData))),

        .conditionalOnRadio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisFeatNameTitle,
            selectizeInput(.input_FUN(.featAssayXAxisFeatName),
                label="X-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.featAssayXAxisRowTable), label=NULL,
                choices=tab_by_row, selected=slot(x, .featAssayXAxisRowTable)),
            checkboxInput(.input_FUN(.featAssayXAxisFeatDynamic),
                label="Use dynamic feature selection for the x-axis",
                value=slot(x, .featAssayXAxisFeatDynamic))
        )
    )
})

#' @export
#' @importFrom shiny updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "FeatureAssayPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.featAssayAssay, .featAssayXAxisColData),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".singleSelectionSlots", "FeatureAssayPlot", function(x) {
    c(callNextMethod(),
        list(
            list(
                parameter=.featAssayXAxisFeatName,
                source=.featAssayXAxisRowTable,
                dimension="feature",
                dynamic=.featAssayXAxisFeatDynamic,
                use_mode=.featAssayXAxis,
                use_value=.featAssayXAxisFeatNameTitle,
                protected=TRUE
            ),
            list(
                parameter=.featAssayYAxisFeatName,
                source=.featAssayYAxisRowTable,
                dimension="feature",
                dynamic=.featAssayYAxisFeatDynamic,
                use_mode=NA,
                use_value=NA,
                protected=TRUE
            )
        )
    )
})

#' @export
setMethod(".multiSelectionInvalidated", "FeatureAssayPlot", function(x) {
    slot(x, .featAssayXAxis) == .featAssayXAxisSelectionsTitle || callNextMethod()
})

#' @export
setMethod(".fullName", "FeatureAssayPlot", function(x) "Feature assay plot")

#' @export
setMethod(".panelColor", "FeatureAssayPlot", function(x) "#7BB854")

#' @export
setMethod(".generateDotPlotData", "FeatureAssayPlot", function(x, envir) {
    data_cmds <- list()

    ## Setting up the y-axis:
    gene_selected_y <- slot(x, .featAssayYAxisFeatName)
    assay_choice <- slot(x, .featAssayAssay)
    plot_title <- gene_selected_y
    y_lab <- sprintf("%s (%s)", gene_selected_y, assay_choice)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %s)[%s, ], row.names=colnames(se))",
        deparse(assay_choice), deparse(gene_selected_y)
    )

    ## Checking X axis choice:
    x_choice <- slot(x, .featAssayXAxis)

    if (x_choice == .featAssayXAxisColDataTitle) { # colData column selected
        x_lab <- slot(x, .featAssayXAxisColData)
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[, %s];", deparse(x_lab))

    } else if (x_choice == .featAssayXAxisFeatNameTitle) { # gene selected
        gene_selected_x <- slot(x, .featAssayXAxisFeatName)
        plot_title <- paste(plot_title, "vs", gene_selected_x)
        x_lab <- sprintf("%s (%s)", gene_selected_x, assay_choice)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %s)[%s, ];",
            deparse(assay_choice), deparse(gene_selected_x)
        )

    } else if (x_choice == .featAssayXAxisSelectionsTitle) {
        x_lab <- "Column selection"
        plot_title <- paste(plot_title, "vs column selection")

        if (exists("col_selected", envir=envir, inherits=FALSE)) {
            target <- "col_selected"
        } else {
            target <- "list()"
        }
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- iSEE::multiSelectionToFactor(%s, colnames(se));", 
            target
        )

    } else { # no x axis variable specified: show single violin
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    }

    data_cmds <- unlist(data_cmds)
    .textEval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})

#' @export
setMethod(".definePanelTour", "FeatureAssayPlot", function(x) {
    collated <- character(0)

    collated <- rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Feature assay plot</font> panel shows assay values for a particular feature (i.e., row) of a <code>SummarizedExperiment</code> object or one of its subclasses. Here, each point corresponds to a column (usually a sample) of the <code>SummarizedExperiment</code> object, and the y-axis represents the assay values.", .getPanelColor(x))),
        .addTourStep(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .addTourStep(x, .featAssayYAxisFeatName, "We can manually choose the feature of interest based on the row names of our <code>SummarizedExperiment</code> object.", is_selectize=TRUE),
        .addTourStep(x, .featAssayYAxisRowTable, sprintf("Alternatively, we can link the choice of feature to a single selection from another panel such as a <font color=\"%s\">Row data table</font>.", .getPanelColor(RowDataTable())), is_selectize=TRUE),
        .addTourStep(x, .featAssayYAxisFeatDynamic, "The upstream panel can even be chosen dynamically, where a single selection of a feature from any panel in the current instance can be used to specify the feature to be shown on the y-axis in this pane."),
        .addTourStep(x, .featAssayXAxis, "A variety of choices are available for the variable to be plotted on the x-axis.<br/><br/><strong>Action:</strong> click on <i>Column data</i> to stratify values by a column metadata field."),
        .addTourStep(x, .featAssayXAxisColData, "This exposes a new interface element that can be used that can be used to choose a covariate to show on the x-axis. Similar logic applies for plotting against the assay values of another feature with the <i>Feature name</i> choice.", is_selectize=TRUE)
    )

    rbind(
        data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE),
        callNextMethod()
    )
})
