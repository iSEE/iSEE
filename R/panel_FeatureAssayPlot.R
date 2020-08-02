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
#' This can be any one of \code{"None"}, \code{"Feature name"} or \code{"Column data"}.
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
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, mostly related to the choice of feature on the x- and y-axes.
#' This includes the output of \code{callNextMethod}.
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
    x <- .replace_na_with_first(x, .featAssayAssay, all_assays)

    for (field in c(.featAssayXAxisFeatName, .featAssayYAxisFeatName)) {
        x <- .replace_na_with_first(x, field, rownames(se))
    }

    column_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
    x <- .replace_na_with_first(x, .featAssayXAxisColData, column_covariates)

    x
})

.featAssayXAxisNothingTitle <- "None"
.featAssayXAxisColDataTitle <- "Column data"
.featAssayXAxisFeatNameTitle <- "Feature name"

#' @importFrom S4Vectors setValidity2
setValidity2("FeatureAssayPlot", function(object) {
    msg <- character(0)

    msg <- .allowable_choice_error(msg, object, .featAssayXAxis,
        c(.featAssayXAxisNothingTitle, .featAssayXAxisColDataTitle, .featAssayXAxisFeatNameTitle))

    msg <- .single_string_error(msg, object,
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
    tab_by_row <- select_info$single$row

    xaxis_choices <- c(.featAssayXAxisNothingTitle)
    if (length(column_covariates)) { # As it is possible for this plot to be _feasible_ but for no column data to exist.
        xaxis_choices <- c(xaxis_choices, .featAssayXAxisColDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .featAssayXAxisFeatNameTitle)

    list(
        selectizeInput(.input_FUN(.featAssayYAxisFeatName),
            label="Y-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(.input_FUN(.featAssayYAxisRowTable), label=NULL, choices=tab_by_row,
            selected=.choose_link(x[[.featAssayYAxisRowTable]], tab_by_row)),
        checkboxInput(.input_FUN(.featAssayYAxisFeatDynamic),
            label="Use dynamic feature selection for the y-axis",
            value=x[[.featAssayYAxisFeatDynamic]]),

        selectInput(paste0(.getEncodedName(x), "_", .featAssayAssay), label=NULL,
            choices=all_assays, selected=x[[.featAssayAssay]]),
        radioButtons(.input_FUN(.featAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=x[[.featAssayXAxis]]),

        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisColDataTitle,
            selectInput(.input_FUN(.featAssayXAxisColData),
                label="X-axis column data:",
                choices=column_covariates, selected=x[[.featAssayXAxisColData]])),

        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisFeatNameTitle,
            selectizeInput(.input_FUN(.featAssayXAxisFeatName),
                label="X-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.featAssayXAxisRowTable), label=NULL,
                choices=tab_by_row, selected=x[[.featAssayXAxisRowTable]]),
            checkboxInput(.input_FUN(.featAssayXAxisFeatDynamic),
                label="Use dynamic feature selection for the x-axis",
                value=x[[.featAssayXAxisFeatDynamic]])
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
setMethod(".fullName", "FeatureAssayPlot", function(x) "Feature assay plot")

#' @export
setMethod(".panelColor", "FeatureAssayPlot", function(x) "#7BB854")

#' @export
setMethod(".generateDotPlotData", "FeatureAssayPlot", function(x, envir) {
    data_cmds <- list()

    ## Setting up the y-axis:
    gene_selected_y <- x[[.featAssayYAxisFeatName]]
    assay_choice <- x[[.featAssayAssay]]
    plot_title <- gene_selected_y
    y_lab <- sprintf("%s (%s)", gene_selected_y, assay_choice)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %s)[%s, ], row.names=colnames(se))",
        deparse(assay_choice), deparse(gene_selected_y)
    )

    ## Checking X axis choice:
    x_choice <- x[[.featAssayXAxis]]

    if (x_choice == .featAssayXAxisColDataTitle) { # colData column selected
        x_lab <- x[[.featAssayXAxisColData]]
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[, %s];", deparse(x_lab))

    } else if (x_choice == .featAssayXAxisFeatNameTitle) { # gene selected
        gene_selected_x <- x[[.featAssayXAxisFeatName]]
        plot_title <- paste(plot_title, "vs", gene_selected_x)
        x_lab <- sprintf("%s (%s)", gene_selected_x, assay_choice)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %s)[%s, ];",
            deparse(assay_choice), deparse(gene_selected_x)
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
        c(paste0("#", .getEncodedName(x)), "The <font color=\"#402ee8\">Feature assay plot</font> panel shows assay values for a particular feature (i.e., row) of a <code>SummarizedExperiment</code> object or one of its subclasses. Here, each point corresponds to a column (usually a sample) of the <code>SummarizedExperiment</code>, and the y-axis represents the assay values."),
        .add_tour_step(x, .dataParamBoxOpen, "The <font color=\"#402ee8\">Data parameters</font> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        .add_tour_step(x, .featAssayYAxisFeatName, "We can manually choose the feature of interest based on the row names of our <code>SummarizedExperiment</code>.",
            element=paste0("#", .getEncodedName(x), "_", .featAssayYAxisFeatName, " + .selectize-control")),
        .add_tour_step(x, .featAssayYAxisRowTable, "Alternatively, we can link the choice of feature to a single selection from another panel such as a <code>RowDataTable</code>.",
            element=paste0("#", .getEncodedName(x), "_", .featAssayYAxisRowTable, " + .selectize-control")),
        .add_tour_step(x, .featAssayYAxisFeatDynamic, "The upstream panel can even be chosen dynamically, where a single selection of a feature from any panel in the current instance can be used to specify the feature to be shown on the y-axis in this pane."),
        .add_tour_step(x, .featAssayXAxis, "A variety of choices are available for the variable to be plotted on the x-axis.<br/><br/><strong>Action:</strong> click on <font color=\"#402ee8\">Column data</font> to stratify values by a column metadata field."),
        .add_tour_step(x, .featAssayXAxisColData, "This exposes a new interface element that can be used that can be used to choose a covariate to show on the x-axis. Similar logic applies for plotting against the assay values of another feature with the <font color=\"#402ee8\">Feature name</font> choice.",
            element=paste0("#", .getEncodedName(x), "_", .featAssayXAxisColData, " + .selectize-control"))
    )

    rbind(
        data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE),
        callNextMethod()
    )
})
