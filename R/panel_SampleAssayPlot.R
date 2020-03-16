#' The SampleAssayPlot panel
#'
#' The SampleAssayPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents the expression of a sample of interest, using the \code{\link{assay}} values of the \linkS4class{SummarizedExperiment}.
#' It provides slots and methods for specifying which sample to use and what to plot on the x-axis.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{YAxisSampleName}, a string specifying the name of the sample to plot on the y-axis.
#' If \code{NA}, defaults to the first column name of the SummarizedExperiment object.
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{YAxisSampleSource}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{YAxisSampleName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Sample name"} or \code{"Column data"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColumnData}, string specifying which column of the \code{\link{colData}} should be shown on the x-axis,
#' if \code{XAxis="Column data"}.
#' Defaults to the first valid \code{\link{colData}} field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"} for details).
#' \item \code{XAaxisSampleName}, string specifying the name of the sample to plot on the x-axis,
#' if \code{XAxis="Sample name"}.
#' Defaults to the first column name.
#' \item \code{XAxisSampleSource}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{XAxisSampleName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
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
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, mostly related to the choice of sample on the x- and y-axes.
#' This includes the output of \code{callNextMethod}.
#' }
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
#' .fullName,SampleAssayPlot-method
#' .panelColor,SampleAssayPlot-method
#' .generateDotPlotData,SampleAssayPlot-method
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
    args <- .emptyDefault(args, .sampAssayAssay, NA_character_)
    args <- .emptyDefault(args, .sampAssayXAxis, .sampAssayXAxisNothingTitle)
    args <- .emptyDefault(args, .sampAssayXAxisRowData, NA_character_)

    args <- .emptyDefault(args, .sampAssayXAxisColTable, .noSelection)
    args <- .emptyDefault(args, .sampAssayXAxisSampName, NA_character_)
    args <- .emptyDefault(args, .sampAssayXAxisSampDynamic, iSEEOptions$get("selection.dynamic.single"))

    args <- .emptyDefault(args, .sampAssayYAxisColTable, .noSelection)
    args <- .emptyDefault(args, .sampAssayYAxisSampName, NA_character_)
    args <- .emptyDefault(args, .sampAssayYAxisSampDynamic, iSEEOptions$get("selection.dynamic.single"))

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

    all_assays <- c(intersect(iSEEOptions$get("assay"), all_assays), all_assays)
    x <- .replace_na_with_first(x, .sampAssayAssay, all_assays)

    for (field in c(.sampAssayXAxisSampName, .sampAssayYAxisSampName)) {
        x <- .replace_na_with_first(x, field, colnames(se))
    }

    row_covariates <- .getCachedCommonInfo(se, "RowDotPlot")$valid.rowData.names
    x <- .replace_na_with_first(x, .sampAssayXAxisRowData, row_covariates)

    x
})

.sampAssayXAxisNothingTitle <- "None"
.sampAssayXAxisRowDataTitle <- "Row data"
.sampAssayXAxisSampNameTitle <- "Sample name"

#' @importFrom S4Vectors setValidity2
setValidity2("SampleAssayPlot", function(object) {
    msg <- character(0)

    msg <- .allowable_choice_error(msg, object, .sampAssayXAxis,
        c(.sampAssayXAxisNothingTitle, .sampAssayXAxisRowDataTitle, .sampAssayXAxisSampNameTitle))

    msg <- .single_string_error(msg, object,
        c(.sampAssayAssay, .sampAssayXAxisRowData, .sampAssayXAxisColTable,
        .sampAssayXAxisSampName, .sampAssayYAxisColTable, .sampAssayYAxisSampName))

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
    tab_by_col <- select_info$single$column

    xaxis_choices <- c(.sampAssayXAxisNothingTitle)
    if (length(row_covariates)) { # As it is possible for this plot to be _feasible_ but for no row data to exist.
        xaxis_choices <- c(xaxis_choices, .sampAssayXAxisRowDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .sampAssayXAxisSampNameTitle)

    list(
        selectizeInput(
            .input_FUN(.sampAssayYAxisSampName),
            label="Sample of interest (Y-axis):",
            choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(
            .input_FUN(.sampAssayYAxisColTable), label=NULL, choices=tab_by_col,
            selected=.choose_link(x[[.sampAssayYAxisColTable]], tab_by_col)),
        checkboxInput(.input_FUN(.sampAssayYAxisSampDynamic),
            label="Use dynamic sample selection for the y-axis",
            value=x[[.sampAssayYAxisSampDynamic]]),

        selectInput(paste0(.getEncodedName(x), "_", .sampAssayAssay), label=NULL,
            choices=all_assays, selected=x[[.sampAssayAssay]]),
        radioButtons(
            .input_FUN(.sampAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=x[[.sampAssayXAxis]]),

        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisRowDataTitle,
            selectInput(
                .input_FUN(.sampAssayXAxisRowData),
                label="Row data of interest (X-axis):",
                choices=row_covariates, selected=x[[.sampAssayXAxisRowData]])),

        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisSampNameTitle,
            selectizeInput(
                .input_FUN(.sampAssayXAxisSampName),
                label="Sample of interest (X-axis):",
                choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.sampAssayXAxisColTable), label=NULL,
                choices=tab_by_col, selected=x[[.sampAssayXAxisColTable]]),
            checkboxInput(.input_FUN(.sampAssayXAxisSampDynamic),
                label="Use dynamic sample selection for the x-axis",
                value=x[[.sampAssayXAxisSampDynamic]])
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
        fields=c(.sampAssayAssay, .sampAssayXAxisRowData),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".singleSelectionSlots", "SampleAssayPlot", function(x) {
    c(callNextMethod(),
        list(
            list(
                parameter=.sampAssayXAxisSampName,
                source=.sampAssayXAxisColTable,
                dimension="sample",
                dynamic=.sampAssayXAxisSampDynamic,
                use_mode=.sampAssayXAxis,
                use_value=.sampAssayXAxisSampNameTitle,
                protected=TRUE
            ),
            list(
                parameter=.sampAssayYAxisSampName,
                source=.sampAssayYAxisColTable,
                dimension="sample",
                dynamic=.sampAssayYAxisSampDynamic,
                use_mode=NA,
                use_value=NA,
                protected=TRUE
            )
        )
    )
})

#' @export
setMethod(".fullName", "SampleAssayPlot", function(x) "Sample assay plot")

#' @export
setMethod(".panelColor", "SampleAssayPlot", function(x) "#07A274")

#' @export
setMethod(".generateDotPlotData", "SampleAssayPlot", function(x, envir) {
    data_cmds <- list()

    samp_selected_y <- x[[.sampAssayYAxisSampName]]
    assay_choice <- x[[.sampAssayAssay]]

    plot_title <- samp_selected_y
    y_lab <- sprintf("%s (%s)", samp_selected_y, assay_choice)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %s)[,%s], row.names=rownames(se));",
        deparse(assay_choice), deparse(samp_selected_y)
    )

    # Prepare X-axis data.
    x_choice <- x[[.sampAssayXAxis]]

    if (x_choice == .sampAssayXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)));"

    } else if (x_choice == .sampAssayXAxisRowDataTitle) {
        x_lab <- x[[.sampAssayXAxisRowData]]
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))

    } else {
        samp_selected_x <- x[[.sampAssayXAxisSampName]]
        plot_title <- paste(plot_title, "vs", samp_selected_x)
        x_lab <- sprintf("%s (%s)", samp_selected_x, assay_choice)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %s, withDimnames=FALSE)[, %s];",
            deparse(assay_choice), deparse(samp_selected_x)
        )
    }

    data_cmds <- unlist(data_cmds)
    .textEval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
