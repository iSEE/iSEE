#' The FeatAssayPlot panel
#'
#' The FeatAssayPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents the expression of a feature of interest, using the \code{\link{assay}} values of the \linkS4class{SummarizedExperiment}.
#' It provides slots and methods for specifying which feature to use and what to plot on the x-axis.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{YAxisFeatName}, a string specifying the name of the feature to plot on the y-axis.
#' If \code{NA}, defaults to the first row name of the SummarizedExperiment object.
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{YAxisRowTable}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{YAxisFeatName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Feature name"} or \code{"Column data"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColData}, string specifying which column of the \code{\link{colData}} should be shown on the x-axis,
#' if \code{XAxis="Column data"}.
#' Defaults to the first valid \code{\link{colData}} field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"} for details).
#' \item \code{XAaxisFeatName}, string specifying the name of the feature to plot on the x-axis,
#' if \code{XAxis="Feature name"}.
#' Defaults to the first row name.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{FeatAssayPlot(...)} creates an instance of a FeatAssayPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Contract description:
#' The FeatAssayPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{FeatAssayPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces any \code{NA} values in \code{XAxisFeatName} and \code{YAxisFeatName} with the first row name; any \code{NA} value in \code{Assay} with the first valid assay name; and any \code{NA} value in \code{XAxisColData} with the first valid column metadata field.
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
#' x <- FeatAssayPlot()
#' x[["XAxis"]]
#' x[["Assay"]] <- "logcounts"
#' x[["XAxisColData"]] <- "stuff"
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
#' @aliases FeatAssayPlot FeatAssayPlot-class
#' initialize,FeatAssayPlot-method
#' .refineParameters,FeatAssayPlot-method
#' .defineDataInterface,FeatAssayPlot-method
#' .createObservers,FeatAssayPlot-method
#' .singleSelectionSlots,FeatAssayPlot-method
#' .fullName,FeatAssayPlot-method
#' .panelColor,FeatAssayPlot-method
#' .generateDotPlotData,FeatAssayPlot-method
#'
#' @name FeatAssayPlot-class
NULL

#' @export
FeatAssayPlot <- function(...) {
    new("FeatAssayPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "FeatAssayPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .featAssayAssay, NA_character_)
    args <- .empty_default(args, .featAssayXAxis, .featAssayXAxisNothingTitle)
    args <- .empty_default(args, .featAssayXAxisColData, NA_character_)
    args <- .empty_default(args, .featAssayXAxisRowTable, .noSelection)
    args <- .empty_default(args, .featAssayXAxisFeatName, NA_character_)
    args <- .empty_default(args, .featAssayYAxisRowTable, .noSelection)
    args <- .empty_default(args, .featAssayYAxisFeatName, NA_character_)
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "FeatAssayPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (nrow(se)==0L) {
        warning(sprintf("no rows available for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    x <- .replace_na_with_first(x, .featAssayAssay, all_assays)

    for (field in c(.featAssayXAxisFeatName, .featAssayYAxisFeatName)) {
        x <- .replace_na_with_first(x, field, rownames(se))
    }

    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    x <- .replace_na_with_first(x, .featAssayXAxisColData, column_covariates)

    x
})

.featAssayXAxisNothingTitle <- "None"
.featAssayXAxisColDataTitle <- "Column data"
.featAssayXAxisFeatNameTitle <- "Feature name"

#' @importFrom S4Vectors setValidity2
setValidity2("FeatAssayPlot", function(object) {
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
setMethod(".defineDataInterface", "FeatAssayPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
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
                choices=tab_by_row, selected=x[[.featAssayXAxisRowTable]]))
    )
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "FeatAssayPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.featAssayAssay, .featAssayXAxisColData),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".singleSelectionSlots", "FeatAssayPlot", function(x) {
    c(callNextMethod(),
        list(
            list(parameter=.featAssayXAxisFeatName, source=.featAssayXAxisRowTable, dimension="row",
                use_mode=.featAssayXAxis, use_value=.featAssayXAxisFeatNameTitle, protected=TRUE),
            list(parameter=.featAssayYAxisFeatName, source=.featAssayYAxisRowTable, dimension="row",
                use_mode=NA, use_value=NA, protected=TRUE)
        )
    )
})

#' @export
setMethod(".fullName", "FeatAssayPlot", function(x) "Feature assay plot")

#' @export
setMethod(".panelColor", "FeatAssayPlot", function(x) "#7BB854")

#' @export
setMethod(".generateDotPlotData", "FeatAssayPlot", function(x, envir) {
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
            "plot.data$X <- assay(se, %s, withDimnames=FALSE)[%s, ];",
            deparse(assay_choice), deparse(gene_selected_x)
        )

    } else { # no x axis variable specified: show single violin
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    }

    data_cmds <- unlist(data_cmds)
    .text_eval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
